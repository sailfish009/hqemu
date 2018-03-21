/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include <fstream>
#include "llvm/Support/ManagedStatic.h"
#include "llvm-types.h"
#include "llvm-profile.h"
#include "llvm-annotate.h"
#include "llvm-translator.h"
#include "llvm-state.h"
#include "llvm-opc.h"
#include "llvm.h"
#include "tracer.h"
#include "optimization.h"


#define MAX_TRANSLATORS     8
#define MAX_SEARCH_DEPTH    8
#define ACTIVE_QUEUE_SIZE   (1 << 16)
#define ACTIVE_QUEUE_MASK   (ACTIVE_QUEUE_SIZE - 1)


cl::OptionCategory CategoryHQEMU("HQEMU Options");

static cl::opt<std::string> DebugLevel("debuglv", cl::init(""),
    cl::cat(CategoryHQEMU), cl::desc("Set debug level"));

static cl::opt<std::string> ProfileLevel("profile", cl::init(""),
    cl::cat(CategoryHQEMU), cl::desc("Set profile level"));

static cl::opt<unsigned> NumThreads("threads", cl::init(1),
    cl::cat(CategoryHQEMU), cl::desc("Number of threads used in the hybridm mode"));

static cl::opt<unsigned> NumTranslations("count", cl::init(-1U),
    cl::cat(CategoryHQEMU),
    cl::desc("Maximum number of traces to translate (default = 2^32)"));


/* static members */
bool LLVMEnv::InitOnce = false;
int LLVMEnv::TransMode = TRANS_MODE_NONE;
uint8_t *LLVMEnv::TraceCache = nullptr;
size_t LLVMEnv::TraceCacheSize = 0;

LLVMDebug DM;
ProfileFactory PF;
LLVMEnv *LLEnv;
QueueManager *QM;
AnnotationFactory *AF;

hqemu::Mutex llvm_global_lock;
hqemu::Mutex llvm_debug_lock;

bool ThreadStop = false;
bool ThreadExit = false;
bool TraceCacheFull = false;
unsigned NumPendingThread = 0;

extern ControlFlowGraph GlobalCFG;

/*
 * LLVMEnv()
 *  Intialize LLVM translator(s) and globally shared resources. The LLVMEnv
 *  instance must be initialized before using the underlying transaltion
 *  service and should be initialized only ONCE.
 */
LLVMEnv::LLVMEnv() : NumTranslator(1), UseThreading(false), NumFlush(0)
{
    /* Set LLVMEnv pointer first so other classes can access it. */
    LLEnv = this;

    ParseCommandLineOptions();

    dbg() << DEBUG_LLVM << "Initializing LLVM Environment.\n";

    /* Initialize LLVM targets. */
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllAsmPrinters();
    InitializeAllAsmParsers();
    InitializeAllTargetMCs();
    InitializeAllDisassemblers();

    qemu_mutex_init(&mutex);

    Translator.resize(NumTranslator);
    HelperThread.resize(NumTranslator);
    ThreadEnv.resize(NumTranslator);
    for (unsigned i = 0; i < NumTranslator; ++i) {
        CPUState *cpu = ThreadEnv[i] = cpu_create();
        CPUArchState *env = (CPUArchState *)cpu->env_ptr;
        cpu->cpu_index = -i -1;
        env->build_mode = BUILD_LLVM;
        Translator[i] = nullptr;
    }

    QM = new QueueManager;
    AF = new AnnotationFactory;

    /* Create the memory manager and intialize the optimized code cache. There
     * is only copy of the optimized code cache and is shared by all underlying
     * translators. */
    MM = std::shared_ptr<MemoryManager>(
                MemoryManager::Create(TraceCache, TraceCacheSize));

    CreateTranslator();

    dbg() << DEBUG_LLVM << "LLVM environment initialized. "
          << format("guest_base=0x%lx.\n", GUEST_BASE)
          << format("\tBlock code cache: addr=%p size=%zd bytes.\n",
                    tcg_ctx_global.code_gen_buffer,
                    tcg_ctx_global.code_gen_buffer_size)
          << format("\tTrace code cache: addr=%p size=%zd bytes.\n",
                    TraceCache, TraceCacheSize);
}

LLVMEnv::~LLVMEnv()
{
    if (TransMode == TRANS_MODE_BLOCK) {
        size_t BlockCodeSize = MM->getCodeSize();
        dbg() << DEBUG_LLVM << "Finalizing LLVM environment."
              << "\n\tBlock code size: " << BlockCodeSize << " bytes.\n";
    } else {
        size_t BlockCodeSize = (uintptr_t)tcg_ctx_global.code_gen_ptr -
                               (uintptr_t)tcg_ctx_global.code_gen_buffer;
        size_t TraceCodeSize = MM->getCodeSize();
        dbg() << DEBUG_LLVM << "Finalizing LLVM environment."
              << "\n\tBlock code size  : " << format("%8d", BlockCodeSize) << " bytes"
              << "\n\tTrace code size  : " << format("%8d", TraceCodeSize) << " bytes"
              << "\n\tTrace/Block ratio: "
              << format("%.2f%%\n\n", (double)TraceCodeSize * 100 / BlockCodeSize);
    }

    if (UseThreading && !ThreadExit)
        StopThread();

    for (int i = 0, e = tcg_ctx_global.tb_ctx->nb_tbs; i != e; ++i) {
        if (tbs[i].image) delete_image(&tbs[i]);
        if (tbs[i].state) delete_state(&tbs[i]);
        if (tbs[i].chain) ChainInfo::free(&tbs[i]);
    }

    if (PF.getProfile() & PROFILE_HOTSPOT) {
        char buf[128] = {'\0'};
        std::ofstream OS;
        OS.open("hotspot.out");
        sprintf(buf, "%lx:%lx", (uintptr_t)TraceCache, (uintptr_t)TraceCache + TraceCacheSize);
        OS << "trace code cache: " << buf << "\n";
        for (unsigned i = 0, e = TransCode.size(); i != e; ++i) {
            TranslatedCode *TC = TransCode[i];
            sprintf(buf, "%p:%d", TC->Code, TC->Size);
            OS << "trace " << i << ": " << buf << "\t";
            int j = 0;
            for (auto *TB: TC->Trace->TBs) {
                std::stringstream ss;
                ss << std::hex << TB->pc;
                OS << (j++ == 0 ? "" : ",") << ss.str();
            }
            OS << "\n";
        }
        OS.close();
    }

    PF.printProfile();

    delete QM;
    delete AF;

    /* Delete all translated code. */
    for (unsigned i = 0, e = TransCode.size(); i != e; ++i)
        delete TransCode[i];

    dbg() << DEBUG_LLVM << "LLVM environment finalized.\n";
}

static void PrintVersion()
{
    Triple HostTriple(sys::getDefaultTargetTriple());
    raw_ostream &OS = outs();

    OS << "HQEMU (http://itanium.iis.sinica.edu.tw/hqemu/):\n"
       << "  HQEMU version: " << PACKAGE_VERSION_MAJOR << "."
                        << PACKAGE_VERSION_MINOR << "\n"
       << "  QEMU version: " << QEMU_VERSION << "\n"
       << "  Guest ISA: " << TARGET_NAME << "\n"
       << "  Host ISA: " << HostTriple.getArchName() << "\n";
    OS << "\n";
    cl::PrintVersionMessage();
}

void LLVMEnv::ParseCommandLineOptions()
{
    /* Disable passes that would change the DebugLoc metadata which
     * may fail our block/trace chaining. */
    static const char *argv[] = {
        "-disable-tail-duplicate",
        "-disable-early-taildup",
        "-disable-block-placement",
#if defined(TCG_TARGET_ARM) || defined(TCG_TARGET_AARCH64)
        "-disable-branch-fold",
#endif
    };

    cl::SetVersionPrinter(PrintVersion);

    /* Hide LLVM builtin options. */
#if defined(LLVM_V35)
    StringMap<cl::Option*> opts;
    cl::getRegisteredOptions(opts);
#else
    StringMap<cl::Option*> &opts = cl::getRegisteredOptions();
#endif
    for (auto &I : opts) {
        auto opt = I.second;
        if (opt->Category == &cl::GeneralCategory)
            opt->setHiddenFlag(cl::Hidden);
    }

    dbg() << DEBUG_LLVM << "Parsing command line options.\n";

    /* Get translation mode from LLVM_MODE. */
    TransMode = getTransMode();
    if (TransMode == TRANS_MODE_INVALID)
        hqemu_error("invalid LLVM_MODE.\n");

    /* Get command-line options from LLVM_CMD and update them in LLVM. */
    std::vector<const char *> PassArgs;
    char *p = getenv("LLVM_CMD");
    if (p) {
        const char *token = strtok(p, " ");
        while (token) {
            PassArgs.push_back(token);
            token = strtok(nullptr, " ");
        }
    }

    SmallVector<const char *, 16> Args;
    Args.push_back("qemu-" TARGET_NAME);
    for (unsigned i = 0, e = ARRAY_SIZE(argv); i < e; ++i)
        Args.push_back(argv[i]);
    for (const char *s : PassArgs)
        Args.push_back(s);
    Args.push_back(nullptr);
    cl::ParseCommandLineOptions(Args.size() - 1,
                                const_cast<char **>(&Args[0]));

    /* 
     * After this point, command-line options are all set.
     * We need to update functions that are controlled by the options.
     */

    DM.setDebugMode(DebugLevel);
    PF.setProfileMode(ProfileLevel);

    /* Update threading number if hybridm is enabled. */
    UseThreading = (TransMode == TRANS_MODE_HYBRIDM);
    if (!UseThreading)
        return;

    if (NumThreads != 1)
        NumTranslator = (NumThreads < 1) ? 1 : MIN(MAX_TRANSLATORS, NumThreads);
}

#if defined(CONFIG_USER_ONLY)
#define TIMEOUT_INTERVAL 1
#else
#define TIMEOUT_INTERVAL 1000
#endif

/*
 * WorkerFunc()
 *  The thread routine of the LLVM translation threads.
 */
void *WorkerFunc(void *argv)
{
    unsigned MyID = (unsigned long)argv;
    LLVMTranslator *Translator = LLEnv->getTranslator(MyID);
    MemoryManager *MM = LLEnv->getMemoryManager().get();
    CPUState *cpu = LLEnv->getThreadEnv(MyID);
    CPUArchState *env = (CPUArchState *)cpu->env_ptr;

    /* Block all signals. */
    sigset_t set;
    sigfillset(&set);
    pthread_sigmask(SIG_SETMASK, &set, nullptr);

    copy_tcg_context();
    optimization_init(env);

    Atomic<unsigned>::inc_return(&NumPendingThread);

    for (;;) {
        /* Exit the loop if a request is received. */
        if (unlikely(ThreadExit))
            break;

        if (unlikely(ThreadStop)) {
            Atomic<unsigned>::inc_return(&NumPendingThread);
            while (ThreadStop)
                usleep(100);

            Translator = LLEnv->getTranslator(MyID);
        }

        /* Exit the loop if the trace cache is full. */
        if (unlikely(!MM->isSizeAvailable())) {
            TraceCacheFull = true;
            ThreadStop = true;
            continue;
        }

        /* Everything is fine. Process an optimization request. */
        OptimizationInfo *Opt = (OptimizationInfo *)QM->Dequeue();
        if (Opt)
            Translator->GenTrace(env, Opt);

        usleep(TIMEOUT_INTERVAL);
    }

    pthread_exit(nullptr);
    return nullptr;
}

/*
 * CreateTranslator()
 *  Create LLVM translators and worker threads. We create the instances of
 *  translators and helper threads during the initialization of LLVMEnv and
 *  each helper thread will pick its own translator instance later.
 */
void LLVMEnv::CreateTranslator()
{
    dbg() << DEBUG_LLVM << "Creating " << NumTranslator << " translator(s).\n";

    for (unsigned i = 0; i < NumTranslator; ++i) {
        CPUArchState *env = (CPUArchState *)ThreadEnv[i]->env_ptr;
        Translator[i] = LLVMTranslator::CreateLLVMTranslator(i, env);
    }

    ThreadStop = false;
    ThreadExit = false;
    TraceCacheFull = false;

    if (UseThreading)
        StartThread();
}

/*
 * DeleteTranslator()
 *  Destroy LLVMTranslator.
 */
void LLVMEnv::DeleteTranslator()
{
    dbg() << DEBUG_LLVM << "Destroying " << NumTranslator << " translator(s).\n";

    /* Wait for worker threads finishing their jobs, clear all optimization
     * requests and flush trace code cache. */
    if (UseThreading) {
        ThreadStop = true;
        while (NumPendingThread != NumTranslator)
            usleep(100);

        QM->Flush();
        MM->Flush();
    }

    for (unsigned i = 0; i < NumTranslator; ++i) {
        delete Translator[i];
        Translator[i] = nullptr;
    }
}

void LLVMEnv::RestartTranslator()
{
    dbg() << DEBUG_LLVM << "Restarting " << NumTranslator << " translator(s).\n";

    for (unsigned i = 0; i < NumTranslator; ++i) {
        CPUArchState *env = (CPUArchState *)ThreadEnv[i]->env_ptr;
        Translator[i] = LLVMTranslator::CreateLLVMTranslator(i, env);
    }

    TraceCacheFull = false;
    NumPendingThread = 0;
    ThreadStop = false;;
}

void LLVMEnv::StartThread()
{
    ThreadExit = false;
    for (unsigned i = 0; i < NumTranslator; ++i) {
        int ret = pthread_create(&HelperThread[i], nullptr, WorkerFunc,
                                 (void*)(long)i);
        if (ret != 0)
            hqemu_error("failed to create worker thread.\n");
    }

    /* Wait until all threads are ready. */
    while (NumPendingThread != NumTranslator)
        usleep(200);
    NumPendingThread = 0;
}

void LLVMEnv::StopThread()
{
    ThreadExit = true;
    for (unsigned i = 0; i < NumTranslator; ++i)
        pthread_join(HelperThread[i], nullptr);
}

LLVMTranslator *LLVMEnv::AcquireSingleTranslator()
{
    if (Translator.empty())
        hqemu_error("internal error.\n");

    qemu_mutex_lock(&mutex);
    return Translator[0];
}

void LLVMEnv::ReleaseSingleTranslator()
{
    qemu_mutex_unlock(&mutex);
}


/*
 * CreateLLVMEnv()
 *  The interface to create the LLVMEnv instance.
 */
void LLVMEnv::CreateLLVMEnv()
{
    if (InitOnce == true)
        hqemu_error("LLVM environment already initialized.\n");

    if (TraceCache == nullptr)
        hqemu_error("llvm_alloc_cache() must be called before this function.\n");

    new LLVMEnv;
    InitOnce = true;
}

void LLVMEnv::DeleteLLVMEnv()
{
    if (InitOnce == false)
        hqemu_error("LLVM environment already destroyed.\n");

    /* Stop the LLVM translation threads before the program is terminated. */
    delete LLEnv;
    InitOnce = false;
}

TraceID LLVMEnv::insertTransCode(TranslatedCode *TC)
{
    TraceID tid = TransCode.size();
    TransCode.push_back(TC);
    SortedCode[(uintptr_t)TC->Code] = TC;

    for (auto TB : TC->Trace->TBs) {
        ChainInfo &Chain = *ChainInfo::get(TB);
        Chain.insertDepTrace(TC->EntryTB->id);
    }
    return tid;
}

LLVMEnv::SlotInfo LLVMEnv::getChainSlot()
{
    hqemu::MutexGuard locked(llvm_global_lock);

    size_t Key = ChainPoint.size();
    uintptr_t RetVal = (Key << 2) | TB_EXIT_LLVM;
    ChainPoint.push_back(0);
    return SlotInfo(Key, RetVal);
}

static bool OptimizeOrSkip()
{
    static unsigned curr = 0;

    dbg() << DEBUG_LLVM << "Received an optimization request ID=" << curr << "."
          << (curr >= NumTranslations ? " (skip)\n" : "\n");

    return curr++ >= NumTranslations;
}

int LLVMEnv::OptimizeBlock(CPUArchState *env, OptRequest Request)
{
    if (InitOnce == false)
        hqemu_error("internal error.\n");

    if (OptimizeOrSkip() == true)
        return 0;

    env->build_mode = BUILD_LLVM | BUILD_TCG;
    LLVMTranslator *Translator = LLEnv->AcquireSingleTranslator();
    Translator->GenBlock(env, Request.release());
    LLEnv->ReleaseSingleTranslator();
    env->build_mode = BUILD_NONE;
    return 1;
}

int LLVMEnv::OptimizeTrace(CPUArchState *env, OptRequest Request)
{
    if (InitOnce == false)
        return 0;

    if (TransMode == TRANS_MODE_NONE)
        return 0;
    if (OptimizeOrSkip() == true)
        return 0;

    OptimizationInfo *Opt = Request.release();
    Opt->ComposeCFG();

    if (TransMode == TRANS_MODE_HYBRIDS) {
        if (!TraceCacheFull) {
            if (!LLEnv->getMemoryManager()->isSizeAvailable())
                TraceCacheFull = true;
            else {
                LLVMTranslator *Translator = LLEnv->AcquireSingleTranslator();
                Translator->GenTrace(env, Opt);
                LLEnv->ReleaseSingleTranslator();
            }
        }

        if (TraceCacheFull)
            return 0;
    } else if (TransMode == TRANS_MODE_HYBRIDM) {
        /* Put the optimization request into the request queue and continue. */
        QM->Enqueue(Opt);
    }

    return 1;
}

#if defined(CONFIG_USER_ONLY)
QueueManager::QueueManager()
{
    CurrentQueue = new Queue;
}

QueueManager::~QueueManager()
{
    delete CurrentQueue;
}

void QueueManager::Enqueue(OptimizationInfo *Opt)
{
    CurrentQueue->enqueue(Opt);
}

void *QueueManager::Dequeue()
{
    return CurrentQueue->dequeue();
}

void QueueManager::Flush()
{
    while (1) {
        OptimizationInfo *Opt = (OptimizationInfo *)CurrentQueue->dequeue();
        if (Opt == nullptr)
            break;
        delete Opt;
    }
}

#else
QueueManager::QueueManager()
{
    ActiveQueue.resize(ACTIVE_QUEUE_SIZE);
    for (unsigned i = 0, e = ActiveQueue.size(); i != e; ++i)
        ActiveQueue[i] = nullptr;
}

QueueManager::~QueueManager()
{
    for (unsigned i = 0, e = ActiveQueue.size(); i != e; ++i) {
        if (ActiveQueue[i])
            delete ActiveQueue[i];
    }
}

void QueueManager::Enqueue(OptimizationInfo *Opt)
{
    Queue *CurrentQueue = ActiveQueue[pcid & ACTIVE_QUEUE_MASK];
    if (unlikely(!CurrentQueue))
        CurrentQueue = ActiveQueue[pcid & ACTIVE_QUEUE_MASK] = new Queue;
    CurrentQueue->enqueue(Opt);
}

void *QueueManager::Dequeue()
{
    Queue *CurrentQueue = ActiveQueue[pcid & ACTIVE_QUEUE_MASK];
    if (unlikely(!CurrentQueue))
        return nullptr;
    return CurrentQueue->dequeue();
}

void QueueManager::Flush()
{
    for (unsigned i = 0, e = ActiveQueue.size(); i != e; ++i) {
        if (!ActiveQueue[i])
            continue;

        while (1) {
            OptimizationInfo *Opt = (OptimizationInfo *)ActiveQueue[i]->dequeue();
            if (!Opt)
                break;
            delete Opt;
        }
    }
}
#endif


/*
 * OptimizationInfo
 */

OptimizationInfo::OptimizationInfo(TranslationBlock *HeadTB, TraceEdge &Edges)
    : isUserTrace(true), isBlock(false), CFG(nullptr)
{
    for (auto E : Edges)
        Trace.push_back(E.first);

#if defined(CONFIG_USER_ONLY)
    if (!llvm_has_annotation(HeadTB->pc, ANNOTATION_LOOP))
        ExpandTrace(HeadTB, Edges);
#endif

    /* Build CFG from the edges. */
    std::map<TranslationBlock *, GraphNode *> NodeMap;

    NodeMap[HeadTB] = new GraphNode(HeadTB);
    for (auto E : Edges) {
        TranslationBlock *Parent = E.first;
        if (NodeMap.find(Parent) == NodeMap.end())
            NodeMap[Parent] = new GraphNode(Parent);

        GraphNode *ParentNode = NodeMap[Parent];
        for (auto Child : E.second) {
            if (NodeMap.find(Child) == NodeMap.end())
                NodeMap[Child] = new GraphNode(Child);

            ParentNode->insertChild(NodeMap[Child]);
        }
    }

    CFG = NodeMap[HeadTB];
}

void OptimizationInfo::SearchCycle(TraceNode &SearchNodes, TraceEdge &Edges,
                                   TBVec &Visited, int Depth)
{
    TranslationBlock *Curr = Visited.back();

    if (llvm_has_annotation(Curr->pc, ANNOTATION_LOOP))
        return;

    /* If the current node is one of the main NET trace node, we found a cyclic path.
     * The links of such cyclic path are added to the trace edges. */
    if (SearchNodes.find(Curr) != SearchNodes.end()) {
        for (unsigned i = 1, e = Visited.size(); i != e; ++i) {
            TranslationBlock *Pred = Visited[i - 1];
            TranslationBlock *Succ = Visited[i];
            Edges[Pred].insert(Succ);
        }
        return;
    }
    /* Stop if we reach the maximum search depth. */
    if (Depth == MAX_SEARCH_DEPTH)
        return;

    /* Still cannot find a cyclic path? Keep looking for the successors. */
    for (auto Succ : GlobalCFG.getSuccessor(Curr)) {
        Visited.push_back(Succ);
        SearchCycle(SearchNodes, Edges, Visited, Depth + 1);
        Visited.pop_back();
    }
}

/*
 * ExpandTrace()
 *  Expand a NET trace to a bigger region with the NETPlus algorithm.
 *  NETPlus: trace formation algorithm based on the paper published in
 *  RESoLVE'11. D. Davis and K. Hazelwood, "Improving Region Selection Through
 *  Loop Completion," in ASPLOS Workshop on Runtime Environments/Systems,
 *  Layering, and Virtualized Environments, 2011.
 */
void OptimizationInfo::ExpandTrace(TranslationBlock *HeadTB, TraceEdge &Edges)
{
    TraceNode MainTraceNodes;
    std::map<target_ulong, TranslationBlock*> NodeMap;
#ifdef USE_TRACETREE_ONLY
    MainTraceNodes.insert(HeadTB);
    NodeMap[HeadTB->pc] = HeadTB;
#else
    for (auto E : Edges) {
        TranslationBlock *TB = E.first;
        MainTraceNodes.insert(TB);
        NodeMap[TB->pc] = TB;
    }
#endif

    /* Put critical section when traversing GlobalCFG. */
    hqemu::MutexGuard locked(GlobalCFG.getLock());

    for (auto TB : Trace) {
        TBVec Visited;
        Visited.push_back(TB);
        if (NodeMap.find(TB->jmp_pc[0]) != NodeMap.end())
            Edges[TB].insert(NodeMap[TB->jmp_pc[0]]);
        if (TB->jmp_pc[1] != (target_ulong)-1 &&
            NodeMap.find(TB->jmp_pc[1]) != NodeMap.end())
            Edges[TB].insert(NodeMap[TB->jmp_pc[1]]);

        for (auto Succ : GlobalCFG.getSuccessor(TB)) {
            Visited.push_back(Succ);
            SearchCycle(MainTraceNodes, Edges, Visited, 0);
            Visited.pop_back();
        }
    }
}

/*
 * ComposeCFG()
 *  Compose a trace of CFG from a list of TBs.
 */
void OptimizationInfo::ComposeCFG()
{
    bool isUser = true;
    TranslationBlock *HeadTB = Trace[0];

#if defined(CONFIG_SOFTMMU)
    isUser = isUserTB(HeadTB) ? true : false;
    for (auto TB : Trace) {
        if (unlikely(TB->mode == BLOCK_INVALID)) {
            /* A NET trace may contain invalidated block because the block
             * is invalidated during trace formation. */
            dbg() << DEBUG_LLVM << __func__ << ": skip due to invalidated block\n";
            return;
        }

        if (isUser && isUserTB(TB) == false) {
            dbg() << DEBUG_LLVM << __func__ << ": skip due to mixed mode\n";
            return;
        }

        /* Our translator assumes that component blocks have the same cs_base. */
        if (TB->cs_base != HeadTB->cs_base) {
            dbg() << DEBUG_LLVM << __func__ << ": skip due to inconsistent cs\n";
            return;
        }
    }
#endif

    /* Check if the consecutive blocks are really connected. */
    TraceEdge Edges;

    TranslationBlock *Curr = Trace[0];
    for (unsigned i = 1, e = Trace.size(); i != e; ++i) {
        TranslationBlock *Pred = Trace[i - 1];
        Curr = Trace[i];
        if (Pred->jmp_pc[0] != (target_ulong)-1 &&
            Pred->jmp_pc[0] != Curr->pc &&
            Pred->jmp_pc[1] != Curr->pc) {
            /* Disconnected. Discard the tailing blocks. */
            Trace.resize(i);
            LoopHeadIdx = -1;
            break;
        }

        /* Connected. */
        Edges[Pred].insert(Curr);
    }
    if (LoopHeadIdx != -1)
        Edges[Curr].insert(Trace[LoopHeadIdx]);

#if defined(CONFIG_USER_ONLY)
    if (!llvm_has_annotation(Trace[0]->pc, ANNOTATION_LOOP))
        ExpandTrace(HeadTB, Edges);
#endif

    /* Build CFG from the edges. */
    std::map<TranslationBlock *, GraphNode *> NodeMap;

    NodeMap[HeadTB] = new GraphNode(HeadTB);
    for (auto E : Edges) {
        TranslationBlock *Parent = E.first;
        if (NodeMap.find(Parent) == NodeMap.end())
            NodeMap[Parent] = new GraphNode(Parent);

        GraphNode *ParentNode = NodeMap[Parent];
        for (auto Child : E.second) {
            if (NodeMap.find(Child) == NodeMap.end())
                NodeMap[Child] = new GraphNode(Child);

            ParentNode->insertChild(NodeMap[Child]);
        }
    }

    CFG = NodeMap[HeadTB];
    isUserTrace = isUser;
}


/* The following implements routines of the C interfaces for QEMU. */
extern "C" {

void hqemu_help(void)
{
    /* Hide LLVM builtin options. */
#if defined(LLVM_V35)
    StringMap<cl::Option*> opts;
    cl::getRegisteredOptions(opts);
#else
    StringMap<cl::Option*> &opts = cl::getRegisteredOptions();
#endif
    for (auto &I : opts) {
        auto opt = I.second;
        if (opt->Category == &cl::GeneralCategory)
            opt->setHiddenFlag(cl::Hidden);
    }

    SmallVector<const char *, 16> Args;
    Args.push_back("\n  export LLVM_CMD='[OPTION1] [OPTION2]'\n  qemu-" TARGET_NAME);
    Args.push_back(nullptr);
    cl::ParseCommandLineOptions(Args.size() - 1,
                                const_cast<char **>(&Args[0]));
    cl::PrintHelpMessage(false, false);
}

int llvm_init()
{
    LLVMEnv::CreateLLVMEnv();
    return 0;
}

int llvm_finalize()
{
    LLVMEnv::DeleteLLVMEnv();
#if 0
    llvm_shutdown();
#endif
    return 0;
}

int llvm_alloc_cache()
{
    size_t BlockCacheSize = (tcg_ctx.code_gen_buffer_size / 2)
                             & qemu_real_host_page_mask;
    LLVMEnv::TraceCacheSize = tcg_ctx.code_gen_buffer_size - BlockCacheSize;
    LLVMEnv::TraceCache = (uint8_t *)tcg_ctx.code_gen_buffer + BlockCacheSize;

    tcg_ctx.code_gen_buffer_size = BlockCacheSize;
    return 0;
}

int llvm_check_cache(void)
{
    if (LLVMEnv::InitOnce == false)
        return 1;
    return TraceCacheFull ? 1 : 0;
}

/*
 * llvm_tb_flush()
 *  Wrapper fucntion to flush the optmizated code cache.
 */
int llvm_tb_flush(void)
{
    if (LLVMEnv::InitOnce == false)
        return 1;
    if (LLVMEnv::TransMode == TRANS_MODE_NONE)
        return 1;

    dbg() << DEBUG_LLVM << __func__ << " entered.\n";

    LLEnv->DeleteTranslator();

    for (int i = 0, e = tcg_ctx_global.tb_ctx->nb_tbs; i != e; ++i) {
        if (tbs[i].image) delete_image(&tbs[i]);
        if (tbs[i].state) delete_state(&tbs[i]);
        if (tbs[i].chain) ChainInfo::free(&tbs[i]);

        tbs[i].image = tbs[i].state = tbs[i].chain = nullptr;
    }

    /* Remove all translated code. */
    LLVMEnv::TransCodeList &TransCode = LLEnv->getTransCode();
    for (unsigned i = 0, e = TransCode.size(); i != e; ++i)
        delete TransCode[i];

    TransCode.clear();
    LLEnv->getSortedCode().clear();
    LLEnv->getChainPoint().clear();

    /* Clear global cfg. */
    GlobalCFG.reset();

    LLEnv->RestartTranslator();
    LLEnv->incNumFlush();

    dbg() << DEBUG_LLVM << __func__ << ": trace cache flushed.\n";

    return 0;
}

static void llvm_suppress_chaining(TranslationBlock *tb)
{
    /* TODO: add unlinking rule for non-x86 hosts. */
    std::vector<uintptr_t> &Chains = ChainInfo::get(tb)->Chains;
    if (Chains.empty())
        return;

    for (unsigned i = 0, e = Chains.size(); i != e; ++i)
        patch_jmp(Chains[i], Chains[i] + 5);
    Chains.clear();
}

/*
 * llvm_tb_remove()
 *  Remove the traces containing the `tb' that is invalidated by QEMU.
 */
int llvm_tb_remove(TranslationBlock *tb)
{
    if (LLVMEnv::TransMode == TRANS_MODE_NONE)
        return 1;
    if (!tb->chain)
        return 1;

    /* Unlink traces that jump to this tb. */
    llvm_suppress_chaining(tb);

    if (LLVMEnv::TransMode == TRANS_MODE_BLOCK) {
        patch_jmp(tb_get_jmp_entry(tb), tb_get_jmp_next(tb));
        ChainInfo::free(tb);
        return 1;
    }

    LLVMEnv::TransCodeList &TransCode = LLEnv->getTransCode();
    LLVMEnv::TransCodeMap &SortedCode = LLEnv->getSortedCode();
    std::vector<BlockID> &DepTraces = ChainInfo::get(tb)->DepTraces;

    hqemu::MutexGuard locked(llvm_global_lock);

    /* Remove traces that contain this tb. */
    if (DepTraces.empty())
        return 0;

    for (unsigned i = 0, e = DepTraces.size(); i != e; ++i) {
        TranslationBlock *EntryTB = &tbs[DepTraces[i]];
        if (EntryTB->tid == -1) {
            /* This can happen when a trace block (not head) was removed
             * before and at that time the tid of the trace head block is
             * set to -1. Now, the trace head block is going to be removed
             * and we just skip it. */
            continue;
        }

        TranslatedCode *TC = TransCode[EntryTB->tid];
        if (!TC->Active)
            hqemu_error("fatal error.\n");

        TC->Active = false;
        SortedCode.erase((uintptr_t)TC->Code);
        patch_jmp(tb_get_jmp_entry(EntryTB), tb_get_jmp_next(EntryTB));

        /* For system-mode emulation, since the source traces do not directly
         * jump to the trace code, we do not need to suppress the traces
         * chaining to the trace head block. Unlinking the jump from the
         * trace head block to the trace code is sufficient to make execution
         * from going to the trace code. */
#if defined(CONFIG_USER_ONLY)
        llvm_suppress_chaining(EntryTB);
#endif

        EntryTB->mode = BLOCK_ACTIVE;
        EntryTB->exec_count = 0;
        EntryTB->opt_ptr = EntryTB->tc_ptr;
        EntryTB->tid = -1;
    }

    DepTraces.clear();
    ChainInfo::free(tb);

    return 1;
}

/*
 * llvm_resolve_address()
 *  Given the value returned when leaving the code cache, return the patch
 *  address for the region chaining.
 */
static void llvm_resolve_address(uintptr_t &addr)
{
    if (LLVMEnv::InitOnce == false)
        return;

    hqemu::MutexGuard locked(llvm_global_lock);

    LLVMEnv::ChainSlot &ChainPoint = LLEnv->getChainPoint();
    size_t Key = addr >> 2;
    addr = ChainPoint[Key];
}

#if defined(CONFIG_USER_ONLY)
#define cross_page(__tb)            (0)
#define trace_add_jump(src, dst)    patch_jmp(next_tb, tb->opt_ptr)
#else
#define cross_page(__tb)            (__tb->page_addr[1] != (unsigned long)-1)
#define trace_add_jump(src, dst)    patch_jmp(next_tb, tb->tc_ptr)
#endif

void llvm_handle_chaining(uintptr_t next_tb, TranslationBlock *tb)
{
    if ((next_tb & TB_EXIT_MASK) == TB_EXIT_LLVM) {
        if (update_tb_mode(tb, BLOCK_ACTIVE, BLOCK_TRACEHEAD))
            start_trace_profiling(tb);

        llvm_resolve_address(next_tb);
        if (next_tb && !cross_page(tb)) {
            /* Keep track of traces (i.e., next_tb) that jump to this tb. */
            ChainInfo &Chain = *ChainInfo::get(tb);
            Chain.insertChain(next_tb);

            /* For system-mode emulation, we only let the source traces
             * jump to the trace head 'block' in the block code cache. */
            trace_add_jump(next_tb, tb);
        }
    } else if (next_tb != 0 && !cross_page(tb)) {
        TranslationBlock *pred_tb = (TranslationBlock *)(next_tb & ~TB_EXIT_MASK);
        int n = next_tb & TB_EXIT_MASK;

        tb_add_jump(pred_tb, n, tb);
    }
}

int llvm_locate_trace(uintptr_t searched_pc)
{
    uintptr_t Start = (uintptr_t)LLVMEnv::TraceCache;
    uintptr_t End = Start + LLVMEnv::TraceCacheSize;
    return (searched_pc >= Start && searched_pc < End);
}

TranslationBlock *llvm_find_pc(CPUState *cpu, uintptr_t searched_pc)
{
    LLVMEnv::TransCodeMap &SortedCode = LLEnv->getSortedCode();
    CPUArchState *env = (CPUArchState *)cpu->env_ptr;

    if (LLVMEnv::InitOnce == false)
        return nullptr;
    if (!llvm_locate_trace(searched_pc))
        return nullptr;

    hqemu::MutexGuard locked(llvm_global_lock);

    LLVMEnv::TransCodeMap::iterator I = SortedCode.upper_bound(searched_pc);
    TranslatedCode *TC = (--I)->second;

    if (env->restore_val >= TC->Restore.size()) {
        auto HostDisAsm = LLEnv->getTranslator(0)->getHostDisAsm();
        if (HostDisAsm)
            HostDisAsm->PrintOutAsm((uint64_t)TC->Code, TC->Size);
        hqemu_error("got exception at 0x%zx\n", searched_pc);
    }

    /* Since restore_val is no longer used, we set it to the
     * the opc index so the later restore can quickly get it. */
    std::pair<BlockID, uint16_t> RestoreInfo = TC->Restore[env->restore_val];
    env->restore_val = RestoreInfo.second - 1;
    return &tbs[RestoreInfo.first];
}

/*
 * llvm_restore_state()
 *  The cpu state corresponding to 'searched_pc' is restored.
 */
int llvm_restore_state(CPUState *cpu, TranslationBlock *tb,
                       uintptr_t searched_pc)
{
    target_ulong data[TARGET_INSN_START_WORDS] = { tb->pc };
    CPUArchState *env = (CPUArchState *)cpu->env_ptr;
    uintptr_t host_pc = (uintptr_t)tb->tc_ptr;
    uint8_t *p = tb->tc_search;

    /* Reconstruct the stored insn data while looking for the point at
       which the end of the insn exceeds the searched_pc.  */
    for (unsigned i = 0, e = tb->icount; i != e; ++i) {
        for (unsigned j = 0; j < TARGET_INSN_START_WORDS; ++j) {
            data[j] += decode_sleb128(&p);
        }
        host_pc += decode_sleb128(&p);
        if (env->restore_val == i)
            goto found;
    }
    return -1;

found:
    restore_state_to_opc(env, tb, data);

    return 0;
}

/*
 * llvm_fork_start()
 *  Wrapper function to stop the optimization service before performing fork.
 */
void llvm_fork_start(void)
{
    if (!LLEnv->isThreading())
        return;

    dbg() << DEBUG_LLVM << __func__ << " entered.\n";

    LLEnv->StopThread();
}

/*
 * llvm_fork_end()
 *  Wrapper function to restart the optimization service after performing fork.
 */
void llvm_fork_end(int child)
{
    if (!LLEnv->isThreading())
        return;

    dbg() << DEBUG_LLVM << __func__ << " entered.\n";

    if (child == 0) {
        LLEnv->StartThread();
    } else {
        ThreadExit = true;
        LLVMEnv::setTransMode(TRANS_MODE_NONE);

        qemu_mutex_init(&LLEnv->mutex);
    }
}

int llvm_has_annotation(target_ulong addr, int annotation)
{
    if (annotation == ANNOTATION_LOOP)
        return AF->hasLoopAnnotation(addr) == true;
    return 0;
}

}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
