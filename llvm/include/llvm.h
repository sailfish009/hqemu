/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __LLVM_H
#define __LLVM_H

#include <memory>
#include <vector>
#include "llvm/ADT/STLExtras.h"
#include "llvm-types.h"
#include "llvm-debug.h"
#include "utils.h"

#if defined(ENABLE_MCJIT)
#include "llvm/ExecutionEngine/MCJIT.h"
#include "MCJITMemoryManager.h"
typedef class DefaultMCJITMemoryManager MemoryManager;
#else
#if defined(LLVM_V35)
#include "JIT.h"
#include "JITMemoryManager.h"
#else
#  error "LLVM version >3.5 supports MCJIT only. ENABLE_MCJIT must be enabled."
#endif
typedef class DefaultJITMemoryManager MemoryManager;
#endif


extern cl::OptionCategory CategoryHQEMU;

class LLVMTranslator;
class OptimizationInfo;
class TranslatedCode;

typedef std::unique_ptr<OptimizationInfo> OptRequest;


/*
 * LLVMEnv is the top level container of whole LLVM translation environment
 * which manages the LLVM translator(s) and globally shared resources. The
 * LLVMEnv instance must be initialized before using the underlying transaltion
 * service and can only be initialized ONCE.
 */
class LLVMEnv {
public:
    typedef std::vector<TranslatedCode *> TransCodeList;
    typedef std::map<uintptr_t, TranslatedCode *> TransCodeMap;
    typedef std::vector<uintptr_t> ChainSlot;
    typedef std::pair<size_t, uintptr_t> SlotInfo;

private:
    std::shared_ptr<MemoryManager> MM;        /* Trace cache manager */
    unsigned NumTranslator;                   /* The amount of LLVM translators */
    std::vector<LLVMTranslator *> Translator; /* LLVM translators */
    std::vector<pthread_t> HelperThread;      /* LLVM translation threads */
    std::vector<CPUState *> ThreadEnv;

    TransCodeList TransCode;  /* Translated traces. */
    TransCodeMap SortedCode;  /* Sorted traces in code cache address order. */
    ChainSlot ChainPoint;     /* Address of stubs for trace-to-block linking */

    bool UseThreading; /* running threaded translators or not. */
    unsigned NumFlush;

    LLVMEnv();

    /* ParseCommandLineOptions - Parse the command line options. */
    void ParseCommandLineOptions();

public:
    QemuMutex mutex;

    ~LLVMEnv();

    /* CreateTranslator/DeleteTranslator - Create and destroy LLVM translators
     * and worker threads. */
    void CreateTranslator();
    void DeleteTranslator();
    void RestartTranslator();
    void StartThread();
    void StopThread();

    /* getTranslator - Get specific LLVM translator with index. */
    LLVMTranslator *getTranslator(unsigned ID) {
        if (ID >= Translator.size())
            hqemu_error("invalid translator ID.\n");
        return Translator[ID];
    }

    /* AcquireSingleTranslator - Get and lock the first LLVM translator. */
    LLVMTranslator *AcquireSingleTranslator();

    /* ReleaseSingleTranslator - Release the first LLVM translator. */
    void ReleaseSingleTranslator();

    /* getThreadEnv - Get specific CPU state of helper thread with index. */
    CPUState *getThreadEnv(int ID)              { return ThreadEnv[ID];  }

    std::vector<pthread_t> &getHelperThread()   { return HelperThread;   }
    std::shared_ptr<MemoryManager> getMemoryManager() { return MM;       }
    TransCodeList &getTransCode()               { return TransCode;      }
    TransCodeMap &getSortedCode()               { return SortedCode;     }
    ChainSlot &getChainPoint()                  { return ChainPoint;     }
    TraceID insertTransCode(TranslatedCode *TC);
    SlotInfo getChainSlot();

    bool isThreading()     { return UseThreading;      }
    void incNumFlush()     { NumFlush++;               }
    unsigned getNumFlush() { return NumFlush;          }

    /*
     * static public members
     */
    static bool InitOnce;  /* LLVMEnv is initialized or not? */
    static int TransMode;
    static uint8_t *TraceCache;
    static size_t TraceCacheSize;

    static void CreateLLVMEnv();
    static void DeleteLLVMEnv();
    static int OptimizeBlock(CPUArchState *env, OptRequest Request);
    static int OptimizeTrace(CPUArchState *env, OptRequest Request);
    static void setTransMode(int Mode) { TransMode = Mode; }
    static int isTraceMode() {
        return (TransMode == TRANS_MODE_HYBRIDS ||
                TransMode == TRANS_MODE_HYBRIDM);
    }
};

class QueueManager {
    std::vector<Queue *> ActiveQueue;
    Queue *CurrentQueue;

public:
    QueueManager();
    ~QueueManager();
    void Enqueue(OptimizationInfo *Opt);
    void *Dequeue();
    void Flush();
};

/*
 * OptimizationInfo is the description to an optimization request. It consists
 * of the optimization mode and the control-flow-graph of the trace.
 */
class OptimizationInfo {
public:
    typedef std::set<TranslationBlock *> TraceNode;
    typedef std::map<TranslationBlock *, TraceNode> TraceEdge;

    ~OptimizationInfo() {
        if (CFG)
            GraphNode::DeleteCFG(CFG);
    }

    void ComposeCFG();
    GraphNode *getCFG()    { return CFG;      }
    bool isTrace()         { return !isBlock; }

    static OptRequest CreateRequest(TranslationBlock *tb) {
        return OptRequest(new OptimizationInfo(tb));
    }
    static OptRequest CreateRequest(TBVec &trace, int idx) {
        return OptRequest(new OptimizationInfo(trace, idx));
    }
    static OptRequest CreateRequest(TranslationBlock *head, TraceEdge &edges) {
        return OptRequest(new OptimizationInfo(head, edges));
    }

private:
    TBVec Trace;       /* Trace of a list of TBs */
    int LoopHeadIdx;   /* Index to the loopback block */
    bool isUserTrace;  /* Trace of all user-mode blocks */
    bool isBlock;      /* Trace of a single block */
    GraphNode *CFG;    /* CFG of the trace */

    OptimizationInfo(TranslationBlock *tb)
        : isUserTrace(true), isBlock(true) {
        Trace.push_back(tb);
        LoopHeadIdx = -1;
        CFG = new GraphNode(tb);
    }
    OptimizationInfo(TBVec &trace, int idx)
        : isUserTrace(true), isBlock(false), CFG(nullptr) {
        if (trace.empty())
            hqemu_error("trace length cannot be zero.\n");
        Trace = trace;
        LoopHeadIdx = idx;
    }
    OptimizationInfo(TranslationBlock *HeadTB, TraceEdge &Edges);

    void SearchCycle(TraceNode &SearchNodes, TraceEdge &Edges,
                     TBVec &Visited, int Depth);
    void ExpandTrace(TranslationBlock *HeadTB, TraceEdge &Edges);
};

class TraceInfo {
public:
    TBVec TBs;
    unsigned NumLoop;
    unsigned NumExit;
    unsigned NumIndirectBr;
    uint64_t **ExecCount;
    uint64_t TransTime;
    uint32_t Attribute;

    TraceInfo(NodeVec &Nodes, uint32_t Attr = A_None)
        : NumLoop(0), NumExit(0), NumIndirectBr(0), ExecCount(nullptr),
          TransTime(0), Attribute(Attr)
    {
        if (Nodes.empty())
            hqemu_error("number of nodes cannot be zero.\n");
        for (unsigned i = 0, e = Nodes.size(); i != e; ++i)
            TBs.push_back(Nodes[i]->getTB());
    }

    TranslationBlock *getEntryTB() { return TBs[0]; }
    target_ulong getEntryPC() { return TBs[0]->pc; }
    unsigned getNumBlock()    { return TBs.size(); }
    void setTransTime(struct timeval *start, struct timeval *end) {
        struct timeval t;
        timersub(end, start, &t);
        TransTime = t.tv_sec * 1e6 + t.tv_usec;
    }
    bool hasAttribute(uint32_t Attr) {
        return Attribute & Attr;
    }
};

struct ChainInfo {
    std::vector<uintptr_t> Chains;
    std::vector<BlockID> DepTraces;

    void insertChain(uintptr_t addr) {
        Chains.push_back(addr);
    }
    void insertDepTrace(BlockID id) {
        DepTraces.push_back(id);
    }
    static ChainInfo *get(TranslationBlock *tb) {
        if (!tb->chain)
            tb->chain = (ChainInfo *)new ChainInfo;
        return (ChainInfo *)tb->chain;
    }
    static void free(TranslationBlock *tb) {
        delete (ChainInfo *)tb->chain;
        tb->chain = nullptr;
    }
};

class TranslatedCode {
public:
    TranslatedCode() : Trace(nullptr) {}
    ~TranslatedCode() {
        if (Trace)
            delete Trace;
    }

    bool Active;
    uint32_t Size;             /* Size of the translated host code */
    uint8_t *Code;             /* Start PC of the translated host code */
    TranslationBlock *EntryTB; /* The entry block of the region */
    RestoreVec Restore;
    TraceInfo *Trace;
};


#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
