/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 *
 *   This file provides LLVM IR generator in terms of basic block and trace.
 */

#include "llvm/CodeGen/Passes.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm-debug.h"
#include "llvm-pass.h"
#include "llvm-translator.h"
#include "llvm-target.h"
#include "llvm-state.h"
#include "llvm-opc.h"


#define INLINE_THRESHOLD    100     /* max # inlined instructions */
#define INLINE_INSTCOUNT    20      /* max instruction count for inlining a small function */

/* Options enabled by default. */
static cl::opt<bool> DisableStateMapping("disable-sm", cl::init(false),
    cl::cat(CategoryHQEMU), cl::desc("Disable state mapping"));

/* Options Disabled by default. */
static cl::opt<bool> EnableSimplifyPointer("enable-simptr", cl::init(false),
    cl::cat(CategoryHQEMU), cl::desc("Enable SimplifyPointer"));


TCGOpDef llvm_op_defs[] = {
#define DEF(s, oargs, iargs, cargs, flags) \
    { #s , oargs, iargs, cargs, iargs + oargs + cargs, flags },
#include "tcg-opc.h"
#undef DEF
};

static IRFactory::FuncPtr OpcFunc[] = {
#define DEF(name, oargs, iargs, cargs, flags) &IRFactory::op_ ## name,
#include "tcg-opc.h"
#undef DEF
};

extern LLVMEnv *LLEnv;
extern hqemu::Mutex llvm_global_lock;
extern hqemu::Mutex llvm_debug_lock;

/*
 * IRFactory()
 */
IRFactory::IRFactory(LLVMTranslator *Trans)
    : InitOnce(false), Translator(*Trans), EE(nullptr),
      HostDisAsm(Translator.getHostDisAsm()), Helpers(Translator.getHelpers()),
      BaseReg(Translator.getBaseReg()), NI(Translator.getNotifyInfo())
{
    /* Track TCG virtual registers. */
    Reg.resize(TCG_MAX_TEMPS);

    TCGContext *s = &tcg_ctx_global;
    int NumGlobals = s->nb_globals;
    for (int i = 0; i < NumGlobals; ++i) {
        TCGTemp *T = &s->temps[i];
        if (T->type != TCG_TYPE_I32 && T->type != TCG_TYPE_I64)
            hqemu_error("unsupported register type.\n");

        int Base = (T->fixed_reg) ? T->reg : T->mem_reg;
        intptr_t Off = (T->fixed_reg) ? -1 : T->mem_offset;
        Reg[i].set(Base, Off, T->name);
    }

    for (int i = 0; i < NumGlobals; ++i) {
        TCGTemp *T1 = &s->temps[i];
        for (int j = i + 1; j < NumGlobals; ++j) {
            TCGTemp *T2 = &s->temps[j];
            if (T1->fixed_reg || T2->fixed_reg)
                continue;
            if (Reg[j].Alias)
                continue;
            if (T1->mem_offset == T2->mem_offset && T1->type == T2->type)
                Reg[j].Alias = &Reg[i];
        }
    }

    Segment = 0;
#if defined(__x86_64__) && defined(__linux__)
    if (GUEST_BASE)
        Segment = 256;  /* GS: 256 */
#endif

    dbg() << DEBUG_LLVM << "LLVM IR Factory initialized.\n";
}

IRFactory::~IRFactory()
{
    if (EE) {
        EE->UnregisterJITEventListener(Listener);
        EE->removeModule(Mod);
        delete Listener;
        delete EE;
    }
}

void IRFactory::CreateSession(TraceBuilder *builder)
{
    Builder = builder;

    CreateJIT();
    InitializeTypes();

    MF = new MDFactory(Mod);
    ExitAddr = CONSTPtr((uintptr_t)tb_ret_addr);

    runPasses = true;

    /* Reset data structures. */
    StatePtr.clear();
    InlineCalls.clear();
    IndirectBrs.clear();
    CommonBB.clear();
    toErase.clear();
    toSink.clear();
    ClonedFuncs.clear();
    NI.reset();
}

void IRFactory::DeleteSession()
{
    if (Func) {
        Func->removeFromParent();
        delete Func;
        Func = nullptr;
    }
    delete MF;
    DeleteJIT();
}

static void setHostAttrs(std::string &MCPU, std::vector<std::string> &MAttrs,
                         TargetOptions &Options)
{
    MCPU = sys::getHostCPUName();

    StringMap<bool> HostFeatures;
    sys::getHostCPUFeatures(HostFeatures);
    for (auto &F : HostFeatures)
        MAttrs.push_back((F.second ? "+" : "-") + F.first().str());

    if (MCPU == "core-avx2" || MCPU == "haswell" || MCPU == "knl")
        Options.AllowFPOpFusion = FPOpFusion::Fast;
}

#if defined(ENABLE_MCJIT)
#if defined(LLVM_V35)
void IRFactory::CreateJIT()
{
    Module *InitMod = Translator.getModule();
    Context = &InitMod->getContext();
    Mod = new Module(InitMod->getModuleIdentifier(), *Context);
    Mod->setDataLayout(InitMod->getDataLayout());
    Mod->setTargetTriple(InitMod->getTargetTriple());

    DL = getDataLayout(Mod);

    /* Create JIT execution engine. */
    std::string ErrorMsg, MCPU;
    std::vector<std::string> MAttrs;
    TargetOptions Options;

    setHostAttrs(MCPU, MAttrs, Options);

    EngineBuilder builder(Mod);
    builder.setMCPU(MCPU);
    builder.setMAttrs(MAttrs);
    builder.setErrorStr(&ErrorMsg);
    builder.setEngineKind(EngineKind::JIT);
    builder.setOptLevel(CodeGenOpt::Default);
    builder.setUseMCJIT(true);
    builder.setMCJITMemoryManager(LLEnv->getMemoryManager().get());
    builder.setTargetOptions(Options);

    EE = builder.create();

    if (!EE)
        hqemu_error("%s\n", ErrorMsg.c_str());

    /* Create JIT event listener and link target machine. */
    Listener = new EventListener(NI);

    EE->RegisterJITEventListener(Listener);

    /* Ask LLVM to reserve basereg. */
    auto TM = EE->getTargetMachine();
    auto TRI = const_cast<TargetRegisterInfo*>(TM->getRegisterInfo());
    TRI->setHQEMUReservedRegs(BaseReg[TCG_AREG0].Name);

    dbg() << DEBUG_LLVM << "LLVM MCJIT initialized.\n";
}
#else
void IRFactory::CreateJIT()
{
    Module *InitMod = Translator.getModule();
    Context = &InitMod->getContext();
    std::unique_ptr<Module> Owner(
                new Module(InitMod->getModuleIdentifier(), *Context));
    Mod = Owner.get();
    Mod->setDataLayout(InitMod->getDataLayout());
    Mod->setTargetTriple(InitMod->getTargetTriple());

    DL = getDataLayout(Mod);

    /* Create JIT execution engine. */
    std::string ErrorMsg, MCPU;
    std::vector<std::string> MAttrs;
    TargetOptions Options;

    setHostAttrs(MCPU, MAttrs, Options);

    EngineBuilder builder(std::move(Owner));
    builder.setMCPU(MCPU);
    builder.setMAttrs(MAttrs);
    builder.setErrorStr(&ErrorMsg);
    builder.setEngineKind(EngineKind::JIT);
    builder.setOptLevel(CodeGenOpt::Default);
    builder.setMCJITMemoryManager(LLEnv->getMemoryManager());
    builder.setTargetOptions(Options);

    EE = builder.create();

    if (!EE)
        hqemu_error("%s\n", ErrorMsg.c_str());

    /* Create JIT event listener and link target machine. */
    Listener = new EventListener(NI);
    EE->RegisterJITEventListener(Listener);

#if LLVM_USE_INTEL_JITEVENTS
    IntelJIT = JITEventListener::createIntelJITEventListener();
    EE->RegisterJITEventListener(IntelJIT);
#endif

    /* Ask LLVM to reserve basereg. */
    auto TM = EE->getTargetMachine();
    auto MII = const_cast<MCInstrInfo *>(TM->getMCInstrInfo());
    auto TRI = const_cast<TargetRegisterInfo*>(TM->getRegisterInfo());
    MII->setHQEMUExitAddr((unsigned long)tb_ret_addr);
    TRI->setHQEMUReservedRegs(BaseReg[TCG_AREG0].Name);

    dbg() << DEBUG_LLVM << "LLVM MCJIT initialized.\n";
}
#endif

void IRFactory::DeleteJIT()
{
    EE->UnregisterJITEventListener(Listener);
#if LLVM_USE_INTEL_JITEVENTS
    EE->UnregisterJITEventListener(IntelJIT);
    delete IntelJIT;
#endif
    EE->removeModule(Mod);
    delete Listener;
    delete EE;
    delete Mod;
    EE = nullptr;
}

Function *IRFactory::ResolveFunction(std::string Name)
{
    Function *NF = Mod->getFunction(Name);
    if(NF)
        return NF;

    ValueToValueMapTy VMap;
    Module *InitMod = Translator.getModule();
    Function *F = InitMod->getFunction(Name);
    if (!F)
        IRError("%s: unknown function %s.\n", __func__, Name.c_str());

    NF = Function::Create(cast<FunctionType>(F->getType()->getElementType()),
                          F->getLinkage(), F->getName(), Mod);
    NF->copyAttributesFrom(F);
    VMap[F] = NF;

    if (Helpers.find(Name) != Helpers.end() && !F->isDeclaration()) {
        Function::arg_iterator DestI = NF->arg_begin();
        for (auto J = F->arg_begin(); J != F->arg_end(); ++J) {
            DestI->setName(J->getName());
            VMap[&*J] = &*DestI++;
        }
        SmallVector<ReturnInst*, 8> Returns;
        CloneFunctionInto(NF, F, VMap, /*ModuleLevelChanges=*/true, Returns);
    }

    ClonedFuncs.insert(NF);
    return NF;
}

#else
void IRFactory::CreateJIT()
{
    if (InitOnce)
        return;

    Context = Translator.getContext();
    Mod = Translator.getModule();
    DL = getDataLayout(Mod);

    /* Create JIT execution engine. */
    std::string ErrorMsg, MCPU;
    std::vector<std::string> MAttrs;
    TargetOptions Options;

    setHostAttrs(MCPU, MAttrs, Options);

    EngineBuilder builder(Mod);
    builder.setMCPU(MCPU);
    builder.setMAttrs(MAttrs);
    builder.setAllocateGVsWithCode(false);
    builder.setJITMemoryManager(LLEnv->getMemoryManager().get());
    builder.setErrorStr(&ErrorMsg);
    builder.setEngineKind(EngineKind::JIT);
    builder.setOptLevel(CodeGenOpt::Default);
    builder.setTargetOptions(Options);

    EE = builder.create();

    if (!EE)
        hqemu_error("%s\n", ErrorMsg.c_str());

    /* Create JIT event listener and link target machine. */
    Listener = new EventListener(NI);

    EE->RegisterJITEventListener(Listener);
    EE->DisableLazyCompilation(false);

    /* Ask LLVM to reserve basereg. */
    auto TM = EE->getTargetMachine();
    auto TRI = const_cast<TargetRegisterInfo*>(TM->getRegisterInfo());
    TRI->setHQEMUReservedRegs(BaseReg[TCG_AREG0].Name);

    /* Bind addresses to external symbols. */
    SymbolMap &Symbols = Translator.getSymbols();
    for (auto I = Symbols.begin(), E = Symbols.end(); I != E; ++I) {
        std::string Name = I->first;
        if (!Mod->getNamedValue(Name))
            continue;
        EE->updateGlobalMapping(Mod->getNamedValue(Name), (void*)I->second);
    }

    dbg() << DEBUG_LLVM << "LLVM JIT initialized.\n";

    InitOnce = true;
}

void IRFactory::DeleteJIT()
{
    /* Do nothing with the old JIT. */
}

Function *IRFactory::ResolveFunction(std::string Name)
{
    Function *F = Mod->getFunction(Name);
    if (!F)
        IRError("%s: unknown function %s.\n", __func__, Name.c_str());
    return F;
}
#endif

/*
 * InitializeTypes()
 *  Initialize basic types that will be used during IR conversion.
 */
void IRFactory::InitializeTypes()
{
    VoidTy   = Type::getVoidTy(*Context);
    Int8Ty   = IntegerType::get(*Context, 8);
    Int16Ty  = IntegerType::get(*Context, 16);
    Int32Ty  = IntegerType::get(*Context, 32);
    Int64Ty  = IntegerType::get(*Context, 64);
    Int128Ty = IntegerType::get(*Context, 128);

    IntPtrTy    = DL->getIntPtrType(*Context);
    Int8PtrTy   = Type::getInt8PtrTy(*Context, 0);
    Int16PtrTy  = Type::getInt16PtrTy(*Context, 0);
    Int32PtrTy  = Type::getInt32PtrTy(*Context, 0);
    Int64PtrTy  = Type::getInt64PtrTy(*Context, 0);

    FloatTy  = Type::getFloatTy(*Context);
    DoubleTy = Type::getDoubleTy(*Context);
}

/*
 * getConvertFn()
 *  Get the function pointer of the IR converion routines.
 */
void *IRFactory::getOpcFunc()
{
    return OpcFunc;
}


/*
 * getDefaultCPU
 *  Get the CPU pointer. If the CPU pointer is not at the entry block of
 *  function F, return null.
 */
Instruction *IRFactory::getDefaultCPU(Function &F)
{
    if (!CPU)
        return nullptr;
    if (!CPU->getParent() || CPU->getParent() != &F.getEntryBlock())
        return nullptr;
    return CPU;
}

static inline std::string getGuestSymbol(target_ulong pc)
{
#if defined(CONFIG_USER_ONLY)
    hqemu::MutexGuard locked(llvm_global_lock);

    std::string Symbol = lookup_symbol(pc);
    if (Symbol != "")
        Symbol = "<" + Symbol + ">:";
    return Symbol;
#else
    return "";
#endif
}

/*
 * CreateFunction()
 *  Prepare an LLVM Function and arrange initial BasicBlocks and variable
 *  declaration.
 */
void IRFactory::CreateFunction()
{
    target_ulong pc = Builder->getEntryNode()->getGuestPC();
    std::string Name = getGuestSymbol(pc) +
                       Builder->getPCString(Builder->getEntryNode());

    dbg() << DEBUG_LLVM << "Requested trace info: pc "
          << format("0x%" PRIx, pc) << " length " << Builder->getNumNodes()
          << "\n";

    FunctionType *FuncTy = FunctionType::get(IntPtrTy, false);
    Func = Function::Create(FuncTy, GlobalVariable::ExternalLinkage, Name, Mod);
    Func->setCallingConv(CallingConv::C);
    Func->addFnAttr(Attribute::NoUnwind);
    Func->addFnAttr(Attribute::Naked);

    /* Prepare all basic blocks. */
    InitBB = BasicBlock::Create(*Context, "init", Func);
    ExitBB = BasicBlock::Create(*Context, "exit", Func);
    CurrBB = BasicBlock::Create(*Context, "entry", Func);
    LastInst = BranchInst::Create(CurrBB, InitBB);
    new UnreachableInst(*Context, ExitBB);

    /* Setup base register for CPUArchState pointer.
     * NOTE: Currently only one base register is reserved (CPUArchState). */
    for (int i = 0; i < TCG_TARGET_NB_REGS; i++)
        BaseReg[i].Base = nullptr;

    BaseRegister &CPUReg = BaseReg[TCG_AREG0];
    char Constraint[16] = {'\0'};
    sprintf(Constraint, "={%s}", CPUReg.Name.c_str());
    auto IA = InlineAsm::get(FunctionType::get(Int8PtrTy, false), "",
                             Constraint, true);
    CPUReg.Base = CallInst::Create(IA, "cpu", LastInst);

    CPU = CPUReg.Base;
    CPUStruct = new BitCastInst(CPU, CPUReg.Ty, "cpu.struct", LastInst);
    GEPInsertPos = CPUStruct;
}

/*
 * CreateBlock()
 *  Prepare an LLVM BasicBlock for a new guest block.
 */
void IRFactory::CreateBlock()
{
    GraphNode *CurrNode = Builder->getCurrNode();
    bool isEntryNode = CurrNode == Builder->getEntryNode();
    std::string pc = Builder->getPCString(CurrNode);

    dbg() << DEBUG_LLVM << "  - Process block pc "
          << format("0x%" PRIx, CurrNode->getGuestPC()) << "\n";

    if (!isEntryNode)
        CurrBB = BasicBlock::Create(*Context, pc, Func);

    LastInst = BranchInst::Create(ExitBB, CurrBB);
    Builder->setBasicBlock(CurrNode, CurrBB);

    /* Check if the register has legal type. */
    int NumGlobals = tcg_ctx.nb_globals;
    int NumTemps = tcg_ctx.nb_temps;
    for (int i = 0; i < NumTemps; ++i) {
        TCGTemp *T = &tcg_ctx.temps[i];
        if (T->type != TCG_TYPE_I32 && T->type != TCG_TYPE_I64)
            hqemu_error("unsupported register type.\n");
    }

    /* Initialize global registers. */
    for (int i = 0; i < NumGlobals; ++i) {
        TCGTemp *T = &tcg_ctx.temps[i];
        int State = (T->fixed_reg) ? Register::STATE_REV | Register::STATE_MEM :
                                     Register::STATE_MEM;
        int Size = (T->type == TCG_TYPE_I32) ? 32 : 64;
        Type *Ty = (T->type == TCG_TYPE_I32) ? Int32Ty : Int64Ty;
        Reg[i].reset(State, Size, Ty);
    }

    /* Initialize temporary registers. */
    for (int i = NumGlobals; i < NumTemps; ++i) {
        TCGTemp *T = &tcg_ctx.temps[i];
        int State = (T->temp_local) ? Register::STATE_LOC :
                                      Register::STATE_TMP;
        int Size = (T->type == TCG_TYPE_I32) ? 32 : 64;
        Type *Ty = (T->type == TCG_TYPE_I32) ? Int32Ty : Int64Ty;
        Reg[i].reset(State, Size, Ty);
    }

    Labels.clear();

#ifdef VERIFY_TB
    Function *F = ResolveFunction("helper_verify_tb");
    SmallVector<Value *, 4> Params;
    Params.push_back(CPUStruct);
    Params.push_back(CONST32(CurrNode->getTB()->id));
    CallInst *CI = CallInst::Create(F, Params, "", LastInst);
    MF->setConst(CI);
#endif
}


/*
 * setSuccessor()
 *  Wrapper function to set an unconditional branch.
 */
void IRFactory::setSuccessor(BranchInst *BI, BasicBlock *BB)
{
    BI->setSuccessor(0, BB);
}

/*
 * AnalyzeInlineCost()
 *  The heuristic used to determine if we should inline the helper function
 *  or not.
 */
int IRFactory::AnalyzeInlineCost(CallSite CS)
{
    Function *Callee = CS.getCalledFunction();
    HelperInfo *Helper = Helpers[Callee->getName()];
    int InlineCost = INLINE_THRESHOLD - Helper->Metrics.NumInsts;
    unsigned ArgNo = 0;

    if (Helper->Metrics.NumInsts <= INLINE_INSTCOUNT)
        return 1;

    InlineCost *= InlineConstants::InstrCost;
    for (CallSite::arg_iterator I = CS.arg_begin(), E = CS.arg_end();
         I != E; ++I, ++ArgNo) {
        InlineCost -= InlineConstants::InstrCost;
        if (isa<AllocaInst>(I))
            InlineCost += Helper->ArgumentWeights[ArgNo].AllocaWeight;
        else if (isa<Constant>(I))
            InlineCost += Helper->ArgumentWeights[ArgNo].ConstantWeight;
    }

    return InlineCost;
}


/*
 * ProcessInline()
 *  Perform helper function inlining.
 */
void IRFactory::ProcessInline()
{
    while (!InlineCalls.empty()) {
        CallInst *CI = static_cast<CallInst *>(InlineCalls.back());
        InlineCalls.pop_back();

        InlineFunctionInfo IFI;
        InlineFunction(CI, IFI);
    }
}

void IRFactory::VerifyFunction(Function &F)
{
    if (DM.getDebugMode() & DEBUG_VERIFY)
        verifyFunction(F, &errs());
}

/*
 * PreProcess()
 *  Format function to a legal format and inline calls. Be sure to make the
 *  function in a well form before doing any furthur optimization (i.e.
 *  inlining calls). Otherwise, the optimization may fail or the result may be
 *  wrong.
 */
void IRFactory::PreProcess()
{
    dbg() << DEBUG_LLVM << __func__ << " entered.\n";

    ProcessErase(toErase);

    /* Insert terminator instruction to basic blocks that branch to ExitBB.
     * This could happen when the last TCG opc is a call instruction. */
    for (auto PI = pred_begin(ExitBB), PE = pred_end(ExitBB); PI != PE; PI++) {
        Instruction *InsertPos = (*PI)->getTerminator();
        new UnreachableInst(*Context, InsertPos);
        toErase.push_back(InsertPos);
    }
    ProcessErase(toErase);
    ExitBB->eraseFromParent();

    /* Remove instructions after indirect branches. */
    std::set<Instruction *> AfterIB;
    for (unsigned i = 0, e = IndirectBrs.size(); i != e; ++i) {
        BasicBlock *BB = IndirectBrs[i]->getParent();
        for (auto I = ++BasicBlock::iterator(IndirectBrs[i]), E = BB->end();
             I != E; ++I)
            AfterIB.insert(&*I);
    }
    for (auto I = AfterIB.begin(), E = AfterIB.end(); I != E; ++I)
        toErase.push_back(*I);
    ProcessErase(toErase);

    /* Sink blocks to the end. */
    Function::iterator InsertPos = Func->end();
    Function::BasicBlockListType &Blocks = Func->getBasicBlockList();
    for (unsigned i = 0, e = toSink.size(); i != e; ++i) {
        if (&*InsertPos == toSink[i])
            continue;
        Blocks.splice(InsertPos, Blocks, toSink[i]);
    }

    VerifyFunction(*Func);

    /* Inline helper functions. */
    ProcessInline();

    SmallVector<std::pair<const BasicBlock*,const BasicBlock*>, 32> BackEdges;
    FindFunctionBackedges(*Func, BackEdges);

    TraceInfo *Trace = Builder->getTrace();
    Trace->NumLoop = BackEdges.size();
    dbg() << DEBUG_LLVM << __func__ << ": trace formation with pc "
          << format("0x%" PRIx, Trace->getEntryPC())
          << " length " << Trace->getNumBlock()
          << " is_loop " << (Trace->NumLoop ? true : false) << "\n";

#if 1 || defined(CONFIG_SOFTMMU)
    if (Trace->NumLoop) {
        intptr_t Offset = offsetof(CPUState, tcg_exit_req) - ENV_OFFSET;
        Value *ExitRequestPtr = GetElementPtrInst::CreateInBounds(CPU,
                                        CONSTPtr(Offset),
                                        "", InitBB->getTerminator());
        ExitRequestPtr = new BitCastInst(ExitRequestPtr, Int32PtrTy,
                                        "tcg_exit_req",
                                        InitBB->getTerminator());

        /* Create the exit stub. */
        BasicBlock *TCGExitBB = BasicBlock::Create(*Context, "exit", Func);
        LastInst = BranchInst::Create(TCGExitBB, TCGExitBB);
        StoreInst *SI = new StoreInst(CONST32(0), ExitRequestPtr, true, LastInst);
        InsertExit(0);
        LastInst->eraseFromParent();

        MF->setExit(SI);

        for (unsigned i = 0, e = BackEdges.size(); i != e; ++i) {
            auto BackEdgeBB = const_cast<BasicBlock*>(BackEdges[i].first);
            auto LoopHeader = const_cast<BasicBlock*>(BackEdges[i].second);
            auto BI = const_cast<TerminatorInst *>(BackEdgeBB->getTerminator());

            toErase.push_back(BI);

            Value *ExitRequest = new LoadInst(ExitRequestPtr, "", true, BI);
            Value *Cond = new ICmpInst(BI, ICmpInst::ICMP_EQ, ExitRequest,
                                       CONST32(0), "");
            BI = BranchInst::Create(LoopHeader, TCGExitBB, Cond, BI);
            BI->getParent()->setName("loopback");
            MF->setLoop(BI);
        }
    }
#else
    if (Trace->NumLoop) {
        for (unsigned i = 0, e = BackEdges.size(); i != e; ++i) {
            auto BackEdgeBB = const_cast<BasicBlock*>(BackEdges[i].first);
            auto BI = const_cast<TerminatorInst *>(BackEdgeBB->getTerminator());
            BI->getParent()->setName("loopback");
            MF->setLoop(BI);

            for (auto BI = BackEdgeBB->begin(), BE = BackEdgeBB->end(); BI != BE; ++BI) {
                if (auto SI = dyn_cast<StoreInst>(BI)) {
                    intptr_t Off = 0;
                    Value *Base = getBaseWithConstantOffset(DL, getPointerOperand(SI), Off);
                    if (Base == CPU && isStateOfPC(Off))
                        toErase.push_back(SI);
                }
            }
        }
    }
#endif

    ProcessErase(toErase);

    if (DM.getDebugMode() & DEBUG_IR) {
        hqemu::MutexGuard locked(llvm_debug_lock);
        Func->dump();
    }
}

void IRFactory::InitializeLLVMPasses(legacy::FunctionPassManager *FPM)
{
    auto TM = EE->getTargetMachine();
#if defined(LLVM_V35)
    TM->addAnalysisPasses(*FPM);
    FPM->add(new DataLayoutPass(Mod));
    FPM->add(createBasicTargetTransformInfoPass(TM));
#else
    PassRegistry &PassReg = *PassRegistry::getPassRegistry();
    initializeTargetTransformInfoWrapperPassPass(PassReg);

    FPM->add(createTargetTransformInfoWrapperPass(TM->getTargetIRAnalysis()));
#endif
}

void IRFactory::Optimize()
{
#define addPass(PM, P) do { PM->add(P); } while(0)
#define addPassOptional(PM, P, Disable) \
    do { \
        if (!Disable) PM->add(P); \
    } while(0)

#if defined(ENABLE_PASSES)
    if (runPasses) {
        legacy::FunctionPassManager *FPM = new legacy::FunctionPassManager(Mod);

        InitializeLLVMPasses(FPM);

        addPass(FPM, createProfileExec(this));
        addPass(FPM, createCombineGuestMemory(this));
        addPass(FPM, createCombineZExtTrunc());
        addPassOptional(FPM, createStateMappingPass(this), DisableStateMapping);
        addPass(FPM, createPromoteMemoryToRegisterPass());
        addPass(FPM, createCombineCasts(this));
        addPassOptional(FPM, createSimplifyPointer(this), !EnableSimplifyPointer);
        addPass(FPM, createAggressiveDCEPass());
        addPass(FPM, createCFGSimplificationPass());
        addPass(FPM, createInstructionCombiningPass());

        FPM->run(*Func);

        delete FPM;
    }
#endif

#undef addPass
#undef addPassOptional
}


/*
 * PostProcess()
 *  Legalize LLVM IR after running the pre-defined passes.
 */
void IRFactory::PostProcess()
{
    dbg() << DEBUG_LLVM << __func__ << " entered.\n";

#if defined(ENABLE_MCJIT)
    for (auto I = ClonedFuncs.begin(), E = ClonedFuncs.end(); I != E; ++I) {
        Function *F = *I;
        if (!F->isDeclaration())
            F->removeFromParent();
    }
    /* Bind addresses to external symbols. */
    SymbolMap &Symbols = Translator.getSymbols();
    for (auto I = Symbols.begin(), E = Symbols.end(); I != E; ++I) {
        std::string Name = I->first;
        if (!Mod->getNamedValue(Name))
            continue;
        EE->updateGlobalMapping(Mod->getNamedValue(Name), (void*)I->second);
    }
#endif

    if (DM.getDebugMode() & DEBUG_IR_OPT) {
        hqemu::MutexGuard locked(llvm_debug_lock);
        Func->dump();
    }
}

/*
 * PostProcess()
 *  Legalize LLVM IR after running the pre-defined passes.
 */
void IRFactory::FinalizeObject()
{
    dbg() << DEBUG_LLVM << __func__ << " entered.\n";

    uintptr_t Code = (uintptr_t)NI.Code;
    uint32_t Size = NI.Size;

#if defined(ENABLE_MCJIT)
    for (unsigned i = 0, e = NI.Patches.size(); i != e; ++i) {
        NotifyInfo::PatchInfo &Patch = NI.Patches[i];
        uintptr_t Addr = Patch.Addr;
        code_ostream OS(Addr);

        /* If the address to patch is outside this code region, skip this
         * invalid patch point. Actually this should not happen, but LLVM v35
         * seems to report such invalid address. */
        if (Addr >= Code + Size)
            continue;
        if (Patch.Type == PATCH_EXIT_TB) {
#if defined(LLVM_V35)
            EmitByte(OS, 0xE9);
            EmitConstant(OS, (uintptr_t)tb_ret_addr - Addr - 5, 4);
#endif
        } else if (Patch.Type == PATCH_TRACE_BLOCK_CHAINING) {
            unsigned NumSkip = 3 - Addr % 4;
            OS.Skip(NumSkip);
            EmitByte(OS, 0xE9);
            EmitConstant(OS, 0, 4);

            NI.ChainSlot[Patch.Idx].Addr = Addr + NumSkip;
        }
    }
#endif

    /* Flush instruction cache */
    flush_icache_range(Code, Code + Size);

    if (DM.getDebugMode() & DEBUG_OUTASM) {
        hqemu::MutexGuard locked(llvm_debug_lock);
        if (HostDisAsm)
            HostDisAsm->PrintOutAsm((uint64_t)Code, (uint64_t)Size);
        else {
            auto &OS = DM.debug();
            OS << "\nOUT: [size=" << Size << "]\n";
            disas(stderr, (void *)Code, Size);
            OS << "\n";
        }
    }
}

/*
 * Compile()
 *  Start the LLVM JIT compilation.
 */
void IRFactory::Compile()
{
    dbg() << DEBUG_LLVM
          << "Translator " << Translator.getID() << " starts compiling...\n";

    /* Run optimization passes. */
    PreProcess();
    Optimize();
    PostProcess();

    VerifyFunction(*Func);

    /* JIT. */
    NI.Func = Func;
    EE->getPointerToFunction(Func);
    EE->finalizeObject();

    FinalizeObject();

    dbg() << DEBUG_LLVM << __func__ << ": done.\n";
}

PointerType *IRFactory::getPointerTy(int Size, unsigned AS)
{
    switch (Size) {
    case 32: return Type::getInt32PtrTy(*Context, AS);
    case 64: return Type::getInt64PtrTy(*Context, AS);
    case 16: return Type::getInt16PtrTy(*Context, AS);
    case 8:  return Type::getInt8PtrTy(*Context, AS);
    default:
         IRError("%s: invalid bit type %d.\n", __func__, Size);
    }
    return nullptr;
}

Value *IRFactory::getExtendValue(Value *V, Type *Ty, int opc)
{
    int OldSize = DL->getTypeSizeInBits(V->getType());
    int NewSize = DL->getTypeSizeInBits(Ty);

    if (OldSize > NewSize)
        IRError("%s: invalid size old=%d new=%d\n", __func__, OldSize, NewSize);
    if (OldSize == NewSize)
        return V;

    if (opc & MO_SIGN)
        return SEXT(V, Ty);
    return ZEXT(V, Ty);
}

Value *IRFactory::getTruncValue(Value *V, int opc)
{
    int OldSize = DL->getTypeSizeInBits(V->getType());
    int NewSize = getSizeInBits(opc);

    if (OldSize < NewSize)
        IRError("%s: invalid size old=%d new=%d\n", __func__, OldSize, NewSize);
    if (OldSize == NewSize)
        return V;

    Type *Ty = Type::getIntNTy(*Context, NewSize);
    return TRUNC(V, Ty);
}

Value *IRFactory::ConvertEndian(Value *V, int opc)
{
#ifdef NEED_BSWAP
    switch (opc & MO_SIZE) {
    case MO_8:  return V;
    case MO_16: return BSWAP16(V);
    case MO_32: return BSWAP32(V);
    case MO_64: return BSWAP64(V);
    default:
        IRError("%s: invalid size (opc=%d)\n", __func__, opc);
        break;
    }
    return V;
#else
    return V;
#endif
}

Value *IRFactory::CreateBSwap(Type *Ty, Value *V, Instruction *InsertPos)
{
    SmallVector<Value *, 4> Params;
    Type *Tys[] = { Ty };

    Function *Fn = Intrinsic::getDeclaration(Mod, Intrinsic::bswap, Tys);
    Params.push_back(V);
    return CallInst::Create(Fn, Params, "", InsertPos);
}

Value *IRFactory::ConvertCPUType(Function *F, int Idx, Instruction *InsertPos)
{
    Type *ParamTy = F->getFunctionType()->getParamType(Idx);
    if (CPUStruct->getType() != ParamTy)
        return new BitCastInst(CPU, ParamTy, "", InsertPos);
    return CPUStruct;
}

Value *IRFactory::ConvertCPUType(Function *F, int Idx, BasicBlock *InsertPos)
{
    Type *ParamTy = F->getFunctionType()->getParamType(Idx);
    if (CPUStruct->getType() != ParamTy)
        return new BitCastInst(CPU, ParamTy, "", InsertPos);
    return CPUStruct;
}

/*
 * isStateOfPC()
 *  Return true if the offset is for the state of PC.
 */
bool IRFactory::isStateOfPC(intptr_t Off)
{
    intptr_t IPOffset;
#if defined(TARGET_ALPHA)
    IPOffset = offsetof(CPUArchState, pc);
#elif defined(TARGET_AARCH64)
    IPOffset = offsetof(CPUArchState, pc);
#elif defined(TARGET_ARM)
    IPOffset = offsetof(CPUArchState, regs[15]);
#elif defined(TARGET_CRIS)
    IPOffset = offsetof(CPUArchState, pc);
#elif defined(TARGET_I386)
    IPOffset = offsetof(CPUArchState, eip);
#elif defined(TARGET_M68K)
    IPOffset = offsetof(CPUArchState, pc);
#elif defined(TARGET_MICROBLAZE)
    IPOffset = offsetof(CPUArchState, sregs[0]);
#elif defined(TARGET_MIPS)
    IPOffset = offsetof(CPUArchState, active_tc.PC);
#elif defined(TARGET_PPC)
    IPOffset = offsetof(CPUArchState, nip);
#elif defined(TARGET_SH4)
    IPOffset = offsetof(CPUArchState, pc);
#elif defined(TARGET_SPARC)
    intptr_t IPOffset2;
    IPOffset = offsetof(CPUArchState, pc);
    IPOffset2 = offsetof(CPUArchState, npc);
#else
#error "unsupported processor type"
#endif

#if defined(TARGET_ALPHA) || defined(TARGET_ARM) || defined(TARGET_AARCH64)   || \
    defined(TARGET_CRIS) || defined(TARGET_I386) || defined(TARGET_M68K)      || \
    defined(TARGET_MICROBLAZE) || defined(TARGET_MIPS) || defined(TARGET_PPC) || \
    defined(TARGET_SH4)
    return (Off >= IPOffset && Off < IPOffset + TARGET_LONG_SIZE);
#elif defined(TARGET_SPARC)
    return ((Off >= IPOffset && Off < IPOffset + TARGET_LONG_SIZE) ||
            (Off >= IPOffset2 && Off < IPOffset2 + TARGET_LONG_SIZE));
#endif
}

/*
 * CreateStorePC()
 *  Trace building requires store IP instruction to link basic blocks.
 *  But in some archirecture, IP is promoted to register and we need to
 *  regenerate the store IP instruction.
 */
void IRFactory::CreateStorePC(Instruction *InsertPos)
{
    for (int i = 0, e = tcg_ctx.nb_globals; i != e; ++i) {
        Register &reg = Reg[i];
        if (reg.isReg() && reg.isDirty()) {
            if (isStateOfPC(reg.Off)) {
                StoreState(reg, InsertPos);
                reg.Demote();
            }
        }
    }
}

/*
 * SaveGlobals()
 *  Store dirty states back to CPUArchState in memory.
 */
void IRFactory::SaveGlobals(int level, Instruction *InsertPos)
{
    if (level == COHERENCE_NONE)
        return;

    int NumGlobals = tcg_ctx.nb_globals;
    int NumTemps = tcg_ctx.nb_temps;
    for (int i = 0; i < NumGlobals; ++i) {
        Register &reg = Reg[i];
        if (reg.isReg() && reg.isDirty())
            StoreState(reg, InsertPos);
        reg.Demote();
    }

    if (level == COHERENCE_GLOBAL)
        return;

    /* Store local registers to stack. */
    for (int i = NumGlobals; i < NumTemps; ++i) {
        Register &reg = Reg[i];
        if (reg.isReg() && reg.isLocal() && reg.isDirty())
            StoreState(reg, InsertPos);
        reg.Demote();
    }
}

/*
 * StatePointer()
 *  Get or insert the pointer to the CPU register in the AddrCache.
 */
Value *IRFactory::StatePointer(Register &reg)
{
    intptr_t Off = reg.Off;
    PointerType *PTy = (reg.Size == 32) ? Int32PtrTy : Int64PtrTy;
    std::pair<intptr_t, Type *> Key(Off, PTy);
    if (StatePtr.find(Key) == StatePtr.end()) {
        std::string Name = isStateOfPC(Off) ? "pc" : reg.Name;
        auto GEP = GetElementPtrInst::CreateInBounds(BaseReg[reg.Base].Base,
                                             CONSTPtr(Off), "", GEPInsertPos);
        StatePtr[Key] = new BitCastInst(GEP, PTy, Name, InitBB->getTerminator());
    }
    return StatePtr[Key];
}

Value *IRFactory::StatePointer(Register &reg, intptr_t Off, Type *PTy)
{
    if (!reg.isRev())
        IRError("%s: internal error.\n", __func__);

    std::pair<intptr_t, Type *> Key(Off, PTy);
    if (StatePtr.find(Key) == StatePtr.end()) {
        std::string Name = isStateOfPC(Off) ? "pc" : "";
        auto GEP = GetElementPtrInst::CreateInBounds(BaseReg[reg.Base].Base,
                                             CONSTPtr(Off), "", GEPInsertPos);
        StatePtr[Key] = new BitCastInst(GEP, PTy, Name, InitBB->getTerminator());
    }
    return StatePtr[Key];
}

/*
 * LoadState()
 *  Retrieve value from CPUArchState.
 */
Value *IRFactory::LoadState(Register &reg)
{
    if (reg.isRev())
        return BaseReg[reg.Base].Base;
    if (reg.isAlias())
        return LoadState(reg.getAlias());
    if (reg.isReg())
        return reg.getData();
    if (reg.isLocal()) {
        if (!reg.AI)
            reg.AI = new AllocaInst(reg.Ty, "loc", InitBB->getTerminator());
        return new LoadInst(reg.AI, "", false, LastInst);
    }

    /* If we go here, the state is not loaded into a LLVM virtual register.
     * Load it from CPUArchState. */
    Value *V = new LoadInst(StatePointer(reg), "", false, LastInst);
    reg.setData(V);

    return V;
}

void IRFactory::StoreState(Register &reg, Instruction *InsertPos)
{
#ifdef ASSERT
    int Size = DL->getTypeSizeInBits(reg.getData()->getType());
    if (Size != reg.Size)
        IRError("%s: internal error\n", __func__);
#endif
    if (reg.isRev())
        IRError("%s: fatal error\n", __func__);
    if (reg.isLocal()) {
        if (!reg.AI)
            reg.AI = new AllocaInst(reg.Ty, "loc", InitBB->getTerminator());
        new StoreInst(reg.getData(), reg.AI, false, InsertPos);
    } else {
        bool Volatile = isStateOfPC(reg.Off);
        new StoreInst(reg.getData(), StatePointer(reg), Volatile, InsertPos);
    }
}


/*
 * TCG opcode to LLVM IR translation functions.
 */
void IRFactory::op_trace_prolog(const TCGArg *args)
{
    IRDebug(INDEX_op_trace_prolog);
}

void IRFactory::op_trace_epilog(const TCGArg *args)
{
    IRDebug(INDEX_op_trace_epilog);
}

void IRFactory::op_jmp(const TCGArg *args)
{
    IRDebug(INDEX_op_jmp);

    Register &In = Reg[args[0]];
    Value *InData = LoadState(In);

    SaveGlobals(COHERENCE_ALL, LastInst);
    if (!InData->getType()->isPointerTy())
        InData = ITP8(InData);

    IndirectBrInst *IB = IndirectBrInst::Create(InData, 1, LastInst);
    MF->setExit(IB);
}

/*
 * op_discard()
 *  args[0]: In
 */
void IRFactory::op_discard(const TCGArg *args)
{
    IRDebug(INDEX_op_discard);
    Register &In = Reg[args[0]];
    if (In.isReg())
        In.Demote();
}

/*
 * op_set_label()
 *  args[0]: Label number
 */
void IRFactory::op_set_label(const TCGArg *args)
{
    IRDebug(INDEX_op_set_label);

    SaveGlobals(COHERENCE_ALL, LastInst);

    TCGArg label = args[0];
    if (Labels.find(label) == Labels.end())
        Labels[label] = BasicBlock::Create(*Context, "true_dest", Func);

    CurrBB = Labels[label];
    if (LastInst) {
        if (LastInst != &*LastInst->getParent()->begin() &&
            isa<IndirectBrInst>(--BasicBlock::iterator(LastInst)))
            LastInst->eraseFromParent();
        else
            setSuccessor(LastInst, CurrBB);
    }

    LastInst = BranchInst::Create(ExitBB, CurrBB);
}

/*
 * op_call()
 *  args[0]                       : [nb_oargs:16][nb_iargs:16]
 *  args[1~#nb_oargs]             : out args
 *  args[1+#nb_oargs~#nb_iargs-2] : function parameters
 *  args[1+#nb_oargs+#nb_iargs-1] : function address
 *  args[1+#nb_oargs+#nb_iargs]   : flags
 */
void IRFactory::op_call(const TCGArg *args)
{
    IRDebug(INDEX_op_call);

    TCGOp * const op = NI.Op;
    int nb_oargs = op->callo;
    int nb_iargs = op->calli;
    int nb_params = nb_iargs;
    tcg_insn_unit *func_addr = (tcg_insn_unit *)(intptr_t)args[nb_oargs + nb_iargs];
    int flags = args[nb_oargs + nb_iargs + 1];
    SmallVector<Value *, 4> Params;

    /* If the called function is an illegal helper, skip this trace. */
    if (isIllegalHelper((void *)func_addr))
        runPasses = false;

    /* Get function declaration from LLVM module. */
    TCGHelperMap &TCGHelpers = Translator.getTCGHelpers();
    if (TCGHelpers.find((uintptr_t)func_addr) == TCGHelpers.end())
        IRError("%s: cannot resolve funtion.\n", __func__);

    std::string FName = TCGHelpers[(uintptr_t)func_addr];
    Function *F = ResolveFunction(FName);

    std::set<std::string> &ConstHelpers = Translator.getConstHelpers();
    if (ConstHelpers.find(FName) != ConstHelpers.end())
        flags |= TCG_CALL_NO_READ_GLOBALS;

    /* Package the function parameters.
       NOTE: There are situations where the numbers of given arguments
       are greater than the *real* function parameters. Ex:
           declare void foo(int64, int64);
              and
           call foo(int32, int32, int32, int32);
     */
    int real_nb_params = F->getFunctionType()->getNumParams();
    if (nb_params == real_nb_params) {
        for (int i = 0; i < real_nb_params; ++i) {
            Type *ParamTy = F->getFunctionType()->getParamType(i);
            Register &In = Reg[args[nb_oargs + i]];
            Value *InData = LoadState(In);

            size_t real_size = DL->getTypeSizeInBits(ParamTy);
            size_t size = DL->getTypeSizeInBits(InData->getType());

            if (ParamTy->isPointerTy() && !InData->getType()->isPointerTy())
                InData = ITP8(InData);
            else if (real_size < size)
                InData = TRUNC(InData, IntegerType::get(*Context, real_size));

            if (InData->getType() != ParamTy)
                InData = new BitCastInst(InData, ParamTy, "", LastInst);
            Params.push_back(InData);
        }
    } else {
        int idx = 0;
        for (int i = 0; i < real_nb_params; ++i) {
            Value *V = nullptr;
            Type *ParamTy = F->getFunctionType()->getParamType(i);
            size_t real_size = DL->getTypeSizeInBits(ParamTy);
            size_t size, remain = real_size;

next:
            Register &In = Reg[args[nb_oargs + idx]];
            Value *InData = LoadState(In);

            size = DL->getTypeSizeInBits(InData->getType());
            if (size == real_size) {
                if (InData->getType() != ParamTy)
                    InData = new BitCastInst(InData, ParamTy, "", LastInst);
                Params.push_back(InData);
                idx++;
            } else {
                if (remain == real_size)
                    V = ZEXT(InData, IntegerType::get(*Context, real_size));
                else {
                    InData = ZEXT(InData, ParamTy);
                    InData = SHL(InData, ConstantInt::get(ParamTy, real_size-remain));
                    V = OR(V, InData);
                }

                if (remain < size)
                    IRError("%s: fatal error.\n", __func__);

                remain -= size;
                idx++;

                if (remain)
                    goto next;

                Params.push_back(V);
            }
        }

        if (idx != nb_params)
            IRError("%s: num params not matched.\n", __func__);
    }


    /* Save global registers if this function is not TCG constant function.
       Otherwise, mark this call instruction for state mapping use.
       The rules can be found in tcg_reg_alloc_call() in tcg/tcg.c */
    if (!(flags & TCG_CALL_NO_READ_GLOBALS))
        SaveGlobals(COHERENCE_GLOBAL, LastInst);

    /* handle COREMU's lightweight memory transaction helper */
    if (isLMTFunction(FName)) {
        uint32_t Idx = NI.setRestorePoint();
        Value *ResVal = GetElementPtrInst::CreateInBounds(CPU,
                CONSTPtr(offsetof(CPUArchState, restore_val)), "", LastInst);
        ResVal = new BitCastInst(ResVal, Int32PtrTy, "", LastInst);
        new StoreInst(CONST32(Idx), ResVal, true, LastInst);
    }

    CallInst *CI = CallInst::Create(F, Params, "", LastInst);

    if (flags & TCG_CALL_NO_READ_GLOBALS)
        MF->setConst(CI);

    /* Determine if this function can be inlined. */
    if (Helpers.find(FName) != Helpers.end()) {
        bool MustInline = false;
        HelperInfo *Helper = Helpers[FName];
        if (AnalyzeInlineCost(CallSite(CI)) > 0) {
            MustInline = true;
            InlineCalls.push_back(CI);
        }

        if (!MustInline) {
            Function *NoInlineF = ResolveFunction(Helper->FuncNoInline->getName());
            CI->setCalledFunction(NoInlineF);
        }
    }

    /* Format the return value.
       NOTE: There are situations where the return value is split and
       is used by different instructions. Ex:
           int64 ret = call foo();
           ... = opcode ret[0..31];
           ... = opcode ret[32..64];
     */
    if (nb_oargs == 1) {
        Register &Out = Reg[args[0]];
        Out.setData(CI, true);
    } else if (nb_oargs > 1) {
        Value *V = CI;
        size_t size = DL->getTypeSizeInBits(F->getReturnType());
        size_t subsize = size / nb_oargs;
        for (int i = 0; i < nb_oargs; ++i) {
            Register &Out = Reg[args[i]];
            Value *OutData = TRUNC(V, IntegerType::get(*Context, subsize));
            Out.setData(OutData, true);
            if (i != nb_oargs - 1)
                V = LSHR(V, ConstantInt::get(IntegerType::get(*Context, size), subsize));
        }
    }
}

/*
 * op_br()
 *  args[0]: Label number
 */
void IRFactory::op_br(const TCGArg *args)
{
    IRDebug(INDEX_op_br);

    SaveGlobals(COHERENCE_ALL, LastInst);

    TCGArg label = args[0];
    if (Labels.find(label) == Labels.end())
        Labels[label] = BasicBlock::Create(*Context, "direct_jump_tb", Func);

    setSuccessor(LastInst, Labels[label]);
    LastInst = nullptr;
}

/*
 * op_mov_i32()
 *  args[0]: Out
 *  args[1]: In
 */
void IRFactory::op_mov_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_mov_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 32);

    Value *InData = LoadState(In);

    size_t Size = DL->getTypeSizeInBits(InData->getType());
    if (Size != 32)
        InData = TRUNC32(InData);

    Out.setData(InData, true);
}

/*
 * op_movi_i32()
 *  args[0]: Out
 *  args[1]: In  (const value)
 */
void IRFactory::op_movi_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_movi_i32);

    Register &Out = Reg[args[0]];

    AssertType(Out.Size == 32);

    Out.setData(CONST32(args[1]), true);
}

static inline CmpInst::Predicate getPred(const TCGArg cond)
{
    CmpInst::Predicate pred = ICmpInst::BAD_ICMP_PREDICATE;
    switch (cond) {
        case TCG_COND_EQ: pred = ICmpInst::ICMP_EQ;  break;
        case TCG_COND_NE: pred = ICmpInst::ICMP_NE;  break;
        case TCG_COND_LT: pred = ICmpInst::ICMP_SLT; break;
        case TCG_COND_GE: pred = ICmpInst::ICMP_SGE; break;
        case TCG_COND_LE: pred = ICmpInst::ICMP_SLE; break;
        case TCG_COND_GT: pred = ICmpInst::ICMP_SGT; break;
        /* unsigned */
        case TCG_COND_LTU: pred = ICmpInst::ICMP_ULT; break;
        case TCG_COND_GEU: pred = ICmpInst::ICMP_UGE; break;
        case TCG_COND_LEU: pred = ICmpInst::ICMP_ULE; break;
        case TCG_COND_GTU: pred = ICmpInst::ICMP_UGT; break;
        default:
            IRError("%s - unsupported predicate\n", __func__);
    }
    return pred;
}

/*
 * op_setcond_i32()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 *  args[3]: In3 (condition code)
 */
void IRFactory::op_setcond_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_setcond_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];
    CmpInst::Predicate Pred = getPred(args[3]);

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = ICMP(InData1, InData2, Pred);
    OutData = ZEXT32(OutData);
    Out.setData(OutData, true);
}

/*
 * op_movcond_i32()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 *  args[3]: In3
 *  args[4]: In4
 *  args[5]: In5 (condition code)
 */
void IRFactory::op_movcond_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_movcond_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];
    Register &In3 = Reg[args[3]];
    Register &In4 = Reg[args[4]];
    CmpInst::Predicate Pred = getPred(args[5]);

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32 &&
               In3.Size == 32 && In4.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *InData3 = LoadState(In3);
    Value *InData4 = LoadState(In4);
    Value *Cond = ICMP(InData1, InData2, Pred);
    Value *OutData = SelectInst::Create(Cond, InData3, InData4, "", LastInst);
    Out.setData(OutData, true);
}

/* load/store */
/*
 * op_ld8u_i32()
 *  args[0]: Out (ret)
 *  args[1]: In1 (addr)
 *  args[2]: In2 (offset)
 */
void IRFactory::op_ld8u_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_ld8u_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(Out.Size == 32);

    Value *InData = LoadState(In);
    if (InData->getType() != Int8PtrTy)
        InData = CASTPTR8(InData);

    InData = GetElementPtrInst::CreateInBounds(InData, CONSTPtr(Off), "", LastInst);
    InData = new LoadInst(InData, "", false, LastInst);
    InData = ZEXT32(InData);
    Out.setData(InData, true);
}

void IRFactory::op_ld8s_i32(const TCGArg *args)
{
    IRError("%s: test me.\n", __func__);
    IRDebug(INDEX_op_ld8s_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(Out.Size == 32);

    Value *InData = LoadState(In);
    if (InData->getType() != Int8PtrTy)
        InData = CASTPTR8(InData);

    InData = GetElementPtrInst::CreateInBounds(InData, CONSTPtr(Off), "", LastInst);
    InData = new LoadInst(InData, "", false, LastInst);
    InData = SEXT32(InData);
    Out.setData(InData, true);
}

void IRFactory::op_ld16u_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_ld16u_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(Out.Size == 32);

    Value *InData = LoadState(In);
    if (InData->getType() != Int8PtrTy)
        InData = CASTPTR8(InData);

    InData = GetElementPtrInst::CreateInBounds(InData, CONSTPtr(Off), "", LastInst);
    InData = CASTPTR16(InData);
    InData = new LoadInst(InData, "", false, LastInst);
    InData = ZEXT32(InData);
    Out.setData(InData, true);
}

void IRFactory::op_ld16s_i32(const TCGArg *args)
{
    IRError("%s: test me.\n", __func__);
    IRDebug(INDEX_op_ld16s_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(Out.Size == 32);

    Value *InData = LoadState(In);
    if (InData->getType() != Int8PtrTy)
        InData = CASTPTR8(InData);

    InData = GetElementPtrInst::CreateInBounds(InData, CONSTPtr(Off), "", LastInst);
    InData = CASTPTR16(InData);
    InData = new LoadInst(InData, "", false, LastInst);
    InData = SEXT32(InData);
    Out.setData(InData, true);
}

/*
 * op_ld_i32()
 *  args[0]: Out (ret)
 *  args[1]: In1 (addr)
 *  args[2]: In2 (offset)
 */
void IRFactory::op_ld_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_ld_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(Out.Size == 32);

    Value *InData;
    if (In.isRev()) {
        InData = StatePointer(In, Off, Int32PtrTy);
        InData = new LoadInst(InData, "", false, LastInst);
        if (isStateOfPC(Off))
            static_cast<LoadInst*>(InData)->setVolatile(true);
    } else {
        InData = LoadState(In);
        if (InData->getType() != Int8PtrTy)
            InData = CASTPTR8(InData);
        InData = GetElementPtrInst::CreateInBounds(InData, CONSTPtr(Off), "", LastInst);
        InData = CASTPTR32(InData);
        InData = new LoadInst(InData, "", false, LastInst);
    }
    Out.setData(InData, true);
}

void IRFactory::op_st8_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_st8_i32);

    Register &In1 = Reg[args[0]];
    Register &In2 = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(In1.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    InData1 = TRUNC8(InData1);
    if (InData2->getType() != Int8PtrTy)
        InData2 = CASTPTR8(InData2);
    InData2 = GetElementPtrInst::CreateInBounds(InData2, CONSTPtr(Off), "", LastInst);
    new StoreInst(InData1, InData2, false, LastInst);
}

void IRFactory::op_st16_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_st16_i32);

    Register &In1 = Reg[args[0]];
    Register &In2 = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(In1.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    InData1 = TRUNC16(InData1);
    if (InData2->getType() != Int8PtrTy)
        InData2 = CASTPTR8(InData2);
    InData2 = GetElementPtrInst::CreateInBounds(InData2, CONSTPtr(Off), "", LastInst);
    InData2 = CASTPTR16(InData2);
    new StoreInst(InData1, InData2, false, LastInst);
}

/*
 * op_st_i32()
 *  args[0]: In1
 *  args[1]: In2 (base)
 *  args[2]: In3 (offset)
 */
void IRFactory::op_st_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_st_i32);

    Register &In1 = Reg[args[0]];
    Register &In2 = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(In1.Size == 32);

    Value *InData1 = LoadState(In1);

    if (In2.isRev()) {
        Value *InData2 = StatePointer(In2, Off, Int32PtrTy);
        StoreInst *SI = new StoreInst(InData1, InData2, false, LastInst);
        if (isStateOfPC(Off))
            SI->setVolatile(true);
    } else {
        Value *InData2 = LoadState(In2);
        if (InData2->getType() != Int8PtrTy)
            InData2 = CASTPTR8(InData2);
        InData2 = GetElementPtrInst::CreateInBounds(InData2, CONSTPtr(Off), "", LastInst);
        InData2 = CASTPTR32(InData2);
        new StoreInst(InData1, InData2, false, LastInst);
    }
}

/* arith */
/*
 * op_add_i32()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 */
void IRFactory::op_add_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_add_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData;
    if (In1.isRev()) {
        intptr_t Off = static_cast<ConstantInt*>(InData2)->getSExtValue();
        OutData = StatePointer(In1, Off, Int32PtrTy);
    } else
        OutData = ADD(InData1, InData2);

    Out.setData(OutData, true);
}

/*
 * op_sub_i32()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 */
void IRFactory::op_sub_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_sub_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = SUB(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_mul_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_mul_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = MUL(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_div_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_div_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = SDIV(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_divu_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_divu_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = UDIV(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_rem_i32(const TCGArg *args)
{
    IRError("%s: test me.\n", __func__);
    IRDebug(INDEX_op_rem_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = SREM(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_remu_i32(const TCGArg *args)
{
    IRError("%s: test me.\n", __func__);
    IRDebug(INDEX_op_remu_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = UREM(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_div2_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_div2_i32);

    Register &Out1 = Reg[args[0]];
    Register &Out2 = Reg[args[1]];
    Register &In1 = Reg[args[2]];
#if 0
    Register &In2 = Reg[args[3]];
#endif
    Register &In3 = Reg[args[4]];

    AssertType(Out1.Size == 32 && Out2.Size == 32 &&
               In1.Size == 32 && In3.Size == 32);

    Value *InData1 = LoadState(In1);
#if 0
    Value *InData2 = LoadState(In2);
#endif
    Value *InData3 = LoadState(In3);
    Value *OutData1 = SDIV(InData1, InData3);
    Value *OutData2 = SREM(InData1, InData3);
    Out1.setData(OutData1, true);
    Out2.setData(OutData2, true);
}

/*
 * op_divu2_i32()
 *  args[0]: Out1
 *  args[1]: Out2
 *  args[2]: In1
 *  args[3]: In2
 *  args[4]: In3
 */
void IRFactory::op_divu2_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_divu2_i32);

    Register &Out1 = Reg[args[0]];
    Register &Out2 = Reg[args[1]];
    Register &In1 = Reg[args[2]];
#if 0
    Register &In2 = Reg[args[3]];
#endif
    Register &In3 = Reg[args[4]];

    AssertType(Out1.Size == 32 && Out2.Size == 32 &&
               In1.Size == 32 && In3.Size == 32);

    Value *InData1 = LoadState(In1);
#if 0
    Value *InData2 = LoadState(In2);
#endif
    Value *InData3 = LoadState(In3);
    Value *OutData1 = UDIV(InData1, InData3);
    Value *OutData2 = UREM(InData1, InData3);
    Out1.setData(OutData1, true);
    Out2.setData(OutData2, true);
}

/*
 * op_and_i32()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 */
void IRFactory::op_and_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_and_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = AND(InData1, InData2);
    Out.setData(OutData, true);
}

/*
 * op_or_i32()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 */
void IRFactory::op_or_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_or_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = OR(InData1, InData2);
    Out.setData(OutData, true);
}

/*
 * op_xor_i32()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 */
void IRFactory::op_xor_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_xor_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = XOR(InData1, InData2);
    Out.setData(OutData, true);
}

/* shifts/rotates */
/*
 * op_shl_i32()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 */
void IRFactory::op_shl_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_shl_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = SHL(InData1, InData2);
    Out.setData(OutData, true);
}

/*
 * op_shr_i32()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 */
void IRFactory::op_shr_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_shr_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = LSHR(InData1, InData2);
    Out.setData(OutData, true);
}

/*
 * op_sar_i32()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 */
void IRFactory::op_sar_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_sar_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = ASHR(InData1, InData2);
    Out.setData(OutData, true);
}

/*
 * op_rotl_i32()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 */
void IRFactory::op_rotl_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_rotl_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    Value *C = LSHR(InData1, SUB(CONST32(32), InData2));
    Value *OutData = SHL(InData1, InData2);
    OutData = OR(OutData, C);
    Out.setData(OutData, true);
}

void IRFactory::op_rotr_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_rotr_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    Value *c = SHL(InData1, SUB(CONST32(32), InData2));
    Value *OutData = LSHR(InData1, InData2);
    OutData = OR(OutData, c);
    Out.setData(OutData, true);
}

/*
 * op_deposit_i32()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 *  args[3]: In3 (offset from LSB)
 *  args[4]: In4 (length)
 */
void IRFactory::op_deposit_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_deposit_i32);

    /* Deposit the lowest args[4] bits of register args[2] into register
     * args[1] starting from bits args[3]. */
    APInt mask = APInt::getBitsSet(32, args[3], args[3] + args[4]);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    if (args[3])
        InData2 = SHL(InData2, CONST32(args[3]));
    InData2 = AND(InData2, ConstantInt::get(*Context, mask));
    InData1 = AND(InData1, ConstantInt::get(*Context, ~mask));
    InData1 = OR(InData1, InData2);
    Out.setData(InData1, true);
}

/*
 * op_brcond_i32()
 *  args[0]: In1
 *  args[1]: In2
 *  args[2]: In3 (condition code)
 *  args[3]: In4 (label)
 */
void IRFactory::op_brcond_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_brcond_i32);

    /* brcond_i32 format:
     *  brcond_i32 op1,op2,cond,<ifTrue>
     *  <ifFalse>:
     *      A
     *  <ifTrue>:
     *      B
     */
    Register &In1 = Reg[args[0]];
    Register &In2 = Reg[args[1]];
    CmpInst::Predicate Pred = getPred(args[2]);

    AssertType(In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    SaveGlobals(COHERENCE_ALL, LastInst);

    TCGArg label = args[3];
    if (Labels.find(label) == Labels.end())
        Labels[label] = BasicBlock::Create(*Context, "succ", Func);

    BasicBlock *ifTrue = Labels[label];
    BasicBlock *ifFalse = BasicBlock::Create(*Context, "succ", Func);

    Value *Cond = ICMP(InData1, InData2, Pred);
    BranchInst::Create(ifTrue, ifFalse, Cond, LastInst);
    LastInst->eraseFromParent();

    CurrBB = ifFalse;
    LastInst = BranchInst::Create(ExitBB, CurrBB);
}

void IRFactory::op_add2_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_add2_i32);

    Register &Out1 = Reg[args[0]];
    Register &Out2 = Reg[args[1]];
    Register &In1 = Reg[args[2]];
    Register &In2 = Reg[args[3]];
    Register &In3 = Reg[args[4]];
    Register &In4 = Reg[args[5]];

    AssertType(Out1.Size == 32 && Out2.Size == 32 &&
               In1.Size == 32 && In2.Size == 32 &&
               In3.Size == 32 && In4.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *InData3 = LoadState(In3);
    Value *InData4 = LoadState(In4);

    InData1 = ZEXT64(InData1);
    InData2 = SHL(ZEXT64(InData2), CONST64(32));
    InData2 = OR(InData2, InData1);

    InData3 = ZEXT64(InData3);
    InData4 = SHL(ZEXT64(InData4), CONST64(32));
    InData4 = OR(InData4, InData3);

    InData2 = ADD(InData2, InData4);

    Value *OutData1 = TRUNC32(InData2);
    Value *OutData2 = TRUNC32(LSHR(InData2, CONST64(32)));
    Out1.setData(OutData1, true);
    Out2.setData(OutData2, true);
}

void IRFactory::op_sub2_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_sub2_i32);

    Register &Out1 = Reg[args[0]];
    Register &Out2 = Reg[args[1]];
    Register &In1 = Reg[args[2]];
    Register &In2 = Reg[args[3]];
    Register &In3 = Reg[args[4]];
    Register &In4 = Reg[args[5]];

    AssertType(Out1.Size == 32 && Out2.Size == 32 &&
               In1.Size == 32 && In2.Size == 32 &&
               In3.Size == 32 && In4.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *InData3 = LoadState(In3);
    Value *InData4 = LoadState(In4);

    InData1 = ZEXT64(InData1);
    InData2 = SHL(ZEXT64(InData2), CONST64(32));
    InData2 = OR(InData2, InData1);

    InData3 = ZEXT64(InData3);
    InData4 = SHL(ZEXT64(InData4), CONST64(32));
    InData4 = OR(InData4, InData3);

    InData2 = SUB(InData2, InData4);

    Value *OutData1 = TRUNC32(InData2);
    Value *OutData2 = TRUNC32(LSHR(InData2, CONST64(32)));
    Out1.setData(OutData1, true);
    Out2.setData(OutData2, true);
}

void IRFactory::op_mulu2_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_mulu2_i32);

    Register &Out1 = Reg[args[0]];
    Register &Out2 = Reg[args[1]];
    Register &In1 = Reg[args[2]];
    Register &In2 = Reg[args[3]];

    AssertType(Out1.Size == 32 && Out2.Size == 32 &&
               In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    InData1 = ZEXT64(InData1);
    InData2 = ZEXT64(InData2);

    Value *OutData = MUL(InData1, InData2);
    Value *Low = TRUNC32(OutData);
    Value *High = TRUNC32(LSHR(OutData, CONST64(32)));
    Out1.setData(Low, true);
    Out2.setData(High, true);
}

void IRFactory::op_muls2_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_muls2_i32);

    Register &Out1 = Reg[args[0]];
    Register &Out2 = Reg[args[1]];
    Register &In1 = Reg[args[2]];
    Register &In2 = Reg[args[3]];

    AssertType(Out1.Size == 32 && Out2.Size == 32 &&
               In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    InData1 = SEXT64(InData1);
    InData2 = SEXT64(InData2);

    Value *OutData = MUL(InData1, InData2);
    Value *Low = TRUNC32(OutData);
    Value *High = TRUNC32(LSHR(OutData, CONST64(32)));
    Out1.setData(Low, true);
    Out2.setData(High, true);
}

void IRFactory::op_muluh_i32(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_mulsh_i32(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_brcond2_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_brcond2_i32);

    Register &In1 = Reg[args[0]];
    Register &In2 = Reg[args[1]];
    Register &In3 = Reg[args[2]];
    Register &In4 = Reg[args[3]];
    CmpInst::Predicate Pred = getPred(args[4]);

    AssertType(In1.Size == 32 && In2.Size == 32 &&
               In3.Size == 32 && In4.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *InData3 = LoadState(In3);
    Value *InData4 = LoadState(In4);

    SaveGlobals(COHERENCE_ALL, LastInst);

    InData1 = ZEXT64(InData1);
    InData2 = SHL(ZEXT64(InData2), CONST64(32));
    InData3 = ZEXT64(InData3);
    InData4 = SHL(ZEXT64(InData4), CONST64(32));

    InData2 = OR(InData2, InData1);
    InData4 = OR(InData4, InData3);

    TCGArg label = args[5];
    if (Labels.find(label) == Labels.end())
        Labels[label] = BasicBlock::Create(*Context, "succ", Func);

    BasicBlock *ifTrue = Labels[label];
    BasicBlock *ifFalse = BasicBlock::Create(*Context, "succ", Func);

    Value *Cond = ICMP(InData2, InData4, Pred);
    BranchInst::Create(ifTrue, ifFalse, Cond, LastInst);
    LastInst->eraseFromParent();

    CurrBB = ifFalse;
    LastInst = BranchInst::Create(ExitBB, CurrBB);
}

void IRFactory::op_setcond2_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_setcond2_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];
    Register &In3 = Reg[args[3]];
    Register &In4 = Reg[args[4]];
    CmpInst::Predicate Pred = getPred(args[5]);

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32 &&
               In3.Size == 32 && In4.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *InData3 = LoadState(In3);
    Value *InData4 = LoadState(In4);

    InData1 = ZEXT64(InData1);
    InData2 = SHL(ZEXT64(InData2), CONST64(32));
    InData3 = ZEXT64(InData3);
    InData4 = SHL(ZEXT64(InData4), CONST64(32));

    InData2 = OR(InData2, InData1);
    InData4 = OR(InData4, InData3);

    Value *OutData = ICMP(InData2, InData4, Pred);
    OutData = ZEXT32(OutData);
    Out.setData(OutData, true);
}

void IRFactory::op_ext8s_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_ext8s_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 32 && In.Size == 32);

    Value *InData = LoadState(In);
    InData = TRUNC8(InData);
    InData = SEXT32(InData);
    Out.setData(InData, true);
}

void IRFactory::op_ext16s_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_ext16s_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 32 && In.Size == 32);

    Value *InData = LoadState(In);
    InData = TRUNC16(InData);
    InData = SEXT32(InData);
    Out.setData(InData, true);
}

void IRFactory::op_ext8u_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_ext8u_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 32 && In.Size == 32);

    Value *InData = LoadState(In);
    InData = AND(InData, CONST32(0xFF));
    Out.setData(InData, true);
}

void IRFactory::op_ext16u_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_ext16u_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 32 && In.Size == 32);

    Value *InData = LoadState(In);
    InData = AND(InData, CONST32(0xFFFF));
    Out.setData(InData, true);
}

void IRFactory::op_bswap16_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_bswap16_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 32 && In.Size == 32);

    Value *InData = LoadState(In);
    InData = TRUNC16(InData);
    InData = BSWAP16(InData);
    InData = ZEXT32(InData);
    Out.setData(InData, true);
}

void IRFactory::op_bswap32_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_bswap32_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 32 && In.Size == 32);

    Value *InData = LoadState(In);
    InData = BSWAP32(InData);
    Out.setData(InData, true);
}

/*
 * op_not_i32()
 *  args[0]: Out
 *  args[1]: In
 */
void IRFactory::op_not_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_not_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 32 && In.Size == 32);

    Value *InData = LoadState(In);
    Value *OutData = XOR(InData, CONST32((uint32_t)-1));
    Out.setData(OutData, true);
}

/*
 * op_neg_i32()
 *  args[0]: Out
 *  args[1]: In
 */
void IRFactory::op_neg_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_neg_i32);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 32 && In.Size == 32);

    Value *InData = LoadState(In);
    Value *OutData = SUB(CONST32(0), InData);
    Out.setData(OutData, true);
}

void IRFactory::op_andc_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_andc_i32);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 32 && In1.Size == 32 && In2.Size == 32);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    InData2 = XOR(InData2, CONST32((uint32_t)-1));
    Value *OutData = AND(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_orc_i32(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_eqv_i32(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_nand_i32(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_nor_i32(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_mov_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_mov_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64);

    Value *InData = LoadState(In);

    size_t Size = DL->getTypeSizeInBits(InData->getType());
    if (Size != 64)
        InData = ZEXT64(InData);

    Out.setData(InData, true);
}

/*
 * op_movi_i64()
 *  args[0]: Out
 *  args[1]: In  (const value)
 */
void IRFactory::op_movi_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_movi_i64);

    Register &Out = Reg[args[0]];

    AssertType(Out.Size == 64);

    Out.setData(CONST64(args[1]), true);
}

void IRFactory::op_setcond_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_setcond_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];
    CmpInst::Predicate Pred = getPred(args[3]);

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = ICMP(InData1, InData2, Pred);
    OutData = ZEXT64(OutData);
    Out.setData(OutData, true);
}

void IRFactory::op_movcond_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_movcond_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];
    Register &In3 = Reg[args[3]];
    Register &In4 = Reg[args[4]];
    CmpInst::Predicate Pred = getPred(args[5]);

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64 &&
               In3.Size == 64 && In4.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *InData3 = LoadState(In3);
    Value *InData4 = LoadState(In4);
    Value *Cond = ICMP(InData1, InData2, Pred);
    Value *OutData = SelectInst::Create(Cond, InData3, InData4, "", LastInst);
    Out.setData(OutData, true);
}


/* load/store */
void IRFactory::op_ld8u_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ld8u_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(Out.Size == 64);

    Value *InData = LoadState(In);
    if (InData->getType() != Int8PtrTy)
        InData = CASTPTR8(InData);

    InData = GetElementPtrInst::CreateInBounds(InData, CONSTPtr(Off), "", LastInst);
    InData = new LoadInst(InData, "", false, LastInst);
    InData = ZEXT64(InData);
    Out.setData(InData, true);
}

void IRFactory::op_ld8s_i64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_ld16u_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ld16u_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(Out.Size == 64);

    Value *InData = LoadState(In);
    if (InData->getType() != Int8PtrTy)
        InData = CASTPTR8(InData);

    InData = GetElementPtrInst::CreateInBounds(InData, CONSTPtr(Off), "", LastInst);
    InData = CASTPTR16(InData);
    InData = new LoadInst(InData, "", false, LastInst);
    InData = ZEXT64(InData);
    Out.setData(InData, true);
}

void IRFactory::op_ld16s_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ld16s_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(Out.Size == 64);

    Value *InData = LoadState(In);
    if (InData->getType() != Int8PtrTy)
        InData = CASTPTR8(InData);

    InData = GetElementPtrInst::CreateInBounds(InData, CONSTPtr(Off), "", LastInst);
    InData = CASTPTR16(InData);
    InData = new LoadInst(InData, "", false, LastInst);
    InData = SEXT64(InData);
    Out.setData(InData, true);
}

void IRFactory::op_ld32u_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ld32u_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(Out.Size == 64);

    Value *InData = LoadState(In);
    if (InData->getType() != Int8PtrTy)
        InData = CASTPTR8(InData);

    InData = GetElementPtrInst::CreateInBounds(InData, CONSTPtr(Off), "", LastInst);
    InData = CASTPTR32(InData);
    InData = new LoadInst(InData, "", false, LastInst);
    InData = ZEXT64(InData);
    Out.setData(InData, true);
}

void IRFactory::op_ld32s_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ld32s_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(Out.Size == 64);

    Value *InData = LoadState(In);
    if (InData->getType() != Int8PtrTy)
        InData = CASTPTR8(InData);

    InData = GetElementPtrInst::CreateInBounds(InData, CONSTPtr(Off), "", LastInst);
    InData = CASTPTR32(InData);
    InData = new LoadInst(InData, "", false, LastInst);
    InData = SEXT64(InData);
    Out.setData(InData, true);
}

/*
 * op_ld_i64()
 *  args[0]: Out
 *  args[1]: In  (base)
 *  args[2]: In  (offset)
 */
void IRFactory::op_ld_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ld_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(Out.Size == 64);

    Value *InData;
    if (In.isRev()) {
        InData = StatePointer(In, Off, Int64PtrTy);
        InData = new LoadInst(InData, "", false, LastInst);
        if (isStateOfPC(Off))
            static_cast<LoadInst*>(InData)->setVolatile(true);
    } else {
        InData = LoadState(In);
        if (InData->getType() != Int8PtrTy)
            InData = CASTPTR8(InData);
        InData = GetElementPtrInst::CreateInBounds(InData, CONSTPtr(Off), "", LastInst);
        InData = CASTPTR64(InData);
        InData = new LoadInst(InData, "", false, LastInst);
    }
    Out.setData(InData, true);
}

void IRFactory::op_st8_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_st8_i64);

    Register &In1 = Reg[args[0]];
    Register &In2 = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(In1.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    InData1 = TRUNC8(InData1);
    if (InData2->getType() != Int8PtrTy)
        InData2 = CASTPTR8(InData2);
    InData2 = GetElementPtrInst::CreateInBounds(InData2, CONSTPtr(Off), "", LastInst);
    new StoreInst(InData1, InData2, false, LastInst);
}

void IRFactory::op_st16_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_st16_i64);

    Register &In1 = Reg[args[0]];
    Register &In2 = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(In1.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    InData1 = TRUNC16(InData1);
    if (InData2->getType() != Int8PtrTy)
        InData2 = CASTPTR8(InData2);
    InData2 = GetElementPtrInst::CreateInBounds(InData2, CONSTPtr(Off), "", LastInst);
    InData2 = CASTPTR16(InData2);
    new StoreInst(InData1, InData2, false, LastInst);
}

void IRFactory::op_st32_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_st32_i64);

    Register &In1 = Reg[args[0]];
    Register &In2 = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(In1.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    InData1 = TRUNC32(InData1);
    if (InData2->getType() != Int8PtrTy)
        InData2 = CASTPTR8(InData2);
    InData2 = GetElementPtrInst::CreateInBounds(InData2, CONSTPtr(Off), "", LastInst);
    InData2 = CASTPTR32(InData2);
    new StoreInst(InData1, InData2, false, LastInst);
}

/*
 * op_st_i64()
 *  args[0]: In1
 *  args[1]: In2 (base)
 *  args[2]: In3 (offset)
 */
void IRFactory::op_st_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_st_i64);

    Register &In1 = Reg[args[0]];
    Register &In2 = Reg[args[1]];
    TCGArg Off = args[2];

    AssertType(In1.Size == 64);

    Value *InData1 = LoadState(In1);

    if (In2.isRev()) {
        Value *InData2 = StatePointer(In2, Off, Int64PtrTy);
        StoreInst *SI = new StoreInst(InData1, InData2, false, LastInst);
        if (isStateOfPC(Off))
            SI->setVolatile(true);
    } else {
        Value *InData2 = LoadState(In2);
        if (InData2->getType() != Int8PtrTy)
            InData2 = CASTPTR8(InData2);
        InData2 = GetElementPtrInst::CreateInBounds(InData2, CONSTPtr(Off), "", LastInst);
        InData2 = CASTPTR64(InData2);
        new StoreInst(InData1, InData2, false, LastInst);
    }
}

/* arith */
/*
 * op_add_i64()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 */
void IRFactory::op_add_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_add_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData;
    if (In1.isRev()) {
        intptr_t Off = static_cast<ConstantInt*>(InData2)->getSExtValue();
        OutData = StatePointer(In1, Off, Int64PtrTy);
    } else
        OutData = ADD(InData1, InData2);

    Out.setData(OutData, true);
}

void IRFactory::op_sub_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_sub_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = SUB(InData1, InData2);
    Out.setData(OutData, true);
}

/*
 * op_mul_i64()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 */
void IRFactory::op_mul_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_mul_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = MUL(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_div_i64(const TCGArg *args)
{
    IRError("%s: test me.\n", __func__);
    IRDebug(INDEX_op_div_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = SDIV(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_divu_i64(const TCGArg *args)
{
    IRError("%s: test me.\n", __func__);
    IRDebug(INDEX_op_divu_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = UDIV(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_rem_i64(const TCGArg *args)
{
    IRError("%s: test me.\n", __func__);
    IRDebug(INDEX_op_rem_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = SREM(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_remu_i64(const TCGArg *args)
{
    IRError("%s: test me.\n", __func__);
    IRDebug(INDEX_op_remu_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = UREM(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_div2_i64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_divu2_i64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_and_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_and_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = AND(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_or_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_or_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = OR(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_xor_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_xor_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = XOR(InData1, InData2);
    Out.setData(OutData, true);
}

/* shifts/rotates */
void IRFactory::op_shl_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_shl_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = SHL(InData1, InData2);
    Out.setData(OutData, true);
}

/*
 * op_shr_i64()
 *  args[0]: Out
 *  args[1]: In1
 *  args[2]: In2
 */
void IRFactory::op_shr_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_shr_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = LSHR(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_sar_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_sar_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *OutData = ASHR(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_rotl_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_rotl_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    Value *C = LSHR(InData1, SUB(CONST64(64), InData2));
    Value *OutData = SHL(InData1, InData2);
    OutData = OR(OutData, C);
    Out.setData(OutData, true);
}

void IRFactory::op_rotr_i64(const TCGArg *args)
{
    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    Value *c = SHL(InData1, SUB(CONST64(64), InData2));
    Value *OutData = LSHR(InData1, InData2);
    OutData = OR(OutData, c);
    Out.setData(OutData, true);
}

void IRFactory::op_deposit_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_deposit_i64);

    /* Deposit the lowest args[4] bits of register args[2] into register
     * args[1] starting from bits args[3]. */
    APInt mask = APInt::getBitsSet(64, args[3], args[3] + args[4]);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    if (args[3])
        InData2 = SHL(InData2, CONST64(args[3]));
    InData2 = AND(InData2, ConstantInt::get(*Context, mask));
    InData1 = AND(InData1, ConstantInt::get(*Context, ~mask));
    InData1 = OR(InData1, InData2);
    Out.setData(InData1, true);
}

void IRFactory::op_ext_i32_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ext_i32_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64 && In.Size == 32);

    Value *InData = LoadState(In);
    InData = SEXT64(InData);
    Out.setData(InData, true);
}

void IRFactory::op_extu_i32_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_extu_i32_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64 && In.Size == 32);

    Value *InData = LoadState(In);
    InData = ZEXT64(InData);
    Out.setData(InData, true);
}

void IRFactory::op_extrl_i64_i32(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
    IRDebug(INDEX_op_extrl_i64_i32);
}

void IRFactory::op_extrh_i64_i32(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
    IRDebug(INDEX_op_extrh_i64_i32);
}

void IRFactory::op_brcond_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_brcond_i64);

    /* brcond_i32 format:
     *  brcond_i32 op1,op2,cond,<ifTrue>
     *  <ifFalse>:
     *      A
     *  <ifTrue>:
     *      B
     */
    Register &In1 = Reg[args[0]];
    Register &In2 = Reg[args[1]];
    CmpInst::Predicate Pred = getPred(args[2]);

    AssertType(In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    SaveGlobals(COHERENCE_ALL, LastInst);

    TCGArg label = args[3];
    if (Labels.find(label) == Labels.end())
        Labels[label] = BasicBlock::Create(*Context, "succ", Func);

    BasicBlock *ifTrue = Labels[label];
    BasicBlock *ifFalse = BasicBlock::Create(*Context, "succ", Func);

    Value *Cond = ICMP(InData1, InData2, Pred);
    BranchInst::Create(ifTrue, ifFalse, Cond, LastInst);
    LastInst->eraseFromParent();

    CurrBB = ifFalse;
    LastInst = BranchInst::Create(ExitBB, CurrBB);
}

void IRFactory::op_ext8s_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ext8s_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64 && In.Size == 64);

    Value *InData = LoadState(In);
    InData = TRUNC8(InData);
    InData = SEXT64(InData);
    Out.setData(InData, true);
}

void IRFactory::op_ext16s_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ext16s_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64 && In.Size == 64);

    Value *InData = LoadState(In);
    InData = TRUNC16(InData);
    InData = SEXT64(InData);
    Out.setData(InData, true);
}

/*
 * op_ext32s_i64()
 *  args[0]: Out
 *  args[1]: In
 */
void IRFactory::op_ext32s_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ext32s_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64 && In.Size == 64);

    Value *InData = LoadState(In);
    if (DL->getTypeSizeInBits(InData->getType()) != 32)
        InData = TRUNC32(InData);
    InData = SEXT64(InData);
    Out.setData(InData, true);
}

void IRFactory::op_ext8u_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ext8u_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64 && In.Size == 64);

    Value *InData = LoadState(In);
    InData = AND(InData, CONST64(0xFF));
    Out.setData(InData, true);
}

void IRFactory::op_ext16u_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ext16u_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64 && In.Size == 64);

    Value *InData = LoadState(In);
    InData = AND(InData, CONST64(0xFFFF));
    Out.setData(InData, true);
}

/*
 * op_ext32u_i64()
 *  args[0]: Out
 *  args[1]: In
 */
void IRFactory::op_ext32u_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_ext32u_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64 && In.Size == 64);

    Value *InData = LoadState(In);
    if (DL->getTypeSizeInBits(InData->getType()) == 32)
        InData = ZEXT64(InData);
    else
        InData = AND(InData, CONST64(0xFFFFFFFF));
    Out.setData(InData, true);
}

void IRFactory::op_bswap16_i64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_bswap32_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_bswap32_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64 && In.Size == 64);

    Value *InData = LoadState(In);
    InData = TRUNC32(InData);
    InData = BSWAP32(InData);
    InData = ZEXT64(InData);
    Out.setData(InData, true);
}

void IRFactory::op_bswap64_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_bswap64_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64 && In.Size == 64);

    Value *InData = LoadState(In);
    InData = BSWAP64(InData);
    Out.setData(InData, true);

}

void IRFactory::op_not_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_not_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64 && In.Size == 64);

    Value *InData = LoadState(In);
    Value *OutData = XOR(InData, CONST64((uint64_t)-1));
    Out.setData(OutData, true);
}

void IRFactory::op_neg_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_neg_i64);

    Register &Out = Reg[args[0]];
    Register &In = Reg[args[1]];

    AssertType(Out.Size == 64 && In.Size == 64);

    Value *InData = LoadState(In);
    Value *OutData = SUB(CONST64(0), InData);
    Out.setData(OutData, true);
}

void IRFactory::op_andc_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_andc_i64);

    Register &Out = Reg[args[0]];
    Register &In1 = Reg[args[1]];
    Register &In2 = Reg[args[2]];

    AssertType(Out.Size == 64 && In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    InData2 = XOR(InData2, CONST64((uint64_t)-1));
    Value *OutData = AND(InData1, InData2);
    Out.setData(OutData, true);
}

void IRFactory::op_orc_i64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_eqv_i64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_nand_i64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_nor_i64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_add2_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_add2_i64);

    Register &Out1 = Reg[args[0]];
    Register &Out2 = Reg[args[1]];
    Register &In1 = Reg[args[2]];
    Register &In2 = Reg[args[3]];
    Register &In3 = Reg[args[4]];
    Register &In4 = Reg[args[5]];

    AssertType(Out1.Size == 64 && Out2.Size == 64 &&
               In1.Size == 64 && In2.Size == 64 &&
               In3.Size == 64 && In4.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *InData3 = LoadState(In3);
    Value *InData4 = LoadState(In4);

    InData1 = ZEXT128(InData1);
    InData2 = SHL(ZEXT128(InData2), CONST128(64));
    InData2 = OR(InData2, InData1);

    InData3 = ZEXT128(InData3);
    InData4 = SHL(ZEXT128(InData4), CONST128(64));
    InData4 = OR(InData4, InData3);

    InData2 = ADD(InData2, InData4);

    Value *OutData1 = TRUNC64(InData2);
    Value *OutData2 = TRUNC64(LSHR(InData2, CONST128(64)));
    Out1.setData(OutData1, true);
    Out2.setData(OutData2, true);
}

void IRFactory::op_sub2_i64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_mulu2_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_mulu2_i64);

    Register &Out1 = Reg[args[0]];
    Register &Out2 = Reg[args[1]];
    Register &In1 = Reg[args[2]];
    Register &In2 = Reg[args[3]];

    AssertType(Out1.Size == 64 && Out2.Size == 64 &&
               In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    InData1 = ZEXT128(InData1);
    InData2 = ZEXT128(InData2);

    Value *OutData = MUL(InData1, InData2);
    Value *Low = TRUNC64(OutData);
    Value *High = TRUNC64(LSHR(OutData, CONST128(64)));
    Out1.setData(Low, true);
    Out2.setData(High, true);
}

void IRFactory::op_muls2_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_muls2_i64);

    Register &Out1 = Reg[args[0]];
    Register &Out2 = Reg[args[1]];
    Register &In1 = Reg[args[2]];
    Register &In2 = Reg[args[3]];

    AssertType(Out1.Size == 64 && Out2.Size == 64 &&
               In1.Size == 64 && In2.Size == 64);

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);

    InData1 = SEXT128(InData1);
    InData2 = SEXT128(InData2);

    Value *OutData = MUL(InData1, InData2);
    Value *Low = TRUNC64(OutData);
    Value *High = TRUNC64(LSHR(OutData, CONST128(64)));
    Out1.setData(Low, true);
    Out2.setData(High, true);
}

void IRFactory::op_muluh_i64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_mulsh_i64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

/* QEMU specific */
void IRFactory::op_insn_start(const TCGArg *args)
{
    IRDebug(INDEX_op_insn_start);
    NI.NumInsts++;
}

void IRFactory::InsertLinkAndExit(Instruction *InsertPos)
{
    auto ChainSlot = LLEnv->getChainSlot();
    size_t Key = ChainSlot.first;
    uintptr_t RetVal = ChainSlot.second;
    unsigned Idx = NI.setChainSlot(Key);
    uintptr_t Addr = NI.getChainSlotAddr(Idx);

    /* Here we use the llvm.trap intrinsic to notify LLVM backend to insert
     * jump instruction for chaining. */
    ConstantInt *Meta[] = { CONST32(PATCH_TRACE_BLOCK_CHAINING), CONSTPtr(Addr) };
    Function *TrapFn = Intrinsic::getDeclaration(Mod, Intrinsic::trap);
    CallInst *CI = CallInst::Create(TrapFn, "", InsertPos);
    DebugLoc DL = MF->getDebugLoc(PATCH_TRACE_BLOCK_CHAINING, Idx, Func, Meta);
    CI->setDebugLoc(DL);

    MF->setExit(CI);

    InsertExit(RetVal);
}

void IRFactory::InsertExit(uintptr_t RetVal, bool setExit)
{
    ConstantInt *Meta[] = { CONST32(PATCH_EXIT_TB), ExitAddr };
    ReturnInst *RI = ReturnInst::Create(*Context, CONSTPtr(RetVal), LastInst);
    DebugLoc DL = MF->getDebugLoc(PATCH_EXIT_TB, 0, Func, Meta);
    RI->setDebugLoc(DL);

    if (setExit)
        MF->setExit(RI);
}

void IRFactory::InsertLookupIBTC(GraphNode *CurrNode)
{
    BasicBlock *BB = nullptr;

    if (CommonBB.find("ibtc") == CommonBB.end()) {
        BB = CommonBB["ibtc"] = BasicBlock::Create(*Context, "ibtc", Func);

        SmallVector<Value *, 4> Params;
        Function *F = ResolveFunction("helper_lookup_ibtc");
        Value *Env = ConvertCPUType(F, 0, BB);

        Params.push_back(Env);
        CallInst *CI = CallInst::Create(F, Params, "", BB);
        IndirectBrInst *IB = IndirectBrInst::Create(CI, 1, BB);
        MF->setConst(CI);
        MF->setExit(CI);

        IndirectBrs.push_back(IB);
        toSink.push_back(BB);
    }

    BB = CommonBB["ibtc"];
    BranchInst::Create(BB, LastInst);
}

void IRFactory::InsertLookupCPBL(GraphNode *CurrNode)
{
    SmallVector<Value *, 4> Params;
    Function *F = ResolveFunction("helper_lookup_cpbl");
    Value *Env = ConvertCPUType(F, 0, LastInst);

    Params.push_back(Env);
    CallInst *CI = CallInst::Create(F, Params, "", LastInst);
    IndirectBrInst *IB = IndirectBrInst::Create(CI, 1, LastInst);
    MF->setConst(CI);
    MF->setExit(CI);

    IndirectBrs.push_back(IB);
    toSink.push_back(CurrBB);
}

void IRFactory::TraceValidateCPBL(GraphNode *NextNode, StoreInst *StorePC)
{
    TranslationBlock *NextTB = NextNode->getTB();
    Value *Cond;

    SmallVector<Value *, 4> Params;
    Function *F = ResolveFunction("helper_validate_cpbl");
    Value *Env = ConvertCPUType(F, 0, LastInst);

    Params.push_back(Env);
    Params.push_back(ConstantInt::get(StorePC->getValueOperand()->getType(),
                                      NextTB->pc));
    Params.push_back(CONST32(NextTB->id));
    CallInst *CI = CallInst::Create(F, Params, "", LastInst);
    Cond = ICMP(CI, CONST32(1), ICmpInst::ICMP_EQ);

    MF->setConst(CI);

    BasicBlock *Valid = BasicBlock::Create(*Context, "cpbl.valid", Func);
    BasicBlock *Invalid = BasicBlock::Create(*Context, "cpbl.invalid", Func);
    toSink.push_back(Invalid);

    BranchInst::Create(Valid, Invalid, Cond, LastInst);
    LastInst->eraseFromParent();

    LastInst = BranchInst::Create(ExitBB, Invalid);
    Instruction *SI = StorePC->clone();
    SI->insertBefore(LastInst);
    InsertExit(0);
    LastInst->eraseFromParent();

    MF->setExit(SI);

    CurrBB = Valid;
    LastInst = BranchInst::Create(ExitBB, CurrBB);
}

/*
 * TraceLinkIndirectJump()
 *  This routine implements IB inlining, i.e., linking two intra-trace blocks
 *  via indirect branch.
 *  Note that we don't need to validate CPBL because this routine is only
 *  used for user-mode emulation.
 */
void IRFactory::TraceLinkIndirectJump(GraphNode *NextNode, StoreInst *SI)
{
    dbg() << DEBUG_LLVM << "    - Found an indirect branch. Guess pc "
          << format("0x%" PRIx, NextNode->getGuestPC()) << "\n";

    BasicBlock *ifTrue = BasicBlock::Create(*Context, "main_path", Func);
    BasicBlock *ifFalse = BasicBlock::Create(*Context, "exit_stub", Func);

    Value *NextPC = SI->getValueOperand();
    Value *GuessPC = ConstantInt::get(NextPC->getType(),
                                      Builder->getGuestPC(NextNode));

    Value *Cond = ICMP(NextPC, GuessPC, ICmpInst::ICMP_EQ);
    BranchInst::Create(ifTrue, ifFalse, Cond, LastInst);
    LastInst->eraseFromParent();

    CurrBB = ifTrue;

    /* First set the branch to exit BB, and the link will be resolved
       at the trace finalization procedure. */
    BranchInst *BI = BranchInst::Create(ExitBB, CurrBB);
    Builder->setBranch(BI, NextNode);

    CurrBB = ifFalse;
    LastInst = BranchInst::Create(ExitBB, CurrBB);
}

void IRFactory::TraceLinkDirectJump(GraphNode *NextNode, StoreInst *SI)
{
    ConstantInt *NextPC = static_cast<ConstantInt *>(SI->getValueOperand());
    target_ulong next_pc = NextPC->getZExtValue() +
                           Builder->getCurrNode()->getTB()->cs_base;
    NextPC = ConstantInt::get(NextPC->getType(), next_pc);

    dbg() << DEBUG_LLVM << "    - Found a direct branch to pc "
          << format("0x%" PRIx, next_pc) << "\n";

#if defined(CONFIG_SOFTMMU)
    TranslationBlock *tb = Builder->getCurrNode()->getTB();
    TranslationBlock *next_tb = NextNode->getTB();
    /* If two blocks are not in the same page or the next block is across
     * the page boundary, we have to handle it with CPBL.  */
    if ((tb->pc & TARGET_PAGE_MASK) != (next_tb->pc & TARGET_PAGE_MASK) ||
        next_tb->page_addr[1] != (tb_page_addr_t)-1)
        TraceValidateCPBL(NextNode, SI);
#endif
    /* First set the branch to exit BB, and the link will be resolved
       at the trace finalization procedure. */
    BranchInst *BI = BranchInst::Create(ExitBB, LastInst);
    Builder->setBranch(BI, NextNode);
}

void IRFactory::TraceLinkDirectJump(StoreInst *SI)
{
    ConstantInt *NextPC = static_cast<ConstantInt *>(SI->getValueOperand());
    target_ulong next_pc = NextPC->getZExtValue() +
                           Builder->getCurrNode()->getTB()->cs_base;
    NextPC = ConstantInt::get(NextPC->getType(), next_pc);

    dbg() << DEBUG_LLVM << "    - Found a direct branch to pc "
          << format("0x%" PRIx, next_pc) << " (exit)\n";

#if defined(CONFIG_SOFTMMU)
    TranslationBlock *tb = Builder->getCurrNode()->getTB();
    if ((tb->pc & TARGET_PAGE_MASK) != (next_pc & TARGET_PAGE_MASK)) {
        InsertLookupCPBL(Builder->getCurrNode());
        return;
    }
#endif
    InsertLinkAndExit(SI);
}

GraphNode *IRFactory::findNextNode(target_ulong pc)
{
#ifdef USE_TRACETREE_ONLY
    for (auto Child : Builder->getCurrNode()->getChildren()) {
        if (pc == Builder->getGuestPC(Child))
            return Child;
    }
    return nullptr;
#else
    return Builder->getNode(pc);
#endif
}

void IRFactory::TraceLink(StoreInst *SI)
{
    GraphNode *CurrNode = Builder->getCurrNode();
    ConstantInt *CI = dyn_cast<ConstantInt>(SI->getValueOperand());
    if (!CI) {
        /* Indirect branch */
        SaveGlobals(COHERENCE_ALL, LastInst);

#if defined(CONFIG_USER_ONLY)
        for (auto NextNode : CurrNode->getChildren())
            TraceLinkIndirectJump(NextNode, SI);
#endif
        InsertLookupIBTC(CurrNode);
    } else {
        /* Direct branch. */
        target_ulong pc = CI->getZExtValue();
        GraphNode *NextNode = findNextNode(pc);
        if (NextNode) {
            TraceLinkDirectJump(NextNode, SI);
            return;
        }

        TraceLinkDirectJump(SI);
        std::string Name = CurrBB->getName().str() + ".exit";
        CurrBB->setName(Name);
        toSink.push_back(CurrBB);
    }
}

StoreInst *IRFactory::getStorePC()
{
#if TARGET_LONG_BITS > TCG_TARGET_REG_BITS
    std::vector<std::pair<intptr_t, StoreInst *> > StorePC;

    /* Search for store instructions that write value to PC in this block. */
    bool hasAllConstantPC = true;
    BasicBlock *BB = LastInst->getParent();
    for (auto BI = BB->begin(), BE = BB->end(); BI != BE; ++BI) {
        if (StoreInst *SI = dyn_cast<StoreInst>(BI)) {
            intptr_t Off = 0;
            Value *Base = getBaseWithConstantOffset(DL,
                                SI->getPointerOperand(), Off);
            if (Base == BaseReg[TCG_AREG0].Base && isStateOfPC(Off)) {
                StorePC.push_back(std::make_pair(Off, SI));
                if (!isa<ConstantInt>(SI->getValueOperand()))
                    hasAllConstantPC = false;
            }
        }
    }

    if (StorePC.empty())
        return nullptr;
    if (StorePC.size() == 1)
        return StorePC[0].second;

    /* We only consider the last two stores. */
    unsigned I1 = StorePC.size() - 2, I2 = StorePC.size() - 1;
    if (StorePC[I1].first > StorePC[I2].first) {
        unsigned tmp = I1;
        I1 = I2;
        I2 = tmp;
    }

    intptr_t OffsetA = StorePC[I1].first;
    intptr_t OffsetB = StorePC[I2].first;
    StoreInst *SA = StorePC[I1].second;
    StoreInst *SB = StorePC[I2].second;
    intptr_t SzA = DL->getTypeSizeInBits(SA->getValueOperand()->getType());
    intptr_t SzB = DL->getTypeSizeInBits(SB->getValueOperand()->getType());
    if (SzA != SzB || OffsetA + SzA != OffsetB || SzA + SzB != TARGET_LONG_BITS)
        return nullptr;

    Value *NewPC;
    Type *Ty = (TARGET_LONG_BITS == 32) ? Int32Ty : Int64Ty;
    Type *PTy = (TARGET_LONG_BITS == 32) ? Int32PtrTy : Int64PtrTy;
    if (hasAllConstantPC) {
        target_ulong PCA = static_cast<ConstantInt*>(SA->getValueOperand())->getZExtValue();
        target_ulong PCB = static_cast<ConstantInt*>(SA->getValueOperand())->getZExtValue();
        NewPC = ConstantInt::get(Ty, PCA | (PCB << SzA));
    } else {
        Value *PCA = ZEXT(SA->getValueOperand(), Ty);
        Value *PCB = ZEXT(SB->getValueOperand(), Ty);
        PCB = SHL(PCB, ConstantInt::get(Ty, SzA));
        NewPC = OR(PCA, PCB);
    }

    toErase.push_back(SA);
    toErase.push_back(SB);

    Value *Addr = CAST(SA->getPointerOperand(), PTy);
    return new StoreInst(NewPC, Addr, true, LastInst);

#else
    return dyn_cast<StoreInst>(--BasicBlock::iterator(LastInst));
#endif
}

/*
 * op_exit_tb()
 *  args[0]: return value
 */
void IRFactory::op_exit_tb(const TCGArg *args)
{
    IRDebug(INDEX_op_exit_tb);

    if (!LastInst)
        return;

    /* Some guest architectures (e.g., ARM) do not explicitly generete a store
     * instruction to sync the PC value to the memory before exit_tb. We
     * generate the store PC instruction here so that the following routine can
     * analyze the PC value it will branch to. Note that other dirty states will
     * be synced later. */
    CreateStorePC(LastInst);

    if (LastInst == &*LastInst->getParent()->begin()) {
        SaveGlobals(COHERENCE_ALL, LastInst);
        InsertExit(0, true);
    } else if (isa<CallInst>(--BasicBlock::iterator(LastInst))) {
        /* Tail call. */
        for (int i = 0, e = tcg_ctx.nb_globals; i != e; ++i) {
            Register &reg = Reg[i];
            if (reg.isReg() && reg.isDirty())
                runPasses = false;
        }

        SaveGlobals(COHERENCE_ALL, LastInst);
        InsertExit(0, true);
    } else if (StoreInst *SI = getStorePC()) {
        SaveGlobals(COHERENCE_ALL, SI);
        TraceLink(SI);
    } else {
        runPasses = false;
        SaveGlobals(COHERENCE_ALL, LastInst);
        InsertExit(0, true);
    }

    LastInst->eraseFromParent();
    LastInst = nullptr;
}

/*
 * op_goto_tb()
 *  args[0]: jump index
 */
void IRFactory::op_goto_tb(const TCGArg *args)
{
    IRDebug(INDEX_op_goto_tb);
}

void IRFactory::op_qemu_ld_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_qemu_ld_i32);

    TCGArg DataLo = *args++;
    TCGArg AddrLo = *args++;
    TCGArg AddrHi = (TARGET_LONG_BITS > TCG_TARGET_REG_BITS) ? *args++ : 0;
    TCGMemOpIdx oi = *args++;
    TCGMemOp opc = get_memop(oi);

    Register &Out = Reg[DataLo];
    Register &In1 = Reg[AddrLo];

    Value *InData1 = LoadState(In1);
    Value *InData2 = (AddrHi) ? LoadState(Reg[AddrHi]) : nullptr;

    AssertType(In1.Size == 32 || In1.Size == 64);

    SaveStates();

    Value *OutData = QEMULoad(InData1, InData2, oi);
    OutData = getExtendValue(OutData, Out.Ty, opc);
    Out.setData(OutData, true);
}

void IRFactory::op_qemu_st_i32(const TCGArg *args)
{
    IRDebug(INDEX_op_qemu_st_i32);

    TCGArg DataLo = *args++;
    TCGArg AddrLo = *args++;
    TCGArg AddrHi = (TARGET_LONG_BITS > TCG_TARGET_REG_BITS) ? *args++ : 0;
    TCGMemOpIdx oi = *args++;
    TCGMemOp opc = get_memop(oi);

    Register &In1 = Reg[DataLo];
    Register &In2 = Reg[AddrLo];

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *InData3 = (AddrHi) ? LoadState(Reg[AddrHi]) : nullptr;

    AssertType(In1.Size == 32 || In1.Size == 64);

    SaveStates();

    InData1 = getTruncValue(InData1, opc);
    QEMUStore(InData1, InData2, InData3, oi);
}

void IRFactory::op_qemu_ld_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_qemu_ld_i64);

    TCGArg DataLo = *args++;
    TCGArg DataHi = (TCG_TARGET_REG_BITS == 32) ? *args++ : 0;
    TCGArg AddrLo = *args++;
    TCGArg AddrHi = (TARGET_LONG_BITS > TCG_TARGET_REG_BITS) ? *args++ : 0;
    TCGMemOpIdx oi = *args++;
    TCGMemOp opc = get_memop(oi);

    Register &Out = Reg[DataLo];
    Register &In1 = Reg[AddrLo];

    Value *InData1 = LoadState(In1);
    Value *InData2 = (AddrHi) ? LoadState(Reg[AddrHi]) : nullptr;

    AssertType(In1.Size == 32 || In1.Size == 64);

    SaveStates();

    Value *OutData = QEMULoad(InData1, InData2, oi);
    OutData = getExtendValue(OutData, Out.Ty, opc);

    if (DataHi == 0)
        Out.setData(OutData, true);
    else {
        Register &Out2 = Reg[DataHi];
        Value *OutData1 = TRUNC32(OutData);
        Value *OutData2 = TRUNC32(LSHR(OutData, CONST64(32)));
        Out.setData(OutData1, true);
        Out2.setData(OutData2, true);
    }
}

void IRFactory::op_qemu_st_i64(const TCGArg *args)
{
    IRDebug(INDEX_op_qemu_st_i64);

    TCGArg DataLo = *args++;
    TCGArg DataHi = (TCG_TARGET_REG_BITS == 32) ? *args++ : 0;
    TCGArg AddrLo = *args++;
    TCGArg AddrHi = (TARGET_LONG_BITS > TCG_TARGET_REG_BITS) ? *args++ : 0;
    TCGMemOpIdx oi = *args++;
    TCGMemOp opc = get_memop(oi);

    Register &In1 = Reg[DataLo];
    Register &In2 = Reg[AddrLo];

    Value *InData1 = LoadState(In1);
    Value *InData2 = LoadState(In2);
    Value *InData3 = (AddrHi) ? LoadState(Reg[AddrHi]) : nullptr;

    AssertType(In2.Size == 32 || In2.Size == 64);

    SaveStates();

    Value *InData;
    if (DataHi == 0)
        InData = InData1;
    else {
        InData = LoadState(Reg[DataHi]);
        InData = SHL(ZEXT64(InData), CONST64(32));
        InData = OR(InData, ZEXT64(InData1));
    }

    InData = getTruncValue(InData, opc);
    QEMUStore(InData, InData2, InData3, oi);
}

void IRFactory::op_annotate(const TCGArg *args)
{
    IRDebug(INDEX_op_annotate);

    uint32_t Annotation = *args;
    if (Annotation == A_SetCC) {
        if (LastInst)
            MF->setCondition(&*--BasicBlock::iterator(LastInst));
    } else if (Annotation == A_NoSIMDization) {
        Builder->addAttribute(A_NoSIMDization);
    }
}


/*
 * Metadata Factory
 */
MDFactory::MDFactory(Module *M) : UID(0), Context(M->getContext())
{
    Dummy = getMDNode(ArrayRef<ConstantInt*>(getUID()));
}

MDFactory::~MDFactory() {}

#if defined(LLVM_V35)
void MDFactory::setConstStatic(LLVMContext &Context, Instruction *I,
                               ArrayRef<ConstantInt*> V)
{
    SmallVector<Value *, 4> MDs;
    for (unsigned i = 0, e = V.size(); i != e; ++i)
        MDs.push_back(V[i]);
    I->setMetadata(META_CONST, MDNode::get(Context, MDs));
}

MDNode *MDFactory::getMDNode(ArrayRef<ConstantInt*> V)
{
    SmallVector<Value *, 4> MDs;
    MDs.push_back(getUID());
    for (unsigned i = 0, e = V.size(); i != e; ++i)
        MDs.push_back(V[i]);
    return MDNode::get(Context, MDs);
}
#else
void MDFactory::setConstStatic(LLVMContext &Context, Instruction *I,
                               ArrayRef<ConstantInt*> V)
{
    SmallVector<Metadata *, 4> MDs;
    for (unsigned i = 0, e = V.size(); i != e; ++i)
        MDs.push_back(ConstantAsMetadata::get(V[i]));
    I->setMetadata(META_CONST, MDNode::get(Context, MDs));
}

MDNode *MDFactory::getMDNode(ArrayRef<ConstantInt*> V)
{
    SmallVector<Metadata *, 4> MDs;
    MDs.push_back(ConstantAsMetadata::get(getUID()));
    for (unsigned i = 0, e = V.size(); i != e; ++i)
        MDs.push_back(ConstantAsMetadata::get(V[i]));
    return MDNode::get(Context, MDs);
}
#endif

#if defined(ENABLE_MCJIT)
DebugLoc MDFactory::getDebugLoc(unsigned Line, unsigned Col, Function *F,
                                ArrayRef<ConstantInt*> Meta)
{
    Module *M = F->getParent();
    DIBuilder DIB(*M);
    auto File = DIB.createFile(F->getName(), "hqemu/");
    auto CU = DIB.createCompileUnit(dwarf::DW_LANG_Cobol74, F->getName(),
                                    "hqemu/", "hqemu", true, "", 0);
#if defined(LLVM_V35)
    auto Type = DIB.createSubroutineType(File,
                DIB.getOrCreateArray(ArrayRef<Value *>()));
    auto SP = DIB.createFunction(CU, F->getName(), "", File, 1, Type, false,
                                 true, 1, 0, true);
    auto Scope = DIB.createLexicalBlockFile(SP, File);
    DebugLoc DL = DebugLoc::get(Line, Col, Scope);
    DIB.finalize();
    SP.replaceFunction(F);
#else
    auto Type = DIB.createSubroutineType(DIB.getOrCreateTypeArray(None));
    auto SP = DIB.createFunction(CU, F->getName(), "", File, 1, Type, false,
                                 true, 1, 0, true);
    auto Scope = DIB.createLexicalBlockFile(SP, File, 0);
    DebugLoc DL = DebugLoc::get(Line, Col, Scope);
    DIB.finalize();
    F->setSubprogram(SP);
#endif

    return DL;
}
#else
DebugLoc MDFactory::getDebugLoc(unsigned Line, unsigned Col, Function *F,
                                ArrayRef<ConstantInt*> Meta)
{
    return DebugLoc::get(Line, Col, getMDNode(Meta));
}
#endif


/*
 * TraceBuilder()
 */
TraceBuilder::TraceBuilder(IRFactory *IRF, OptimizationInfo *Opt)
    : IF(IRF), Opt(Opt), Aborted(false), Attribute(A_None), Trace(nullptr)
{
    GraphNode *EntryNode = Opt->getCFG();
    if (!EntryNode)
        hqemu_error("invalid optimization request.\n");

    /* Find unique nodes. */
    NodeVec VisitStack;
    NodeSet Visited;
    VisitStack.push_back(EntryNode);
    do {
        GraphNode *Node = VisitStack.back();
        VisitStack.pop_back();
        if (Visited.find(Node) == Visited.end()) {
            Visited.insert(Node);

            setUniqueNode(Node);

            for (auto Child : Node->getChildren())
                VisitStack.push_back(Child);
        }
    } while (!VisitStack.empty());

    /* Add entry node into the building queue. */
    NodeQueue.push_back(EntryNode);

    IF->CreateSession(this);
    IF->CreateFunction();
}

void TraceBuilder::ConvertToTCGIR(CPUArchState *env)
{
    TranslationBlock *tb = CurrNode->getTB();

    if (LLEnv->isTraceMode()) {
        env->image_base = (uintptr_t)tb->image - tb->pc;
        tcg_copy_state(env, tb);
    }

    tcg_func_start(&tcg_ctx, tb);
    gen_intermediate_code(env, tb);
    tcg_liveness_analysis(&tcg_ctx);
}

static inline bool isVecOp(TCGOpcode opc)
{
    switch (opc) {
    case INDEX_op_vector_start ... INDEX_op_vector_end:
        return true;
    default:
        return false;
    }
}

void TraceBuilder::ConvertToLLVMIR()
{
    IF->CreateBlock();

    auto OpcFunc = (IRFactory::FuncPtr *)IF->getOpcFunc();
    TCGArg *VecArgs = tcg_ctx.vec_opparam_buf;

    IF->NI.setTB(CurrNode->getTB());
    for (int oi = tcg_ctx.gen_first_op_idx; oi >= 0; ) {
        TCGOp * const op = &tcg_ctx.gen_op_buf[oi];
        TCGArg *args = &tcg_ctx.gen_opparam_buf[op->args];
        oi = op->next;

        if (isVecOp(op->opc)) {
            args = VecArgs;
            VecArgs += 3;
        }

        IF->NI.setOp(op);
        (IF->*OpcFunc[op->opc])(args);

        if (isAborted())
            return;
    }
}

void TraceBuilder::Abort()
{
    IF->DeleteSession();
    Aborted = true;
}

void TraceBuilder::Finalize()
{
    /* Reconnect links of basic blocks. The links are previously
       set to ExitBB. */
    for (unsigned i = 0, e = Branches.size(); i != e; ++i) {
        BranchInst *BI = Branches[i].first;
        GraphNode *Node = Branches[i].second;
        IF->setSuccessor(BI, getBasicBlock(Node));
    }

    Trace = new TraceInfo(NodeUsed, Attribute);
    IF->Compile();
    IF->DeleteSession();
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
