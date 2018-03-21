/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Analysis/InlineCost.h"
#include "fpu/softfloat-native-def.h"
#include "utils.h"
#include "tracer.h"
#include "llvm.h"
#include "llvm-debug.h"
#include "llvm-profile.h"
#include "llvm-target.h"
#include "llvm-pass.h"
#include "llvm-opc.h"
#include "llvm-state.h"
#include "llvm-translator.h"


static cl::opt<bool> DisableFastMath("disable-fast-math", cl::init(false),
    cl::cat(CategoryHQEMU), cl::desc("Disable fast-math optimizations"));


static char include_helper[][64] = {
#include "llvm-helper.h"
};

extern LLVMEnv *LLEnv;
extern hqemu::Mutex llvm_global_lock;
extern hqemu::Mutex llvm_debug_lock;

extern bool TraceCacheFull;


#if defined(TCG_TARGET_I386)
#  if defined(__i386__)
#    define AREG0 "ebp"
#  elif defined(__x86_64__)
#    define AREG0 "r14"
#  endif
#elif defined(TCG_TARGET_PPC) || defined(TCG_TARGET_PPC64)
#  define AREG0 "r27"
#elif defined(TCG_TARGET_ARM)
#  define AREG0 "r7"
#else
#  error "unsupported processor type"
#endif
const char *BaseRegStr = AREG0;    /* The base register name */



/*
 * LLVM Translator
 */
LLVMTranslator::LLVMTranslator(unsigned id, CPUArchState *env)
    : MyID(id), Env(env)
{
    dbg() << DEBUG_LLVM << "Starting LLVM Translator " << MyID << ".\n";

    if (!Env)
        hqemu_error("internal error. LLVMEnv is not initialized.\n");

    /* Create LLVM module and basic types. */
    InitializeModule();
    InitializeType();
    InitializeTarget();
    InitializeHelpers();
    InitializeDisasm();

    /* Create the TCG IR to LLVM IR conversion module. */
    IF = new IRFactory(this);

#if defined(ENABLE_MCJIT)
    if (MyID == 0)
        LLEnv->getMemoryManager()->AddSymbols(Symbols);
#endif

    dbg() << DEBUG_LLVM << "LLVM Translator " << MyID << " initialized.\n";
}

LLVMTranslator::~LLVMTranslator()
{
    if (GuestDisAsm) delete GuestDisAsm;
    if (HostDisAsm) delete HostDisAsm;
    delete IF;
    delete Mod;
}

/*
 * Perform the initialization of the LLVM module.
 */
void LLVMTranslator::InitializeModule()
{
    SMDiagnostic Err;
    struct stat buf;

    if (stat(CONFIG_LLVM_BITCODE, &buf) != 0)
        hqemu_error("cannot find bitcode file %s.\n", CONFIG_LLVM_BITCODE);

#if defined(LLVM_V35)
    Mod = ParseIRFile(CONFIG_LLVM_BITCODE, Err, Context);
#else
    std::unique_ptr<Module> Owner = parseIRFile(CONFIG_LLVM_BITCODE, Err, Context);
    Mod = Owner.release();
#endif
    if (!Mod)
        hqemu_error("%s.\n", Err.getMessage().data());

    DL = getDataLayout(Mod);

    dbg() << DEBUG_LLVM << "LLVM module initialized for target ("
          << Mod->getTargetTriple() << ").\n";
}

void LLVMTranslator::InitializeType()
{
    VoidTy   = Type::getVoidTy(Context);
    Int8Ty   = IntegerType::get(Context, 8);
    Int16Ty  = IntegerType::get(Context, 16);
    Int32Ty  = IntegerType::get(Context, 32);
    Int64Ty  = IntegerType::get(Context, 64);
    Int128Ty = IntegerType::get(Context, 128);
    
    IntPtrTy    = DL->getIntPtrType(Context);
    Int8PtrTy   = Type::getInt8PtrTy(Context, 0);
    Int16PtrTy  = Type::getInt16PtrTy(Context, 0);
    Int32PtrTy  = Type::getInt32PtrTy(Context, 0);
    Int64PtrTy  = Type::getInt64PtrTy(Context, 0);
    
    FloatTy  = Type::getFloatTy(Context);
    DoubleTy = Type::getDoubleTy(Context);

    FloatPtrTy  = Type::getFloatPtrTy(Context, 0);
    DoublePtrTy = Type::getDoublePtrTy(Context, 0);
}

/*
 * Setup guest-dependent data structures.
 */
void LLVMTranslator::InitializeTarget()
{
    /* TODO: any smart way to hack into CPUArchState type? */
    Value *Base = Mod->getNamedValue("basereg");
    if (!Base)
        hqemu_error("cannot resolve cpu_proto.\n");

    BaseReg.resize(TCG_TARGET_NB_REGS);
    BaseReg[TCG_AREG0].RegNo = TCG_AREG0;
    BaseReg[TCG_AREG0].Name = BaseRegStr;
    BaseReg[TCG_AREG0].Ty = Base->getType();
    BaseReg[TCG_AREG0].Base = nullptr;

    /* Define the new types of special registers. */
    std::map<Type *, Type *> SpecialReg;
    DefineSpecialReg(SpecialReg);

    /* Convert the CPUArchState of aggregate type to the list of single element
     * of primitive type. */
    intptr_t Off = 0;
    FlattenCPUState(Base->getType()->getContainedType(0), Off, SpecialReg);
}

/*
 * This function defines the special registers and the new types to be reset.
 */
void LLVMTranslator::DefineSpecialReg(std::map<Type *, Type *> &SpecialReg)
{
#if defined(TARGET_I386)
    Value *SIMDReg = Mod->getNamedValue("xmm_reg");
    if (SIMDReg) {
        /* remap XMMReg --> <64 x i8> */
        Type *Int8Ty = IntegerType::get(Context, 8);
        Type *OldTy = SIMDReg->getType()->getContainedType(0);
        Type *NewTy = VectorType::get(Int8Ty, 16);
        SpecialReg[OldTy] = NewTy;
    }
#endif
}

/*
 * Convert the CPUArchState of the aggregate type to a list of single element of
 * primitive type. Each element contains a pair of offset to CPUArchState and its
 * type. This list of flattened type will be used for the state mapping pass.
 */
void LLVMTranslator::FlattenCPUState(Type *Ty, intptr_t &Off,
                                     std::map<Type *, Type *> &SpecialReg)
{
    switch (Ty->getTypeID()) {
        default:
        {
            StateType[Off] = Ty;
            Off += DL->getTypeSizeInBits(Ty) / 8;
            break;
        }
        case Type::StructTyID:
        {
            /* Map a special register to another type with the same size as the
             * original type. E.g., mapping a <16 * i8> type to <2 * i64>. */
            if (SpecialReg.find(Ty) != SpecialReg.end()) {
                Type *NewTy = SpecialReg[Ty];
                StateType[Off] = NewTy;
                Off += DL->getTypeSizeInBits(Ty) / 8;
                break;
            }

            StructType *STy = cast<StructType>(Ty);
            intptr_t Size = DL->getTypeSizeInBits(STy) / 8;
            intptr_t SubOff;

            const StructLayout *SL = DL->getStructLayout(STy);
            for (unsigned i = 0, e = STy->getNumElements(); i != e; ++i) {
                SubOff = Off + SL->getElementOffset(i);
                FlattenCPUState(STy->getElementType(i), SubOff, SpecialReg);
            }

            Off += Size;

            /* Structure could have padding at the end of the struct. Expand
             * the size of the last struct member by adding the padding size. */
            if (Off != SubOff) {
                intptr_t LastOff = StateType.rbegin()->first;
                intptr_t NewSize = (Off - LastOff) * 8;
                Type *NewTy = IntegerType::get(Context, NewSize);
                StateType[LastOff] = NewTy;
            }
            break;
        }
        case Type::ArrayTyID:
        {
#if defined(CONFIG_SOFTMMU)
            /* Do not flatten the SoftTLB because it could create a huge amount
             * of flattened states. */
            if (Off == offsetof(CPUArchState, tlb_table[0][0])) {
                StateType[Off] = Ty;
                Off += DL->getTypeSizeInBits(Ty) / 8;
                break;
            }
#endif
            ArrayType *ATy = cast<ArrayType>(Ty);
            intptr_t ElemSize = DL->getTypeSizeInBits(ATy->getElementType()) / 8;
            for (unsigned i = 0, e = ATy->getNumElements(); i != e; ++i) {
                intptr_t SubOff = Off + i * ElemSize;
                FlattenCPUState(ATy->getElementType(), SubOff, SpecialReg);
            }
            Off += DL->getTypeSizeInBits(ATy) / 8;
            break;
        }
    }
}

static inline void Materialize(Function &F)
{
#if defined(LLVM_V35)
    std::string ErrInfo;
    F.Materialize(&ErrInfo);
#else
    F.materialize();
#endif
}

/*
 * Materialize helper functions and compute inline costs.
 */
void LLVMTranslator::InitializeHelpers()
{
    /* Set target-specific symbols. */
    AddDependentSymbols(this);

    /* Set const helpers. (i.e., helpers that have no side effect) */
    InitializeConstHelpers();

    /* Materialize fpu helper functions. */
    TCGHelperInfo *FPUHelper = (TCGHelperInfo *)get_native_fpu_helpers();
    for (int i = 0, e = num_native_fpu_helpers(); i != e; ++i) {
        std::string ErrInfo;
        Function *Func = Mod->getFunction(FPUHelper[i].name);
        if (Func && Func->isMaterializable())
            Materialize(*Func);
    }

    /* Materialize defined helper functions that are allowed for inlining. */
    for (int i = 0, e = ARRAY_SIZE(include_helper); i < e; ++i) {
        std::string ErrInfo;
        Helpers[include_helper[i]] = new HelperInfo;
        Function *Func = Mod->getFunction(include_helper[i]);
        if (Func && Func->isMaterializable())
            Materialize(*Func);
    }

    /* Initialize all TCG helper functions. */
    const TCGHelperInfo *all_helpers = get_tcg_helpers();
    for (int i = 0, e = tcg_num_helpers(); i != e; ++i) {
        uintptr_t func = (uintptr_t)all_helpers[i].func;
        const char *name = all_helpers[i].name;
        if (!name)
            hqemu_error("invalid helper name.\n");

        TCGHelpers[func] = std::string("helper_") + std::string(name);
    }

    for (int i = 0, e = tcg_num_helpers(); i != e; ++i) {
        std::string FName = std::string("helper_") +
                            std::string(all_helpers[i].name);
        std::string FNameNoInline = FName + std::string("_noinline");
        if (Helpers.find(FName) != Helpers.end()) {
            HelperInfo *Helper = Helpers[FName];
            Function *F = Mod->getFunction(FName);
            if (!F)
                hqemu_error("fatal error - %s\n", FName.c_str());
            Helper->Func = F;
            Mod->getOrInsertFunction(FNameNoInline, F->getFunctionType());
            Helper->FuncNoInline = Mod->getFunction(FNameNoInline);
            Helpers[FNameNoInline] = Helper;

            AddSymbol(FNameNoInline, all_helpers[i].func);
        }
    }

    /* Analyze the inline cost for each helper function and make a non-inlined
     * counterpart object in LLVM Module. For the non-inlined function, just
     * remap the function address in LLVM module which causes the JIT to emit a
     * call instruction to the function address. */
    for (int i = 0, e = tcg_num_helpers(); i != e; ++i) {
        const TCGHelperInfo *th = &all_helpers[i];
        std::string FName = std::string("helper_") + std::string(th->name);
        if (Helpers.find(FName) != Helpers.end()) {
            HelperInfo *Helper = Helpers[FName];
            bool ret = OptimizeHelper(*Helper);
            if (!ret) {
                /* If the helper function consists of loops, it is not suitable
                 * to be inlined because it conflicts to the state mapping
                 * pass. */
                Helpers.erase(FName);
                goto skip;
            }

            Helper->CalculateMetrics(Helper->Func);
            continue;
        }
skip:
        AddSymbol(FName, th->func);
    }

    /* Add all states of the nested helpers to the calling helper.
     * Then, calculate state boundary and determine if we can know all states
     * (included in the nested functions) by this helper function.
     *
     * Note that we only allow one-level helper inlining. */
    for (auto &I : Helpers) {
        HelperInfo *Helper = I.second;
        bool hasNestNestedCall = false;
        for (CallInst *CI : Helper->NestedCalls) {
            std::string FName = CI->getCalledFunction()->getName();
            HelperInfo *NestedHelper = Helpers[FName];
            Helper->States.insert(Helper->States.begin(),
                                  NestedHelper->States.begin(),
                                  NestedHelper->States.end());

            CI->setCalledFunction(NestedHelper->FuncNoInline);
            if (I.first != FName && NestedHelper->hasNestedCall)
                hasNestNestedCall = true;
        }
        /* Clear hasNestedCall if onle one level nested functions. If the
         * helper has only one level nested helpers, then all states are found. */
        Helper->hasNestedCall = hasNestNestedCall;

        /* Compute state boundaries. */
        StateAnalyzer Analyzer(DL);
        for (auto J : Helper->States)
            Analyzer.addStateRef(J.first, J.second);

        StateRange Reads, Writes;
        Analyzer.computeStateRange(Reads, Writes);

        Helper->insertState(Reads, false);
        Helper->insertState(Writes, true);
    }

    for (auto &I : Helpers) {
        HelperInfo *Helper = I.second;
        Helper->States.clear();
        Helper->NestedCalls.clear();
    }
}

void LLVMTranslator::InitializeDisasm()
{
    std::string TargetTriple = "UnknownArch";

#if defined(TARGET_I386)
  #if defined(TARGET_X86_64)
    TargetTriple = "x86_64";
  #else
    TargetTriple = "i386";
  #endif
#elif defined(TARGET_ARM)
  #if defined(TARGET_AARCH64)
    TargetTriple = "aarch64";
  #else
    TargetTriple = "arm";
  #endif
#elif defined(TARGET_PPC)
    TargetTriple = "ppc";
#endif

   GuestDisAsm = MCDisasm::CreateMCDisasm(TargetTriple, false);
   HostDisAsm = MCDisasm::CreateMCDisasm(Mod->getTargetTriple(), true);

   if (GuestDisAsm)
       dbg() << DEBUG_INASM << __func__
             << ": use LLVM disassembler for guest (" << TargetTriple << ").\n";
   else
       dbg() << DEBUG_INASM << __func__
             << ": can't find LLVM disassembler for guest ("
             << TargetTriple << "). Use QEMU disas.\n";

   if (HostDisAsm)
       dbg() << DEBUG_OUTASM << __func__
             << ": use LLVM disassembler for host ("
             << Mod->getTargetTriple() << ").\n";
   else
       dbg() << DEBUG_OUTASM << __func__
             << ": can't find LLVM disassembler for host ("
             << Mod->getTargetTriple() << "). Use QEMU disas.\n";
}

static bool isLegalIntrinsic(IntrinsicInst *II)
{
    switch (II->getIntrinsicID()) {
        case Intrinsic::memset:
        case Intrinsic::memcpy:
        case Intrinsic::memmove:
        case Intrinsic::dbg_declare:
            return false;
        default:
            break;
    }
    return true;
}

/*
 * Determine if the function argument and Ptr are alias.
 */
static Value *isFromFuncArgument(Function &F, Value *Ptr)
{
    Ptr = StripPointer(Ptr);
    for (auto I = F.arg_begin(), E = F.arg_end(); I != E; ++I) {
        if (Ptr == &*I)
            return Ptr;
    }
    return nullptr;
}

/* Create function pass manager to optimize the helper function. */
static void Optimize(Function &F)
{
    auto FPM = new legacy::FunctionPassManager(F.getParent());

    FPM->add(createReplaceIntrinsic());
    if (!DisableFastMath)
        FPM->add(createFastMathPass());
    FPM->run(F);

    delete FPM;
}

/* Analyze and optimize a helper function. */
bool LLVMTranslator::OptimizeHelper(HelperInfo &Helper)
{
    Function &F = *Helper.Func;

    /* We don't want to inline helper functions that contain loop. */
    SmallVector<std::pair<const BasicBlock*,const BasicBlock*>, 32> BackEdges;
    FindFunctionBackedges(F, BackEdges);
    if (BackEdges.size())
        return false;

    Optimize(F);

    /* Collect and analyze memory and call instructions. */
    SmallVector<CallInst *, 16> Calls;
    for (auto II = inst_begin(F), EE = inst_end(F); II != EE; ++II) {
        Instruction *I = &*II;

        if (isa<LoadInst>(I) || isa<StoreInst>(I)) {
            intptr_t Off = 0;
            Value *Base = getBaseWithConstantOffset(DL, getPointerOperand(I), Off);

            /* XXX: We assume the pointer is derived from the function argument.
             *      Skip it if not from the the function argument. */
            Value *Arg = isFromFuncArgument(F, Base);
            if (!Arg)
                return false;

            if (Base->getType() == BaseReg[TCG_AREG0].Ty) {
                /* This is a load/store of CPU state plus a constant offset.
                 * Track the state. */
                Helper.States.push_back(std::make_pair(I, Off));
            } else {
                /* This is a load/store of unknown pointer.
                 * Track the maximum access size. */
                Type *Ty = cast<PointerType>(Arg->getType())->getElementType();
                intptr_t Size = DL->getTypeSizeInBits(Ty) / 8;
                Helper.mayConflictArg = true;
                Helper.ConflictSize = std::max(Helper.ConflictSize, Size);
            }
        } else if (CallInst *CI = dyn_cast<CallInst>(I)) {
            Calls.push_back(CI);
        }
    }

    /* Analyze calls. */
    for (CallInst *CI : Calls) {
        if (CI->isInlineAsm())
            continue;

        if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(CI)) {
            if (!isLegalIntrinsic(II))
                return false;
            continue;
        }

        if (!CI->getCalledFunction())
            return false;

        std::string FName = CI->getCalledFunction()->getName();
        if (isLibcall(FName) || isSoftFPcall(FName)) {
            /* Libcalls/SoftFPCalls are always const function. Mark it. */
            ConstantInt *Meta[] = { CONST32(0) };
            MDFactory::setConstStatic(Context, CI, Meta);
            continue;
        }

        if (Helpers.find(FName) == Helpers.end())
            return false;

        Helper.hasNestedCall = true;
        Helper.NestedCalls.push_back(CI);
    }

    return true;
}

/* 
 * Figure out an approximation for how many instructions will be constant
 * folded if the specified value is constant.
 */
static unsigned CountCodeReductionForConstant(Value *V, CodeMetrics &Metrics)
{
    unsigned IndirectCallBonus;
    IndirectCallBonus = -InlineConstants::IndirectCallThreshold;

    unsigned Reduction = 0;
    for (Value::use_iterator UI = V->use_begin(), E = V->use_end(); UI != E;++UI) {
        User *U = UI->getUser();
        if (isa<BranchInst>(U) || isa<SwitchInst>(U)) {
            /* We will be able to eliminate all but one of the successors. */
            const TerminatorInst &TI = cast<TerminatorInst>(*U);
            const unsigned NumSucc = TI.getNumSuccessors();
            unsigned Instrs = 0;
            for (unsigned I = 0; I != NumSucc; ++I)
                Instrs += Metrics.NumBBInsts[TI.getSuccessor(I)];
            /* We don't know which blocks will be eliminated, so use the average size. */
            Reduction += InlineConstants::InstrCost*Instrs*(NumSucc-1)/NumSucc*2;
        } else if (CallInst *CI = dyn_cast<CallInst>(U)) {
            /* Turning an indirect call into a direct call is a BIG win */
            if (CI->getCalledValue() == V)
                Reduction += IndirectCallBonus;
        } else if (InvokeInst *II = dyn_cast<InvokeInst>(U)) {
            /* Turning an indirect call into a direct call is a BIG win */
            if (II->getCalledValue() == V)
                Reduction += IndirectCallBonus;
        } else {
            Instruction &Inst = cast<Instruction>(*U);
            
            if (Inst.mayReadFromMemory() || Inst.mayHaveSideEffects() ||
                    isa<AllocaInst>(Inst))
                continue;
            
            bool AllOperandsConstant = true;
            for (unsigned i = 0, e = Inst.getNumOperands(); i != e; ++i)
                if (!isa<Constant>(Inst.getOperand(i)) && Inst.getOperand(i) != V) {
                    AllOperandsConstant = false;
                    break;
                }
            
            if (AllOperandsConstant) {
                /* We will get to remove this instruction... */
                Reduction += InlineConstants::InstrCost;
                Reduction += CountCodeReductionForConstant(&Inst, Metrics);
            }
        }
    }
    return Reduction;
}

/* 
 * Figure out an approximation of how much smaller the function will be if
 * it is inlined into a context where an argument becomes an alloca.
 */
static unsigned CountCodeReductionForAlloca(Value *V) 
{
    if (!V->getType()->isPointerTy()) return 0; 

    unsigned Reduction = 0;
    for (Value::use_iterator UI = V->use_begin(), E = V->use_end(); UI != E;++UI) {
        Instruction *I = cast<Instruction>(UI->getUser());

        if (isa<LoadInst>(I) || isa<StoreInst>(I))
            Reduction += InlineConstants::InstrCost;
        else if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(I)) {
            /* If the GEP has variable indices, we won't be able to do much with it. */
            if (GEP->hasAllConstantIndices())
                Reduction += CountCodeReductionForAlloca(GEP);
        } else if (BitCastInst *BCI = dyn_cast<BitCastInst>(I)) {
            /* Track pointer through bitcasts. */
            Reduction += CountCodeReductionForAlloca(BCI);
        } else
            return 0;
    }
    
    return Reduction;
}

void HelperInfo::CalculateMetrics(Function *F)
{
    Metrics.NumInsts = 0;

    for (auto FI = F->begin(); FI != F->end(); FI++) {
        unsigned NumInsts = 0;
        BasicBlock *BB = &*FI;
        for (auto BI = FI->begin(); BI != FI->end(); BI++) {
            if (isa<PHINode>(BI)) /* PHI nodes don't count. */
                continue;
            NumInsts++;
        }
        Metrics.NumBlocks++;
        Metrics.NumInsts += NumInsts;
        Metrics.NumBBInsts[BB] = NumInsts;
    }

    ArgumentWeights.reserve(F->arg_size());
    for (auto I = F->arg_begin(), E = F->arg_end(); I != E; ++I) {
        Value *V = &*I;
        ArgumentWeights.push_back(ArgInfo(
                    CountCodeReductionForConstant(V, Metrics),
                    CountCodeReductionForAlloca(V)));
    }
}

void LLVMTranslator::InitializeConstHelpers()
{
#if defined(TARGET_I386)
    ConstHelpers.insert("helper_outb");
    ConstHelpers.insert("helper_inb");
    ConstHelpers.insert("helper_outw");
    ConstHelpers.insert("helper_inw");
    ConstHelpers.insert("helper_outl");
    ConstHelpers.insert("helper_inl");
#endif
}

void LLVMTranslator::Abort(TraceBuilder &Builder)
{
    target_ulong pc = Builder.getEntryNode()->getGuestPC();
    dbg() << DEBUG_LLVM << __func__
          << ": abort trace pc " << format("0x%" PRIx "", pc) << "\n";
}

/*
 * Make a jump from the head block in the block code cache to the translated
 * host code of this region in the optimized code cache. Also patch previous
 * built regions that have direct branch to this region.
 */
void LLVMTranslator::Commit(TraceBuilder &Builder)
{
    bool Invalid = false;
    OptimizationInfo *Opt = Builder.getOpt();
    TraceInfo *Trace = Builder.getTrace();
    TBVec &TBs = Trace->TBs;

    for (unsigned i = 0, e = TBs.size(); i != e; ++i) {
        if (TBs[i]->mode == BLOCK_INVALID) {
            Invalid = true;
            break;
        }
    }

    if (Invalid || llvm_check_cache() == 1) {
        delete Trace;
        delete Opt;
        return;
    }

    TranslatedCode *TC = new TranslatedCode;
    TC->Active = true;
    TC->Size = NI.Size;
    TC->Code = NI.Code;
    TC->EntryTB = Trace->getEntryTB();
    TC->Restore = NI.Restore;
    TC->Trace = Trace;

    /* If we go here, this is a legal trace. */
    LLVMEnv::ChainSlot &ChainPoint = LLEnv->getChainPoint();
    TranslationBlock *EntryTB = TC->EntryTB;

    hqemu::MutexGuard locked(llvm_global_lock);

    for (unsigned i = 0; i != NI.NumChainSlot; ++i)
        ChainPoint[NI.ChainSlot[i].Key] = NI.ChainSlot[i].Addr;

    TraceID tid = LLEnv->insertTransCode(TC);
    EntryTB->tid = tid;
    EntryTB->mode = BLOCK_OPTIMIZED;
    EntryTB->opt_ptr = TC->Code;

    /* Set the jump from the block to the trace */
    patch_jmp(tb_get_jmp_entry(EntryTB), TC->Code);

    if (!PF.isEnabled()) {
        delete Trace;
        TC->Trace = nullptr;
    }

    delete Opt;
}

void LLVMTranslator::dump(CPUArchState *env, TranslationBlock *tb)
{
    auto &DebugMode = DM.getDebugMode();
    if (DebugMode & (DEBUG_INASM | DEBUG_OP)) {
        hqemu::MutexGuard locked(llvm_debug_lock);
        dbg() << DEBUG_LLVM << "Translator " << MyID << " dumps asm...\n";
        if (DebugMode & DEBUG_INASM)
            printAsm(Env, tb);
        if (DebugMode & DEBUG_OP)
           printOp(Env, tb);
    }
}

void LLVMTranslator::GenBlock(CPUArchState *env, OptimizationInfo *Opt)
{
    struct timeval start, end;
    if (PF.isEnabled())
        gettimeofday(&start, nullptr);

    TraceBuilder Builder(IF, Opt);
    GraphNode *Node = Builder.getNextNode();
    if (!Node)
        hqemu_error("fatal error.\n");

    Builder.ConvertToTCGIR(env);

    if (DM.getDebugMode() & (DEBUG_INASM | DEBUG_OP))
        dump(env, Opt->getCFG()->getTB());

    Builder.ConvertToLLVMIR();
    Builder.Finalize();

    if (PF.isEnabled()) {
        gettimeofday(&end, nullptr);
        Builder.getTrace()->setTransTime(&start, &end);
    }

    Commit(Builder);
}

void LLVMTranslator::GenTrace(CPUArchState *env, OptimizationInfo *Opt)
{
    struct timeval start, end;
    if (PF.isEnabled())
        gettimeofday(&start, nullptr);

    TraceBuilder Builder(IF, Opt);
    for (;;) {
        GraphNode *Node = Builder.getNextNode();
        if (!Node)
            break;

        Builder.ConvertToTCGIR(Env);

        if (DM.getDebugMode() & (DEBUG_INASM | DEBUG_OP))
            dump(Env, Node->getTB());

        Builder.ConvertToLLVMIR();

        if (Node->getTB()->mode == BLOCK_INVALID || Builder.isAborted()) {
            Abort(Builder);
            return;
        }
    }
    Builder.Finalize();

    if (PF.isEnabled()) {
        gettimeofday(&end, nullptr);
        Builder.getTrace()->setTransTime(&start, &end);
    }

    Commit(Builder);
}

/*
 * Display the guest assembly code of the given basic block.
 */
void LLVMTranslator::printAsm(CPUArchState *env, TranslationBlock *tb)
{
    auto &OS = DM.debug();
    if (GuestDisAsm) {
        OS << "----------------\n"
           << "IN: [size=" << tb->size << "]\n";
#if defined(CONFIG_USER_ONLY)
        GuestDisAsm->PrintInAsm((uint64_t)g2h(tb->pc), tb->size, tb->pc);
#else
        GuestDisAsm->PrintInAsm((uint64_t)tb->image, tb->size, tb->pc);
#endif
        OS << "\n";
        return;
    }

#if defined(CONFIG_USER_ONLY)
    /* The guest is not supported by the LLVM MCDisassembler. Use QEMU disas. */
    int disas_flags = 0;

#if defined(TARGET_I386)
  #if defined(TARGET_X86_64)
    if ((tb->flags >> HF_CS64_SHIFT) & 1)
        disas_flags = 2;
    else
  #endif
        disas_flags = !((tb->flags >> HF_CS32_SHIFT) & 1);
#elif defined(TARGET_ARM)
  #if defined(TARGET_AARCH64)
    disas_flags = 4 | (0 << 1);
  #else
    disas_flags = env->thumb;
  #endif
#elif defined(TARGET_PPC)
    int le_mode = env->hflags & (1 << MSR_LE) ? 1 : 0;
    disas_flags = env->bfd_mach;
    disas_flags |= le_mode << 16;
#endif
    
    OS << "----------------\n";
    OS << "IN: [size=" << tb->size << "%d]\n";
    target_disas(stderr, ENV_GET_CPU(env), tb->pc, tb->size, disas_flags);
    OS << "\n";
#endif
}

/*
 * Display TCG IR of the given basic block.
 */
void LLVMTranslator::printOp(CPUArchState *env, TranslationBlock *tb)
{
    auto &OS = DM.debug();
    OS << "OP:\n";
    tcg_dump_ops_stderr(&tcg_ctx);
    OS << "\n";
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
