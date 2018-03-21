/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "llvm-debug.h"
#include "llvm-opc.h"
#include "llvm-target.h"
#include "llvm-pass.h"

#define PASS_NAME "StateMapping"


/*
 * StateMappingPass is used to eliminate the redundant loads and stores to the
 * CPUArchState. The loads and stores of the guest memory operations are not
 * removed in order not to violate the memory model of the guest architecture.
 *
 * The state mapping rules are:
 * - A guest state is not overlapped: (i.e., same access size)
 *   - Same type: map to this type.
 *   - Different type: select type in the order: vector, float and integer;
 *                     use bitcast to convert between different types.
 * - A guest state is overlapped with other state(s):
 *   - Query StateType to find state size (i.e., boundary) and type:
 *     - Vector type: use insert/extract to manipulate a vector element.
 *     - Other types: use shift to manipulate a vector element.
 */
class StateMappingPass : public FunctionPass  {
    IRFactory *IF; /* Uplink to the IRFactory */

public:
    static char ID;
    explicit StateMappingPass() : FunctionPass(ID) {}
    explicit StateMappingPass(IRFactory *IF) : FunctionPass(ID), IF(IF) {}

    bool runOnFunction(Function &F);
};

struct StateMapping {
    StateMapping()
        : State(nullptr), Addr(nullptr), Ty(nullptr), AI(nullptr),
          hasLoad(false), hasStore(false) {}

    StateData *State;
    Value *Addr;
    Type *Ty;
    AllocaInst *AI;
    bool hasLoad;
    bool hasStore;

    intptr_t getSize()  { return State->End - State->Start; }
    intptr_t getStart() { return State->Start;     }
    intptr_t getEnd()   { return State->End;       }
    Value *getAddr()    { return Addr;             }
    Type *getType()     { return Ty;               }
    bool isVector()     { return Ty->isVectorTy(); }

    bool overlap(StateRange &Range) {
        if (Range.empty())
            return false;
        intptr_t Start = getStart();
        intptr_t End = getEnd();
        auto I = --Range.upper_bound(Start);
        for (; I != Range.end() && I->first < End; ++I) {
            if (I->second > Start)
                return true;
        }
        return false;
    }
};

struct ElementInfo {
    ElementInfo() : Shift(0), NumElts(0), EltTy(nullptr), StateTy(nullptr) {}

    intptr_t Shift;
    unsigned NumElts;
    Type *EltTy;
    Type *StateTy;
};

class StateMapper {
    typedef std::vector<StateMapping> StateMapList;

    IRFactory *IF;
    const DataLayout *DL;
    Instruction *CPU;         /* The CPU pointer */
    Instruction *PreCastPos;  /* The position to cast CPU states */
    Instruction *PreLoadPos;  /* The position to preload CPU states */
    IVec toErase;             /* The instructions to be removed */

    FlatType &StateType;
    StateAnalyzer Analyzer;
    StateMapList StateMaps;

public:
    StateMapper(IRFactory *IF)
        : IF(IF), DL(IF->getDL()), StateType(IF->getTranslator().getStateType()),
          Analyzer(DL) {}

    bool run(Function &F) {
        if (!init(F))
            return false;

        AnalyzeState(F);
        if (!StateMaps.empty())
            PromoteState(F);

        ProcessErase(toErase);
        return true;
    }

    /* Rearrange instructions in the 'init' block. */
    bool init(Function &F);

    /* Analyze instructions in a Function that access CPU states. */
    void AnalyzeState(Function &F);

    /* Compute state mapping information. */
    void ComputeStateMap(StateMapping &StateMap, StateData &State);

    /* Determine if the state can be operated as a vector. */
    Type *TryVectorState(StateData &State, Type *Ty);

    /* Map state references to the virtual states. */
    void PromoteState(Function &F);

    /* Rewrite state loads and stores. */
    void RewriteLoad(StateMapping &StateMap, StateRef &Ref);
    void RewriteStore(StateMapping &StateMap, StateRef &Ref);
    void RewriteLoadVector(StateMapping &StateMap, StateRef &Ref);
    void RewriteStoreVector(StateMapping &StateMap, StateRef &Ref);

    /* Compute state and element types for element insertion and extraction. */
    void getElementInfo(StateMapping &StateMap, StateRef &Ref, ElementInfo &Info);

    /* Sync CPU states around helper calls. */
    void SyncHelperState();

    /* Store dirty states at the leaf blocks. */
    void ProcessExitBB(BasicBlock *BB);

    /* Get the pointer without GEP and BitCast. */
    void StripPointer(Value *V, IVec &IV);

    /* Move the pointer before InsertPos. */
    void MoveStatePointer(Value *V);

    /* Load state from Src and store it to Dest. */
    void CopyState(Value *Dest, Value *Src, Instruction *InsertPos);

    bool isLegalState(Value *Ptr, intptr_t &Off);

    /* Return true if the input is alias of a state pointer. */
    bool isStatePointer(Value *V) {
        if (auto BCI = dyn_cast<BitCastInst>(V)) {
            if (BCI->getOperand(0) == CPU)
                return true;
            return isStatePointer(BCI->getOperand(0));
        } else if (auto GEP = dyn_cast<GetElementPtrInst>(V))
            return GEP->getOperand(0) == CPU;
        return false;
    }

    bool isSimpleFunction(Function *F) {
        HelperMap &Helpers = IF->getHelpers();
        if (Helpers.find(F->getName()) == Helpers.end() ||
            Helpers[F->getName()]->hasNestedCall)
            return false;
        return true;
    }

    Value *ConvertType(Value *V, Type *Ty, Instruction *InsertPos) {
        return V->getType() == Ty ? V : new BitCastInst(V, Ty, "", InsertPos);
    }
};

/* Return a pre-defined state name. */
static std::string getStateName(intptr_t Off)
{
#if defined(TARGET_I386)
    if (Off == offsetof(CPUArchState,xmm_regs[0])) return "xmm0";
    if (Off == offsetof(CPUArchState,xmm_regs[1])) return "xmm1";
    if (Off == offsetof(CPUArchState,xmm_regs[2])) return "xmm2";
    if (Off == offsetof(CPUArchState,xmm_regs[3])) return "xmm3";
    if (Off == offsetof(CPUArchState,xmm_regs[4])) return "xmm4";
    if (Off == offsetof(CPUArchState,xmm_regs[5])) return "xmm5";
    if (Off == offsetof(CPUArchState,xmm_regs[6])) return "xmm6";
    if (Off == offsetof(CPUArchState,xmm_regs[7])) return "xmm7";
    if (Off == offsetof(CPUArchState,xmm_t0)) return "xmm_t0";
#endif
    return "";
}

/* Determine if the offset is to access the temporary state. */
static inline bool isLocalState(intptr_t Off)
{
#if defined(TARGET_I386)
    if (Off == offsetof(CPUArchState, xmm_t0))
        return true;
#endif
    return false;
}

/* Return states that should be ignored during state mapping. */
static bool isSkipState(intptr_t Off)
{
    if (Off == (intptr_t)(offsetof(CPUState, tcg_exit_req) - ENV_OFFSET))
        return true;
#if defined(TARGET_ARM)
    if (Off == offsetof(CPUArchState, vfp.fp_status.float_detect_tininess) ||
        Off == offsetof(CPUArchState, vfp.fp_status.float_rounding_mode) ||
        Off == offsetof(CPUArchState, vfp.fp_status.float_exception_flags) ||
        Off == offsetof(CPUArchState, vfp.fp_status.floatx80_rounding_precision) ||
        Off == offsetof(CPUArchState, vfp.fp_status.flush_to_zero) ||
        Off == offsetof(CPUArchState, vfp.fp_status.flush_inputs_to_zero) ||
        Off == offsetof(CPUArchState, vfp.fp_status.default_nan_mode))
        return true;
#elif defined(TARGET_I386)
    if (Off == offsetof(CPUArchState, fp_status.float_detect_tininess) ||
        Off == offsetof(CPUArchState, fp_status.float_rounding_mode) ||
        Off == offsetof(CPUArchState, fp_status.float_exception_flags) ||
        Off == offsetof(CPUArchState, fp_status.floatx80_rounding_precision) ||
        Off == offsetof(CPUArchState, fp_status.flush_to_zero) ||
        Off == offsetof(CPUArchState, fp_status.flush_inputs_to_zero) ||
        Off == offsetof(CPUArchState, fp_status.default_nan_mode))
        return true;
#endif
    return false;
}

/* Check if the state is legal for state mapping. A legal state must have CPU
 * as the base pointer, plus a positive constant offset. */
bool StateMapper::isLegalState(Value *Ptr, intptr_t &Off)
{
    Value *Base = getBaseWithConstantOffset(DL, Ptr, Off);
    if (Off < 0)
        return false;
    if (Base == CPU && !isSkipState(Off) && !IRFactory::isStateOfPC(Off))
        return true;
    return false;
}

/* Get the pointer without GEP and BitCast. The stripped GEP and BitCast
 * instructions are returned to the caller. */
void StateMapper::StripPointer(Value *V, IVec &IV)
{
    std::set<Value *> Visited;
    Visited.insert(V);
    do {
        if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(V)) {
            IV.push_back(GEP);
            V = GEP->getOperand(0);
        } else if (BitCastInst *BCI = dyn_cast<BitCastInst>(V)) {
            IV.push_back(BCI);
            V = BCI->getOperand(0);
        } else
            return;
        if (Visited.find(V) != Visited.end())
            break;
        Visited.insert(V);
    } while (true);
}

/* Move the pointer before InsertPos. */
void StateMapper::MoveStatePointer(Value *V)
{
    IVec toMove;
    StripPointer(V, toMove);
    while (!toMove.empty()) {
        Instruction *I = toMove.back();
        toMove.pop_back();
        if (I->getParent() == CPU->getParent())
            continue;
        I->moveBefore(PreCastPos);
    }
}

/* Copy state data from src address to destination address. */
void StateMapper::CopyState(Value *Dest, Value *Src, Instruction *InsertPos)
{
    if (!isa<AllocaInst>(Src)) {
        MoveStatePointer(Src);
        LoadInst *LI = new LoadInst(Src, "", false, InsertPos);
        new StoreInst(LI, Dest, false, InsertPos);

        if (Src->getType()->getPointerElementType()->isVectorTy())
            LI->setAlignment(4);
    } else {
        MoveStatePointer(Dest);
        LoadInst *LI = new LoadInst(Src, "", false, InsertPos);
        StoreInst *SI = new StoreInst(LI, Dest, false, InsertPos);

        if (Dest->getType()->getPointerElementType()->isVectorTy())
            SI->setAlignment(4);
    }
}

/* Store dirty states at the leaf blocks. */
void StateMapper::ProcessExitBB(BasicBlock *BB)
{
    Instruction *InsertPos = nullptr;
    for (auto BI = BB->begin(), BE = BB->end(); BI != BE; ++BI) {
        if (MDFactory::isExit(&*BI)) {
            InsertPos = &*BI;
            break;
        }
    }
    if (!InsertPos)
        InsertPos = BB->getTerminator();

    for (auto &StateMap : StateMaps) {
        if (!StateMap.hasStore || isLocalState(StateMap.getStart()))
            continue;
        CopyState(StateMap.Addr, StateMap.AI, InsertPos);
    }
}

/* Sync CPU states around helper calls. */
void StateMapper::SyncHelperState()
{
    CallList &Calls = Analyzer.getCalls();
    if (Calls.empty())
        return;

    /*
     * Rules of syncing states around calls:
     * 1. Dirty states (i.e., stores) are written back before calls.
     * 2. All states, including loads and stores, are read back after calls.
     *
     * If the helper is a simple function, only dependent states are synced.
     * If the helper is a complicated function, all states are synced.
     */
    HelperMap &Helpers = IF->getHelpers();
    DenseMap<CallInst*, std::set<unsigned> > StoreBeforeCall;
    DenseMap<CallInst*, std::set<unsigned> > LoadAfterCall;

    for (auto CI : Calls) {
        Function *Func = CI->getCalledFunction();
        std::string Name = Func->getName();

        if (isSimpleFunction(Func)) {
            /* A pre-defined helper without nested call. */
            HelperInfo *Helper = Helpers[Name];
            for (unsigned i = 0, e = StateMaps.size(); i != e; ++i) {
                auto &StateMap = StateMaps[i];
                if (StateMap.hasStore && StateMap.overlap(Helper->StateUse))
                    StoreBeforeCall[CI].insert(i);

                if (StateMap.overlap(Helper->StateDef))
                    LoadAfterCall[CI].insert(i);

                if (Helper->mayConflictArg) {
                    unsigned NumArgs = CI->getNumArgOperands();
                    for (unsigned j = 1; j < NumArgs; ++j) {
                        intptr_t Off = 0;
                        Value *Arg = CI->getArgOperand(j);
                        if (!isLegalState(Arg, Off))
                            continue;
                        if (Off + Helper->ConflictSize <= StateMap.getStart() ||
                            Off >= StateMap.getEnd())
                            continue;
                        if (StateMap.hasStore)
                            StoreBeforeCall[CI].insert(i);
                        LoadAfterCall[CI].insert(i);
                    }
                }
            }
        } else {
            /* Sync states for a complicated function (an unknown helper or a
             * helper with nested calls). */
            for (unsigned i = 0, e = StateMaps.size(); i != e; ++i) {
                auto &StateMap = StateMaps[i];
                if (StateMap.hasStore)
                    StoreBeforeCall[CI].insert(i);
                LoadAfterCall[CI].insert(i);
            }
        }
    }

    /* Perform state syncing. */
    for (auto CI : Calls) {
        Instruction *InsertPos = CI;

        if (!StoreBeforeCall.empty()) {
            for (auto i : StoreBeforeCall[CI]) {
                auto &StateMap = StateMaps[i];
                CopyState(StateMap.Addr, StateMap.AI, InsertPos);
            }
        }

        InsertPos = &*std::next(BasicBlock::iterator(InsertPos));
        if (isa<UnreachableInst>(InsertPos)) {
            /* No read back is required after tail call. */
            continue;
        }

        if (!LoadAfterCall.empty()) {
            for (auto i : LoadAfterCall[CI]) {
                auto &StateMap = StateMaps[i];
                CopyState(StateMap.AI, StateMap.Addr, InsertPos);
            }
        }
    }
}

static inline bool isSameSize(StateMapping &StateMap, StateRef &Ref)
{
    return StateMap.getSize() == Ref.getSize();
}

/* Compute state and element types for element insertion and extraction. */
void StateMapper::getElementInfo(StateMapping &StateMap, StateRef &Ref,
                                 ElementInfo &Info)
{
    intptr_t StateSize = StateMap.getSize();
    intptr_t Size = Ref.getSize();
    intptr_t Shift = Ref.Start - StateMap.getStart();
    Type *StateTy = StateMap.getType();
    LLVMContext &Context = StateTy->getContext();

    if (!StateMap.isVector()) {
        /* Use int-N to emulate the state. */
        Info.NumElts = 1;
        Info.EltTy = Type::getIntNTy(Context, Size * 8);
        Info.StateTy = Type::getIntNTy(Context, StateSize * 8);
        Info.Shift = Shift;
        return;
    }

    /* The state is emulated as a vector. */
    if (StateSize % Size == 0 && Shift % Size == 0) {
        Type *EltTy = Type::getIntNTy(Context, Size * 8);

        Info.NumElts = 1;
        Info.EltTy = EltTy;
        Info.StateTy = VectorType::get(EltTy, StateSize / Size);
        Info.Shift = Shift / Size;
    } else {
        VectorType *VecTy = cast<VectorType>(StateTy);
        Type *EltTy = VecTy->getScalarType();
        intptr_t EltSize = DL->getTypeSizeInBits(EltTy) / 8;

        Info.NumElts = Size / EltSize;
        Info.EltTy = VectorType::get(EltTy, Info.NumElts);
        Info.StateTy = StateTy;
        Info.Shift = Shift / EltSize;
    }
}

void StateMapper::RewriteLoad(StateMapping &StateMap, StateRef &Ref)
{
    LoadInst *LI = cast<LoadInst>(Ref.I);
    Type *Ty = LI->getType();
    Instruction *InsertPos = LI;

    /* The same reference size as the state size. */
    if (isSameSize(StateMap, Ref)) {
        Value *V = new LoadInst(StateMap.AI, "", false, InsertPos);
        V = ConvertType(V, Ty, InsertPos);
        LI->replaceAllUsesWith(V);
        toErase.push_back(LI);
        return;
    }

    if (StateMap.isVector()) {
        RewriteLoadVector(StateMap, Ref);
        return;
    }

    /* This is a non-vector state. Transform the state to the type of Int-N
     * and use logical shift to extract/insert element data. */
    ElementInfo Info;
    getElementInfo(StateMap, Ref, Info);

    Value *V = new LoadInst(StateMap.AI, "", false, InsertPos);
    V = ConvertType(V, Info.StateTy, InsertPos);

    /* Extract the element. */
    if (Info.Shift) {
        Value *Shift = ConstantInt::get(V->getType(), Info.Shift * 8);
        V = BinaryOperator::Create(Instruction::LShr, V, Shift, "", InsertPos);
    }
    V = new TruncInst(V, Info.EltTy, "", InsertPos);
    V = ConvertType(V, Ty, InsertPos);

    LI->replaceAllUsesWith(V);
    toErase.push_back(LI);
}

void StateMapper::RewriteStore(StateMapping &StateMap, StateRef &Ref)
{
    StoreInst *SI = cast<StoreInst>(Ref.I);
    Value *Data = SI->getValueOperand();
    Instruction *InsertPos = SI;

    /* The same reference size as the state size. */
    if (isSameSize(StateMap, Ref)) {
        Value *V = ConvertType(Data, StateMap.getType(), InsertPos);
        new StoreInst(V, StateMap.AI, false, InsertPos);
        toErase.push_back(SI);
        return;
    }

    if (StateMap.isVector()) {
        RewriteStoreVector(StateMap, Ref);
        return;
    }

    /* This is a non-vector state. Transform the state to the type of Int-N
     * and use logical shift to extract/insert element data. */
    ElementInfo Info;
    getElementInfo(StateMap, Ref, Info);

    Value *V = new LoadInst(StateMap.AI, "", false, InsertPos);
    V = ConvertType(V, Info.StateTy, InsertPos);

    /* Insert the element. */
    Data = ConvertType(Data, Info.EltTy, InsertPos);
    Data = new ZExtInst(Data, Info.StateTy, "", InsertPos);

    if (Info.Shift) {
        Value *Shift = ConstantInt::get(Data->getType(), Info.Shift * 8);
        Data = BinaryOperator::Create(Instruction::Shl, Data, Shift, "", InsertPos);
    }

    unsigned numBits = StateMap.getSize() * 8;
    unsigned loBit = Info.Shift * 8, hiBit = loBit + Ref.getSize() * 8;
    APInt mask = ~APInt::getBitsSet(numBits, loBit, hiBit);
    Value *Mask = ConstantInt::get(Data->getContext(), mask);

    V = BinaryOperator::Create(Instruction::And, V, Mask, "", InsertPos);
    V = BinaryOperator::Create(Instruction::Or, V, Data, "", InsertPos);
    V = ConvertType(V, StateMap.getType(), InsertPos);

    new StoreInst(V, StateMap.AI, false, InsertPos);
    toErase.push_back(SI);
}

void StateMapper::RewriteLoadVector(StateMapping &StateMap, StateRef &Ref)
{
    LoadInst *LI = cast<LoadInst>(Ref.I);
    Type *Ty = LI->getType();
    Instruction *InsertPos = LI;

    /* Compute offset, size and element type of this vector operation. */
    ElementInfo Info;
    getElementInfo(StateMap, Ref, Info);

    Value *V = new LoadInst(StateMap.AI, "", false, InsertPos);
    V = ConvertType(V, Info.StateTy, InsertPos);

    /* Extract the element(s) from the vector value. */
    IntegerType *I32 = IntegerType::get(V->getContext(), 32);

    if (Info.EltTy->isVectorTy()) {
        /* Multiple elements to load. Use shufflevector. */
        Value *UndefVal = UndefValue::get(Info.StateTy);
        SmallVector<Constant*, 8> Indices;
        for (unsigned i = 0, e = Info.Shift; i != e; ++i)
            Indices.push_back(ConstantInt::get(I32, Info.Shift + i));
        Value *CV = ConstantVector::get(Indices);
        V = new ShuffleVectorInst(V, UndefVal, CV, "", InsertPos);
    } else {
        /* Only one element. Use extractelement. */
        V = ExtractElementInst::Create(V,
                ConstantInt::get(I32, Info.Shift), "", InsertPos);
    }

    V = ConvertType(V, Ty, InsertPos);

    LI->replaceAllUsesWith(V);
    toErase.push_back(LI);
}

void StateMapper::RewriteStoreVector(StateMapping &StateMap, StateRef &Ref)
{
    StoreInst *SI = cast<StoreInst>(Ref.I);
    Value *Data = SI->getValueOperand();
    Instruction *InsertPos = SI;

    /* Compute offset, size and element type of this vector operation. */
    ElementInfo Info;
    getElementInfo(StateMap, Ref, Info);

    Value *V = new LoadInst(StateMap.AI, "", false, InsertPos);
    V = ConvertType(V, Info.StateTy, InsertPos);
    Data = ConvertType(Data, Info.EltTy, InsertPos);

    /* Extract element(s) from data and insert it into the vector value. */
    IntegerType *I32 = IntegerType::get(V->getContext(), 32);

    if (Info.EltTy->isVectorTy()) {
        SmallVector<Value *, 8> Partial;
        for (unsigned i = 0, e = Info.NumElts; i != e; ++i) {
            Partial.push_back(ExtractElementInst::Create(Data,
                        ConstantInt::get(I32, i), "", InsertPos));
        }
        for (unsigned i = 0, e = Info.NumElts; i != e; ++i) {
            V = InsertElementInst::Create(V, Partial[i],
                    ConstantInt::get(I32, Info.Shift + i), "", InsertPos);
        }
    } else {
        /* Only one element. Use insertelement. */
        V = InsertElementInst::Create(V, Data,
                ConstantInt::get(I32, Info.Shift), "", InsertPos);
    }

    V = ConvertType(V, StateMap.getType(), InsertPos);

    new StoreInst(V, StateMap.AI, false, InsertPos);
    toErase.push_back(SI);
}

/* Map state references to the virtual states. */
void StateMapper::PromoteState(Function &F)
{
    /* Pre-load CPU states. */
    Type *IntPtrTy = DL->getIntPtrType(CPU->getContext());
    for (auto &StateMap : StateMaps) {
        if (!StateMap.Addr) {
            Value *Off = ConstantInt::get(IntPtrTy, StateMap.getStart());
            Value *GEP = GetElementPtrInst::CreateInBounds(CPU, Off, "",
                                                           PreCastPos);
            StateMap.Addr = new BitCastInst(GEP,
                            PointerType::getUnqual(StateMap.getType()), "",
                            PreCastPos);
        }

        std::string StateName = StateMap.Addr->getName();
        if (StateName == "")
            StateName = getStateName(StateMap.getStart());
        if (StateName == "")
            StateName = "state";
        StateName.append(".a");

        StateMap.AI = new AllocaInst(StateMap.getType(), StateName, PreCastPos);
        CopyState(StateMap.AI, StateMap.Addr, PreLoadPos);
    }

    /* Rewrite loads and stores. */
    for (auto &StateMap : StateMaps) {
        for (auto Ref : StateMap.State->Refs) {
            if (isa<LoadInst>(Ref->I))
                RewriteLoad(StateMap, *Ref);
            else
                RewriteStore(StateMap, *Ref);
        }
    }

    /* Sync CPU states around helper calls. */
    SyncHelperState();

    /* Post-store dirty values back to CPU states for each exiting block. */
    for (auto BI = F.begin(), BE = F.end(); BI != BE; ++BI) {
        BasicBlock *BB = &*BI;
        if (distance(succ_begin(BB), succ_end(BB)) == 0)    /* leaf node */
            ProcessExitBB(BB);
    }
}

/* Determine if the state can be operated as a vector. */
Type *StateMapper::TryVectorState(StateData &State, Type *Ty)
{
    intptr_t StateStart = State.Start;
    intptr_t StateEnd = State.End;
    intptr_t StateSize = StateEnd - StateStart;

    /* If the reference type (from the IR) is already a vector type, use it.
     * Otherwise, query StateType to determine if it is a vector state. */
    VectorType *VecTy = dyn_cast<VectorType>(Ty);
    if (!VecTy) {
        auto TI = --StateType.upper_bound(StateStart);
        for (; TI != StateType.end() && TI->first < StateEnd; ++TI) {
            if (TI->second->isVectorTy()) {
                VecTy = cast<VectorType>(TI->second);
                break;
            }
        }
    }

    if (!VecTy)
        return nullptr;

    /* This is a vector state. Now, we need to check whether all state refs can
     * be composed by the vector element type: (a) the state size is a multiple
     * of the vector element size, and (b) the size and shift of each state ref
     * are both a multiple of the vector element size. */
    Type *ElementTy = VecTy->getScalarType();
    intptr_t ElementSize = DL->getTypeSizeInBits(ElementTy) / 8;
    if (StateSize % ElementSize != 0)
        return nullptr;

    for (auto Ref : State.Refs) {
        if (Ref->getSize() % ElementSize != 0 ||
            (Ref->Start - StateStart) % ElementSize != 0)
            return nullptr;
    }
    return VectorType::get(ElementTy, StateSize / ElementSize);
}

/* Compute state mapping information based on the state mapping rules. */
void StateMapper::ComputeStateMap(StateMapping &StateMap, StateData &State)
{
    /* State mapping rule:
     * - A guest state is not overlapped: (i.e., same access size)
     *   - Same type: map to this type.
     *   - Different type: select type in the order: vector, float and integer;
     *                     use bitcast to convert between different types.
     * - A guest state is overlapped with other state(s):
     *   - Query StateType to find state size (i.e., boundary) and type:
     *     - Vector type: use insert/extract to manipulate a vector element.
     *     - Other types: use shift to manipulate a sub-register element. */
    bool sameSize = true;
    bool hasLoad = false;
    bool hasStore = false;

    for (auto Ref : State.Refs) {
        hasLoad  |= isa<LoadInst>(Ref->I);
        hasStore |= isa<StoreInst>(Ref->I);
    }

    StateRef *Ref = State.Refs.front();
    Type *Ty = Ref->getType();
    Value *Addr = getPointerOperand(Ref->I);
    intptr_t Size = Ref->getSize();

    for (unsigned i = 1, e = State.Refs.size(); i != e; ++i) {
        StateRef *NextRef = State.Refs[i];
        Type *NextTy = NextRef->getType();
        Value *NextAddr = getPointerOperand(NextRef->I);

        /* Check type. */
        if (Ty != NextTy) {
            /* Select type in the order: vector, float and integer. */
            bool Swap = false;
            if (Ty->isVectorTy() && NextTy->isVectorTy()) {
                /* We prefer a vector type of small element type. */
                Type *ATy = cast<VectorType>(Ty)->getScalarType();
                Type *BTy = cast<VectorType>(NextTy)->getScalarType();
                if (DL->getTypeSizeInBits(BTy) < DL->getTypeSizeInBits(ATy))
                    Swap = true;
            } else if (!Ty->isVectorTy() && NextTy->isVectorTy()) {
                Swap = true;
            } else if (Ty->isIntegerTy() && NextTy->isFloatTy()) {
                Swap = true;
            }

            if (Swap) {
                std::swap(Ty, NextTy);
                std::swap(Addr, NextAddr);
            }
        }

        /* Check size. */
        if (Size != NextRef->getSize())
            sameSize = false;
    }

    if (sameSize) {
        /* The same reference size as the state size. */
        StateMap.Ty = Ty;
        StateMap.Addr = Addr;
    } else {
        /* Different reference sizes. */
        intptr_t StateSize = State.End - State.Start;
        Type *VecTy = TryVectorState(State, Ty);
        StateMap.Ty = VecTy ? VecTy
                            : Type::getIntNTy(Ty->getContext(), StateSize * 8);
        StateMap.Addr = nullptr;
    }
    StateMap.State = &State;
    StateMap.hasLoad = hasLoad;
    StateMap.hasStore = hasStore;
}

/* Analyze instructions in a Function that access CPU states. */
void StateMapper::AnalyzeState(Function &F)
{
    /* Collect instructions (load/store/call) that access CPU states.
     * Loads/stores that access guest memory or are tagged with volatile
     * (e.g., accessing the states: %pc and %tcg_exit_req) are ignored. */

    for (auto II = inst_begin(F), EE = inst_end(F); II != EE; ++II) {
        Instruction *I = &*II;
        intptr_t Off = 0;
        if (LoadInst *LI = dyn_cast<LoadInst>(I)) {
            if (MDFactory::isGuestMemory(I) || LI->isVolatile())
                continue;

            if (isLegalState(LI->getPointerOperand(), Off))
                Analyzer.addStateRef(I, Off);
        } else if (StoreInst *SI = dyn_cast<StoreInst>(I)) {
            if (MDFactory::isGuestMemory(I) || SI->isVolatile())
                continue;

            if (isLegalState(SI->getPointerOperand(), Off))
                Analyzer.addStateRef(I, Off);
        } else if (CallInst *CI = dyn_cast<CallInst>(I)) {
            /* Skip const helper, inlineasm and intrinsic function call. */
            if (MDFactory::isConst(CI))
                continue;
            if (CI->isInlineAsm() || isa<IntrinsicInst>(CI))
                continue;

            Analyzer.addCall(CI);
        }
    }

    /* Ask Analyzer to put state references into groups. */
    Analyzer.computeState();

    StateList &States = Analyzer.getStateList();
    if (States.empty())
        return;

    /* Compute state mapping info. */
    StateMaps.resize(States.size());
    for (unsigned i = 0, e = States.size(); i != e; ++i)
        ComputeStateMap(StateMaps[i], States[i]);
}

/* Rearrange instructions in the 'init' block. */
bool StateMapper::init(Function &F)
{
    /*
     * We would like to rearrange the instructions in the 'init' block, in which
     * gep/cast instructions are in front of other instructions in the block.
     * For example:
     *   %0 = getelementptr i8* %cpu, i64 0
     *   %1 = bitcast i8* %0 to i32*               # gep/cast insns
     *   --------------------------------------    # precast_pos
     *   --------------------------------------    # preload_pos
     *   %2 = load i32, i32* %1                    # the other insns
     *   br label %entry
     */
    CPU = IF->getDefaultCPU(F);
    if (!CPU || CPU->getParent() != &F.getEntryBlock())
        return false;

    Instruction *InsertPos = &*std::next(BasicBlock::iterator(CPU));
    PreLoadPos = new UnreachableInst(CPU->getContext(), InsertPos);
    PreCastPos = new UnreachableInst(CPU->getContext(), PreLoadPos);

    toErase.push_back(PreLoadPos);
    toErase.push_back(PreCastPos);

    /* Move gep/cast instructions. */
    IVec toMove;
    BasicBlock *BB = CPU->getParent();
    for (auto BI = BB->begin(), BE = BB->end(); BI != BE; ++BI) {
        Instruction *I = &*BI;
        if (isStatePointer(I))
            toMove.push_back(I);
    }
    for (auto I : toMove)
        I->moveBefore(PreCastPos);

    return true;
}

/*
 * StateMappingPass
 */
bool StateMappingPass::runOnFunction(Function &F)
{
    return StateMapper(IF).run(F);
}

char StateMappingPass::ID = 0;
INITIALIZE_PASS(StateMappingPass, "statemap",
        "Eliminate redundant loads/stores by mapping CPU states", false, false)

FunctionPass *llvm::createStateMappingPass(IRFactory *IF)
{
    return new StateMappingPass(IF);
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
