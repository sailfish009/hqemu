/*
 *  (C) 2015 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "llvm-debug.h"
#include "llvm-opc.h"
#include "llvm-target.h"
#include "llvm-pass.h"
#include "utils.h"

#define PASS_NAME "CombineGuestMemory"

/*
 * CombineGuestMemory Pass
 */
class CombineGuestMemory : public FunctionPass {

    struct StateInfo {
        StateInfo() : Ptr(nullptr) {}
        StateInfo(Value *ptr, APInt &offset, APInt &size)
            : Ptr(ptr), Offset(offset), Size(size) {}
        Value *Ptr;
        APInt Offset;
        APInt Size;
    };

    typedef std::pair<Value *, Value *> ValuePair;
    typedef std::map<size_t, size_t> StateMap;
    typedef DenseMap<ValuePair, StateInfo> CSMap;

    IRFactory *IF;
    const DataLayout *DL;
    MDFactory *MF;
    IntegerType *Int8Ty;
    IntegerType *Int32Ty;
    IntegerType *Int64Ty;
    IntegerType *IntPtrTy;
    PointerType *Int8PtrTy;
    PointerType *Int32PtrTy;
    PointerType *Int64PtrTy;
    Value *CPU;
    Instruction *InitLastInst;
    StateMap LegalStates;
    IVec toErase;

public:
    static char ID;
    explicit CombineGuestMemory() : FunctionPass(ID) {}
    explicit CombineGuestMemory(IRFactory *IF)
        : FunctionPass(ID), IF(IF), DL(IF->getDL()), MF(IF->getMDFactory())
    {
        LLVMContext &Context = IF->getContext();;
        Int8Ty      = IntegerType::get(Context, 8);
        Int32Ty     = IntegerType::get(Context, 32);
        Int64Ty     = IntegerType::get(Context, 64);
        IntPtrTy    = DL->getIntPtrType(Context);
        Int8PtrTy   = Type::getInt8PtrTy(Context, 0);
        Int32PtrTy  = Type::getInt32PtrTy(Context, 0);
        Int64PtrTy  = Type::getInt64PtrTy(Context, 0);

        addLegalStates();
    }

    unsigned getAddressSpaceOperand(Value *I) {
        if (LoadInst *LI = dyn_cast<LoadInst>(I))
            return LI->getPointerAddressSpace();
        if (StoreInst *SI = dyn_cast<StoreInst>(I))
            return SI->getPointerAddressSpace();
        return -1U;
    }

    int getNumUsers(Instruction *I) {
        return distance(I->user_begin(), I->user_end());
    }

    void addLegalStates();
    bool isLegalState(Value *Ptr, APInt &Offset, APInt &Size);
    bool isConsecutiveAccess(Value *A, Value *B, Value *&Ptr, APInt &Offset, APInt &Size);
    bool tryCombineLoad(Value *A, Value *B, CSMap &States);
    bool tryCombineStore(Value *A, Value *B, CSMap &States);
    bool combineMemory(SmallVector<Value *, 8> &Memory, SmallVector<Value *, 8> &States);
    bool runOnFunction(Function &F);
};

char CombineGuestMemory::ID = 0;
INITIALIZE_PASS(CombineGuestMemory, "combinegm",
        "Combine guest memory loads and stores", false, false)

FunctionPass *llvm::createCombineGuestMemory(IRFactory *IF) 
{
    return new CombineGuestMemory(IF);
}


void CombineGuestMemory::addLegalStates()
{
#if defined(TARGET_I386)
    size_t Start = offsetof(CPUArchState, xmm_regs[0]);
    size_t Size = sizeof(XMMReg);
    for (int i = 0; i < CPU_NB_REGS; ++i)
        LegalStates[Start + Size * i] = Size;
#elif defined(TARGET_ARM)
    size_t Start = offsetof(CPUArchState, vfp.regs[0]);
    size_t Size = sizeof(float64) * 2;
    for (int i = 0; i < 32; ++i)
        LegalStates[Start + Size * i] = Size;
#endif
}

bool CombineGuestMemory::isConsecutiveAccess(Value *A, Value *B, Value *&Ptr,
                                             APInt &Offset, APInt &Size)
{
    Value *PtrA = getPointerOperand(A);
    Value *PtrB = getPointerOperand(B);
    unsigned ASA = getAddressSpaceOperand(A);
    unsigned ASB = getAddressSpaceOperand(B);

    if (!PtrA || !PtrB || (ASA != ASB))
        return false;

    Type *TyA = cast<PointerType>(PtrA->getType())->getElementType();
    Type *TyB = cast<PointerType>(PtrB->getType())->getElementType();
    if (DL->getTypeStoreSize(TyA) != DL->getTypeStoreSize(TyB))
        return false;

    unsigned PtrBitWidth = DL->getTypeSizeInBits(TyA);
    APInt Sz(PtrBitWidth, DL->getTypeStoreSize(TyA));

    APInt OffsetA(PtrBitWidth, 0), OffsetB(PtrBitWidth, 0);
    PtrA = StripPointer(DL, PtrA, OffsetA);
    PtrB = StripPointer(DL, PtrB, OffsetB);

    APInt OffsetDelta = OffsetB - OffsetA;
    if (PtrA == PtrB && OffsetDelta == Sz) {
        Ptr = PtrA;
        Offset = OffsetA;
        Size = Sz + Sz;
        return true;
    }

    return false;
}

bool CombineGuestMemory::isLegalState(Value *Ptr, APInt &Offset, APInt &Size)
{
    if (Ptr != CPU)
        return false;
    uint64_t Start = Offset.getZExtValue();
    if (LegalStates.find(Start) == LegalStates.end() ||
        Size.getZExtValue() > LegalStates[Start])
        return false;
    return true;
}

static bool hasMemoryViolation(Instruction *SA, Instruction *SB,
                               Instruction *EA, Instruction *EB)
{
    std::set<Value*> Insts;
    Insts.insert(SA);
    Insts.insert(SB);
    Insts.insert(EA);
    Insts.insert(EB);

    BasicBlock::iterator BI = BasicBlock::iterator(SA);
    BasicBlock::iterator BE = BasicBlock::iterator(EA);
    for (; BI != BE; ++BI) {
        Instruction *I = &*BI;
        if (isa<CallInst>(I))
            return true;
        if (!isa<LoadInst>(I) && !isa<StoreInst>(I))
            continue;
        if (Insts.find(I) == Insts.end())
            return true;
    }

    BI = BasicBlock::iterator(SB);
    BE = BasicBlock::iterator(EB);
    for (; BI != BE; ++BI) {
        Instruction *I = &*BI;
        if (isa<CallInst>(I))
            return true;
        if (!isa<LoadInst>(I) && !isa<StoreInst>(I))
            continue;
        if (Insts.find(I) == Insts.end())
            return true;
    }
    return false;
}

bool CombineGuestMemory::tryCombineLoad(Value *A, Value *B, CSMap &States)
{
    /* First, check if the guest loads are 'only' used by the store instructions
     * to consecutive CPU states, and if any other loads/stores occurs between
     * the queried operation. */
    LoadInst *LA = cast<LoadInst>(A);
    LoadInst *LB = cast<LoadInst>(B);
    if (getNumUsers(LA) != 1 || getNumUsers(LB) != 1)
        return false;

    Value *VA = *LA->user_begin();
    Value *VB = *LB->user_begin();
    CSMap::iterator CSI = States.find(ValuePair(VA, VB));
    if (CSI == States.end())
        return false;

    Instruction *SA = cast<Instruction>(VA);
    Instruction *SB = cast<Instruction>(VB);

    if (hasMemoryViolation(LA, LB, SA, SB))
        return false;

    /* Here we found the guest memory operations are loaded and stored to the
     * CPU states immediately. The operations are safe to combine. */
    Instruction *InsertPos = SA;
    StateInfo &SI = CSI->second;
    uint64_t Size = SI.Size.getZExtValue();
    unsigned AS = getAddressSpaceOperand(LA);
    unsigned Align = Size / 2;
    Type *Ty = PointerType::get(VectorType::get(Int8Ty, Size), AS);
    Instruction *Ptr = cast<Instruction>(LA->getPointerOperand());
    if (isa<IntToPtrInst>(Ptr))
        Ptr = new IntToPtrInst(Ptr->getOperand(0), Ty, "", InsertPos);
    else
        Ptr = new BitCastInst(Ptr, Ty, "", InsertPos);
    Instruction *NewLI = new LoadInst(Ptr, "", true, Align, InsertPos);
    MF->setGuestMemory(NewLI);

    Ty = PointerType::getUnqual(VectorType::get(Int8Ty, Size));
    Value *Offset = ConstantInt::get(Ty->getContext(), SI.Offset);
    Ptr = GetElementPtrInst::CreateInBounds(CPU, Offset, "", InitLastInst);
    Ptr = new BitCastInst(Ptr, Ty, "", InitLastInst);
    new StoreInst(NewLI, Ptr, false, InsertPos);

    States.erase(CSI);
    toErase.push_back(SA);
    toErase.push_back(SB);
    return true;
}

bool CombineGuestMemory::tryCombineStore(Value *A, Value *B, CSMap &States)
{
    /* First, check if the CPU state loads are 'only' used by the guest store
     * instructions, and if any other loads/stores occurs between the
     * queried operation. */
    StoreInst *SA = cast<StoreInst>(A);
    StoreInst *SB = cast<StoreInst>(B);
    Instruction *LA = dyn_cast<Instruction>(SA->getOperand(0));
    Instruction *LB = dyn_cast<Instruction>(SB->getOperand(0));

    if (!LA || !LB)
        return false;
    if (getNumUsers(LA) != 1 || getNumUsers(LB) != 1)
        return false;

    CSMap::iterator CSI = States.find(ValuePair(LA, LB));
    if (CSI == States.end())
        return false;

    if (hasMemoryViolation(LA, LB, SA, SB))
        return false;

    /* Here we found the CPU states are loaded and stored to the guest memory
     * immediately. The operations are safe to combine. */
    Instruction *InsertPos = SA;
    StateInfo &SI = CSI->second;
    uint64_t Size = SI.Size.getZExtValue();
    Type *Ty = PointerType::getUnqual(VectorType::get(Int8Ty, Size));
    Value *Offset = ConstantInt::get(Ty->getContext(), SI.Offset);
    Instruction *Ptr = GetElementPtrInst::CreateInBounds(CPU, Offset, "", InitLastInst);
    Ptr = new BitCastInst(Ptr, Ty, "", InitLastInst);
    Value *V = new LoadInst(Ptr, "", false, InsertPos);

    unsigned AS = getAddressSpaceOperand(SA);
    unsigned Align = Size / 2;
    Ty = PointerType::get(VectorType::get(Int8Ty, Size), AS);
    Ptr = cast<Instruction>(SA->getPointerOperand());
    if (isa<IntToPtrInst>(Ptr))
        Ptr = new IntToPtrInst(Ptr->getOperand(0), Ty, "", InsertPos);
    else
        Ptr = new BitCastInst(Ptr, Ty, "", InsertPos);
    Instruction *NewSI = new StoreInst(V, Ptr, true, Align, InsertPos);
    MF->setGuestMemory(NewSI);

    States.erase(CSI);
    toErase.push_back(SA);
    toErase.push_back(SB);
    return true;
}

bool CombineGuestMemory::combineMemory(SmallVector<Value *, 8> &Memory,
                                       SmallVector<Value *, 8> &States)
{
    bool Changed = false;
    SmallPtrSet<Value *, 4> Used;
    CSMap ConsecutiveStates;
    Value *Ptr;
    APInt Offset, Size;

    /* Find consecutive CPU states. */
    for (unsigned i = 1, e = States.size(); i != e; i++) {
        if (!isConsecutiveAccess(States[i-1], States[i], Ptr, Offset, Size))
            continue;

        if (!isLegalState(Ptr, Offset, Size))
            continue;

        ConsecutiveStates[ValuePair(States[i-1], States[i])] =
            StateInfo(Ptr, Offset, Size);
    }

    if (ConsecutiveStates.size() == 0)
        return false;

    /* Find and combine consecutive guest memory accesses if their referrenced
     * CPU states are also consecutive. */
    for (unsigned i = 1, e = Memory.size(); i != e; i++) {
        if (Used.count(Memory[i-1]) || Used.count(Memory[i]))
            continue;
        if (!isConsecutiveAccess(Memory[i-1], Memory[i], Ptr, Offset, Size))
            continue;

        bool ret = false;
        if (isa<LoadInst>(Memory[i-1]) && isa<LoadInst>(Memory[i])) {
            ret = tryCombineLoad(Memory[i-1], Memory[i], ConsecutiveStates);
        } else if (isa<StoreInst>(Memory[i-1]) && isa<StoreInst>(Memory[i])) {
            ret = tryCombineStore(Memory[i-1], Memory[i], ConsecutiveStates);
        }
        if (ret) {
            Used.insert(Memory[i-1]);
            Used.insert(Memory[i]);
            Changed = true;
        }
    }
    return Changed;
}

bool CombineGuestMemory::runOnFunction(Function &F)
{
    bool Changed = false;

#if defined(CONFIG_SOFTMMU)
    return Changed;
#endif

    /* Skip if no state is allowed to be combined. */
    if (LegalStates.empty())
        return Changed;

    CPU = IF->getDefaultCPU(F);
    if (!CPU) {
        dbg() << DEBUG_PASS << "CombineGuestMemory: Cannot find CPU pointer.\n";
        return false;
    }

    InitLastInst = F.getEntryBlock().getTerminator();

    for (auto FI = F.begin(), FE = F.end(); FI != FE; ++FI) {
        SmallVector<Value *, 8> Memory;
        SmallVector<Value *, 8> States;
        for (auto BI = FI->begin(), BE = FI->end(); BI != BE; ++BI) {
            Instruction *I = &*BI;
            if (MF->isGuestMemory(I)) {
                Memory.push_back(I);
            } else if (LoadInst *LI = dyn_cast<LoadInst>(I)) {
                if (!LI->isVolatile())
                    States.push_back(LI);
            } else if (StoreInst *SI = dyn_cast<StoreInst>(I)) {
                if (!SI->isVolatile())
                    States.push_back(SI);
            }
        }
        if (Memory.size() >= 2 && States.size() >= 2)
            Changed |= combineMemory(Memory, States);
    }

    if (!toErase.empty())
        ProcessErase(toErase);

    return Changed;
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

