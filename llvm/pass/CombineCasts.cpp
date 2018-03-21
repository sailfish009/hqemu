/*
 *  (C) 2015 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "llvm/Transforms/Utils/Local.h"
#include "llvm-target.h"
#include "llvm-opc.h"
#include "llvm-pass.h"
#include "utils.h"

#define PASS_NAME "CombineCasts"

/*
 * CombineCasts Pass
 */
class CombineCasts : public FunctionPass {
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
    IVec toErase;

public:
    static char ID;
    explicit CombineCasts() : FunctionPass(ID) {}
    explicit CombineCasts(IRFactory *IF)
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
    }

    bool combineLoadCast(LoadInst *LI);
    bool combineStoreCast(StoreInst *SI);
    bool combineCastCast(Function &F);
    bool runOnFunction(Function &F);
};

char CombineCasts::ID = 0;
INITIALIZE_PASS(CombineCasts, "combinecast",
        "Combine bitcast with guest memory loads/stores", false, false)

FunctionPass *llvm::createCombineCasts(IRFactory *IF) 
{
    return new CombineCasts(IF);
}

static bool hasSameCastingTy(ArrayRef<BitCastInst *> IL) {
    Type *SrcTy = IL[0]->getSrcTy();
    Type *DstTy = IL[0]->getDestTy();
    for (BitCastInst *I : IL) {
        if (I->getSrcTy() != SrcTy)
            return false;
        if (I->getDestTy() != DstTy)
            return false;
    }
    return true;
}

/* This function aims to change the load type if (1) the type of loaded data is
 * casted to another type, (2) only one user of the load instruction is bitcast,
 * and (3) all other users of the load instruction are stores.
 *
 * For example:
 *  %0 = load <typeA>*              %0 = load <typeB>*
 *  %1 = bitcast %0, <typeB>        %1 = bitcast %0, <typeA>
 *
 *  %2 = op <typeB> %1, ...    =>   %2 = op <typeB> %0, ...
 *
 *  store %0, <typeA>*              store %1, <typeA>*
 *  store %1, <typeB>*              store %0, <typeB>*
 */
bool CombineCasts::combineLoadCast(LoadInst *LI)
{
    Instruction *Ptr = dyn_cast<Instruction>(LI->getPointerOperand());

    if (!Ptr)
        return false;

    /* Find all bitcast users of this load. */
    SmallVector<BitCastInst *, 4> BCIs;
    for (User *U : LI->users()) {
        Instruction *UI = cast<Instruction>(U);
        switch (UI->getOpcode()) {
        default:
            return false;
        case Instruction::Load:
        case Instruction::Store:
            break;
        case Instruction::BitCast:
            BCIs.push_back(cast<BitCastInst>(UI));
            break;
        }
    }

    if (BCIs.empty() || !hasSameCastingTy(BCIs))
        return false;

    Instruction *InsertPos = LI;
    unsigned Alignment = LI->getAlignment();
    unsigned Volatile = LI->isVolatile();
    Type *SrcTy = LI->getType();
    Type *DstTy = BCIs[0]->getDestTy();

    Type *PtrTy = PointerType::get(DstTy, LI->getPointerAddressSpace());
    if (isa<IntToPtrInst>(Ptr))
        Ptr = new IntToPtrInst(Ptr->getOperand(0), PtrTy, "", InsertPos);
    else
        Ptr = new BitCastInst(Ptr, PtrTy, "", InsertPos);

    Instruction *NewLI = new LoadInst(Ptr, "", Volatile, Alignment, InsertPos);
    Instruction *NewBCI = new BitCastInst(NewLI, SrcTy, "", InsertPos);

    if (MF->isGuestMemory(LI))
        MF->setGuestMemory(NewLI);
    for (BitCastInst *BCI : BCIs)
        BCI->replaceAllUsesWith(NewLI);
    LI->replaceAllUsesWith(NewBCI);

    toErase.push_back(LI);
    for (BitCastInst *BCI : BCIs)
        toErase.push_back(BCI);

    return true;
}

/* This function aims to change the store type if stored data is casted from
 * another type.
 *
 * For example:
 *  %0 = <typeA>
 *  %1 = bitcast %0, <typeB>   =>   store %0, <typeA>*
 *  store %1, <typeB>*
 */
bool CombineCasts::combineStoreCast(StoreInst *SI)
{
    Instruction *Ptr = dyn_cast<Instruction>(SI->getPointerOperand());
    Instruction *Data = dyn_cast<Instruction>(SI->getValueOperand());

    if (!Ptr || !Data || !isa<BitCastInst>(Data))
        return false;

    Instruction *InsertPos = SI;
    unsigned Alignment = SI->getAlignment();
    unsigned Volatile = SI->isVolatile();
    BitCastInst *BCI = cast<BitCastInst>(Data);
    Value *V = BCI->getOperand(0);
    Type *SrcTy = V->getType();

    Type *PtrTy = PointerType::get(SrcTy, SI->getPointerAddressSpace());
    if (isa<IntToPtrInst>(Ptr))
        Ptr = new IntToPtrInst(Ptr->getOperand(0), PtrTy, "", InsertPos);
    else
        Ptr = new BitCastInst(Ptr, PtrTy, "", InsertPos);

    Instruction *NewSI = new StoreInst(V, Ptr, Volatile, Alignment, InsertPos);

    if (MF->isGuestMemory(SI))
        MF->setGuestMemory(NewSI);

    toErase.push_back(SI);
    return true;
}

/* This function aims to eliminate redundant casts.
 * For example:
 *  %0 = <typeA>                   %0 = <typeA>
 *  %1 = bitcast %0, <typeB>  =>
 *  %2 = bitcast %1, <typeC>       %2 = bitcast %0, <typeC>
 *     = op <typeC> %2, ...           = op <typeC> %2, ...
 *
 * And if <typeA> is the same as <typeC>, the code is further optimized to
 *  %0 = <typeA>                   %0 = <typeA>
 *  %1 = bitcast %0, <typeB>  =>
 *  %2 = bitcast %1, <typeC>
 *     = op <typeC> %2, ...           = op <typeA> %0, ...
 */
bool CombineCasts::combineCastCast(Function &F)
{
    SmallVector<Instruction*, 4> Worklist;
    for (auto II = inst_begin(F), EE = inst_end(F); II != EE; II++) {
        Instruction *I = &*II;
        if (isa<BitCastInst>(I))
            Worklist.push_back(I);
    }

    for (auto I : Worklist) {
        BitCastInst *CI = cast<BitCastInst>(I);
        BitCastInst *CSrc = dyn_cast<BitCastInst>(CI->getOperand(0));
        if (!CSrc)
            continue;

        Type *SrcTy = CSrc->getOperand(0)->getType();
        Type *DstTy = CI->getType();
        Value *Result = (SrcTy == DstTy) ? CSrc->getOperand(0) :
            new BitCastInst(CSrc->getOperand(0), CI->getType(), "", CI);
        I->replaceAllUsesWith(Result);
        toErase.push_back(I);
    }
    return true;
}

bool CombineCasts::runOnFunction(Function &F)
{
    bool Changed = false;
    SmallVector<LoadInst *, 16> Loads;
    SmallVector<StoreInst *, 16> Stores;

    /* Collect all guest memory and non-volatile cpu state loads/stores. */
    for (auto II = inst_begin(F), EE = inst_end(F); II != EE; II++) {
        Instruction *I = &*II;

        if (LoadInst *LI = dyn_cast<LoadInst>(I)) {
            if (MF->isGuestMemory(LI) || !LI->isVolatile())
                Loads.push_back(LI);
        } else if (StoreInst *SI = dyn_cast<StoreInst>(I)) {
            if (MF->isGuestMemory(SI) || !SI->isVolatile())
                Stores.push_back(SI);
        }
    }

    for (auto LI : Loads)
        Changed |= combineLoadCast(LI);
    for (auto SI : Stores)
        Changed |= combineStoreCast(SI);

    if (toErase.size())
        ProcessErase(toErase);

    Changed |= combineCastCast(F);

    if (toErase.size())
        ProcessErase(toErase);

    return Changed;
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

