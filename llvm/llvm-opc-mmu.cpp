/*
 *  (C) 2015 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 *
 *   This file provides LLVM IR generator in terms of basic block and trace.
 */

#include "llvm-debug.h"
#include "llvm.h"
#include "llvm-opc.h"
#include "llvm-target.h"
#include "utils.h"

#if defined(CONFIG_SOFTMMU)
extern "C" {
extern const void * const llvm_ld_helpers[16];
extern const void * const llvm_st_helpers[16];
};
#endif


#if defined(CONFIG_USER_ONLY)
Value *IRFactory::QEMULoad(Value *AddrL, Value *AddrH, TCGMemOpIdx oi)
{
    TCGMemOp opc = get_memop(oi);
    Value *Base = AddrL;
    PointerType *PtrTy = getPointerTy(getSizeInBits(opc), Segment);
    LoadInst *LI;

    if (GUEST_BASE == 0 || Segment != 0) {
        Base = ITP(Base, PtrTy);
        LI = new LoadInst(Base, "", true, LastInst);
    } else {
        Base = ITP(Base, Int8PtrTy);
        Base = GetElementPtrInst::CreateInBounds(Base, CONSTPtr(GUEST_BASE), "", LastInst);
        if (Base->getType() != PtrTy)
            Base = CAST(Base, PtrTy);
        LI = new LoadInst(Base, "", true, LastInst);
    }
    MF->setGuestMemory(LI);

    return ConvertEndian(LI, opc);
}

void IRFactory::QEMUStore(Value *Data, Value *AddrL, Value *AddrH, TCGMemOpIdx oi)
{
    TCGMemOp opc = get_memop(oi);
    Value *Base = AddrL;
    PointerType *PtrTy = getPointerTy(getSizeInBits(opc), Segment);
    StoreInst *SI;

    Data = ConvertEndian(Data, opc);

    if (GUEST_BASE == 0 || Segment != 0) {
        Base = ITP(Base, PtrTy);
        SI = new StoreInst(Data, Base, true, LastInst);
    } else {
        Base = ITP(Base, Int8PtrTy);
        Base = GetElementPtrInst::CreateInBounds(Base, CONSTPtr(GUEST_BASE), "", LastInst);
        if (Base->getType() != PtrTy)
            Base = CAST(Base, PtrTy);
        SI = new StoreInst(Data, Base, true, LastInst);
    }
    MF->setGuestMemory(SI);
}

#else /* !CONFIG_USER_ONLY */

inline long getTLBOffset(int mem_index)
{
    long Offset = 0;

    switch (mem_index) {
#if NB_MMU_MODES > 0
    case 0: Offset = offsetof(CPUArchState, tlb_table[0][0]); break;
#endif
#if NB_MMU_MODES > 1
    case 1: Offset = offsetof(CPUArchState, tlb_table[1][0]); break;
#endif
#if NB_MMU_MODES > 2
    case 2: Offset = offsetof(CPUArchState, tlb_table[2][0]); break;
#endif
#if NB_MMU_MODES > 3
    case 3: Offset = offsetof(CPUArchState, tlb_table[3][0]); break;
#endif
#if NB_MMU_MODES > 4
    case 4: Offset = offsetof(CPUArchState, tlb_table[4][0]); break;
#endif
#if NB_MMU_MODES > 5
    case 5: Offset = offsetof(CPUArchState, tlb_table[5][0]);
#endif
    default:
        IRError("%s: internal error. mem_index=%d\n", __func__, mem_index);
    }

    return Offset;
}

Value *IRFactory::ConcatTLBVersion(Value *GVA)
{
#if defined(ENABLE_TLBVERSION_EXT)
    GVA = ZEXT64(GVA);
#endif
    Type *PtrTy = getPointerTy(DL->getTypeSizeInBits(GVA->getType()));
    Value *TLBVersion = GetElementPtrInst::CreateInBounds(CPU,
            CONSTPtr(offsetof(CPUArchState, tlb_version)), "", LastInst);
    TLBVersion = new BitCastInst(TLBVersion, PtrTy, "", LastInst);
    TLBVersion = new LoadInst(TLBVersion, "version", true, LastInst);
    return OR(GVA, TLBVersion);
}

Value *IRFactory::QEMULoad(Value *AddrL, Value *AddrH, TCGMemOpIdx oi)
{
    TCGMemOp opc = get_memop(oi);
    int mem_index = get_mmuidx(oi);
    IntegerType *AccessTy;
    PointerType *GuestPtrTy, *HostPtrTy;
    int Size, s_bits = opc & MO_SIZE;

    Size = 8 * 1 << s_bits; /* data size (bits) for this load */

    const void *helper = llvm_ld_helpers[opc & (MO_BSWAP | MO_SIZE)];
    Function *MissFunc = ResolveFunction(getMMUFName(helper));
    if (!MissFunc)
        IRError("%s: internal error.\n", __func__);

    GuestPtrTy = (TARGET_LONG_BITS == 32) ? Int32PtrTy : Int64PtrTy;
    HostPtrTy = (TCG_TARGET_REG_BITS == 32) ? Int32PtrTy : Int64PtrTy;

#if defined(ENABLE_TLBVERSION_EXT)
    GuestPtrTy = Int64PtrTy;
#endif

    /* Create TLB basic blocks. */
    BasicBlock *tlb_hit = BasicBlock::Create(*Context, "tlb_hit", Func);
    BasicBlock *tlb_miss = BasicBlock::Create(*Context, "tlb_miss", Func);
    BasicBlock *tlb_exit = BasicBlock::Create(*Context, "tlb_exit", Func);
    toSink.push_back(tlb_miss);

    /* Load compared value in TLB. QEMU uses only addrlo to index the TLB entry. */
    Value *TLBEntry, *TLBValue, *CPUAddr;
    AccessTy = (TCG_TARGET_REG_BITS == 64 && TARGET_LONG_BITS == 64) ? Int64Ty : Int32Ty;
    size_t Offset = getTLBOffset(mem_index) + offsetof(CPUTLBEntry, addr_read);
    TLBEntry = LSHR(AddrL, ConstantInt::get(AccessTy, TARGET_PAGE_BITS - CPU_TLB_ENTRY_BITS));
    TLBEntry = AND(TLBEntry, ConstantInt::get(AccessTy, (CPU_TLB_SIZE - 1) << CPU_TLB_ENTRY_BITS));
    TLBEntry = ADD(TLBEntry, ConstantInt::get(AccessTy, Offset));

    if (TLBEntry->getType() != IntPtrTy)
        TLBEntry = new ZExtInst(TLBEntry, IntPtrTy, "", LastInst);

    CPUAddr = new PtrToIntInst(CPU, IntPtrTy, "", LastInst);
    TLBEntry = ADD(CPUAddr, TLBEntry);
    TLBValue = new IntToPtrInst(TLBEntry, GuestPtrTy, "", LastInst);
    TLBValue = new LoadInst(TLBValue, "tlb.read", false, LastInst);

    /* Compare GVA and TLB value. */
    Value *GVA, *Cond, *GuestPC = AddrL;
    AccessTy = (TARGET_LONG_BITS == 32) ? Int32Ty : Int64Ty;
    if (AddrH) { /* guest is 64-bit and host is 32-bit. */
        GuestPC = SHL(ZEXT64(AddrH), CONST64(32));
        GuestPC = OR(GuestPC, ZEXT64(AddrL));
    }
#if defined(ALIGNED_ONLY)
    GVA = AND(GuestPC, ConstantInt::get(AccessTy,
              TARGET_PAGE_MASK | ((1 << s_bits) - 1)));
#elif defined(ENABLE_TLBVERSION)
    GVA = ADD(GuestPC, ConstantInt::get(AccessTy, (1 << s_bits) - 1));
    GVA = AND(GVA, ConstantInt::get(AccessTy, TARGET_PAGE_MASK));
    GVA = ConcatTLBVersion(GVA);
#else
    GVA = ADD(GuestPC, ConstantInt::get(AccessTy, (1 << s_bits) - 1));
    GVA = AND(GVA, ConstantInt::get(AccessTy, TARGET_PAGE_MASK));
#endif
    Cond = ICMP(GVA, TLBValue, ICmpInst::ICMP_EQ);
    BranchInst::Create(tlb_hit, tlb_miss, Cond, LastInst);
    LastInst->eraseFromParent();

    /* TLB hit. */
    Value *PhyAddr, *Addend, *HitData, *Addr=AddrL;

    LastInst = BranchInst::Create(tlb_exit, tlb_hit);
    if (Addr->getType() != IntPtrTy)
        Addr = new ZExtInst(Addr, IntPtrTy, "", LastInst);

    Offset = offsetof(CPUTLBEntry, addend) - offsetof(CPUTLBEntry, addr_read);
    Addend = ADD(TLBEntry, ConstantInt::get(IntPtrTy, Offset));
    Addend = new IntToPtrInst(Addend, HostPtrTy, "", LastInst);
    Addend = new LoadInst(Addend, "tlb.addend", false, LastInst);
    PhyAddr = ADD(Addr, Addend);
    PhyAddr = ITP(PhyAddr, getPointerTy(Size));
    HitData = new LoadInst(PhyAddr, "hit", true, LastInst);

    HitData = ConvertEndian(HitData, opc);

    /* TLB miss. */
    LastInst = BranchInst::Create(tlb_exit, tlb_miss);
    SmallVector<Value *, 4> Params;
    uint32_t restore_val = setRestorePoint(oi);
    Params.push_back(CPUStruct);
    Params.push_back(GuestPC);
    Params.push_back(CONST32(restore_val));

    CallInst *MissCall = CallInst::Create(MissFunc, Params, "", LastInst);
    Value *MissData = MissCall;
    switch (opc & MO_SSIZE) {
    case MO_UB:
    case MO_SB:
        if (DL->getTypeSizeInBits(MissData->getType()) != 8)
            MissData = TRUNC8(MissCall);
        break;
    case MO_UW:
    case MO_SW:
        if (DL->getTypeSizeInBits(MissData->getType()) != 16)
            MissData = TRUNC16(MissCall);
        break;
    case MO_UL:
    case MO_SL:
        if (DL->getTypeSizeInBits(MissData->getType()) != 32)
            MissData = TRUNC32(MissCall);
        break;
    case MO_Q:
        if (DL->getTypeSizeInBits(MissData->getType()) != 64)
            MissData = ZEXT64(MissCall);
        break;
    default:
        IRError("%s: invalid size (opc=%d)\n", __func__, opc);
        break;
    }

    /* TLB exit. */
    CurrBB = tlb_exit;
    LastInst = BranchInst::Create(ExitBB, CurrBB);
    PHINode *PH = PHINode::Create(HitData->getType(), 2, "", LastInst);
    PH->addIncoming(HitData, tlb_hit);
    PH->addIncoming(MissData, tlb_miss);

    return PH;
}

void IRFactory::QEMUStore(Value *Data, Value *AddrL, Value *AddrH, TCGMemOpIdx oi)
{
    TCGMemOp opc = get_memop(oi);
    int mem_index = get_mmuidx(oi);
    IntegerType *AccessTy;
    PointerType *GuestPtrTy, *HostPtrTy;
    int Size, s_bits = opc & MO_SIZE;

    Size = 8 * 1 << s_bits; /* data size (bits) for this load */

    const void *helper = llvm_st_helpers[opc & (MO_BSWAP | MO_SIZE)];
    Function *MissFunc = ResolveFunction(getMMUFName(helper));
    if (!MissFunc)
        IRError("%s: internal error.\n", __func__);

    GuestPtrTy = (TARGET_LONG_BITS == 32) ? Int32PtrTy : Int64PtrTy;
    HostPtrTy = (TCG_TARGET_REG_BITS == 32) ? Int32PtrTy : Int64PtrTy;

#if defined(ENABLE_TLBVERSION_EXT)
    GuestPtrTy = Int64PtrTy;
#endif

    /* Create TLB basic blocks. */
    BasicBlock *tlb_hit = BasicBlock::Create(*Context, "tlb_hit", Func);
    BasicBlock *tlb_miss = BasicBlock::Create(*Context, "tlb_miss", Func);
    BasicBlock *tlb_exit = BasicBlock::Create(*Context, "tlb_exit", Func);
    toSink.push_back(tlb_miss);

    /* Load compared value in TLB. QEMU uses only addrlo to index the TLB entry. */
    Value *TLBEntry, *TLBValue, *CPUAddr;
    AccessTy = (TCG_TARGET_REG_BITS == 64 && TARGET_LONG_BITS == 64) ? Int64Ty : Int32Ty;
    size_t Offset = getTLBOffset(mem_index) + offsetof(CPUTLBEntry, addr_write);
    TLBEntry = LSHR(AddrL, ConstantInt::get(AccessTy, TARGET_PAGE_BITS - CPU_TLB_ENTRY_BITS));
    TLBEntry = AND(TLBEntry, ConstantInt::get(AccessTy, (CPU_TLB_SIZE - 1) << CPU_TLB_ENTRY_BITS));
    TLBEntry = ADD(TLBEntry, ConstantInt::get(AccessTy, Offset));

    if (TLBEntry->getType() != IntPtrTy)
        TLBEntry = new ZExtInst(TLBEntry, IntPtrTy, "", LastInst);

    CPUAddr = new PtrToIntInst(CPU, IntPtrTy, "", LastInst);
    TLBEntry = ADD(CPUAddr, TLBEntry);
    TLBValue = new IntToPtrInst(TLBEntry, GuestPtrTy, "", LastInst);
    TLBValue = new LoadInst(TLBValue, "tlb.write", false, LastInst);

    /* Compare GVA and TLB value. */
    Value *GVA, *Cond, *GuestPC = AddrL;
    AccessTy = (TARGET_LONG_BITS == 32) ? Int32Ty : Int64Ty;
    if (AddrH != nullptr) { /* guest is 64-bit and host is 32-bit. */
        GuestPC = SHL(ZEXT64(AddrH), CONST64(32));
        GuestPC = OR(GuestPC, ZEXT64(AddrL));
    }
#if defined(ALIGNED_ONLY)
    GVA = AND(GuestPC, ConstantInt::get(AccessTy,
              TARGET_PAGE_MASK | ((1 << s_bits) - 1)));
#elif defined(ENABLE_TLBVERSION)
    GVA = ADD(GuestPC, ConstantInt::get(AccessTy, (1 << s_bits) - 1));
    GVA = AND(GVA, ConstantInt::get(AccessTy, TARGET_PAGE_MASK));
    GVA = ConcatTLBVersion(GVA);
#else
    GVA = ADD(GuestPC, ConstantInt::get(AccessTy, (1 << s_bits) - 1));
    GVA = AND(GVA, ConstantInt::get(AccessTy, TARGET_PAGE_MASK));
#endif
    Cond = ICMP(GVA, TLBValue, ICmpInst::ICMP_EQ);
    BranchInst::Create(tlb_hit, tlb_miss, Cond, LastInst);
    LastInst->eraseFromParent();

    /* TLB hit. */
    Value *PhyAddr, *Addend, *Addr=AddrL;

    LastInst = BranchInst::Create(tlb_exit, tlb_hit);
    if (Addr->getType() != IntPtrTy)
        Addr = new ZExtInst(Addr, IntPtrTy, "", LastInst);

    Offset = offsetof(CPUTLBEntry, addend) - offsetof(CPUTLBEntry, addr_write);
    Addend = ADD(TLBEntry, ConstantInt::get(IntPtrTy, Offset));
    Addend = new IntToPtrInst(Addend, HostPtrTy, "", LastInst);
    Addend = new LoadInst(Addend, "tlb.addend", false, LastInst);
    PhyAddr = ADD(Addr, Addend);
    PhyAddr = ITP(PhyAddr, getPointerTy(Size));

    Value *HitData = ConvertEndian(Data, opc);

    new StoreInst(HitData, PhyAddr, true, LastInst);

    /* TLB miss. */
    LastInst = BranchInst::Create(tlb_exit, tlb_miss);
    SmallVector<Value *, 4> Params;
    uint32_t restore_val = setRestorePoint(oi);
    Params.push_back(CPUStruct);
    Params.push_back(GuestPC);
    Params.push_back(Data);
    Params.push_back(CONST32(restore_val));

    CallInst::Create(MissFunc, Params, "", LastInst);

    /* TLB exit. */
    CurrBB = tlb_exit;
    LastInst = BranchInst::Create(ExitBB, CurrBB);
}

#endif /* CONFIG_USER_ONLY */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
