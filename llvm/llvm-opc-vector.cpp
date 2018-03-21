/*
 *  (C) 2015 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 *
 *   This file provides TCG vector IR to LLVM IR conversions.
 */

#include "llvm.h"
#include "llvm-debug.h"
#include "llvm-opc.h"
#include "utils.h"


extern TCGOpDef llvm_op_defs[];


void IRFactory::op_vector_start(const TCGArg *args)
{
    IRError("%s: this function should never be called.\n", __func__);
}

void IRFactory::op_vector_end(const TCGArg *args)
{
    IRError("%s: this function should never be called.\n", __func__);
}

void IRFactory::op_vmov_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vmov_128);

    TCGArg DstOff = args[0];
    TCGArg SrcOff = args[1];
    Value *Dst, *Src;

    VectorType *VectorTy = VectorType::get(Int8Ty, 16);
    PointerType *PtrTy = PointerType::getUnqual(VectorTy);
    Src = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(SrcOff), "", LastInst);
    Src = new BitCastInst(Src, PtrTy, "", LastInst);
    Dst = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(DstOff), "", LastInst);
    Dst = new BitCastInst(Dst, PtrTy, "", LastInst);

    Src = new LoadInst(Src, "", false, LastInst);
    new StoreInst(Src, Dst, false, LastInst);
}

void IRFactory::op_vload_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vload_128);

    TCGArg Off = args[0];
    Register &In = Reg[args[1]];
    TCGArg Alignment = (args[2] == (TCGArg)-1) ? 4 : args[2] / 8;
    Value *Base = LoadState(In);
    LoadInst *LI;

    AssertType(In.Size == 32 || In.Size == 64);

    VectorType *VectorTy = VectorType::get(Int8Ty, 16);
    PointerType *PtrTy = PointerType::get(VectorTy, Segment);

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
    LI->setAlignment(Alignment);

    MF->setGuestMemory(LI);

    PtrTy = PointerType::getUnqual(VectorTy);
    Value *V = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Off), "", LastInst);
    V = new BitCastInst(V, PtrTy, "", LastInst);
    new StoreInst(LI, V, false, LastInst);
}

void IRFactory::op_vstore_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vstore_128);

    TCGArg Off = args[0];
    Register &In = Reg[args[1]];
    TCGArg Alignment = (args[2] == (TCGArg)-1) ? 4 : args[2] / 8;
    Value *Base = LoadState(In);
    StoreInst *SI;

    AssertType(In.Size == 32 || In.Size == 64);

    VectorType *VectorTy = VectorType::get(Int8Ty, 16);
    PointerType *PtrTy = nullptr;

    PtrTy = PointerType::getUnqual(VectorTy);
    Value *V = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Off), "", LastInst);
    V = new BitCastInst(V, PtrTy, "", LastInst);
    V = new LoadInst(V, "", false, LastInst);

    PtrTy = PointerType::get(VectorTy, Segment);
    if (GUEST_BASE == 0 || Segment != 0) {
        Base = ITP(Base, PtrTy);
        SI = new StoreInst(V, Base, true, LastInst);
    } else {
        Base = ITP(Base, Int8PtrTy);
        Base = GetElementPtrInst::CreateInBounds(Base, CONSTPtr(GUEST_BASE), "", LastInst);
        if (Base->getType() != PtrTy)
            Base = CAST(Base, PtrTy);
        SI = new StoreInst(V, Base, true, LastInst);
    }

    SI->setAlignment(Alignment);

    MF->setGuestMemory(SI);
}

#define llvm_gen_vop(_Fn,_Num,_Ty)      \
do {                                    \
    TCGArg Out = args[0];               \
    TCGArg In1 = args[1];               \
    TCGArg In2 = args[2];               \
    Value *OutPtr, *InPtr1, *InPtr2;    \
    VectorType *VectorTy = VectorType::get(_Ty, _Num);     \
    PointerType *PtrTy = PointerType::getUnqual(VectorTy); \
                                                           \
    InPtr1 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst); \
    InPtr1 = new BitCastInst(InPtr1, PtrTy, "", LastInst);                \
    InPtr2 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In2), "", LastInst); \
    InPtr2 = new BitCastInst(InPtr2, PtrTy, "", LastInst);                \
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst); \
    OutPtr = new BitCastInst(OutPtr, PtrTy, "", LastInst);                \
                                                                          \
    Value *InData1 = new LoadInst(InPtr1, "", false, LastInst);           \
    Value *InData2 = new LoadInst(InPtr2, "", false, LastInst);           \
    InData1 = _Fn(InData1, InData2);                                      \
    new StoreInst(InData1, OutPtr, false, LastInst);                      \
} while (0)

#define llvm_gen_vop2(_Fn1,_Fn2,_Num,_Ty)      \
do {                                           \
    TCGArg Out = args[0];                      \
    TCGArg In1 = args[1];                      \
    TCGArg In2 = args[2];                      \
    Value *OutPtr, *InPtr1, *InPtr2;           \
    VectorType *VectorTy = VectorType::get(_Ty, _Num);     \
    PointerType *PtrTy = PointerType::getUnqual(VectorTy); \
                                                           \
    InPtr1 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst); \
    InPtr1 = new BitCastInst(InPtr1, PtrTy, "", LastInst);                \
    InPtr2 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In2), "", LastInst); \
    InPtr2 = new BitCastInst(InPtr2, PtrTy, "", LastInst);                \
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst); \
    OutPtr = new BitCastInst(OutPtr, PtrTy, "", LastInst);                \
                                                                          \
    Value *InData1 = new LoadInst(InPtr1, "", false, LastInst);           \
    Value *InData2 = new LoadInst(InPtr2, "", false, LastInst);           \
    Value *InData3 = new LoadInst(OutPtr, "", false, LastInst);           \
    InData1 = _Fn2(InData3, _Fn1(InData1, InData2));                      \
    new StoreInst(InData1, OutPtr, false, LastInst);                      \
} while (0)


void IRFactory::op_vadd_i8_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vadd_i8_128);
    llvm_gen_vop(ADD, 16, Int8Ty);
}

void IRFactory::op_vadd_i16_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vadd_i16_128);
    llvm_gen_vop(ADD, 8, Int16Ty);
}

void IRFactory::op_vadd_i32_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vadd_i32_128);
    llvm_gen_vop(ADD, 4, Int32Ty);
}

void IRFactory::op_vadd_i64_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vadd_i64_128);
    llvm_gen_vop(ADD, 2, Int64Ty);
}

void IRFactory::op_vadd_i8_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vadd_i8_64);
    llvm_gen_vop(ADD, 8, Int8Ty);
}

void IRFactory::op_vadd_i16_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vadd_i16_64);
    llvm_gen_vop(ADD, 4, Int16Ty);
}

void IRFactory::op_vadd_i32_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vadd_i32_64);
    llvm_gen_vop(ADD, 2, Int32Ty);
}

void IRFactory::op_vsub_i8_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vsub_i8_128);
    llvm_gen_vop(SUB, 16, Int8Ty);
}

void IRFactory::op_vsub_i16_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vsub_i16_128);
    llvm_gen_vop(SUB, 8, Int16Ty);
}

void IRFactory::op_vsub_i32_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vsub_i32_128);
    llvm_gen_vop(SUB, 4, Int32Ty);
}

void IRFactory::op_vsub_i64_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vsub_i64_128);
    llvm_gen_vop(SUB, 2, Int64Ty);
}

void IRFactory::op_vsub_i8_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vsub_i8_64);
    llvm_gen_vop(SUB, 8, Int8Ty);
}

void IRFactory::op_vsub_i16_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vsub_i16_64);
    llvm_gen_vop(SUB, 4, Int16Ty);
}

void IRFactory::op_vsub_i32_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vsub_i32_64);
    llvm_gen_vop(SUB, 2, Int32Ty);
}

void IRFactory::op_vadd_f32_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vadd_f32_128);
    llvm_gen_vop(FADD, 4, FloatTy);
}

void IRFactory::op_vadd_f64_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vadd_f64_128);
    llvm_gen_vop(FADD, 2, DoubleTy);
}

void IRFactory::op_vadd_f32_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vadd_f32_64);
    llvm_gen_vop(FADD, 2, FloatTy);
}

void IRFactory::op_vpadd_f32_128(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_vpadd_f64_128(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_vpadd_f32_64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_vsub_f32_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vsub_f32_128);
    llvm_gen_vop(FSUB, 4, FloatTy);
}

void IRFactory::op_vsub_f64_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vsub_f64_128);
    llvm_gen_vop(FSUB, 2, DoubleTy);
}

void IRFactory::op_vsub_f32_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vsub_f32_64);
    llvm_gen_vop(FSUB, 2, FloatTy);
}

void IRFactory::op_vabd_f32_128(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_vabd_f64_128(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_vabd_f32_64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_vfma_f32_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vfma_f32_128);
    llvm_gen_vop2(FMUL, FADD, 4, FloatTy);
}

void IRFactory::op_vfma_f64_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vfma_f64_128);
    llvm_gen_vop2(FMUL, FADD, 2, DoubleTy);
}

void IRFactory::op_vfma_f32_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vfma_f32_64);
    llvm_gen_vop2(FMUL, FADD, 2, FloatTy);
}

void IRFactory::op_vfms_f32_128(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_vfms_f64_128(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_vfms_f32_64(const TCGArg *args)
{
    IRError("%s not implemented.\n", __func__);
}

void IRFactory::op_vmul_f32_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vmul_f32_128);
    llvm_gen_vop(FMUL, 4, FloatTy);
}

void IRFactory::op_vmul_f64_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vmul_f64_128);
    llvm_gen_vop(FMUL, 2, DoubleTy);
}

void IRFactory::op_vmul_f32_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vmul_f32_64);
    llvm_gen_vop(FMUL, 2, FloatTy);
}

void IRFactory::op_vmla_f32_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vmla_f32_128);
    llvm_gen_vop2(FMUL, FADD, 4, FloatTy);
}

void IRFactory::op_vmla_f64_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vmla_f64_128);
    llvm_gen_vop2(FMUL, FADD, 2, DoubleTy);
}

void IRFactory::op_vmla_f32_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vmla_f32_64);
    llvm_gen_vop2(FMUL, FADD, 2, FloatTy);
}

void IRFactory::op_vmls_f32_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vmls_f32_128);
    llvm_gen_vop2(FMUL, FSUB, 4, FloatTy);
}

void IRFactory::op_vmls_f64_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vmls_f64_128);
    llvm_gen_vop2(FMUL, FSUB, 2, DoubleTy);
}

void IRFactory::op_vmls_f32_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vmls_f32_64);
    llvm_gen_vop2(FMUL, FSUB, 2, FloatTy);
}

void IRFactory::op_vdiv_f32_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vdiv_f32_128);
    llvm_gen_vop(FDIV, 4, FloatTy);
}

void IRFactory::op_vdiv_f64_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vdiv_f64_128);
    llvm_gen_vop(FDIV, 2, DoubleTy);
}

void IRFactory::op_vdiv_f32_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vdiv_f32_64);
    llvm_gen_vop(FDIV, 2, FloatTy);
}

void IRFactory::op_vand_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vand_128);
    if (args[1] == args[2]) {
        op_vmov_128(args);
        return;
    }
    llvm_gen_vop(AND, 4, Int32Ty);
}

void IRFactory::op_vand_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vand_64);
    llvm_gen_vop(AND, 2, Int32Ty);
}

void IRFactory::op_vbic_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vbic_128);

    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg In2 = args[2];
    Value *OutPtr, *InPtr1, *InPtr2;
    VectorType *VectorTy = VectorType::get(Int32Ty, 4);
    PointerType *PtrTy = PointerType::getUnqual(VectorTy);

    InPtr1 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr1 = new BitCastInst(InPtr1, PtrTy, "", LastInst);
    InPtr2 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In2), "", LastInst);
    InPtr2 = new BitCastInst(InPtr2, PtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, PtrTy, "", LastInst);

    std::vector<Constant *> V;
    for (int i = 0; i < 4; i++)
        V.push_back(CONST32(-1U));
    Value *VecMinusOne = ConstantVector::get(V);

    Value *InData1 = new LoadInst(InPtr1, "", false, LastInst);
    Value *InData2 = new LoadInst(InPtr2, "", false, LastInst);
    InData2 = XOR(InData2, VecMinusOne);
    InData1 = AND(InData1, InData2);
    new StoreInst(InData1, OutPtr, false, LastInst);
}

void IRFactory::op_vbic_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vbic_64);

    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg In2 = args[2];
    Value *OutPtr, *InPtr1, *InPtr2;
    VectorType *VectorTy = VectorType::get(Int32Ty, 2);
    PointerType *PtrTy = PointerType::getUnqual(VectorTy);

    InPtr1 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr1 = new BitCastInst(InPtr1, PtrTy, "", LastInst);
    InPtr2 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In2), "", LastInst);
    InPtr2 = new BitCastInst(InPtr2, PtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, PtrTy, "", LastInst);

    std::vector<Constant *> V;
    for (int i = 0; i < 2; i++)
        V.push_back(CONST32(-1U));
    Value *VecMinusOne = ConstantVector::get(V);

    Value *InData1 = new LoadInst(InPtr1, "", false, LastInst);
    Value *InData2 = new LoadInst(InPtr2, "", false, LastInst);
    InData2 = XOR(InData2, VecMinusOne);
    InData1 = AND(InData1, InData2);
    new StoreInst(InData1, OutPtr, false, LastInst);
}

void IRFactory::op_vorr_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vorr_128);
    if (args[1] == args[2]) {
        op_vmov_128(args);
        return;
    }
    llvm_gen_vop(OR, 4, Int32Ty);
}

void IRFactory::op_vorr_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vorr_64);
    llvm_gen_vop(OR, 2, Int32Ty);
}

void IRFactory::op_vorn_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vorn_128);

    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg In2 = args[2];
    Value *OutPtr, *InPtr1, *InPtr2;
    VectorType *VectorTy = VectorType::get(Int32Ty, 4);
    PointerType *PtrTy = PointerType::getUnqual(VectorTy);

    InPtr1 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr1 = new BitCastInst(InPtr1, PtrTy, "", LastInst);
    InPtr2 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In2), "", LastInst);
    InPtr2 = new BitCastInst(InPtr2, PtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, PtrTy, "", LastInst);

    std::vector<Constant *> V;
    for (int i = 0; i < 4; i++)
        V.push_back(CONST32(-1U));
    Value *VecMinusOne = ConstantVector::get(V);

    Value *InData1 = new LoadInst(InPtr1, "", false, LastInst);
    Value *InData2 = new LoadInst(InPtr2, "", false, LastInst);
    InData2 = XOR(InData2, VecMinusOne);
    InData1 = OR(InData1, InData2);
    new StoreInst(InData1, OutPtr, false, LastInst);
}

void IRFactory::op_vorn_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vorn_64);

    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg In2 = args[2];
    Value *OutPtr, *InPtr1, *InPtr2;
    VectorType *VectorTy = VectorType::get(Int32Ty, 2);
    PointerType *PtrTy = PointerType::getUnqual(VectorTy);

    InPtr1 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr1 = new BitCastInst(InPtr1, PtrTy, "", LastInst);
    InPtr2 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In2), "", LastInst);
    InPtr2 = new BitCastInst(InPtr2, PtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, PtrTy, "", LastInst);

    std::vector<Constant *> V;
    for (int i = 0; i < 2; i++)
        V.push_back(CONST32(-1U));
    Value *VecMinusOne = ConstantVector::get(V);

    Value *InData1 = new LoadInst(InPtr1, "", false, LastInst);
    Value *InData2 = new LoadInst(InPtr2, "", false, LastInst);
    InData2 = XOR(InData2, VecMinusOne);
    InData1 = OR(InData1, InData2);
    new StoreInst(InData1, OutPtr, false, LastInst);
}

void IRFactory::op_veor_128(const TCGArg *args)
{
    IRDebug(INDEX_op_veor_128);
    llvm_gen_vop(XOR, 4, Int32Ty);
}

void IRFactory::op_veor_64(const TCGArg *args)
{
    IRDebug(INDEX_op_veor_64);
    llvm_gen_vop(XOR, 2, Int32Ty);
}

void IRFactory::op_vbif_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vbif_128);

    /* vbif rd, rn, rm
     *   operation: rd <- (rd & rm) | (rn & ~rm) */
    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg In2 = args[2];
    Value *OutPtr, *InPtr1, *InPtr2;
    VectorType *VectorTy = VectorType::get(Int32Ty, 4);
    PointerType *PtrTy = PointerType::getUnqual(VectorTy);

    InPtr1 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr1 = new BitCastInst(InPtr1, PtrTy, "", LastInst);
    InPtr2 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In2), "", LastInst);
    InPtr2 = new BitCastInst(InPtr2, PtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, PtrTy, "", LastInst);

    std::vector<Constant *> V;
    for (int i = 0; i < 4; i++)
        V.push_back(CONST32(-1U));
    Value *VecMinusOne = ConstantVector::get(V);

    Value *InData1 = new LoadInst(InPtr1, "", false, LastInst);
    Value *InData2 = new LoadInst(InPtr2, "", false, LastInst);
    Value *InData3 = new LoadInst(OutPtr, "", false, LastInst);

    InData3 = AND(InData3, InData2);
    InData1 = AND(InData1, XOR(InData2, VecMinusOne));
    InData3 = OR(InData1, InData3);
    new StoreInst(InData3, OutPtr, false, LastInst);
}

void IRFactory::op_vbif_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vbif_64);

    /* vbif rd, rn, rm
     *   operation: rd <- (rd & rm) | (rn & ~rm) */
    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg In2 = args[2];
    Value *OutPtr, *InPtr1, *InPtr2;
    VectorType *VectorTy = VectorType::get(Int32Ty, 2);
    PointerType *PtrTy = PointerType::getUnqual(VectorTy);

    InPtr1 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr1 = new BitCastInst(InPtr1, PtrTy, "", LastInst);
    InPtr2 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In2), "", LastInst);
    InPtr2 = new BitCastInst(InPtr2, PtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, PtrTy, "", LastInst);

    std::vector<Constant *> V;
    for (int i = 0; i < 2; i++)
        V.push_back(CONST32(-1U));
    Value *VecMinusOne = ConstantVector::get(V);

    Value *InData1 = new LoadInst(InPtr1, "", false, LastInst);
    Value *InData2 = new LoadInst(InPtr2, "", false, LastInst);
    Value *InData3 = new LoadInst(OutPtr, "", false, LastInst);

    InData3 = AND(InData3, InData2);
    InData1 = AND(InData1, XOR(InData2, VecMinusOne));
    InData3 = OR(InData1, InData3);
    new StoreInst(InData3, OutPtr, false, LastInst);
}

void IRFactory::op_vbit_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vbit_128);

    /* vbit rd, rn, rm
     *   operation: rd <- (rn & rm) | (rd & ~rm) */
    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg In2 = args[2];
    Value *OutPtr, *InPtr1, *InPtr2;
    VectorType *VectorTy = VectorType::get(Int32Ty, 4);
    PointerType *PtrTy = PointerType::getUnqual(VectorTy);

    InPtr1 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr1 = new BitCastInst(InPtr1, PtrTy, "", LastInst);
    InPtr2 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In2), "", LastInst);
    InPtr2 = new BitCastInst(InPtr2, PtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, PtrTy, "", LastInst);

    std::vector<Constant *> V;
    for (int i = 0; i < 4; i++)
        V.push_back(CONST32(-1U));
    Value *VecMinusOne = ConstantVector::get(V);

    Value *InData1 = new LoadInst(InPtr1, "", false, LastInst);
    Value *InData2 = new LoadInst(InPtr2, "", false, LastInst);
    Value *InData3 = new LoadInst(OutPtr, "", false, LastInst);

    InData1 = AND(InData1, InData2);
    InData3 = AND(InData3, XOR(InData2, VecMinusOne));
    InData3 = OR(InData1, InData3);
    new StoreInst(InData3, OutPtr, false, LastInst);
}

void IRFactory::op_vbit_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vbit_64);

    /* vbit rd, rn, rm
     *   operation: rd <- (rn & rm) | (rd & ~rm) */
    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg In2 = args[2];
    Value *OutPtr, *InPtr1, *InPtr2;
    VectorType *VectorTy = VectorType::get(Int32Ty, 2);
    PointerType *PtrTy = PointerType::getUnqual(VectorTy);

    InPtr1 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr1 = new BitCastInst(InPtr1, PtrTy, "", LastInst);
    InPtr2 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In2), "", LastInst);
    InPtr2 = new BitCastInst(InPtr2, PtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, PtrTy, "", LastInst);

    std::vector<Constant *> V;
    for (int i = 0; i < 2; i++)
        V.push_back(CONST32(-1U));
    Value *VecMinusOne = ConstantVector::get(V);

    Value *InData1 = new LoadInst(InPtr1, "", false, LastInst);
    Value *InData2 = new LoadInst(InPtr2, "", false, LastInst);
    Value *InData3 = new LoadInst(OutPtr, "", false, LastInst);

    InData1 = AND(InData1, InData2);
    InData3 = AND(InData3, XOR(InData2, VecMinusOne));
    InData3 = OR(InData1, InData3);
    new StoreInst(InData3, OutPtr, false, LastInst);
}

void IRFactory::op_vbsl_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vbsl_128);

    /* vbsl rd, rn, rm
     *   operation: rd <- (rn & rd) | (rm & ~rd) */
    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg In2 = args[2];
    Value *OutPtr, *InPtr1, *InPtr2;
    VectorType *VectorTy = VectorType::get(Int32Ty, 4);
    PointerType *PtrTy = PointerType::getUnqual(VectorTy);

    InPtr1 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr1 = new BitCastInst(InPtr1, PtrTy, "", LastInst);
    InPtr2 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In2), "", LastInst);
    InPtr2 = new BitCastInst(InPtr2, PtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, PtrTy, "", LastInst);

    std::vector<Constant *> V;
    for (int i = 0; i < 4; i++)
        V.push_back(CONST32(-1U));
    Value *VecMinusOne = ConstantVector::get(V);

    Value *InData1 = new LoadInst(InPtr1, "", false, LastInst);
    Value *InData2 = new LoadInst(InPtr2, "", false, LastInst);
    Value *InData3 = new LoadInst(OutPtr, "", false, LastInst);

    InData1 = AND(InData1, InData3);
    InData2 = AND(InData2, XOR(InData3, VecMinusOne));
    InData3 = OR(InData1, InData2);
    new StoreInst(InData3, OutPtr, false, LastInst);
}

void IRFactory::op_vbsl_64(const TCGArg *args)
{
    IRDebug(INDEX_op_vbsl_64);

    /* vbsl rd, rn, rm
     *   operation: rd <- (rn & rd) | (rm & ~rd) */
    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg In2 = args[2];
    Value *OutPtr, *InPtr1, *InPtr2;
    VectorType *VectorTy = VectorType::get(Int32Ty, 2);
    PointerType *PtrTy = PointerType::getUnqual(VectorTy);

    InPtr1 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr1 = new BitCastInst(InPtr1, PtrTy, "", LastInst);
    InPtr2 = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In2), "", LastInst);
    InPtr2 = new BitCastInst(InPtr2, PtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, PtrTy, "", LastInst);

    std::vector<Constant *> V;
    for (int i = 0; i < 2; i++)
        V.push_back(CONST32(-1U));
    Value *VecMinusOne = ConstantVector::get(V);

    Value *InData1 = new LoadInst(InPtr1, "", false, LastInst);
    Value *InData2 = new LoadInst(InPtr2, "", false, LastInst);
    Value *InData3 = new LoadInst(OutPtr, "", false, LastInst);

    InData1 = AND(InData1, InData3);
    InData2 = AND(InData2, XOR(InData3, VecMinusOne));
    InData3 = OR(InData1, InData2);
    new StoreInst(InData3, OutPtr, false, LastInst);
}

void IRFactory::op_vsitofp_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vsitofp_128);

    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg Size = args[2];

    unsigned NumElements = 0;
    Type *SrcTy = nullptr, *DstTy = nullptr;
    if (Size == 32) {
        NumElements = 4;
        SrcTy = Int32Ty;
        DstTy = FloatTy;
    } else if (Size == 64) {
        NumElements = 2;
        SrcTy = Int64Ty;
        DstTy = DoubleTy;
    } else
        IRError("%s: invalid element size.\n", __func__);

    Value *OutPtr, *InPtr;
    VectorType *VectorInt = VectorType::get(SrcTy, NumElements);
    PointerType *IntPtrTy = PointerType::getUnqual(VectorInt);
    VectorType *VectorFP = VectorType::get(DstTy, NumElements);
    PointerType *FPPtrTy = PointerType::getUnqual(VectorFP);

    InPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr = new BitCastInst(InPtr, IntPtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, FPPtrTy, "", LastInst);

    Value *InData = new LoadInst(InPtr, "", false, LastInst);
    InData = new SIToFPInst(InData, VectorFP, "", LastInst);
    new StoreInst(InData, OutPtr, false, LastInst);
}

void IRFactory::op_vuitofp_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vuitofp_128);

    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg Size = args[2];

    unsigned NumElements = 0;
    Type *SrcTy = nullptr, *DstTy = nullptr;
    if (Size == 32) {
        NumElements = 4;
        SrcTy = Int32Ty;
        DstTy = FloatTy;
    } else if (Size == 64) {
        NumElements = 2;
        SrcTy = Int64Ty;
        DstTy = DoubleTy;
    } else
        IRError("%s: invalid element size.\n", __func__);

    Value *OutPtr, *InPtr;
    VectorType *VectorInt = VectorType::get(SrcTy, NumElements);
    PointerType *IntPtrTy = PointerType::getUnqual(VectorInt);
    VectorType *VectorFP = VectorType::get(DstTy, NumElements);
    PointerType *FPPtrTy = PointerType::getUnqual(VectorFP);

    InPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr = new BitCastInst(InPtr, IntPtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, FPPtrTy, "", LastInst);

    Value *InData = new LoadInst(InPtr, "", false, LastInst);
    InData = new UIToFPInst(InData, VectorFP, "", LastInst);
    new StoreInst(InData, OutPtr, false, LastInst);
}

void IRFactory::op_vfptosi_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vfptosi_128);

    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg Size = args[2];

    unsigned NumElements = 0;
    Type *SrcTy = nullptr, *DstTy = nullptr;
    if (Size == 32) {
        NumElements = 4;
        SrcTy = FloatTy;
        DstTy = Int32Ty;
    } else if (Size == 64) {
        NumElements = 2;
        SrcTy = DoubleTy;
        DstTy = Int64Ty;
    } else
        IRError("%s: invalid element size.\n", __func__);

    Value *OutPtr, *InPtr;
    VectorType *VectorFP = VectorType::get(SrcTy, NumElements);
    PointerType *FPPtrTy = PointerType::getUnqual(VectorFP);
    VectorType *VectorInt = VectorType::get(DstTy, NumElements);
    PointerType *IntPtrTy = PointerType::getUnqual(VectorInt);

    InPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr = new BitCastInst(InPtr, FPPtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, IntPtrTy, "", LastInst);

    Value *InData = new LoadInst(InPtr, "", false, LastInst);
    InData = new FPToSIInst(InData, VectorInt, "", LastInst);
    new StoreInst(InData, OutPtr, false, LastInst);
}

void IRFactory::op_vfptoui_128(const TCGArg *args)
{
    IRDebug(INDEX_op_vfptoui_128);

    TCGArg Out = args[0];
    TCGArg In1 = args[1];
    TCGArg Size = args[2];

    unsigned NumElements = 0;
    Type *SrcTy = nullptr, *DstTy = nullptr;
    if (Size == 32) {
        NumElements = 4;
        SrcTy = FloatTy;
        DstTy = Int32Ty;
    } else if (Size == 64) {
        NumElements = 2;
        SrcTy = DoubleTy;
        DstTy = Int64Ty;
    } else
        IRError("%s: invalid element size.\n", __func__);

    Value *OutPtr, *InPtr;
    VectorType *VectorFP = VectorType::get(SrcTy, NumElements);
    PointerType *FPPtrTy = PointerType::getUnqual(VectorFP);
    VectorType *VectorInt = VectorType::get(DstTy, NumElements);
    PointerType *IntPtrTy = PointerType::getUnqual(VectorInt);

    InPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(In1), "", LastInst);
    InPtr = new BitCastInst(InPtr, FPPtrTy, "", LastInst);
    OutPtr = GetElementPtrInst::CreateInBounds(CPU, CONSTPtr(Out), "", LastInst);
    OutPtr = new BitCastInst(OutPtr, IntPtrTy, "", LastInst);

    Value *InData = new LoadInst(InPtr, "", false, LastInst);
    InData = new FPToUIInst(InData, VectorInt, "", LastInst);
    new StoreInst(InData, OutPtr, false, LastInst);
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
