/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __LLVM_MACRO_H
#define __LLVM_MACRO_H

#if defined(CONFIG_SOFTMMU)
#define SaveStates()    SaveGlobals(COHERENCE_GLOBAL, LastInst)
#else
#define SaveStates()
#endif

#define CONST8(a)       ConstantInt::get(Int8Ty, a)
#define CONST16(a)      ConstantInt::get(Int16Ty, a)
#define CONST32(a)      ConstantInt::get(Int32Ty, a)
#define CONST64(a)      ConstantInt::get(Int64Ty, a)
#define CONST128(a)     ConstantInt::get(Int128Ty, a)
#define CONSTPtr(a)     ConstantInt::get(IntPtrTy, a)

#define FPCONST32(a)    ConstantFP::get(FloatTy, a)
#define FPCONST64(a)    ConstantFP::get(DoubleTy, a)
#define FPCONST80(a)    ConstantFP::get(FP80Ty, a)
#define FPCONST128(a)   ConstantFP::get(FP128Ty, a)

#define ICMP(a,b,pred)  new ICmpInst(LastInst, pred, a, b, "")

#define AND(a,b)        BinaryOperator::Create(Instruction::And,  a, b, "", LastInst)
#define OR(a,b)         BinaryOperator::Create(Instruction::Or,   a, b, "", LastInst)
#define XOR(a,b)        BinaryOperator::Create(Instruction::Xor,  a, b, "", LastInst)
#define SHL(a,b)        BinaryOperator::Create(Instruction::Shl,  a, b, "", LastInst)
#define LSHR(a,b)       BinaryOperator::Create(Instruction::LShr, a, b, "", LastInst)
#define ASHR(a,b)       BinaryOperator::Create(Instruction::AShr, a, b, "", LastInst)
#define ADD(a,b)        BinaryOperator::Create(Instruction::Add,  a, b, "", LastInst)
#define SUB(a,b)        BinaryOperator::Create(Instruction::Sub,  a, b, "", LastInst)
#define MUL(a,b)        BinaryOperator::Create(Instruction::Mul,  a, b, "", LastInst)
#define SDIV(a,b)       BinaryOperator::Create(Instruction::SDiv, a, b, "", LastInst)
#define UDIV(a,b)       BinaryOperator::Create(Instruction::UDiv, a, b, "", LastInst)
#define SREM(a,b)       BinaryOperator::Create(Instruction::SRem, a, b, "", LastInst)
#define UREM(a,b)       BinaryOperator::Create(Instruction::URem, a, b, "", LastInst)

#define FADD(a,b)       BinaryOperator::Create(Instruction::FAdd, a, b, "", LastInst)
#define FSUB(a,b)       BinaryOperator::Create(Instruction::FSub, a, b, "", LastInst)
#define FMUL(a,b)       BinaryOperator::Create(Instruction::FMul, a, b, "", LastInst)
#define FDIV(a,b)       BinaryOperator::Create(Instruction::FDiv, a, b, "", LastInst)

#define CAST(a,t)       new BitCastInst(a, t, "", LastInst)
#define CASTPTR8(a)     CAST(a,Int8PtrTy)
#define CASTPTR16(a)    CAST(a,Int16PtrTy)
#define CASTPTR32(a)    CAST(a,Int32PtrTy)
#define CASTPTR64(a)    CAST(a,Int64PtrTy)

#define ITP(a,t)        new IntToPtrInst(a, t, "", LastInst)
#define ITP8(a)         ITP(a,Int8PtrTy)
#define ITP16(a)        ITP(a,Int16PtrTy)
#define ITP32(a)        ITP(a,Int32PtrTy)
#define ITP64(a)        ITP(a,Int64PtrTy)

#define TRUNC(a,t)      new TruncInst(a, t, "", LastInst)
#define TRUNC8(a)       TRUNC(a, Int8Ty)
#define TRUNC16(a)      TRUNC(a, Int16Ty)
#define TRUNC32(a)      TRUNC(a, Int32Ty)
#define TRUNC64(a)      TRUNC(a, Int64Ty)

#define ZEXT(a,t)       new ZExtInst(a, t, "", LastInst)
#define ZEXT8(a)        ZEXT(a, Int8Ty)
#define ZEXT16(a)       ZEXT(a, Int16Ty)
#define ZEXT32(a)       ZEXT(a, Int32Ty)
#define ZEXT64(a)       ZEXT(a, Int64Ty)
#define ZEXT128(a)      ZEXT(a, Int128Ty)
#define SEXT(a,t)       new SExtInst(a, t, "", LastInst)
#define SEXT8(a)        SEXT(a, Int8Ty)
#define SEXT16(a)       SEXT(a, Int16Ty)
#define SEXT32(a)       SEXT(a, Int32Ty)
#define SEXT64(a)       SEXT(a, Int64Ty)
#define SEXT128(a)      SEXT(a, Int128Ty)

#define BSWAP16(a)      CreateBSwap(Int16Ty, a, LastInst)
#define BSWAP32(a)      CreateBSwap(Int32Ty, a, LastInst)
#define BSWAP64(a)      CreateBSwap(Int64Ty, a, LastInst)


#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
