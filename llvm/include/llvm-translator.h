/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __LLVM_TRANSLATOR_H
#define __LLVM_TRANSLATOR_H

#include <map>
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Analysis/CodeMetrics.h"
#include "llvm-types.h"
#include "llvm-pass.h"
#include "llvm.h"


class OptimizationInfo;
class EventListener;
class NotifyInfo;
class IRFactory;
class TraceBuilder;


/*
 * BaseRegister is used to describe the `reserved' registers by QEMU TCG.
 * Ex: R14 for the x86 host or R7 for the ARM host.
 */
struct BaseRegister {
    BaseRegister() : Base(nullptr) {}
    int RegNo;          /* Register number */
    std::string Name;   /* Register name string */
    Type *Ty;           /* Type (struct CPUArchState) */
    Instruction *Base;  /* CallInst to retrieve basereg */
};

/*
 * Information of helper functions defined in llvm-helper.h.
 */
struct HelperInfo {
    HelperInfo()
        : ConflictSize(0), mayConflictArg(false), hasNestedCall(false) {}

    struct ArgInfo {
        unsigned ConstantWeight;  /* Weight if the argument is a constant */
        unsigned AllocaWeight;    /* Weight if the argument is a alloca */
        ArgInfo(unsigned CWeight, unsigned AWeight)
            : ConstantWeight(CWeight), AllocaWeight(AWeight) {}
    };

    Function *Func;          /* Function symbol to be inlined */
    Function *FuncNoInline;  /* Function symbol not to be inlined */
    std::vector<std::pair<Instruction*, intptr_t> > States;
    std::vector<CallInst*> NestedCalls;
    StateRange StateUse;
    StateRange StateDef;
    CodeMetrics Metrics;     /* Inlining metrics */
    std::vector<ArgInfo> ArgumentWeights;  /* Weight of the function arguments */
    intptr_t ConflictSize;

    bool mayConflictArg;     /* Arguments conflict with state mapping or not */
    bool hasNestedCall;      /* This function has nested function or not */

    void CalculateMetrics(Function *F);

    void insertState(StateRange &Range, bool isWrite) {
        if (isWrite)
            StateDef.insert(Range.begin(), Range.end());
        else
            StateUse.insert(Range.begin(), Range.end());
    }
};

/* 
 * NotifyInfo is used to pass information between LLVMTranslator, IRFactory and
 * the JIT listener.
 */
class NotifyInfo {
#define MAX_CHAINSLOT   256
public:
    struct SlotInfo {
        size_t Key;
        uintptr_t Addr;
    };

    struct PatchInfo {
        PatchInfo(unsigned ty, unsigned idx, uintptr_t addr)
            : Type(ty), Idx(idx), Addr(addr) {}
        unsigned Type;
        unsigned Idx;
        uintptr_t Addr;
    };

    NotifyInfo() : Func(nullptr) {
        ChainSlot = new SlotInfo[MAX_CHAINSLOT];
    }
    ~NotifyInfo() {
        delete ChainSlot;
    }

    Function *Func;        /* LLVM Function of this translation unit */
    TCGOp *Op;
    TranslationBlock *TB;
    uint16_t NumInsts;
    RestoreVec Restore;
    unsigned NumChainSlot;
    SlotInfo *ChainSlot;

    uint32_t Size;         /* Size of the translated host code */
    uint8_t *Code;         /* Start PC of the translated host code */
    std::vector<PatchInfo> Patches;

    void reset() {
        Restore.clear();
        Patches.clear();
        NumInsts = 0;
        NumChainSlot = 0;
    }
    unsigned setChainSlot(size_t Key) {
        if (NumChainSlot >= MAX_CHAINSLOT)
            hqemu_error("run out of chain slot.\n");
        unsigned Curr = NumChainSlot;
        ChainSlot[NumChainSlot++].Key = Key;
        return Curr;
    }
    uintptr_t getChainSlotAddr(unsigned Idx) {
        if (NumChainSlot >= MAX_CHAINSLOT)
            hqemu_error("invalid chain slot index.\n");
        return (uintptr_t)&ChainSlot[Idx].Addr;
    }
    void addPatch(unsigned Type, unsigned Idx, uintptr_t Addr) {
        Patches.push_back(PatchInfo(Type, Idx, Addr));
    }
    void setOp(TCGOp *op) { Op = op; }
    void setTB(TranslationBlock *tb) {
        TB = tb;
        NumInsts = 0;
    }
    uint32_t setRestorePoint() {
        uint32_t Idx = Restore.size();
        if (Idx != (uint16_t)Idx)
            hqemu_error("key value too large.\n");
        Restore.push_back(std::make_pair(TB->id, NumInsts));
        return Idx;
    }
};

/*
 * LLVM Translator
 */
class LLVMTranslator {
    unsigned MyID;           /* Translator ID */
    CPUArchState *Env;

    /* Basic types */
    Type *VoidTy;
    IntegerType *Int8Ty;
    IntegerType *Int16Ty;
    IntegerType *Int32Ty;
    IntegerType *Int64Ty;
    IntegerType *Int128Ty;
    IntegerType *IntPtrTy;
    PointerType *Int8PtrTy;
    PointerType *Int16PtrTy;
    PointerType *Int32PtrTy;
    PointerType *Int64PtrTy;
    Type *FloatTy;
    Type *DoubleTy;
    PointerType *FloatPtrTy;
    PointerType *DoublePtrTy;

    LLVMContext Context;     /* Translator local context */
    Module *Mod;             /* The LLVM module */
    const DataLayout *DL;    /* Data layout */
    NotifyInfo NI;           /* Info to set/use by the JIT listener */

    std::vector<BaseRegister> BaseReg;  /* Reserved base registers */
    FlatType StateType;      /* Offset and type of guest registers */
    TCGHelperMap TCGHelpers;
    HelperMap Helpers;
    std::set<std::string> ConstHelpers;
    SymbolMap Symbols;

    MCDisasm *GuestDisAsm;
    MCDisasm *HostDisAsm;

    IRFactory *IF;           /* TCG-to-LLVM IR converter */

    /* InitializeModule - Initialization of the LLVM module. */
    void InitializeModule();

    /* InitializeJIT - JIT compiler creation. */
    void InitializeJIT();

    void InitializeType();

    /* InitializeTarget - Setup guest- and host-dependent structures. */
    void InitializeTarget();

    /* DefineSpecialReg - Setup special registers. */
    void DefineSpecialReg(std::map<Type*, Type*> &SpecialReg);

    /* FlattenCPUType - Convert the CPUArchState of aggregate type to a list
     * of primitive types. */
    void FlattenCPUState(Type *Ty, intptr_t &Off, std::map<Type*, Type*> &SpecialReg);

    /* InitHelperFunctions - Initialization of helper functions. */
    void InitializeHelpers();

    /* OptimizeHelper - Analyze and optimize a helper function. */
    bool OptimizeHelper(HelperInfo &Helper);

    void InitializeDisasm();

    void InitializeConstHelpers();

    void Commit(TraceBuilder &Builder);

    void Abort(TraceBuilder &Builder);

    void dump(CPUArchState *env, TranslationBlock *tb);

    LLVMTranslator(unsigned id, CPUArchState *env);

public:
    ~LLVMTranslator();

    void GenBlock(CPUArchState *env, OptimizationInfo *Opt);
    void GenTrace(CPUArchState *env, OptimizationInfo *Opt);

    unsigned getID()            { return MyID;      }
    LLVMContext *getContext()   { return &Context;  }
    Module *getModule()         { return Mod;       }
    NotifyInfo &getNotifyInfo() { return NI;        }
    std::vector<BaseRegister> &getBaseReg() { return BaseReg;      }
    TCGHelperMap &getTCGHelpers()      { return TCGHelpers;   }
    HelperMap &getHelpers()            { return Helpers;      }
    std::set<std::string> &getConstHelpers()     { return ConstHelpers; }
    FlatType &getStateType()    { return StateType; } 
    SymbolMap &getSymbols()     { return Symbols;   }
    MCDisasm *getHostDisAsm()   { return HostDisAsm;}

    void AddSymbol(std::string Name, void *FP) {
        Symbols[Name] = (uintptr_t)FP;
    }

    /* createLLVMTranslator - Create the LLVMTranslator instrance. */
    static LLVMTranslator *CreateLLVMTranslator(int id, CPUArchState *env) {
        return new LLVMTranslator(id, env);
    }

    /* printAsm - Show guest assembly code for each compiled TB. */
    void printAsm(CPUArchState *env, TranslationBlock *tb);

    /* printOp - Show TCG micro ops for each compiled TB. */
    void printOp(CPUArchState *env, TranslationBlock *tb);
};

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
