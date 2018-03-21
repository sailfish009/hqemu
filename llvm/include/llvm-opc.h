/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __LLVM_OPC_H
#define __LLVM_OPC_H

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "qemu-types.h"
#include "llvm-types.h"
#include "llvm-translator.h"
#include "llvm.h"

//#define ASSERT
//#define VERIFY_TB


#define IRDebug(idx)     \
    do {                 \
        dbg() << DEBUG_ENTRY << "op_" << llvm_op_defs[idx].name << ": " \
              << llvm_op_defs[idx].nb_oargs << " "   \
              << llvm_op_defs[idx].nb_iargs << " "   \
              << llvm_op_defs[idx].nb_cargs << "\n"; \
    } while (0)
#define IRError(fmt,args...)  hqemu_error(fmt,##args)

#ifdef ASSERT
#define AssertType(t)                           \
    do {                                        \
        if (!(t))                               \
            hqemu_error("invalid type.\n");     \
    } while(0)
#else
#define AssertType(t)
#endif

#define IRAbort()                               \
    do {                                        \
        if (!LLEnv->isTraceMode()) {            \
            Func->dump();                       \
            hqemu_error("fixme.\n");            \
        }                                       \
        Builder->Abort();                       \
    } while (0)


class LLVMTranslator;
class NotifyInfo;
class OptimizationInfo;


/* Patch flag. NOTE: the patch flag must be synchronized with those in
 * the LLVM backend. */
enum {
    PATCH_HQEMU = 0x4182U,
    PATCH_DUMMY,
    PATCH_EXIT_TB,
    PATCH_DIRECT_JUMP,
    PATCH_TRACE_BLOCK_CHAINING,
    PATCH_QMMU,
};

/*
 * Register is used to describe the pseudo registers used by QEMU TCG op.
 */
struct Register {
    /* Status of the register. */
    enum {
        STATE_NONE = 0x0,
        STATE_REV  = 0x1,   /* Register is reserved */
        STATE_REG  = 0x2,   /* Register is promoted */
        STATE_MEM  = 0x4,   /* Register is in CPUArchState memory */
        STATE_LOC  = 0x8,   /* Register is a local register */
        STATE_TMP  = 0x10,  /* Register is a tmp register */
    };

    int State;        /* State of the register */
    int Base;
    intptr_t Off;     /* Register offset of CPUArchState */
    int Size;         /* Register size */
    std::string Name; /* Name string of this register */
    bool Dirty;       /* This register is updated or not */
    Type *Ty;         /* Register type in LLVM */
    Value *Data;      /* Data value if this regisrer is promoted */
    Value *AI;        /* Register as Alloca */
    Register *Alias;

    Register() : State(STATE_NONE), Off(-1), Dirty(false), Ty(nullptr),
                 Data(nullptr), AI(nullptr), Alias(nullptr) {}

    void set(int base, intptr_t off, std::string name) {
        Base = base;
        Off = off;
        Name = name;
    }
    void reset(int state, int size, Type *ty) {
        State = state;
        Size = size;
        Ty = ty;
        Dirty = false;
        Data = AI = nullptr;
    }

    void Promote()            { State |= STATE_REG;  }
    void Demote()             { State &= ~STATE_REG; }

    Value *getData()          { return Data;      }
    Register &getAlias()      { return *Alias;    }

    void setState(int state)  { State = state;    }
    void setData(Value *data, bool dirty = false) {
        if (Alias) {
            Alias->setData(data, dirty);
            return;
        }
        Data = data;
        Dirty = dirty;
        Promote();
    }
    bool isRev()    { return State & STATE_REV; }
    bool isReg()    { return State & STATE_REG; }
    bool isMem()    { return State & STATE_MEM; }
    bool isLocal()  { return State & STATE_LOC; }
    bool isDirty()  { return Dirty;            }
    bool isAlias()  { return Alias != nullptr; }
};

/*
 * TraceBuilder provides the facilities to build a trace in IRFactory.
 */
class TraceBuilder {
    typedef std::map<target_ulong,
                     std::pair<GraphNode*, BasicBlock*> > NodeBuildMap;
    typedef std::vector<std::pair<BranchInst*, GraphNode*> > BranchList;

    IRFactory *IF;
    OptimizationInfo *Opt;
    GraphNode *CurrNode;   /* The current CFG node to process */
    NodeBuildMap Nodes;
    BranchList Branches;
    NodeVec NodeQueue;     /* CFG nodes to be translated */
    NodeSet NodeVisisted;
    NodeVec NodeUsed;
    bool Aborted;
    uint32_t Attribute;

    TraceInfo *Trace;

public:
    TraceBuilder(IRFactory *IRF, OptimizationInfo *Opt);
    ~TraceBuilder() {}

    void ConvertToTCGIR(CPUArchState *env);
    void ConvertToLLVMIR();
    void Abort();
    void Finalize();
    bool isAborted() { return Aborted; }

    OptimizationInfo *getOpt() { return Opt;             }
    TraceInfo *getTrace()      { return Trace;           }
    GraphNode *getEntryNode()  { return Opt->getCFG();   }
    GraphNode *getCurrNode()   { return CurrNode;        }
    unsigned getNumNodes()     { return Nodes.size();    }
    std::string getPCString(GraphNode *Node) {
        std::stringstream ss;
        ss << std::hex << Node->getGuestPC();
        return ss.str();
    }

    GraphNode *getNextNode()  {
        if (NodeQueue.empty())
            return nullptr;
        CurrNode = NodeQueue.back();
        NodeQueue.pop_back();

        if (NodeVisisted.find(CurrNode) != NodeVisisted.end())
            return getNextNode();

        NodeVisisted.insert(CurrNode);
        NodeUsed.push_back(CurrNode);
        return CurrNode;
    }

    target_ulong getGuestPC(GraphNode *Node) {
#if defined(TARGET_I386)
        return Node->getTB()->pc - Node->getTB()->cs_base;
#else
        return Node->getTB()->pc;
#endif
    }
    void setUniqueNode(GraphNode *Node) {
        target_ulong gpc = getGuestPC(Node);
        if (Nodes.find(gpc) == Nodes.end())
            Nodes[gpc] = std::make_pair(Node, nullptr);
    }
    void setBasicBlock(GraphNode *Node, BasicBlock *BB) {
        target_ulong gpc = getGuestPC(Node);
        if (Nodes.find(gpc) == Nodes.end())
            hqemu_error("internal error.\n");
        Nodes[gpc].second = BB;
    }
    void setBranch(BranchInst *BI, GraphNode *Node) {
        Branches.push_back(std::make_pair(BI, Node));
        target_ulong gpc = getGuestPC(Node);
        if (!Nodes[gpc].second)
            NodeQueue.push_back(Node);
    }
    /* lookupNode - Determine if the given ip is in one of the nodes. */
    GraphNode *getNode(target_ulong gpc) {
        return Nodes.find(gpc) == Nodes.end() ? nullptr : Nodes[gpc].first;
    }
    BasicBlock *getBasicBlock(GraphNode *Node) {
        target_ulong gpc = getGuestPC(Node);
        if (Nodes.find(gpc) == Nodes.end())
            hqemu_error("internal error.\n");
        return Nodes[gpc].second;
    }
    void addAttribute(uint32_t Attr) {
        Attribute |= Attr;
    }
};


#define META_CONST  "const"
#define META_GVA    "gva"
#define META_LOOP   "loop"
#define META_EXIT   "exit"
#define META_CC     "cc"

class MDFactory {
    uint32_t UID;
    LLVMContext &Context;
    MDNode *Dummy;

    ConstantInt *getUID() {
        return ConstantInt::get(IntegerType::get(Context, 32), UID++);
    }

public:
    MDFactory(Module *M);
    ~MDFactory();

    MDNode *getMDNode(ArrayRef<ConstantInt*> V);
    DebugLoc getDebugLoc(unsigned Line, unsigned Col, Function *F,
                         ArrayRef<ConstantInt*> Meta);

    void setConst(Instruction *I)       { I->setMetadata(META_CONST, Dummy); }
    void setGuestMemory(Instruction *I) { I->setMetadata(META_GVA, Dummy);   }
    void setLoop(Instruction *I)        { I->setMetadata(META_LOOP, Dummy);  }
    void setExit(Instruction *I)        { I->setMetadata(META_EXIT, Dummy);  }
    void setCondition(Instruction *I)   { I->setMetadata(META_CC, Dummy);    }

    static bool isConst(Instruction *I) {
        return I->getMetadata(META_CONST);
    }
    static bool isGuestMemory(Instruction *I) {
        return I->getMetadata(META_GVA);
    }
    static bool isLoop(Instruction *I) {
        return I->getMetadata(META_LOOP);
    }
    static bool isExit(Instruction *I) {
        return I->getMetadata(META_EXIT);
    }
    static bool isCondition(Instruction *I) {
        return I->getMetadata(META_CC);
    }

    static void setConstStatic(LLVMContext &Context, Instruction *I,
                               ArrayRef<ConstantInt*> V);
};

/*
 * IRFactory is the main component to perform QEMU TCG opcodes to LLVM IR
 * conversion.
 */
class IRFactory {
    typedef std::map<std::pair<intptr_t, Type *>, Value *> StatePtrMap;
    typedef std::map<TCGArg, BasicBlock *> LabelMap;

    enum {
        COHERENCE_NONE = 0,
        COHERENCE_GLOBAL,
        COHERENCE_ALL,
    };

    bool InitOnce;

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
    Type *FP80Ty;
    Type *FP128Ty;

    ConstantInt *ExitAddr;

    LLVMTranslator &Translator; /* Uplink to the LLVMTranslator instance */
    LLVMContext *Context;       /* Translator local context */
    Module *Mod;                /* The LLVM module */
    ExecutionEngine *EE;        /* The JIT compiler */
    EventListener *Listener;    /* The JIT listener */
    JITEventListener *IntelJIT; /* The Intel JIT listener */
    const DataLayout *DL;       /* Data layout */
    TraceBuilder *Builder;
    MDFactory *MF;
    MCDisasm *HostDisAsm;

    HelperMap &Helpers;
    std::vector<BaseRegister> &BaseReg;  /* TCG base register */
    std::vector<Register> Reg;           /* TCG virtual registers */
    LabelMap Labels;                /* TCG labels */
    int Segment;

    Function *Func;          /* The container of LLVM IR to be translated */
    BasicBlock *InitBB;      /* BasicBlock for variable decalaration */
    BasicBlock *CurrBB;      /* Current BasicBlock to insert LLVM IR */
    BasicBlock *ExitBB;      /* Temp BasicBlock as the exit-function stub */
    BranchInst *LastInst;    /* Position to insert LLVM IR */

    Instruction *CPU;           /* Base register with (char*) type */
    Instruction *CPUStruct;     /* Base register with (struct CPUArchState*) type */
    Instruction *GEPInsertPos;  /* Position to insert GEP instruction */

    StatePtrMap StatePtr;
    IVec InlineCalls;    /* Helpers to be inlined */
    std::map<std::string, BasicBlock*> CommonBB;
    IVec IndirectBrs;
    IVec toErase;
    BBVec toSink;
    std::set<Function *> ClonedFuncs;
    bool runPasses;

    void CreateJIT();
    void DeleteJIT();

    /* InitializeTypes - Initialize basic types used during IR conversion. */
    void InitializeTypes();

    /* SaveGlobals - Store dirty states back to CPU state in the memory. */
    void SaveGlobals(int level, Instruction *InsertPos);

    /* CreateStorePC - Sync PC to CPU state in the memory. */
    void CreateStorePC(Instruction *InsertPos);

    /* StatePointer - Get or insert the pointer to the CPU state. */
    Value *StatePointer(Register &reg);
    Value *StatePointer(Register &reg, intptr_t Off, Type *PTy);

    /* LoadState - Load value from the CPU state in the memory. */
    Value *LoadState(Register &reg);
    void StoreState(Register &reg, Instruction *InsertPos);

    /* QEMULoad/QEMUStore - Load/Store data from/to the guest memory. */
    Value *QEMULoad(Value *AddrL, Value *AddrH, TCGMemOpIdx oi);
    void QEMUStore(Value *Data, Value *AddrL, Value *AddrH, TCGMemOpIdx oi);

    Value *ConvertCPUType(Function *F, int Idx, Instruction *InsertPos);
    Value *ConvertCPUType(Function *F, int Idx, BasicBlock *InsertPos);

    Value *ConvertEndian(Value *V, int opc);
    Value *getExtendValue(Value *V, Type *Ty, int opc);
    Value *getTruncValue(Value *V, int opc);
    int getSizeInBits(int opc) {
        return 8 * (1 << (opc & MO_SIZE));
    }

    Value *ConcatTLBVersion(Value *GVA);

    /* getStorePC - Get the instruction of storing PC. For the guest's register
     * size larger than the host, replace the multiple store-PC instructions
     * to one single store-PC instruction. */
    StoreInst *getStorePC();

    /* InsertLinkAndExit - Create both chaining and exiting stubs. */
    void InsertLinkAndExit(Instruction *InsertPos);

    /* InsertExit - Create exit stub */
    void InsertExit(uintptr_t RetVal, bool setExit = false);

    /* findNextNode - Find the next node of a trace according to the brach pc.
     * Return null if we cannot find one. */
    GraphNode *findNextNode(target_ulong pc);

    /* TraceLink - Perform internal linking of basic blocks to form a region. */
    void TraceLink(StoreInst *SI);

    /* TraceLinkDirectJump - Link basic blocks of direct branch. */
    void TraceLinkDirectJump(GraphNode *NextNode, StoreInst *SI);
    void TraceLinkDirectJump(StoreInst *SI);

    /* TraceLinkIndirectJump - Link basic blocks of indirect branch. */
    void TraceLinkIndirectJump(GraphNode *NextNode, StoreInst *SI);

    /* InsertLookupIBTC - Insert code for IBTC hash table lookup. */
    void InsertLookupIBTC(GraphNode *CurrNode);

    /* InsertLookupCPBL - Insert code for CPBL hash table lookup. */
    void InsertLookupCPBL(GraphNode *CurrNode);

    void TraceValidateCPBL(GraphNode *NextNode, StoreInst *StorePC);

    /* CreateBSwap - Insert bswap intrinsic instruction. */
    Value *CreateBSwap(Type *Ty, Value *V, Instruction *InsertPos);

    /* getPointerTy - Given the size, return its PointerType. */
    PointerType *getPointerTy(int Size, unsigned AS = 0);

    /* AnalyzeInlineCost - Analyze a helper function to determine if it will be
     * inlined or not. */
    int AnalyzeInlineCost(CallSite CS);

    /* ProcessInline - Perform helper function inlining. */
    void ProcessInline();

    void VerifyFunction(Function &F);

    /* PreProcess - Legalize LLVM IR before running the pre-defined passes. */
    void PreProcess();

    void Optimize();

    /* PostProcess - Legalize LLVM IR after running the pre-defined passes. */
    void PostProcess();

    void FinalizeObject();

    void InitializeLLVMPasses(legacy::FunctionPassManager *FPM);

    uint32_t setRestorePoint(TCGMemOpIdx oi) {
        if (oi != (uint16_t)oi)
            hqemu_error("key value too large.\n");
        return (NI.setRestorePoint() << 16) | oi;
    }

public:
    typedef void (IRFactory::*FuncPtr)(const TCGArg *);

    NotifyInfo &NI;             /* Info to pass among translator and JIT */

    /* QEMU TCG IR to LLVM IR converion routines. */
#define DEF(name, oargs, iargs, cargs, flags) void op_ ## name(const TCGArg *);
#include "tcg-opc.h"
#undef DEF

    IRFactory(LLVMTranslator *Trans);
    ~IRFactory();

    void CreateSession(TraceBuilder *builder);
    void DeleteSession();

    /* CreateFunction - Prepare the LLVM Function and arrange the initial
     * BasicBlocks and variable declaration. */
    void CreateFunction();
    void CreateBlock();
    void Compile();

    /* setSuccessor - Set the jump from the instruction BI in the predecessor
     * block to the successor block BB. */
    void setSuccessor(BranchInst *BI, BasicBlock *BB);

    /* getOpcFunc - Get the function pointer of the IR converion routines. */
    void *getOpcFunc();

    Function *ResolveFunction(std::string Name);

    LLVMTranslator &getTranslator()   { return Translator;   }
    LLVMContext &getContext()         { return *Context;     }
    const DataLayout *getDL()         { return DL;           }
    MDFactory *getMDFactory()         { return MF;           }
    HelperMap &getHelpers()           { return Helpers;      }
    TraceInfo *getTrace()             { return Builder->getTrace(); }
    Instruction *getDefaultCPU(Function &F);

public:
    static bool isStateOfPC(intptr_t Off);
};

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
