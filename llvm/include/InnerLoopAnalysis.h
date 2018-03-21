/*
 *  (C) 2016 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __INNERLOOPANALYSIS_H
#define __INNERLOOPANALYSIS_H

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm-types.h"


class InductionDesc {
    /* Start value. */
    Value *StartValue;
    /* Step value. */
    const SCEV *Step;

public:
    InductionDesc() : StartValue(nullptr), Step(nullptr) {}
    InductionDesc(Value *Start, const SCEV *Step)
        : StartValue(Start), Step(Step) {}

    Value *getStartValue() const { return StartValue; }
    const SCEV *getStep() const { return Step; }
};

class ReductionDesc {
public:

    enum ReductionKind {
        NoReduction, /* Not a reduction. */
        IntegerAdd,  /* Sum of numbers. */
        IntegerMult, /* Product of numbers. */
        IntegerOr,   /* Bitwise or logical OR of numbers. */
        IntegerAnd,  /* Bitwise or logical AND of numbers. */
        IntegerXor,  /* Bitwise or logical XOR of numbers. */
        FloatAdd,    /* Sum of float numbers. */
        FloatMult,   /* Product of float numbers. */
    };

    ReductionDesc()
        : StartValue(nullptr), LoopExitInstr(nullptr),
          Kind(ReductionKind::NoReduction), Ty(nullptr) {}
    ReductionDesc(Value *Start, Instruction *Exit, ReductionKind K, Type *Ty)
        : StartValue(Start), LoopExitInstr(Exit), Kind(K), Ty(Ty) {}

    Value *getStartValue() const { return StartValue; }
    Value *getNextValue() const { return LoopExitInstr; }
    Instruction *getLoopExitInstr() { return LoopExitInstr; }
    ReductionKind getReductionKind() { return Kind; }
    Type *getScalarType() { return Ty; }

private:
    /* The starting value of the recurrence. */
    Value *StartValue;
    /* The instruction who's value is used outside the loop. */
    Instruction *LoopExitInstr;
    /* The kind of the recurrence.*/
    ReductionKind Kind;
    /* The scalar type. */
    Type *Ty;
};

/*
 * The InnertLoop class represents a single innertmost loop. The InnerLoop has
 * a special shape that is specific to the DBT decoded guest loop, and its loop
 * definition is different to a nature loop, e.g., latch and exiting block.
 */
class InnerLoop {
public:
    typedef std::map<PHINode *, InductionDesc> InductionList;
    typedef std::map<PHINode *, ReductionDesc> ReductionList;

private:
    Loop &TheLoop;

    /* The list of blocks in this loop. First entry is the header node. */
    std::vector<BasicBlock *> Blocks;
    SmallPtrSet<const BasicBlock *, 8> DenseBlockSet;

    std::vector<BasicBlock *> Latches;
    std::map<BasicBlock *, BasicBlock *> SplitLatches;

    bool UnknownPhi;
    InductionList Inductions;
    ReductionList Reductions;

    void addInduction(PHINode *Phi, Value *Start, const SCEV *Step) {
        Inductions[Phi] = InductionDesc(Start, Step);
    }

    void addReduction(PHINode *Phi, Value *Start, Instruction *Exit,
                      ReductionDesc::ReductionKind K, Type *Ty) {
        Reductions[Phi] = ReductionDesc(Start, Exit, K, Ty);
    }

    InnerLoop(const InnerLoop &) = delete;
    const InnerLoop& operator=(const InnerLoop &) = delete;

    friend class InnerLoopAnalysis;

public:
    InnerLoop(Loop *loop);
    ~InnerLoop() {}

    Loop &getLoop() const { return TheLoop; }

    BasicBlock *getHeader() const { return Blocks.front(); }

    /* Return true if the specified basic block is in this loop. */
    bool contains(const BasicBlock *BB) const {
        return DenseBlockSet.count(BB);
    }

    /* Return true if the specified instruction is in this loop. */
    bool contains(const Instruction *Inst) const {
        return contains(Inst->getParent());
    }

    /* Get a list of the basic blocks which make up this loop. */
    typedef typename std::vector<BasicBlock*>::const_iterator block_iterator;
    const std::vector<BasicBlock*> &getBlocks() const { return Blocks; }
    block_iterator block_begin() const { return Blocks.begin(); }
    block_iterator block_end() const { return Blocks.end(); }
    inline iterator_range<block_iterator> blocks() const {
        return make_range(block_begin(), block_end());
    }

    /* Get the number of blocks in this loop in constant time. */
    unsigned getNumBlocks() const { return Blocks.size(); }

    /* True if terminator in the block can branch to another block that is 
     * outside of the current loop. */
    bool isLoopExiting(BasicBlock *BB) const;

    /* Calculate the number of back edges to the loop header. */
    unsigned getNumBackEdges() const;

    /* Return all blocks inside the loop that have successors outside of the
     * loop. */
    void getExitingBlocks(SmallVectorImpl<BasicBlock *> &ExitingBlocks) const;

    /* If getExitingBlocks would return exactly one block, return that block.
     * Otherwise return null. */
    BasicBlock *getExitingBlock() const;

    /* Return all of the successor blocks of this loop. */
    void getExitBlocks(SmallVectorImpl<BasicBlock *> &ExitBlocks) const;

    /* If getExitBlocks would return exactly one block, return that block.
     * Otherwise return null. */
    BasicBlock *getExitBlock() const;

    /* If there is a preheader for this loop, return it. A loop has a preheader
     * if there is only one edge to the header of the loop from outside of the
     * loop. If this is the case, the block branching to the header of the loop
     * is the preheader node.
     *
     * This method returns null if there is no preheader for the loop. */
    BasicBlock *getLoopPreheader() const;

    /* If the given loop's header has exactly one unique predecessor outside the
     * loop, return it. Otherwise return null.
     * This is less strict that the loop "preheader" concept, which requires
     * the predecessor to have exactly one successor. */
    BasicBlock *getLoopPredecessor() const;

    unsigned getNumLoopLatches()  const { return Latches.size(); }
    unsigned getNumSplitLatches() const { return SplitLatches.size(); }

    /* Return all loop latch blocks of this loop. A latch block is a block that
     * contains a branch back to the header. */
    void getLoopLatches(SmallVectorImpl<BasicBlock *> &LoopLatches) const {
        for (auto I : Latches)
            LoopLatches.push_back(I);
    }

    /* If there is a latch tail, return it. */
    BasicBlock *getSingleLatchTail() const {
        return (SplitLatches.size() == 1) ? SplitLatches.begin()->first :
                                            nullptr;
    }

    /* If there is a latch head, return it. */
    BasicBlock *getSingleLatchHead() const {
        return (SplitLatches.size() == 1) ? SplitLatches.begin()->second :
                                            nullptr;
    }

    /* Return all of the latch tails of this loop. */
    void getLatchTails(SmallVectorImpl<BasicBlock *> &LatchTails) const {
        for (auto &I : SplitLatches)
            LatchTails.push_back(I.first);
    }

    /* Given a latch tail, return its latch head. */
    BasicBlock *getLatchHead(BasicBlock *BB) {
        if (SplitLatches.find(BB) == SplitLatches.end())
            return nullptr;
        return SplitLatches[BB];
    }

    /* If the given phi is an induction of the loop, return the induciton. */
    InductionDesc *getInduction(PHINode *Phi) {
        if (Inductions.find(Phi) == Inductions.end())
            return nullptr;
        return &Inductions[Phi];
    }

    /* If the given phi is a reduction of the loop, return the induciton. */
    ReductionDesc *getReduction(PHINode *Phi) {
        if (Reductions.find(Phi) == Reductions.end())
            return nullptr;
        return &Reductions[Phi];
    }

    /* Return true if the loop has unknown phi(s). A loop has unknown phi(s) if
     * a phi node is not identified, or the loop has no preheader or latch tail.
     * 
     * If the loop has unknown phi(s), the data structure of Inductions and
     * Reductions can be undefined. */
    bool hasUnknownPhi() { return UnknownPhi; }

    /* Return true if the instruction `From' can flow to instruction `To' in
     * the loop. */
    bool isReachable(Instruction *From, Instruction *To);
};

class InnerLoopAnalysis {
    std::vector<InnerLoop *> InnerLoops;

    void analyzePhi(InnerLoop &TheLoop, ScalarEvolution *SE);
    bool analyzeInduction(InnerLoop &TheLoop, ScalarEvolution *SE, PHINode *Phi);
    bool analyzeReduction(InnerLoop &TheLoop, PHINode *Phi);

public:
    InnerLoopAnalysis() {}
    ~InnerLoopAnalysis() { releaseMemory(); }

    void releaseMemory() {
        while (!InnerLoops.empty()) {
            InnerLoop *L = InnerLoops.back();
            InnerLoops.pop_back();
            delete L;
        }
    }
    void print(raw_ostream &OS, const Module * = nullptr) const {}
    void verify() const {}
    void analyze(LoopInfo *LI, ScalarEvolution *SE);

    /* iterator/begin/end - The interface to the innermost loops. */
    typedef typename std::vector<InnerLoop *>::const_iterator iterator;
    typedef typename std::vector<InnerLoop *>::const_reverse_iterator
        reverse_iterator;
    iterator begin() const { return InnerLoops.begin(); }
    iterator end() const { return InnerLoops.end(); }
    reverse_iterator rbegin() const { return InnerLoops.rbegin(); }
    reverse_iterator rend() const { return InnerLoops.rend(); }
    bool empty() const { return InnerLoops.empty(); }
    unsigned size() { return InnerLoops.size(); }
};

/*
 * InnerLoopAnalysisWrapperPass Pass
 */
class InnerLoopAnalysisWrapperPass : public FunctionPass {
    InnerLoopAnalysis LA;

public:
    static char ID;
    InnerLoopAnalysisWrapperPass() : FunctionPass(ID) {
        initializeInnerLoopAnalysisWrapperPassPass(*PassRegistry::getPassRegistry());
    }

    InnerLoopAnalysis &getLoopAnalysis() { return LA; }
    const InnerLoopAnalysis &getLoopAnalysis() const { return LA; }

    void releaseMemory() override;
    void getAnalysisUsage(AnalysisUsage &AU) const override;
    void print(raw_ostream &OS, const Module * = nullptr) const override;
    void verifyAnalysis() const override;
    bool runOnFunction(Function &F) override;
};

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
