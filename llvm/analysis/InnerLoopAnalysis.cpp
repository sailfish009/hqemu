/*
 *  (C) 2016 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "llvm/Analysis/ScalarEvolutionExpander.h"
#include "llvm-debug.h"
#include "llvm-opc.h"
#include "llvm-pass.h"
#include "InnerLoopAnalysis.h"


/*
 * The InnertLoop class represents a single innermost loop. The shape of
 * InnerLoop is specific to the DBT decoded guest loop, and its loop definition
 * is different to a nature loop, e.g., latch and exiting block.
 * For example, the binary of a nature loop (a) will be translated to the loop
 * CFG (b), which includes an additional block L(loopback) to check flag
 * `tcg_exit_req' and exits the loop to block E if the flag is raised, otherwise,
 * goes back to the loop header A.
 *
 * In loop (b), a latch is split into two blocks, B and L. The loop bottom test
 * is in block B and the backward branch is included in block L (which also
 * has an exit block E attached to it). We include block L in the loop body and
 * have the following definitions: (1) block B and L are latch head and tail,
 * respectively; (2) a latch tail is the source of a backedge; (3) block B is a
 * loop exiting block, but block L is not, and block E is not included in the
 * exit blocks.
 *
 *  (a) A    (b) A
 *      ||       |
 *      B        B
 *     /        / \
 *    C        C   L -> A
 *                  \
 *                   E
 */
InnerLoop::InnerLoop(Loop *loop)
    : TheLoop(*loop), Blocks(TheLoop.getBlocks()), UnknownPhi(false)
{
    for (auto BB : Blocks)
        DenseBlockSet.insert(BB);

    /* Find all latches and split latches. */
    SmallVector<BasicBlock *, 8> LoopLatches;
    TheLoop.getLoopLatches(LoopLatches);
    for (BasicBlock *BB : LoopLatches) {
        Latches.push_back(BB);

        if (MDFactory::isLoop(BB->getTerminator()) &&
            BB->getSinglePredecessor()) {
            /* Map latch tail to latch head. */
            SplitLatches[BB] = BB->getSinglePredecessor();
        }
    }
}


/* True if terminator in the block can branch to another block that is 
 * outside of the current loop. */
bool InnerLoop::isLoopExiting(BasicBlock *BB) const
{
    if (SplitLatches.find(BB) != SplitLatches.end())
        return false;

    typedef GraphTraits<const BasicBlock*> BlockTraits;
    for (typename BlockTraits::ChildIteratorType SI =
         BlockTraits::child_begin(BB),
         SE = BlockTraits::child_end(BB); SI != SE; ++SI) {
        if (!contains(*SI))
            return true;
    }
    return false;
}

/* Calculate the number of back edges to the loop header. */
unsigned InnerLoop::getNumBackEdges() const
{
    unsigned NumBackEdges = 0;
    BasicBlock *H = getHeader();

    typedef GraphTraits<Inverse<BasicBlock*> > InvBlockTraits;
    for (typename InvBlockTraits::ChildIteratorType I =
         InvBlockTraits::child_begin(H),
         E = InvBlockTraits::child_end(H); I != E; ++I)
        if (contains(*I))
            ++NumBackEdges;

    return NumBackEdges;
}

/* Return all blocks inside the loop that have successors outside of the loop. */
void InnerLoop::getExitingBlocks(SmallVectorImpl<BasicBlock *> &ExitingBlocks) const
{
    typedef GraphTraits<BasicBlock *> BlockTraits;
    for (block_iterator BI = block_begin(), BE = block_end(); BI != BE; ++BI) {
        /* Skip the latch tail block. */
        if (SplitLatches.find(*BI) != SplitLatches.end())
            continue;

        for (typename BlockTraits::ChildIteratorType I =
             BlockTraits::child_begin(*BI), E = BlockTraits::child_end(*BI);
             I != E; ++I)
            if (!contains(*I)) {
                /* Not in current loop? It must be an exit block. */
                ExitingBlocks.push_back(*BI);
                break;
            }
    }
}

/* If getExitingBlocks would return exactly one block, return that block.
 * Otherwise return null. */
BasicBlock *InnerLoop::getExitingBlock() const
{
    SmallVector<BasicBlock *, 8> ExitingBlocks;
    getExitingBlocks(ExitingBlocks);
    if (ExitingBlocks.size() == 1)
        return ExitingBlocks[0];
    return nullptr;
}

/* Return all of the successor blocks of this loop. */
void InnerLoop::getExitBlocks(SmallVectorImpl<BasicBlock *> &ExitBlocks) const
{
    typedef GraphTraits<BasicBlock *> BlockTraits;
    for (block_iterator BI = block_begin(), BE = block_end(); BI != BE; ++BI) {
        /* Skip the latch tail block. */
        if (SplitLatches.find(*BI) != SplitLatches.end())
            continue;

        for (typename BlockTraits::ChildIteratorType I =
             BlockTraits::child_begin(*BI), E = BlockTraits::child_end(*BI);
             I != E; ++I)
            if (!contains(*I))
                /* Not in current loop? It must be an exit block. */
                ExitBlocks.push_back(*I);
    }
}

/* If getExitBlocks would return exactly one block, return that block.
 * Otherwise return null. */
BasicBlock *InnerLoop::getExitBlock() const
{
    SmallVector<BasicBlock *, 8> ExitBlocks;
    getExitBlocks(ExitBlocks);
    if (ExitBlocks.size() == 1)
        return ExitBlocks[0];
    return nullptr;
}

/* If there is a preheader for this loop, return it. A loop has a preheader
 * if there is only one edge to the header of the loop from outside of the
 * loop. If this is the case, the block branching to the header of the loop
 * is the preheader node.
 *
 * This method returns null if there is no preheader for the loop. */
BasicBlock *InnerLoop::getLoopPreheader() const
{
    /* Keep track of nodes outside the loop branching to the header. */
    BasicBlock *Out = getLoopPredecessor();
    if (!Out) return nullptr;

    /* Make sure there is only one exit out of the preheader. */
    typedef GraphTraits<BasicBlock *> BlockTraits;
    typename BlockTraits::ChildIteratorType SI = BlockTraits::child_begin(Out);
    ++SI;
    if (SI != BlockTraits::child_end(Out))
        return nullptr;  /* Multiple exits from the block, must not be a preheader. */

    /* The predecessor has exactly one successor, so it is a preheader. */
    return Out;
}

/* If the given loop's header has exactly one unique predecessor outside the
 * loop, return it. Otherwise return null.
 * This is less strict that the loop "preheader" concept, which requires
 * the predecessor to have exactly one successor. */
BasicBlock *InnerLoop::getLoopPredecessor() const
{
    /* Keep track of nodes outside the loop branching to the header. */
    BasicBlock *Out = nullptr;

    /* Loop over the predecessors of the header node. */
    BasicBlock *Header = getHeader();
    typedef GraphTraits<Inverse<BasicBlock *> > InvBlockTraits;
    for (typename InvBlockTraits::ChildIteratorType PI =
         InvBlockTraits::child_begin(Header),
         PE = InvBlockTraits::child_end(Header); PI != PE; ++PI) {
        typename InvBlockTraits::NodeType *N = *PI;
        if (!contains(N)) {      /* If the block is not in the loop. */
            if (Out && Out != N)
                return nullptr;  /* Multiple predecessors outside the loop */
            Out = N;
        }
    }

    return Out;
}

bool InnerLoop::isReachable(Instruction *From, Instruction *To)
{
    if (!contains(From->getParent()) || !contains(To->getParent()))
        return false;
    if (From == To)
        return true;

    SmallPtrSet<Instruction*, 8> Visited;
    SmallVector<Instruction*, 8> VisitStack;

    VisitStack.push_back(From);
    while (!VisitStack.empty()) {
        Instruction *I = VisitStack.back();
        VisitStack.pop_back();

        if (Visited.count(I))
            continue;

        Visited.insert(I);
        for (User *U : I->users()) {
            Instruction *UI = cast<Instruction>(U);
            if (UI == To)
                return true;

            if (contains(UI->getParent()))
                VisitStack.push_back(UI);
        }
    }

    return false;
}


/*
 * InnerLoopAnalysis
 */
static void addInnerLoop(Loop &L, std::vector<Loop *> &Loops)
{
    if (L.empty()) {
        /* Innermost loop.
         * If any basic block of current loop has been included in another
         * loop, skip this loop. */
        for (Loop *InnerL : Loops) {
            for (auto I = L.begin(), E = L.end(); I != E; ++I) {
                if (InnerL->contains(*I))
                    return;
            }
        }
        Loops.push_back(&L);
        return;
    }
    for (Loop *InnerL : L)
        addInnerLoop(*InnerL, Loops);
}


void InnerLoopAnalysis::analyze(LoopInfo *LI, ScalarEvolution *SE)
{
    std::vector<Loop *> Loops;
    for (Loop *L : *LI)
        addInnerLoop(*L, Loops);

    for (auto L : Loops)
        InnerLoops.push_back(new InnerLoop(L));

    for (auto L : InnerLoops)
        analyzePhi(*L, SE);
}

bool InnerLoopAnalysis::analyzeInduction(InnerLoop &TheLoop,
                                         ScalarEvolution *SE,
                                         PHINode *Phi)
{
    Type *PhiTy = Phi->getType();
    /* We only handle integer and pointer inductions variables. */
    if (!PhiTy->isIntegerTy() && !PhiTy->isPointerTy())
        return false;

    /* We only handle induction that has no outside users (except that the
     * outside users are all stores.) */
    for (User *U : Phi->users()) {
        Instruction *UI = cast<Instruction>(U);
        if (!TheLoop.contains(UI) && !isa<StoreInst>(UI))
            return false;
    }

    const SCEV *PhiScev = SE->getSCEV(Phi);
    const auto *AR = dyn_cast<SCEVAddRecExpr>(PhiScev);
    if (!AR)
        return false;

    const SCEV *Step = AR->getStepRecurrence(*SE);
    const SCEVConstant *ConstStep = dyn_cast<SCEVConstant>(Step);
    if (!ConstStep && !SE->isLoopInvariant(Step, AR->getLoop()))
        return false;

    /* We found an induction variable. */
    Value *StartValue =
        Phi->getIncomingValueForBlock(AR->getLoop()->getLoopPreheader());
    TheLoop.addInduction(Phi, StartValue, Step);

    return true;
}

/*
 * isReductionInstr()
 *  Check if the reduction operation is supported.
 *  We don't allow a reduction to bind more than one operation, so drop a
 *  reduction if it already has one operation.
 */
static bool isReductionInstr(Instruction *I, ReductionDesc::ReductionKind &Kind,
                             Type *&Ty)
{
    ReductionDesc::ReductionKind K = ReductionDesc::NoReduction;
    switch (I->getOpcode()) {
    default:
        return false;
    case Instruction::PHI:
    case Instruction::BitCast:
        return true;
    case Instruction::Add:
    case Instruction::Sub:
        K = ReductionDesc::IntegerAdd;
        break;
    case Instruction::Mul:
        K = ReductionDesc::IntegerMult;
        break;
    case Instruction::And:
        K = ReductionDesc::IntegerAnd;
        break;
    case Instruction::Or:
        K = ReductionDesc::IntegerOr;
        break;
    case Instruction::Xor:
        K = ReductionDesc::IntegerXor;
        break;
    case Instruction::FAdd:
    case Instruction::FSub:
        K = ReductionDesc::FloatAdd;
        break;
    case Instruction::FMul:
        K = ReductionDesc::FloatMult;
        break;
    }

    if (VectorType *VecTy = dyn_cast<VectorType>(I->getType()))
        Ty = VecTy->getScalarType();
    else
        Ty = I->getType();

    if (Kind == ReductionDesc::NoReduction) {
        Kind = K;
        return true;
    }

    if (Kind != K) {
        /* Different reduction operation to the previous one. */
        return false;
    }
    return true;
}

static bool hasMultipleUsesOf(Instruction *I,
                              SmallPtrSet<Instruction *, 8> &Insts)
{
    unsigned NumUses = 0;
    for(User::op_iterator Use = I->op_begin(), E = I->op_end(); Use != E; ++Use) {
        if (Insts.count(dyn_cast<Instruction>(*Use)))
            ++NumUses;
        if (NumUses > 1)
            return true;
    }
    return false;
}

static bool isLegalUser(Instruction *I)
{
    if (isa<StoreInst>(I) && !MDFactory::isGuestMemory(I))
        return true;
    return false;
}

bool InnerLoopAnalysis::analyzeReduction(InnerLoop &TheLoop, PHINode *Phi)
{
    if (Phi->getNumIncomingValues() != 2)
        return false;

    /* Reduction variables are only found in the loop header block. */
    if (Phi->getParent() != TheLoop.getHeader())
        return false;

    /* Obtain the reduction start value from from the loop preheader. */
    Value *StartValue = Phi->getIncomingValueForBlock(TheLoop.getLoopPreheader());

    /* ExitInstruction is the single value which is used outside the loop.
     * We only allow for a single reduction value to be used outside the loop.
     * This includes users of the reduction, variables (which form a cycle
     * which ends in the phi node). */
    Instruction *ExitInstruction = nullptr;
    /* Indicates that we found a reduction operation in our scan. */
    bool FoundReduxOp = false;

    /* We start with the PHI node and scan for all of the users of this
     * instruction. All users must be instructions that can be used as reduction
     * variables (such as ADD). We must have a single out-of-block user. The cycle
     * must include the original PHI. */
    bool FoundStartPHI = false;

    ReductionDesc::ReductionKind Kind = ReductionDesc::NoReduction;
    Type *Ty = nullptr;

    SmallPtrSet<Instruction *, 8> VisitedInsts;
    SmallVector<Instruction *, 8> Worklist;
    Worklist.push_back(Phi);
    VisitedInsts.insert(Phi);

    /* A value in the reduction can be used:
     *   - By the reduction:
     *     - Reduction operation:
     *       - One use of reduction value (safe).
     *       - Multiple use of reduction value (not safe).
     *     - PHI:
     *       - All uses of the PHI must be the reduction (safe).
     *       - Otherwise, not safe.
     *   - By one or no instruction outside of the loop (safe).
     *   - By further instructions outside of the loop (not safe).
     *   - By an instruction that is not part of the reduction (not safe).
     *     This is either:
     *       An instruction type other than PHI or the reduction operation.
     *       A PHI in the header other than the initial PHI. */
    while (!Worklist.empty()) {
        Instruction *Cur = Worklist.back();
        Worklist.pop_back();

        /* No Users.
         * If the instruction has no users then this is a broken chain and
         * cannot be a reduction variable. */
        if (Cur->use_empty())
            return false;

        bool IsAPhi = isa<PHINode>(Cur);
        bool IsBitCast = isa<BitCastInst>(Cur);

        /* Currenly, we don't handle a reduction used by another PHI other than
         * the original PHI. */
        if (IsAPhi && Cur != Phi)
            return false;

        /* Any reduction instruction must be of one of the allowed kinds. */
        if (!isReductionInstr(Cur, Kind, Ty))
            return false;

        /* Reductions of instructions such as Div, and Sub is only possible if the
         * LHS is the reduction variable. */
        if (!IsAPhi && !Cur->isCommutative() &&
            !VisitedInsts.count(dyn_cast<Instruction>(Cur->getOperand(0))))
            return false;

        /* A reduction operation must only have one use of the reduction value. */
        if (!IsAPhi && hasMultipleUsesOf(Cur, VisitedInsts))
            return false;

        /* Check whether we found a reduction operator. */
        FoundReduxOp |= (!IsAPhi && !IsBitCast);

        /* Process users of current instruction. Push non-PHI nodes after PHI
         * nodes onto the stack. This way we are going to have seen all inputs
         * to PHI nodes once we get to them. */
        SmallVector<Instruction *, 8> NonPHIs;
        SmallVector<Instruction *, 8> PHIs;
        for (User *U : Cur->users()) {
            Instruction *UI = cast<Instruction>(U);

            if (isLegalUser(UI))
                continue;

            /* Check if we found the exit user. */
            BasicBlock *Parent = UI->getParent();
            if (!TheLoop.contains(Parent)) {
                /* Exit if you find multiple outside users or if the header phi node is
                 * being used. In this case the user uses the value of the previous
                 * iteration, in which case we would loose "VF-1" iterations of the
                 * reduction operation if we vectorize. */
                if (ExitInstruction != nullptr || Cur == Phi)
                    return false;

                /* The instruction used by an outside user must be the last instruction
                 * before we feed back to the reduction phi. Otherwise, we loose VF-1
                 * operations on the value. */
                if (std::find(Phi->op_begin(), Phi->op_end(), Cur) == Phi->op_end())
                    return false;

                ExitInstruction = Cur;
                continue;
            }

            /* Process instructions only once (termination). Each reduction cycle
             * value must only be used once, except by phi nodes and min/max
             * reductions which are represented as a cmp followed by a select. */
            if (!VisitedInsts.count(UI)) {
                VisitedInsts.insert(UI);
                if (isa<PHINode>(UI))
                    PHIs.push_back(UI);
                else
                    NonPHIs.push_back(UI);
            } else if (!isa<PHINode>(UI))
                return false;

            /* Remember that we completed the cycle. */
            if (UI == Phi)
                FoundStartPHI = true;
        }
        Worklist.append(PHIs.begin(), PHIs.end());
        Worklist.append(NonPHIs.begin(), NonPHIs.end());
    }

    /* Set the exit instruction to the last instruction feed back to the
     * reduction phi if we cannot find an exit instruction. */
    if (!ExitInstruction) {
        Value *NextValue = Phi->getIncomingValueForBlock(TheLoop.getSingleLatchTail());
        if (!isa<Instruction>(NextValue))
            return false;
        ExitInstruction = cast<Instruction>(NextValue);
    }

    if (!FoundStartPHI || !FoundReduxOp)
        return false;

    /* We found an induction variable. */
    TheLoop.addReduction(Phi, StartValue, ExitInstruction, Kind, Ty);

    return true;
}

void InnerLoopAnalysis::analyzePhi(InnerLoop &TheLoop, ScalarEvolution *SE)
{
    BasicBlock *Header = TheLoop.getHeader();
    for (BasicBlock *BB : TheLoop.blocks()) {
        auto I = BB->begin();
        auto E = BasicBlock::iterator(BB->getFirstNonPHI());

        for (; I != E; ++I) {
            /* Currently, we cannot handle PHIs in a non-header block, so set
             * the loop with unknown PHI if we find any of it. */
            if (BB != Header) {
                TheLoop.UnknownPhi = true;
                return;
            }

            /* The loop must have a preheader and one split latch for us to
             * analyze inductions and reductions. */
            if (!TheLoop.getLoopPreheader() || !TheLoop.getSingleLatchTail()) {
                TheLoop.UnknownPhi = true;
                return;
            }

            PHINode *Phi = cast<PHINode>(I);
            if (!analyzeInduction(TheLoop, SE, Phi) &&
                !analyzeReduction(TheLoop, Phi))
                TheLoop.UnknownPhi = true;
        }
    }
}


/*
 * InnerLoopAnalysisWrapperPass Pass
 */
char InnerLoopAnalysisWrapperPass::ID = 0;
INITIALIZE_PASS_BEGIN(InnerLoopAnalysisWrapperPass, "InnerLoopAnalysis",
        "Inner Loop Analysis", true, true)
#if defined(LLVM_V35)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolution)
#else
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
#endif
INITIALIZE_PASS_END(InnerLoopAnalysisWrapperPass, "InnerLoopAnalysis",
        "Inner Loop Analysis", true, true)

void InnerLoopAnalysisWrapperPass::releaseMemory() {
    LA.releaseMemory();
}

void InnerLoopAnalysisWrapperPass::getAnalysisUsage(AnalysisUsage &AU) const {
    AU.setPreservesAll();
#if defined(LLVM_V35)
    AU.addRequired<LoopInfo>();
    AU.addRequired<ScalarEvolution>();
#else
    AU.addRequired<LoopInfoWrapperPass>();
    AU.addRequired<ScalarEvolutionWrapperPass>();
#endif
}
void InnerLoopAnalysisWrapperPass::print(raw_ostream &OS, const Module *) const {
    LA.print(OS);
}

void InnerLoopAnalysisWrapperPass::verifyAnalysis() const {
    LA.verify();
}

bool InnerLoopAnalysisWrapperPass::runOnFunction(Function &F) {
#if defined(LLVM_V35)
    ScalarEvolution *SE = &getAnalysis<ScalarEvolution>();
    LoopInfo *LI = &getAnalysis<LoopInfo>();
#else
    ScalarEvolution *SE = &getAnalysis<ScalarEvolutionWrapperPass>().getSE();
    LoopInfo *LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
#endif

    LA.analyze(LI, SE);
    return false;
}


/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

