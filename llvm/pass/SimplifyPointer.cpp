//===- SimplifyPointer.cpp - Reassociate guest pointer arithmetic ---------===//
//
//           The HQEMU Dynamic Binary Translator Infrastructure
//
// (C) 2016 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
//     COVART Laboratory, CSIE Department, National Taiwan University, Taiwan.
//     See COPYRIGHT in top-level directory.
//
//===----------------------------------------------------------------------===//
// This pass implements a simple pointer arithmetic reassociator for easier
// pointer stripping. It gets scalar evolution results of all guest pointers
// which are in simplest form. Next, it inserts new instructions to evaluate the
// simplified expressions to construct new pointers, and rewrites corresponding
// guest load/store with new pointers.
//
//===----------------------------------------------------------------------===//
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpander.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/InstIterator.h"

#include "llvm-opc.h"
#include "llvm-pass.h"
#include "llvm-target.h"
#include "utils.h"

#define PASS_NAME "SIMPTR"
#define DEBUG_TYPE "SIMPTR"

//#define VERBOSE

/// \brief Dump pass debug message with pass name mark.
static inline llvm::raw_ostream &pout() {
  return dbg() << DEBUG_PASS << PASS_NAME ": ";
}

/// \returns True if \p A dominates \p B.
static bool dominates(Value *A, Value *B, DominatorTree *DT) {
  auto *AI = dyn_cast<Instruction>(A);
  auto *BI = dyn_cast<Instruction>(B);
  if (AI && BI)
    return DT->dominates(AI, BI);
  return false;
}

class SimplifyPointer : public FunctionPass {
public:
  using ValueList  = SmallVector<Value *, 32>;
  using InstrList  = SmallVector<Instruction *, 32>;
  using ExprValMap = DenseMap<const SCEV *, Value *>;

  // Pass identification, replacement for type id.
  static char ID;

  // LLVM pass constructor and destructor.
  explicit SimplifyPointer() : FunctionPass(ID){};
  explicit SimplifyPointer(IRFactory *IF)
      : FunctionPass(ID), IF(IF), MF(IF->getMDFactory()), DL(IF->getDL()) {
    // Initialize all.
    initializeSimplifyPointerPass(*PassRegistry::getPassRegistry());
  }

  // LLVM pass public interfaces.
  void getAnalysisUsage(AnalysisUsage &AU) const override;
  bool runOnFunction(Function &F) override;

private:
  /// \return The evaluation result of expression \p S or null if not cached.
  Value *lookupBaseExpressionCache(const SCEV *S) const {
    auto V = BaseExprVal.find(S);
    if (V != BaseExprVal.end())
      return V->second;
    return nullptr;
  }

  /// \returns True if spread constants in the expression tree of \p S can be
  /// collected by reassociation and reduced to \p FoldVal.
  ///
  /// It traverses the expression tree of \p S and propagates constant nodes
  /// from add, multiply and recurrent add nodes, i.e., (%1 + %2 + 5) * (%3 - 7)
  /// should return 5 * -7 = -35.
  bool foldConstantExpression(const SCEV *S, int64_t &FoldVal) const;

  /// \returns The first non-pointer value traced along the use-define chain of
  /// casting which starts from \p V and ends with a IntToPtrInst, or null if
  /// the length of searching chain exceeds \p MaxLookup.
  ///
  /// In the context of DBT, pointer type is represented and manipulated as
  /// integer data until used as a pointer. Therefore, it follows:
  ///
  /// [Expression Tree]
  ///    +   + +   +
  ///     \ /   \ /
  ///      - ... -
  ///       \   /
  ///         +
  ///   [Scalar Root]
  ///         |
  ///     [Casting]
  ///         |
  ///    [Load/Store]
  ///
  /// This method targets the scalar root value.
  Value *findPointerScalarRoot(Value *V, unsigned MaxLookup = 4);

  /// \brief Simplify the pointer arithmetic of \p LSI based on scalar evolution
  /// results which folds constants into simplest form. After extracting the
  /// folded constant from the expression, the rest nodes can form a base
  /// expression which is likely a common sub-expression of other \p LSI.
  ///
  /// It assumes \p LSI has the following use-define chain starting from its
  /// pointer and containing only add, multiply and recurrent add nodes.
  ///
  /// [Expression Tree]      [Expression Tree]      [Expression Tree]
  ///    +   A B   +            +   + B   A            +   +
  ///     \ /   \ /              \ /   \ /              \ /
  ///      - ... -                - ... -                -   (B-A)
  ///       \   /                  \   /                  \   /
  ///         +                      +                      +
  ///   [Scalar Root]    >>    [Scalar Root]    >>    [Scalar Root]
  ///         |                      |                      |
  ///     [Casting]              [Casting]              [Casting]
  ///         |                      |                      |
  ///       [LSI]                  [LSI]                  [LSI]
  ///
  /// Suppose A and B are constants, they can be folded into (B-A) with scalar
  /// evolution results. Need to insert instructions for other operations in
  /// tree (e.g., the new sub in the right-most figure).
  ///
  /// First it tries to find the folded constant and substract it from root
  /// expression to form the base expression. Then it generates instructions to
  /// evaluate the base expression.
  bool tryToSimplifyPointer(Instruction *I);

  // HQEMU internal infrastructure.
  IRFactory *IF = nullptr;
  MDFactory *MF = nullptr;
  // LLVM analysis and data type info.
  const DataLayout *DL = nullptr;
  DominatorTree *DT    = nullptr;
  ScalarEvolution *SE  = nullptr;

  /// The cache of base expression to corresponding evaluated value map.
  ExprValMap BaseExprVal;
};

bool SimplifyPointer::foldConstantExpression(const SCEV *S,
                                             int64_t &FoldVal) const {
  // Handle expression tree of scalar root containing only add, multiply and
  // recurrent add nodes.
  if (auto *AddSE = dyn_cast<SCEVAddExpr>(S)) {
    FoldVal = 0;
    for (auto Op : AddSE->operands()) {
      int64_t Val;
      if (foldConstantExpression(Op, Val))
        FoldVal += Val;
    }
    return true;
  } else if (auto *MulSE = dyn_cast<SCEVMulExpr>(S)) {
    FoldVal = 1;
    for (auto Op : MulSE->operands()) {
      int64_t Val;
      // If one operand of multiplication fails to report a constant, entire
      // expression becomes non-constant as well.
      if (foldConstantExpression(Op, Val))
        FoldVal *= Val;
      else
        return false;
    }
    return true;
  } else if (auto *RecSE = dyn_cast<SCEVAddRecExpr>(S)) {
    // Trace only the start expression, because the step expression must be
    // multiplied by the loop trip count which is unlikely constant.
    return foldConstantExpression(RecSE->getStart(), FoldVal);
  } else if (auto *ConstSE = dyn_cast<SCEVConstant>(S)) {
    FoldVal = ConstSE->getValue()->getValue().getSExtValue();
    return true;
  }
  return false;
}

Value *SimplifyPointer::findPointerScalarRoot(Value *V, unsigned MaxLookup) {
  if (!V || !V->getType()->isPointerTy())
    return V;

  for (unsigned i = 0; i < MaxLookup; ++i) {
    if (BitCastInst *Cast = dyn_cast<BitCastInst>(V)) {
      V = Cast->getOperand(0);
    } else if (IntToPtrInst *Cast = dyn_cast<IntToPtrInst>(V)) {
      // Found first scalar, return it.
      V = Cast->getOperand(0);
      return V;
    }
  }
  return nullptr;
}

bool SimplifyPointer::tryToSimplifyPointer(Instruction *LSI) {
  Value *Ptr   = getPointerOperand(LSI);
  Value *Root  = findPointerScalarRoot(Ptr);
  Type *RootTy = Root->getType();
  Type *PtrTy  = Ptr->getType();
  if (!Ptr || !Root || !RootTy->isIntegerTy())
    return false;

#ifdef VERBOSE
  if (DM.getDebugMode() & DEBUG_PASS) {
    pout() << "Visiting memory instruction.\n";
    pout() << "- " << *LSI << ".\n";
  }
#endif

  // Traverse the simplest form expression tree and collect folded constants.
  // Note the folded constant can be zero (base = root) if no folded constant
  // is found.
  auto *RootSE         = SE->getSCEV(Root);
  int64_t FoldConst = 0;
  foldConstantExpression(RootSE, FoldConst);

  // Substract offset constant from root expression to get the base expression,
  // then query base expression cache to find whether it has been evaluated.
  auto *BaseSE = SE->getMinusSCEV(RootSE,
                                  SE->getConstant(RootTy, FoldConst, true));
  Value *Base  = lookupBaseExpressionCache(BaseSE);

  // Create instructions to evaluate base expression if cache miss or previously
  // computed value doesn't dominate load/store instruction.
  if (!Base || !dominates(Base, LSI, DT)) {
#ifdef VERBOSE
    pout() << "  Need to build base expression.\n";
    pout() << "  - Base   " << *BaseSE << ".\n";
    pout() << "  - Offset " << FoldConst << ".\n";
#endif
    // Expand the base expression if it is safe.
    if (isSafeToExpand(BaseSE, *SE)) {
#if defined(LLVM_V35)
      SCEVExpander Expander(*SE, "");
#else
      SCEVExpander Expander(*SE, *DL, "");
#endif
      Base = Expander.expandCodeFor(BaseSE, RootTy, LSI);
    }
  } else {
#ifdef VERBOSE
    pout() << "  Use cached base expression value.\n";
    pout() << "  - Base   " << *BaseSE << ".\n";
    pout() << "  - Offset " << FoldConst << ".\n";
#endif
  }

  // Neither using cached value nor re-computing works, abort.
  if (!Base)
    return false;

  // Add back folded constant (offset) to new root value and feed the result as
  // new pointer to load/store instruction.
  IRBuilder<> Builder(IF->getContext());

  bool FoldZero = (FoldConst == 0);
  Value *Offset = ConstantInt::get(RootTy, FoldConst);

  Builder.SetInsertPoint(LSI);
  Value *NewRoot = FoldZero ? Base : Builder.CreateAdd(Base, Offset);
  Value *NewPtr  = Builder.CreateIntToPtr(NewRoot, PtrTy);
  LSI->replaceUsesOfWith(Ptr, NewPtr);

  // Cache base expression value.
  BaseExprVal[BaseSE] = Base;

  return true;
}

void SimplifyPointer::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<DominatorTreeWrapperPass>();
#if defined(LLVM_V35)
  AU.addRequired<ScalarEvolution>();
#else
  AU.addRequired<ScalarEvolutionWrapperPass>();
#endif
}

bool SimplifyPointer::runOnFunction(Function &F) {
  DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
#if defined(LLVM_V35)
  SE = &getAnalysis<ScalarEvolution>();
#else
  SE = &getAnalysis<ScalarEvolutionWrapperPass>().getSE();
#endif

  bool Changed = false;

  InstrList MemoryInstrs;
  for (auto FI = F.begin(), FE = F.end(); FI != FE; ++FI) {
    BasicBlock *BB = &*FI;

    // Skip dead basic blocks.
    if (!DT->isReachableFromEntry(BB))
      continue;

    // Collect all guest memory instructions.
    for (auto BI = BB->begin(), BE = BB->end(); BI != BE; ++BI) {
      Instruction *I = &*BI;
      if (MDFactory::isGuestMemory(I))
        MemoryInstrs.push_back(I);
    }
  }

  // Try to simplify pointers of collected load/store instructions.
  for (Instruction *I : MemoryInstrs)
    Changed |= tryToSimplifyPointer(I);

  return Changed;
}

char SimplifyPointer::ID = 0;
INITIALIZE_PASS_BEGIN(SimplifyPointer, "simplifypointer",
                      "Reassiciate pointer arithmetic", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
#if defined(LLVM_V35)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolution)
#else
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
#endif
INITIALIZE_PASS_END(SimplifyPointer, "simplifypointer",
                    "Reassiciate pointer arithmetic", false, false)

FunctionPass *llvm::createSimplifyPointer(IRFactory *IF) {
  return new SimplifyPointer(IF);
}

/*
 * vim: ts=2 sts=2 sw=2 expandtab
 */
