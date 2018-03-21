/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __LLVM_PASS_H
#define __LLVM_PASS_H

#include <map>
#include <vector>
#include "llvm-types.h"

class IRFactory;


static inline Value *getPointerOperand(Value *I) {
    if (LoadInst *LI = dyn_cast<LoadInst>(I))
        return LI->getPointerOperand();
    if (StoreInst *SI = dyn_cast<StoreInst>(I))
        return SI->getPointerOperand();
    return nullptr;
}

static inline Value *getValueOperand(Value *I) {
    if (LoadInst *LI = dyn_cast<LoadInst>(I))
        return LI;
    if (StoreInst *SI = dyn_cast<StoreInst>(I))
        return SI->getValueOperand();
    return nullptr;
}

static inline unsigned getAddressSpaceOperand(Value *I) {
    if (LoadInst *LI = dyn_cast<LoadInst>(I))
        return LI->getPointerAddressSpace();
    if (StoreInst *SI = dyn_cast<StoreInst>(I))
        return SI->getPointerAddressSpace();
    return -1;
}

/* A CPU state reference. */
struct StateRef {
    StateRef(intptr_t Start, intptr_t End, Instruction *I)
        : Start(Start), End(End), I(I) {}
    intptr_t Start;
    intptr_t End;
    Instruction *I;

    intptr_t getSize() {
        return End - Start;
    }
    Type *getType() {
        return getValueOperand(I)->getType();
    }
};

/* A group of references to a CPU state. */
struct StateData {
    intptr_t Start;
    intptr_t End;
    std::vector<StateRef*> Refs;

    void reset(StateRef &Ref) {
        Start = Ref.Start;
        End = Ref.End;
        Refs.clear();
        Refs.push_back(&Ref);
    }
    void insert(StateRef &Ref) {
        End = std::max(End, Ref.End);
        Refs.push_back(&Ref);
    }
};

typedef std::map<intptr_t, intptr_t> StateRange;
typedef std::vector<StateData> StateList;
typedef std::vector<CallInst*> CallList;

/*
 * The purpose of StateAnalyzer is to analyze loads/stores of CPU states and
 * group loads/stores of the same CPU state into the same bucket (StateData).
 */
class StateAnalyzer {
    const DataLayout *DL;
    std::vector<StateRef> StateRefs;
    CallList Calls;
    StateList States;

    /* Sort state references by the state offset. */
    void sortStateRefs() {
        if (StateRefs.empty())
            return;
        std::sort(StateRefs.begin(), StateRefs.end(),
                  [](const StateRef &lhs, const StateRef &rhs) -> bool {
                     return lhs.Start < rhs.Start;
                  });
    }

public:
    StateAnalyzer(const DataLayout *DL) : DL(DL) {}

    void clear() {
        StateRefs.clear();
        Calls.clear();
        States.clear();
    }

    /* Add a CPU state reference. */
    void addStateRef(Instruction *I, intptr_t Off) {
        Type *Ty = getValueOperand(I)->getType();
        intptr_t Start = Off;
        intptr_t End = Off + DL->getTypeSizeInBits(Ty) / 8;
        StateRefs.push_back(StateRef(Start, End, I));
    }

    /* Add a helper function call. */
    void addCall(CallInst *CI) {
        Calls.push_back(CI);
    }

    /* Return non-overlapped ranges of states. */
    void computeStateRange(StateRange &Reads, StateRange &Writes) {
        computeState();
        if (StateRefs.empty())
            return;

        const uint8_t READ  = 0x1;
        const uint8_t WRITE = 0x2;
        for (auto &State : States) {
            uint8_t RW = 0;
            for (auto &Ref : State.Refs)
                RW |= isa<LoadInst>(Ref->I) ? READ : WRITE;
            if (RW & READ)
                Reads[State.Start] = State.End;
            if (RW & WRITE)
                Writes[State.Start] = State.End;
        }
    }

    /* Compute referenced states and group instructions. */
    void computeState() {
        /* Sort state refs by the offset. */
        sortStateRefs();
        if (StateRefs.empty())
            return;

        StateData State;
        State.reset(StateRefs.front());
        for (unsigned i = 1, e = StateRefs.size(); i != e; ++i) {
            StateRef &Next = StateRefs[i];
            if (State.End <= Next.Start) {
                /* The next reference is not overlapped with the previous
                 * reference. A new state is found. */
                States.push_back(State);
                /* Reset Curr to the next state. */
                State.reset(Next);
            } else {
                /* Overlap and merge. */
                State.insert(Next);
            }
        }
        /* The last state. */
        States.push_back(State);
    }

    StateList &getStateList() {
        return States;
    }

    CallList &getCalls() {
        return Calls;
    }
};


namespace llvm {
/* Passes */
FunctionPass *createReplaceIntrinsic();
FunctionPass *createFastMathPass();
FunctionPass *createProfileExec(IRFactory *IF);
FunctionPass *createStateMappingPass(IRFactory *IF);
FunctionPass *createCombineGuestMemory(IRFactory *IF);
FunctionPass *createCombineCasts(IRFactory *IF);
FunctionPass *createCombineZExtTrunc();
FunctionPass *createSimplifyPointer(IRFactory *IF);

void initializeReplaceIntrinsicPass(llvm::PassRegistry&);
void initializeFastMathPassPass(llvm::PassRegistry&);
void initializeProfileExecPass(llvm::PassRegistry&);
void initializeStateMappingPassPass(llvm::PassRegistry&);
void initializeCombineGuestMemoryPass(llvm::PassRegistry&);
void initializeCombineCastsPass(llvm::PassRegistry&);
void initializeCombineZExtTruncPass(llvm::PassRegistry&);
void initializeSimplifyPointerPass(llvm::PassRegistry&);

/* Analysis */
void initializeInnerLoopAnalysisWrapperPassPass(llvm::PassRegistry&);
}

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
