/*
 *  (C) 2015 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "llvm/Transforms/Utils/Local.h"
#include "llvm-target.h"
#include "llvm-opc.h"
#include "llvm-pass.h"
#include "utils.h"

#define PASS_NAME "CombineZExtTrunc"

/*
 * CombineZExtTrunc Pass
 */
class CombineZExtTrunc : public FunctionPass {
public:
    static char ID;
    explicit CombineZExtTrunc() : FunctionPass(ID) {}
    bool runOnFunction(Function &F);
};

char CombineZExtTrunc::ID = 0;
INITIALIZE_PASS(CombineZExtTrunc, "combinezet",
        "Combine ZExt followed by Trunc", false, false)

FunctionPass *llvm::createCombineZExtTrunc()
{
    return new CombineZExtTrunc;
}

bool CombineZExtTrunc::runOnFunction(Function &F)
{
    bool Changed = false;
    IVec toErase;

    SmallVector<Instruction*, 4> Worklist;
    for (auto II = inst_begin(F), EE = inst_end(F); II != EE; II++) {
        Instruction *I = &*II;
        if (isa<TruncInst>(I))
            Worklist.push_back(I);
    }

    for (auto I : Worklist) {
        TruncInst *TI = cast<TruncInst>(I);
        ZExtInst *ZI = dyn_cast<ZExtInst>(TI->getOperand(0));
        if (!ZI)
            continue;

        Type *SrcTy = ZI->getOperand(0)->getType();
        Type *DstTy = TI->getType();
        if (SrcTy == DstTy) {
            I->replaceAllUsesWith(ZI->getOperand(0));
            if (TI->use_empty())
                toErase.push_back(TI);
            Changed = true;
        }
    }

    if (toErase.size())
        ProcessErase(toErase);

    return Changed;
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

