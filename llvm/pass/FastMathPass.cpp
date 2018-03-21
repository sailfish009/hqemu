/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm-target.h"
#include "llvm-pass.h"
#include "fpu/softfloat-native-def.h"

#define PASS_DEBUG "FastMathPass"

class FastMathPass : public FunctionPass {
public:
    static char ID;
    std::map<std::string, std::string> FPUNameMap;

    explicit FastMathPass() : FunctionPass(ID)
    {
        TCGHelperInfo *FPUHelper = (TCGHelperInfo *)get_native_fpu_helpers();
        for (int i = 0, e = num_native_fpu_helpers(); i != e; ++i) {
            /* ex: llvm_int32_to_float32 --> int32_to_float32 */
            TCGHelperInfo &fpu = FPUHelper[i];
            const char *native = fpu.name;
            const char *soft =  native + 5;
            FPUNameMap[soft] = native;
        }
    }
    bool runOnFunction(Function &F);
};

bool FastMathPass::runOnFunction(Function &F)
{
    IVec toErase;
    SmallVector<CallInst *, 16> InlineCalls;
    Module *Mod = F.getParent();

    for (auto I = inst_begin(F), E = inst_end(F); I != E; ++I) {
        if (CallInst *CI = dyn_cast<CallInst>(&*I)) {
            if (CI->isInlineAsm() ||
                CI->getCalledFunction() == nullptr ||
                CI->getCalledFunction()->isIntrinsic())
                continue;

            std::string Fname = CI->getCalledFunction()->getName();
            if (FPUNameMap.count(Fname) == 0)
                continue;

            Function *Fn = Mod->getFunction(FPUNameMap[Fname]);
            FunctionType *FTy = cast<FunctionType>(
                    cast<PointerType>(Fn->getType())->getElementType());

            unsigned NumArgs = FTy->getNumParams();
            assert(NumArgs <= CI->getNumArgOperands());

            SmallVector<Value *, 4> Params;
            for (unsigned i = 0; i != NumArgs; ++i)
                Params.push_back(CI->getArgOperand(i));

            CallInst *NewCI = CallInst::Create(Fn, Params, "", CI);
            CI->replaceAllUsesWith(NewCI);
            InlineCalls.push_back(NewCI);
            toErase.push_back(CI);
        }
    }

    ProcessErase(toErase);

    while (!InlineCalls.empty()) {
        InlineFunctionInfo IFI;
        InlineFunction(InlineCalls.pop_back_val(), IFI);
    }

    return false;
}

char FastMathPass::ID = 0;
INITIALIZE_PASS(FastMathPass, "fastmath",
        "Transform softfloat subroutines to native FP operations", false, false)

FunctionPass *llvm::createFastMathPass()
{
    return new FastMathPass();
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

