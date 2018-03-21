/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Object/Binary.h"
#include "llvm/DebugInfo/DIContext.h"
#include "llvm/Support/Debug.h"
#include "llvm-debug.h"
#include "llvm-opc.h"
#include "llvm-target.h"

using namespace llvm::object;

extern "C" {
#if defined(TARGET_I386)
extern const int comis_eflags[4];
extern const int fcom_ccval[4];
#endif
}


static std::vector<TCGHelperInfo> MMUHelper = {
#if defined(CONFIG_SOFTMMU)
    { (void *)llvm_ret_ldub_mmu, "llvm_ret_ldub_mmu", },
    { (void *)llvm_le_lduw_mmu,  "llvm_le_lduw_mmu", },
    { (void *)llvm_le_ldul_mmu,  "llvm_le_ldul_mmu", },
    { (void *)llvm_le_ldq_mmu,   "llvm_le_ldq_mmu", },
    { (void *)llvm_be_lduw_mmu,  "llvm_be_lduw_mmu", },
    { (void *)llvm_be_ldul_mmu,  "llvm_be_ldul_mmu", },
    { (void *)llvm_be_ldq_mmu,   "llvm_be_ldq_mmu", },
    { (void *)llvm_ret_ldsb_mmu, "llvm_ret_ldsb_mmu", },
    { (void *)llvm_le_ldsw_mmu,  "llvm_le_ldsw_mmu", },
    { (void *)llvm_le_ldsl_mmu,  "llvm_le_ldsl_mmu", },
    { (void *)llvm_be_ldsw_mmu,  "llvm_be_ldsw_mmu", },
    { (void *)llvm_be_ldsl_mmu,  "llvm_be_ldsl_mmu", },

    { (void *)llvm_ret_stb_mmu, "llvm_ret_stb_mmu", },
    { (void *)llvm_le_stw_mmu,  "llvm_le_stw_mmu", },
    { (void *)llvm_le_stl_mmu,  "llvm_le_stl_mmu", },
    { (void *)llvm_le_stq_mmu,  "llvm_le_stq_mmu", },
    { (void *)llvm_be_stw_mmu,  "llvm_be_stw_mmu", },
    { (void *)llvm_be_stl_mmu,  "llvm_be_stl_mmu", },
    { (void *)llvm_be_stq_mmu,  "llvm_be_stq_mmu", },
#endif
};


/* Helper functions that cause side effect.
 * For example, helpers modifying CPU states that cannot be identified,
 * or helpers that call MMU helpers.
 * During translating qemu_ld/st, we record MMU helper calls so that we
 * know how to restore when page fault is handled. Unfortunately, we lose
 * track of the MMU helper calls in a helper function and the restoration
 * will fail. Currently, we mark such helper functions as illegal ones and
 * we skip trace building when a call to one of them when translating
 * op_call. */
static std::vector<TCGHelperInfo> IllegalHelper = {
#if defined(CONFIG_SOFTMMU)
#  if defined(TARGET_I386)
    { (void *)helper_cmpxchg8b, "helper_cmpxchg8b", },
    { (void *)helper_boundw, "helper_boundw", },
    { (void *)helper_boundl, "helper_boundl", },
#  elif defined(TARGET_ARM)
    { (void *)helper_dc_zva, "helper_dc_zva", },
#  endif
#else
#  if defined(TARGET_AARCH64)
    { (void *)helper_simd_tbl, "helper_simd_tbl", },
#  endif
#endif
};


#define DEF_HELPER_FLAGS_0(name, flags, ret) { (void *)helper_##name, "helper_"#name }, 
#define DEF_HELPER_FLAGS_1(name, flags, ret, t1) DEF_HELPER_FLAGS_0(name, flags, ret)
#define DEF_HELPER_FLAGS_2(name, flags, ret, t1, t2) DEF_HELPER_FLAGS_0(name, flags, ret)
#define DEF_HELPER_FLAGS_3(name, flags, ret, t1, t2, t3) DEF_HELPER_FLAGS_0(name, flags, ret)
#define DEF_HELPER_FLAGS_4(name, flags, ret, t1, t2, t3, t4) DEF_HELPER_FLAGS_0(name, flags, ret)

static std::vector<TCGHelperInfo> LMTHelper = {
#if defined(CONFIG_SOFTMMU)
#include "atomic-helper.h"
#endif
};

#undef DEF_HELPER_FLAGS_0
#undef DEF_HELPER_FLAGS_1
#undef DEF_HELPER_FLAGS_2
#undef DEF_HELPER_FLAGS_3
#undef DEF_HELPER_FLAGS_4


const char *getMMUFName(const void *func)
{
    for (unsigned i = 0, e = MMUHelper.size(); i != e; ++i) {
        if (func == MMUHelper[i].func)
            return MMUHelper[i].name;
    }
    return "";
}

bool isMMUFunction(std::string &Name)
{
    for (unsigned i = 0, e = MMUHelper.size(); i != e; ++i) {
        if (Name == MMUHelper[i].name)
            return true;
    }
    return false;
}

bool isLMTFunction(std::string &Name)
{
    for (unsigned i = 0, e = LMTHelper.size(); i != e; ++i) {
        if (Name == LMTHelper[i].name)
            return true;
    }
    return false;
}

bool isIllegalHelper(const void *func)
{
    for (unsigned i = 0, e = IllegalHelper.size(); i != e; ++i) {
        if (func == IllegalHelper[i].func)
            return true;
    }
    return false;
}

/*
 * isLibcall()
 *  Determine if the function name is a system library.
 */
bool isLibcall(std::string &Name)
{
    if (Name == "fmodf" || Name == "fmod" || Name == "fmodl" ||
        Name == "abs" || Name == "labs" || Name == "llabs" ||
        Name == "fabs" || Name == "fabsf" || Name == "fabsl" ||
        Name == "sqrtf" || Name == "sqrt" || Name == "sqrtl" ||
        Name == "logf" || Name == "log" || Name == "logl" ||
        Name == "log2f" || Name == "log2" || Name == "log2l" ||
        Name == "log10f" || Name == "log10" || Name == "log10l" ||
        Name == "expf" || Name == "exp" || Name == "expl" ||
        Name == "exp2f" || Name == "exp2" || Name == "exp2l" ||
        Name == "ldexpf" || Name == "ldexp" || Name == "ldexpl" ||
        Name == "sinf" || Name == "sin" || Name == "sinl" ||
        Name == "cosf" || Name == "cos" || Name == "cosl" ||
        Name == "tanf" || Name == "tan" || Name == "tanl" ||
        Name == "atanf" || Name == "atan" || Name == "atanl" ||
        Name == "atanf2" || Name == "atan2" || Name == "atanl2" ||
        Name == "powf" || Name == "pow" || Name == "powl" ||
        Name == "ceilf" || Name == "ceil" || Name == "ceill" ||
        Name == "truncf" || Name == "trunc" || Name == "truncl" ||
        Name == "rintf" || Name == "rint" || Name == "rintl" ||
        Name == "lrintf" || Name == "lrint" || Name == "lrintl" ||
        Name == "nearbyintf" || Name == "nearbyint" || Name == "nearbyintl" ||
        Name == "floorf" || Name == "floor" || Name == "floorl" ||
        Name == "copysignf" || Name == "copysign" || Name == "copysignl" ||
        Name == "memcpy" || Name == "memmove" || Name == "memset" ||
        Name == "fegetround" || Name == "fesetround" ||
        Name == "__isinfl" || Name == "__isnanl")
    {
        return true;
    }

    return false;
}

/*
 * isSoftFPcall()
 *  Determine if the function name is a softfloat helper function.
 */
bool isSoftFPcall(std::string &Name)
{
    static char SoftFPName[][128] = {
        "float16_to_float32",
        "float32_add",
        "float32_compare",
        "float32_compare_quiet",
        "float32_div",
        "float32_mul",
        "float32_scalbn",
        "float32_sqrt",
        "float32_sub",
        "float32_to_float16",
        "float32_to_float64",
        "float32_to_int32",
        "float32_to_int64",
        "float32_to_uint32",
        "float64_add",
        "float64_compare",
        "float64_compare_quiet",
        "float64_div",
        "float64_mul",
        "float64_scalbn",
        "float64_sqrt",
        "float64_sub",
        "float64_to_float32",
        "float64_to_int32",
        "float64_to_int64",
        "float64_to_uint32",
        "int32_to_float32",
        "int32_to_float64",
        "int64_to_float32",
        "normalizeRoundAndPackFloat128",
        "propagateFloat128NaN",
        "propagateFloatx80NaN",
        "roundAndPackFloat128",
        "roundAndPackFloat32",
        "roundAndPackFloat64",
        "roundAndPackFloatx80",
        "set_float_rounding_mode",
        "subFloat128Sigs",
        "subFloat32Sigs",
        "subFloat64Sigs",
        "subFloatx80Sigs",
        "uint32_to_float32",
        "uint32_to_float64",
#if 0
        /* FIXME: this function causes LLVM JIT error:
           LLVM ERROR: Error reading function 'set_float_exception_flags' from bitcode file: Malformed block record */
        "set_float_exception_flags",
#endif
        "addFloat32Sigs",
        "addFloat64Sigs",

        "float32_to_int32_round_to_zero",
        "float64_to_int32_round_to_zero",

        "int32_to_floatx80",
        "int64_to_floatx80",
        "float32_to_floatx80",
        "float64_to_floatx80",
        "floatx80_abs",
        "floatx80_chs",
        "floatx80_is_infinity",
        "floatx80_is_neg",
        "floatx80_is_zero",
        "floatx80_is_zero_or_denormal",
        "floatx80_is_any_nan",

        "floatx80_to_int32",
        "floatx80_to_int32_round_to_zero",
        "floatx80_to_int64",
        "floatx80_to_int64_round_to_zero",
        "floatx80_to_float32",
        "floatx80_to_float64",
        "floatx80_to_float128",
        "floatx80_round_to_int",
        "floatx80_add",
        "floatx80_sub",
        "floatx80_mul",
        "floatx80_div",
        "floatx80_rem",
        "floatx80_sqrt",
        "floatx80_eq",
        "floatx80_le",
        "floatx80_lt",
        "floatx80_unordered",
        "floatx80_eq_quiet",
        "floatx80_le_quiet",
        "floatx80_lt_quiet",
        "floatx80_unordered_quiet",
        "floatx80_compare",
        "floatx80_compare_quiet",
        "floatx80_is_quiet_nan",
        "floatx80_is_signaling_nan",
        "floatx80_maybe_silence_nan",
        "floatx80_scalbn",
    };

    for (int i = 0, e = ARRAY_SIZE(SoftFPName); i < e; i++) {
        if (Name == SoftFPName[i])
            return true;
    }
    return false;
}

/*
 * addFPUFunctions()
 *  Bind addresses to the nested functions which are used by the softfloat
 *  helper functions.
 */
void AddFPUSymbols(LLVMTranslator *Translator)
{
#define AddSymbol(a) Translator->AddSymbol(#a, (void*)a)
    AddSymbol(float32_add);
    AddSymbol(float32_sub);
    AddSymbol(float32_mul);
    AddSymbol(float32_div);
    AddSymbol(float32_sqrt);
    AddSymbol(float32_scalbn);
    AddSymbol(float32_compare);
    AddSymbol(float32_compare_quiet);
    AddSymbol(float64_add);
    AddSymbol(float64_sub);
    AddSymbol(float64_mul);
    AddSymbol(float64_div);
    AddSymbol(float64_sqrt);
    AddSymbol(float64_scalbn);
    AddSymbol(float64_compare);
    AddSymbol(float64_compare_quiet);
    AddSymbol(float16_to_float32);
    AddSymbol(float32_to_float16);
    AddSymbol(float32_to_float64);
    AddSymbol(float32_to_int32);
    AddSymbol(float32_to_int64);
    AddSymbol(float32_to_uint32);
    AddSymbol(float64_to_float32);
    AddSymbol(float64_to_int32);
    AddSymbol(float64_to_int64);
    AddSymbol(float64_to_uint32);
    AddSymbol(int32_to_float32);
    AddSymbol(int32_to_float64);
    AddSymbol(int64_to_float32);
    AddSymbol(uint32_to_float32);
    AddSymbol(uint32_to_float64);
    AddSymbol(float32_to_int32_round_to_zero);
    AddSymbol(float64_to_int32_round_to_zero);

    AddSymbol(int32_to_floatx80);
    AddSymbol(int64_to_floatx80);
    AddSymbol(float32_to_floatx80);
    AddSymbol(float64_to_floatx80);
    AddSymbol(floatx80_abs);
    AddSymbol(floatx80_chs);
    AddSymbol(floatx80_is_infinity);
    AddSymbol(floatx80_is_neg);
    AddSymbol(floatx80_is_zero);
    AddSymbol(floatx80_is_zero_or_denormal);
    AddSymbol(floatx80_is_any_nan);

    AddSymbol(floatx80_to_int32);
    AddSymbol(floatx80_to_int32_round_to_zero);
    AddSymbol(floatx80_to_int64);
    AddSymbol(floatx80_to_int64_round_to_zero);
    AddSymbol(floatx80_to_float32);
    AddSymbol(floatx80_to_float64);
    AddSymbol(floatx80_to_float128);
    AddSymbol(floatx80_round_to_int);
    AddSymbol(floatx80_add);
    AddSymbol(floatx80_sub);
    AddSymbol(floatx80_mul);
    AddSymbol(floatx80_div);
    AddSymbol(floatx80_rem);
    AddSymbol(floatx80_sqrt);
    AddSymbol(floatx80_eq);
    AddSymbol(floatx80_le);
    AddSymbol(floatx80_lt);
    AddSymbol(floatx80_unordered);
    AddSymbol(floatx80_eq_quiet);
    AddSymbol(floatx80_le_quiet);
    AddSymbol(floatx80_lt_quiet);
    AddSymbol(floatx80_unordered_quiet);
    AddSymbol(floatx80_compare);
    AddSymbol(floatx80_compare_quiet);
    AddSymbol(floatx80_is_quiet_nan);
    AddSymbol(floatx80_is_signaling_nan);
    AddSymbol(floatx80_maybe_silence_nan);
    AddSymbol(floatx80_scalbn);

    AddSymbol(rint);
    AddSymbol(rintf);
    AddSymbol(lrint);
    AddSymbol(lrintf);
    AddSymbol(llrint);
    AddSymbol(llrintf);
    AddSymbol(remainder);
    AddSymbol(remainderf);
    AddSymbol(fabs);
    AddSymbol(fabsf);
    AddSymbol(sqrt);
    AddSymbol(sqrtf);
    AddSymbol(trunc);
    AddSymbol(exp2);
    AddSymbol(log);
    AddSymbol(ldexp);
    AddSymbol(floor);
    AddSymbol(ceil);
    AddSymbol(sin);
    AddSymbol(cos);
    AddSymbol(tan);
    AddSymbol(atan2);
    AddSymbol(__isinf);
    AddSymbol(__isnan);
#undef AddSymbol
}

void AddLMTSymbols(LLVMTranslator *Translator)
{
    for (unsigned i = 0, e = LMTHelper.size(); i != e; ++i) {
        TCGHelperInfo &H = LMTHelper[i];
        Translator->AddSymbol(H.name, H.func);
    }
}

void AddMMUSymbols(LLVMTranslator *Translator)
{
    for (unsigned i = 0, e = MMUHelper.size(); i != e; ++i) {
        TCGHelperInfo &H = MMUHelper[i];
        Translator->AddSymbol(H.name, H.func);
    }
}

/*
 * Bind addresses to the functions that are used by the helpers.
 */
#if defined(CONFIG_USER_ONLY)
void AddDependentSymbols(LLVMTranslator *Translator)
{
    Translator->AddSymbol("helper_verify_tb", (void*)helper_verify_tb);
    Translator->AddSymbol("helper_lookup_ibtc", (void*)helper_lookup_ibtc);
    Translator->AddSymbol("guest_base", (void*)&guest_base);
    Translator->AddSymbol("cpu_loop_exit", (void*)cpu_loop_exit);
    Translator->AddSymbol("qemu_logfile", (void*)&qemu_logfile);
    Translator->AddSymbol("qemu_loglevel", (void*)&qemu_loglevel);

#if defined(TARGET_I386)
    Translator->AddSymbol("parity_table", (void*)parity_table);
    Translator->AddSymbol("comis_eflags", (void*)comis_eflags);
    Translator->AddSymbol("fcom_ccval", (void*)fcom_ccval);
    Translator->AddSymbol("raise_exception", (void*)raise_exception);
    Translator->AddSymbol("raise_exception_err", (void*)raise_exception_err);
#endif

    AddFPUSymbols(Translator);
}
#else
void AddDependentSymbols(LLVMTranslator *Translator)
{
    Translator->AddSymbol("helper_verify_tb", (void*)helper_verify_tb);
    Translator->AddSymbol("helper_lookup_ibtc", (void*)helper_lookup_ibtc);
    Translator->AddSymbol("helper_lookup_cpbl", (void*)helper_lookup_cpbl);
    Translator->AddSymbol("helper_validate_cpbl", (void*)helper_validate_cpbl);
    Translator->AddSymbol("cpu_loop_exit", (void*)cpu_loop_exit);
    Translator->AddSymbol("qemu_logfile", (void*)&qemu_logfile);
    Translator->AddSymbol("qemu_loglevel", (void*)&qemu_loglevel);
    Translator->AddSymbol("exp2", (void*)exp2);

#if defined(TARGET_I386)
    Translator->AddSymbol("parity_table", (void*)parity_table);
    Translator->AddSymbol("comis_eflags", (void*)comis_eflags);
    Translator->AddSymbol("fcom_ccval", (void*)fcom_ccval);
#endif

    AddFPUSymbols(Translator);
    AddLMTSymbols(Translator);
    AddMMUSymbols(Translator);
}
#endif

/*
 * getBaseWithConstantOffset()
 *  Return base address and offset of a memory access pointer.
 */
Value *getBaseWithConstantOffset(const DataLayout *DL, Value *Ptr,
                                 intptr_t &Offset)
{
    Operator *PtrOp = dyn_cast<Operator>(Ptr);
    if (!PtrOp)
        return Ptr;

    if (PtrOp->getOpcode() == Instruction::BitCast ||
        PtrOp->getOpcode() == Instruction::IntToPtr)
        return getBaseWithConstantOffset(DL, PtrOp->getOperand(0), Offset);

    /* If this is a GEP with constant indices, we can look through it. */
    GEPOperator *GEP = dyn_cast<GEPOperator>(PtrOp);
    if (!GEP || !GEP->hasAllConstantIndices())
        return Ptr;

    gep_type_iterator GTI = gep_type_begin(GEP);
    for (auto I = GEP->idx_begin(), E = GEP->idx_end(); I != E; ++I, ++GTI) {
        ConstantInt *OpC = cast<ConstantInt>(*I);
        if (OpC->isZero())
            continue;
        
        /* Handle a struct and array indices which add their offset to the
         * pointer. */
        if (StructType *STy = dyn_cast<StructType>(*GTI))
            Offset += DL->getStructLayout(STy)->getElementOffset(OpC->getZExtValue());
        else {
            intptr_t Size = DL->getTypeAllocSize(GTI.getIndexedType());
            Offset += OpC->getSExtValue() * Size;
        }
    }

    return getBaseWithConstantOffset(DL, GEP->getPointerOperand(), Offset);
}

static bool accumulateConstantOffset(const DataLayout *DL, GEPOperator *GEP, APInt &Offset)
{
    for (auto GTI = gep_type_begin(GEP), GTE = gep_type_end(GEP); GTI != GTE; ++GTI) {
        ConstantInt *OpC = dyn_cast<ConstantInt>(GTI.getOperand());
        if (!OpC)
            return false;
        if (OpC->isZero())
            continue;

        /* Handle a struct index, which adds its field offset to the pointer. */
        if (StructType *STy = dyn_cast<StructType>(*GTI)) {
            unsigned ElementIdx = OpC->getZExtValue();
            const StructLayout *SL = DL->getStructLayout(STy);
            Offset += APInt(Offset.getBitWidth(),
                            SL->getElementOffset(ElementIdx));
            continue;
        }
        
        /* For array or vector indices, scale the index by the size of the type. */
        APInt Index = OpC->getValue().sextOrTrunc(Offset.getBitWidth());
        Offset += Index * APInt(Offset.getBitWidth(),
                                DL->getTypeAllocSize(GTI.getIndexedType()));
    }
    return true;
}

Value *StripPointer(Value *Ptr)
{
    if (!Ptr->getType()->isPointerTy())
        return Ptr;

    SmallPtrSet<Value *, 8> Visited;
    Visited.insert(Ptr);
    do {
        Operator *PtrOp = cast<Operator>(Ptr);
        unsigned Opcode = PtrOp->getOpcode();
        if (Opcode == Instruction::BitCast  ||
            Opcode == Instruction::IntToPtr ||
            Opcode == Instruction::GetElementPtr)
            Ptr = cast<Operator>(Ptr)->getOperand(0);
        else
            return Ptr;

        if (Visited.count(Ptr))
            break;
        Visited.insert(Ptr);
    } while (true);

    return Ptr;
}

Value *StripPointer(const DataLayout *DL, Value *Ptr, APInt &Offset)
{
    if (!Ptr->getType()->isPointerTy())
        return Ptr;
    
    std::set<Value *> Visited;
    Visited.insert(Ptr);
    Value *V = Ptr;
    do {
        if (GEPOperator *GEP = dyn_cast<GEPOperator>(V)) {
            APInt GEPOffset(Offset);
            if (!accumulateConstantOffset(DL, GEP, GEPOffset))
                return V;
            Offset = GEPOffset;
            V = GEP->getPointerOperand();
            continue;
        }

        Operator *PtrOp = cast<Operator>(V);
        unsigned Opcode = PtrOp->getOpcode();
        if (Opcode == Instruction::BitCast || Opcode == Instruction::IntToPtr) {
            V = cast<Operator>(V)->getOperand(0);
        } else if (Opcode == Instruction::Add ||
                   Opcode == Instruction::Sub) {
            if (!isa<ConstantInt>(PtrOp->getOperand(1)))
                return V;

            int64_t C = cast<ConstantInt>(PtrOp->getOperand(1))->getSExtValue();
            if (Opcode == Instruction::Add)
                Offset += APInt(Offset.getBitWidth(), C, true);
            else
                Offset -= APInt(Offset.getBitWidth(), C, true);
            V = PtrOp->getOperand(0);
        } else
            return V;

        if (Visited.find(V) != Visited.end())
            break;
        Visited.insert(V);
    } while (true);
    
    return V;
}

/*
 * DeleteDeadInstructions()
 *  Remove an instruction from a basic block. Also delete any instrution
 *  used by this instruction if it is no longer being used.
 */
static void DeleteDeadInstructions(Instruction *Inst)
{
    SmallVector<Instruction*, 16> DeadInsts;
    DeadInsts.push_back(Inst);
    do {
        Instruction *I = DeadInsts.pop_back_val();
        for (unsigned i = 0, e = I->getNumOperands(); i != e; ++i) {
            Value *OpV = I->getOperand(i);
            I->setOperand(i, nullptr);

            if (!OpV->use_empty()) continue;

            Instruction *OpI = dyn_cast<Instruction>(OpV);
            if (OpI && OpI->getParent())
                DeadInsts.push_back(OpI);
        }
        I->eraseFromParent();
    } while (!DeadInsts.empty());
}

/*
 * ProcessErase()
 *  Perform instruction removal from the parent container.
 */
void ProcessErase(IVec &toErase)
{
    for (auto I = toErase.begin(), E = toErase.end(); I != E; ++I)
        DeleteDeadInstructions(*I);
    toErase.clear();
}


/*
 * JIT Event Listener
 */
void EventListener::NotifyFunctionEmitted(const Function &F,
                                          void *Code, size_t Size,
                                          const EmittedFunctionDetails &Details)
{
    if (!NI.Func)
        return;

    NI.Code = (uint8_t *)Code;
    NI.Size = Size;
}

#if defined(LLVM_V35)
void EventListener::NotifyObjectEmitted(const ObjectImage &Obj)
{
    StringRef Name;
    uint64_t Code;
    uint64_t Size;
    unsigned NumFunc = 0;
    DIContext* Context = DIContext::getDWARFContext(Obj.getObjectFile());

    for (auto I = Obj.begin_symbols(), E = Obj.end_symbols(); I != E; ++I) {
        object::SymbolRef::Type SymType;
        if (I->getType(SymType)) continue;
        if (SymType == object::SymbolRef::ST_Function) {
            if (I->getName(Name)) continue;
            if (I->getAddress(Code)) continue;
            if (I->getSize(Size)) continue;

            NumFunc++;
            if (!Context)
                continue;
            
            DILineInfoTable  Lines = Context->getLineInfoForAddressRange(Code, Size);
            DILineInfoTable::iterator  Begin = Lines.begin();
            DILineInfoTable::iterator  End = Lines.end();
            for (DILineInfoTable::iterator It = Begin; It != End; ++It)
                NI.addPatch(It->second.Line, It->second.Column, It->first);
        }
    }
    if (NumFunc != 1)
        hqemu_error("internal error.\n");

    NI.Code = (uint8_t *)Code;
    NI.Size = Size;
}

#elif defined(LLVM_V38)
void EventListener::NotifyObjectEmitted(const ObjectFile &Obj,
                                        const RuntimeDyld::LoadedObjectInfo &L)
{
    OwningBinary<ObjectFile> DebugObjOwner = L.getObjectForDebug(Obj);
    const ObjectFile &DebugObj = *DebugObjOwner.getBinary();
    DIContext* Context = new DWARFContextInMemory(DebugObj);
    uint64_t Code;
    uint64_t Size;
    unsigned NumFunc = 0;

    for (const std::pair<SymbolRef, uint64_t> &P : computeSymbolSizes(DebugObj)) {
        SymbolRef Sym = P.first;
        if (Sym.getType() != SymbolRef::ST_Function)
            continue;

        ErrorOr<StringRef> Name = Sym.getName();
        if (!Name)
            continue;

        ErrorOr<uint64_t> AddrOrErr = Sym.getAddress();
        if (AddrOrErr.getError())
            continue;

        Code = *AddrOrErr;
        Size = P.second;
        NumFunc++;

        DILineInfoTable Lines = Context->getLineInfoForAddressRange(Code, Size);
        DILineInfoTable::iterator Begin = Lines.begin();
        DILineInfoTable::iterator End = Lines.end();
        for (DILineInfoTable::iterator It = Begin; It != End; ++It)
            NI.addPatch(It->second.Line, It->second.Column, It->first);
    }

    if (NumFunc != 1)
        hqemu_error("internal error.\n");

    NI.Code = (uint8_t *)Code;
    NI.Size = Size;
}
#else
void EventListener::NotifyObjectEmitted(const ObjectFile &Obj,
                                        const RuntimeDyld::LoadedObjectInfo &L)
{
    OwningBinary<ObjectFile> DebugObjOwner = L.getObjectForDebug(Obj);
    const ObjectFile &DebugObj = *DebugObjOwner.getBinary();
    DIContext* Context = new DWARFContextInMemory(DebugObj);
    uint64_t Code;
    uint64_t Size;
    unsigned NumFunc = 0;

    for (const std::pair<SymbolRef, uint64_t> &P : computeSymbolSizes(DebugObj)) {
        SymbolRef Sym = P.first;
        Expected<SymbolRef::Type> SymTypeOrErr = Sym.getType();
        if (!SymTypeOrErr)
            continue;

        SymbolRef::Type SymType = *SymTypeOrErr;
        if (SymType != SymbolRef::ST_Function)
            continue;

        Expected<StringRef> Name = Sym.getName();
        if (!Name)
            continue;

        Expected<uint64_t> AddrOrErr = Sym.getAddress();
        if (!AddrOrErr)
            continue;

        Code = *AddrOrErr;
        Size = P.second;
        NumFunc++;

        DILineInfoTable Lines = Context->getLineInfoForAddressRange(Code, Size);
        DILineInfoTable::iterator Begin = Lines.begin();
        DILineInfoTable::iterator End = Lines.end();
        for (DILineInfoTable::iterator It = Begin; It != End; ++It)
            NI.addPatch(It->second.Line, It->second.Column, It->first);
    }

    if (NumFunc != 1)
        hqemu_error("internal error.\n");

    NI.Code = (uint8_t *)Code;
    NI.Size = Size;
}

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
