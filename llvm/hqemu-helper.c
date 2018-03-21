#include "cpu.h"
#include "tcg.h"
#include "exec/helper-proto.h"
#include "hqemu.h"
#include "fpu/softfloat-native.h"

CPUArchState basereg;
target_ulong pcid;

#if defined(TARGET_I386)
XMMReg xmm_reg;
#endif

extern TranslationBlock *tbs;

void *ibtc_lookup(CPUArchState *env);
void *cpbl_lookup(CPUArchState *env);
int cpbl_validate(CPUArchState *env, target_ulong pc, int id);

/* This helper is a hack to export symbols of helper functions in the LLVM
 * bitcode file. If a target is alerted with lacks of symbols of function/variable,
 * add such symbols in this helper by accessing it. */
void helper_export_hqemu(CPUArchState *env)
{
    helper_lookup_ibtc(env);
    helper_lookup_cpbl(env);
    helper_validate_cpbl(env, 0, 0);

#if defined(CONFIG_SOFTMMU) && defined(CONFIG_LLVM)
    target_ulong ptr = 0;
    llvm_ret_ldub_mmu(env, ptr, 0);
    llvm_le_lduw_mmu(env, ptr, 0);
    llvm_le_ldul_mmu(env, ptr, 0);
    llvm_le_ldq_mmu(env, ptr, 0);
    llvm_be_lduw_mmu(env, ptr, 0);
    llvm_be_ldul_mmu(env, ptr, 0);
    llvm_be_ldq_mmu(env, ptr, 0);
    llvm_ret_ldsb_mmu(env, ptr, 0);
    llvm_le_ldsw_mmu(env, ptr, 0);
    llvm_le_ldsl_mmu(env, ptr, 0);
    llvm_be_ldsw_mmu(env, ptr, 0);
    llvm_be_ldsl_mmu(env, ptr, 0);
    llvm_ret_stb_mmu(env, ptr, 0, 0);
    llvm_le_stw_mmu(env, ptr, 0, 0);
    llvm_le_stl_mmu(env, ptr, 0, 0);
    llvm_le_stq_mmu(env, ptr, 0, 0);
    llvm_be_stw_mmu(env, ptr, 0, 0);
    llvm_be_stl_mmu(env, ptr, 0, 0);
    llvm_be_stq_mmu(env, ptr, 0, 0);
#endif
}

void helper_verify_tb(CPUArchState *env, int id)
{
    static TranslationBlock *last_tb;
    TranslationBlock *tb = &tbs[id];
    if (tb->mode == BLOCK_INVALID) {
        fprintf(stderr, "%s: tb=%p pc=" TARGET_FMT_lx " last_pc="
                TARGET_FMT_lx "\n", __func__, tb, tb->pc,
                (last_tb) ? last_tb->pc : -1U);
    }
    last_tb = tb;
}

/*
 * helper_profile_exec is used to profile LLVM translated code.
 */
void helper_profile_exec(CPUArchState *env, void *counter_p, int idx)
{
    CPUState *cpu = ENV_GET_CPU(env);
    uint64_t **counter = (uint64_t **)counter_p;
    counter[cpu->cpu_index][idx]++;
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
