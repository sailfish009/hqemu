/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 *
 *   This file implements the basic optimization schemes including indirect
 *   branch target cache (IBTC), indirect branch chain (IB chain), and trace
 *   profiling and prediction routines.
 */

#ifndef __LLVM_STATE_H
#define __LLVM_STATE_H

#define COPY_STATE(_dst, _src, _e) do { _dst->_e = _src->_e; } while(0)

/*
 * The following data structure and routine are used to save/restore the states
 * of CPUArchState. Only the states that could affect decoding the guest binary by
 * the TCG front-end are saved/restored. Such states are saved when translating
 * the block at the first time because the states could change later and are
 * restored to the saved values when the block is decoded again during the
 * trace formation.
 */
#if defined(TARGET_I386) || defined(TARGET_X86_64)
typedef struct i386_env {
    int singlestep_enabled;
    uint32_t hflags;
    target_ulong eflags;
} cpustate;
#elif defined(TARGET_ARM)
typedef struct arm_env {
    int singlestep_enabled;
    uint32_t pstate;
    uint32_t aarch64;
    struct {
        uint32_t c15_cpar;
        uint64_t scr_el3;
    } cp15;
    uint32_t uncached_cpsr;
    uint64_t features;
} cpustate;
#elif defined(TARGET_PPC) || defined(TARGET_PPC64)
typedef struct ppc_env {
    int singlestep_enabled;
    target_ulong msr;
    int mmu_idx;
    uint32_t flags;
    uint64_t insns_flags;
    uint64_t insns_flags2;
    target_ulong hflags;
} cpustate;
#elif defined(TARGET_SH4)
typedef struct sh4_env {
    int singlestep_enabled;
    uint32_t sr;	/* status register */
    uint32_t fpscr;	/* floating point status/control register */
    uint32_t features;
} cpustate;
#elif defined(TARGET_M68K)
typedef struct m68k_env {
    int singlestep_enabled;
    uint32_t sr;	/* status register */
    uint32_t fpcr;	/* floating point status/control register */
} cpustate;
#elif defined(TARGET_MIPS)
typedef struct mips_env {
    int singlestep_enabled;
    target_ulong btarget;
} cpustate;
#else
typedef struct dummy_env {
    int dummy;
} cpustate;
#endif

static inline void tcg_save_state(CPUArchState *env, TranslationBlock *tb)
{
#if defined(TARGET_I386) || defined(TARGET_X86_64)
    CPUState *cpu = ENV_GET_CPU(env);
    struct i386_env *s = new struct i386_env;
    COPY_STATE(s, cpu, singlestep_enabled);
    COPY_STATE(s, env, hflags);
    COPY_STATE(s, env, eflags);
#elif defined(TARGET_ARM)
    CPUState *cpu = ENV_GET_CPU(env);
    struct arm_env *s = new struct arm_env;
    COPY_STATE(s, cpu, singlestep_enabled);
    COPY_STATE(s, env, cp15.c15_cpar);
    COPY_STATE(s, env, cp15.scr_el3);
    COPY_STATE(s, env, uncached_cpsr);
    COPY_STATE(s, env, features);
    COPY_STATE(s, env, pstate);
    COPY_STATE(s, env, aarch64);
#elif defined(TARGET_PPC) || defined(TARGET_PPC64)
    CPUState *cpu = ENV_GET_CPU(env);
    struct ppc_env *s = new struct ppc_env;
    COPY_STATE(s, cpu, singlestep_enabled);
    COPY_STATE(s, env, msr);
    COPY_STATE(s, env, mmu_idx);
    COPY_STATE(s, env, flags);
    COPY_STATE(s, env, insns_flags);
    COPY_STATE(s, env, insns_flags2);
    COPY_STATE(s, env, hflags);
#elif defined(TARGET_SH4)
    CPUState *cpu = ENV_GET_CPU(env);
    struct sh4_env *s = new struct sh4_env;
    COPY_STATE(s, cpu, singlestep_enabled);
    COPY_STATE(s, env, sr);
    COPY_STATE(s, env, fpscr);
    COPY_STATE(s, env, features);
#elif defined(TARGET_M68K)
    CPUState *cpu = ENV_GET_CPU(env);
    struct m68k_env *s = new struct m68k_env;
    COPY_STATE(s, cpu, singlestep_enabled);
    COPY_STATE(s, env, sr);
    COPY_STATE(s, env, fpcr);
#elif defined(TARGET_MIPS)
    CPUState *cpu = ENV_GET_CPU(env);
    struct mips_env *s = new struct mips_env;
    COPY_STATE(s, cpu, singlestep_enabled);
    COPY_STATE(s, env, btarget);
#else
    void *s = nullptr;
#endif

    tb->state = (void *)s;
}

/*
 * tcg_restore_state()
 *  Reset states to those when the block is first translated.
 */
static inline void tcg_copy_state(CPUArchState *env, TranslationBlock *tb)
{
#if defined(TARGET_I386) || defined(TARGET_X86_64)
    CPUState *cpu = ENV_GET_CPU(env);
    struct i386_env *i386e = (struct i386_env *)tb->state;
    COPY_STATE(cpu, i386e, singlestep_enabled);
    COPY_STATE(env, i386e, hflags);
    COPY_STATE(env, i386e, eflags);
#elif defined(TARGET_ARM)
    CPUState *cpu = ENV_GET_CPU(env);
    struct arm_env *arme = (struct arm_env *)tb->state;
    COPY_STATE(cpu, arme, singlestep_enabled);
    COPY_STATE(env, arme, cp15.c15_cpar);
    COPY_STATE(env, arme, cp15.scr_el3);
    COPY_STATE(env, arme, uncached_cpsr);
    COPY_STATE(env, arme, features);
    COPY_STATE(env, arme, pstate);
    COPY_STATE(env, arme, aarch64);
#elif defined(TARGET_PPC) || defined(TARGET_PPC64)
    CPUState *cpu = ENV_GET_CPU(env);
    struct ppc_env *ppce = (struct ppc_env *)tb->state;
    COPY_STATE(cpu, ppce, singlestep_enabled);
    COPY_STATE(env, ppce, msr);
    COPY_STATE(env, ppce, mmu_idx);
    COPY_STATE(env, ppce, flags);
    COPY_STATE(env, ppce, insns_flags);
    COPY_STATE(env, ppce, insns_flags2);
    COPY_STATE(env, ppce, hflags);
#elif defined(TARGET_SH4)
    CPUState *cpu = ENV_GET_CPU(env);
    struct sh4_env *sh4e = (struct sh4_env *)tb->state;
    COPY_STATE(cpu, sh4e, singlestep_enabled);
    COPY_STATE(env, sh4e, sr);
    COPY_STATE(env, sh4e, fpscr);
    COPY_STATE(env, sh4e, features);
#elif defined(TARGET_M68K)
    CPUState *cpu = ENV_GET_CPU(env);
    struct m68k_env *m68ke = (struct m68k_env *)tb->state;
    COPY_STATE(cpu, m68ke, singlestep_enabled);
    COPY_STATE(env, m68ke, sr);
    COPY_STATE(env, m68ke, fpcr);
#elif defined(TARGET_MIPS)
    CPUState *cpu = ENV_GET_CPU(env);
    struct mips_env *mipse = (struct mips_env *)tb->state;
    COPY_STATE(cpu, mipse, singlestep_enabled);
    COPY_STATE(env, mipse, btarget);
#endif
}

static inline void delete_state(TranslationBlock *tb)
{
    delete (cpustate *)tb->state;
    tb->state = nullptr;
}

#undef COPY_STATE
#endif  /* __LLVM_STATE_H */


/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

