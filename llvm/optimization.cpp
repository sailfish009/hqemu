/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 *
 *   This file implements the basic optimization schemes including
 *   (1) instruction TLB (iTLB),
 *   (2) indirect branch target cache (IBTC),
 *   (3) cross-page block linking (CPBL), and
 *   (4) large page table (LPT).
 */

#include "tracer.h"
#include "optimization.h"


#if defined(ENALBE_CPU_PROFILE)
#  define PROFILE(X) do { X; } while (0)
#else
#  define PROFILE(X) do { } while (0)
#endif

/* The following implements routines of the C interfaces for QEMU. */
extern "C" {

TranslationBlock *tbs;

extern uint8_t *ibtc_ret_addr;

/*
 * iTLB (Instruction TLB)
 */
void itlb_update_entry(CPUArchState *env, TranslationBlock *tb)
{
    ITLB &itlb = cpu_get_itlb(env);
    itlb.insert(tb->pc, tb->page_addr[0] & TARGET_PAGE_MASK);
    if (tb->page_addr[1] != (tb_page_addr_t)-1)
        itlb.insert(tb->pc + tb->size, tb->page_addr[1] & TARGET_PAGE_MASK);
}

int itlb_lookup(CPUArchState *env, target_ulong pc, uint64_t paddr)
{
    ITLB &itlb = cpu_get_itlb(env);
    return itlb.get(pc) == (paddr & TARGET_PAGE_MASK);
}

/*
 * IBTC (Indirect Branch Translation Cache)
 */
#if defined(ENABLE_IBTC)
/*
 * update_ibtc_entry()
 *  Update IBTC hash table.
 *  Note: we do not cache TBs that cross page boundary.
 */
void ibtc_update_entry(CPUArchState *env, TranslationBlock *tb)
{
    IBTC &ibtc = cpu_get_ibtc(env);
    if (!ibtc.needUpdate() || tb->page_addr[1] != (tb_page_addr_t)-1)
        return;

    ibtc.insert(tb->pc, tb);
    ibtc.resetUpdate();

    if (update_tb_mode(tb, BLOCK_ACTIVE, BLOCK_TRACEHEAD)) {
        if (tracer.mode == TRANS_MODE_HYBRIDS ||
            tracer.mode == TRANS_MODE_HYBRIDM)
            start_trace_profiling(tb);
    }
}

/*
 * helper_lookup_ibtc()
 *  Helper function to lookup the IBTC hash table.
 */
void *helper_lookup_ibtc(CPUArchState *env)
{
    CPUState *cpu = ENV_GET_CPU(env);
    if (unlikely(cpu->tcg_exit_req != 0)) {
        cpu->tcg_exit_req = 0;
        return ibtc_ret_addr;
    }

    /* A match of 'pc', 'cs_base' and 'flags' results in a IBTC hit. Since
     * cs_base is only meaningful with x86 guest and system mode (cs_base is
     * always 0 for user-mode emulation and non-x86 guest), we only compare
     * cs_base with system mode emulation of x86 guest. */

    target_ulong pc = cpu_get_pc(env);
    IBTC &ibtc = cpu_get_ibtc(env);
    TranslationBlock *next_tb = ibtc.get(pc);

    PROFILE( ibtc.incTotal() );

    if (likely(next_tb)) {
#if defined(CONFIG_SOFTMMU)
        if (likely(itlb_lookup(env, pc, next_tb->page_addr[0])))
#endif
        if (likely(cpu_check_state(env, next_tb->cs_base, next_tb->flags))) {
            cpu->current_tb = next_tb;
            return next_tb->opt_ptr;
        }
    }

    PROFILE( ibtc.incMiss() );

    ibtc.setUpdate();
    return ibtc_ret_addr;
}
#else
void ibtc_update_entry(CPUArchState *env, TranslationBlock *tb) {}
void *helper_lookup_ibtc(CPUArchState *env) { return ibtc_ret_addr; }
#endif /* ENABLE_IBTC */


/*
 * CPBL (Cross-Page Block Linking)
 */
#if defined(ENABLE_CPBL)
void *helper_lookup_cpbl(CPUArchState *env)
{
    CPUState *cpu = ENV_GET_CPU(env);
    if (unlikely(cpu->tcg_exit_req != 0)) {
        cpu->tcg_exit_req = 0;
        return ibtc_ret_addr;
    }

    /* A match of 'pc', 'cs_base' and 'flags' results in a CPBL hit. Since
     * cs_base is only meaningful with x86 guest and system mode (cs_base is
     * always 0 for user-mode emulation and non-x86 guest), we only compare
     * cs_base with system mode emulation of x86 guest. */

    target_ulong pc = cpu_get_pc(env);
    TranslationBlock *next_tb = cpu->tb_jmp_cache[tb_jmp_cache_hash_func(pc)];

    PROFILE( cpu_get_cpbl(env).incTotal() );

    if (likely(next_tb && next_tb->pc == pc))
    if (likely(cpu_check_state(env, next_tb->cs_base, next_tb->flags))) {
        cpu->current_tb = next_tb;
        return next_tb->opt_ptr;
    }

    PROFILE( cpu_get_cpbl(env).incMiss() );

    return ibtc_ret_addr;
}

int helper_validate_cpbl(CPUArchState *env, target_ulong pc, int id)
{
    TranslationBlock *tb = &tbs[id];

    PROFILE( cpu_get_cpbl(env).incValidateTotal() );
    if (tb->page_addr[1] == (tb_page_addr_t)-1 &&
        likely(itlb_lookup(env, pc, tb->page_addr[0])))
        return 1;
    if (likely(itlb_lookup(env, pc + TARGET_PAGE_SIZE, tb->page_addr[1])))
        return 1;
    PROFILE( cpu_get_cpbl(env).incValidateMiss() );
    return 0;
}

#else
void *helper_lookup_cpbl(CPUArchState *env) { return ibtc_ret_addr; }
int helper_validate_cpbl(CPUArchState *env, target_ulong pc, int id) { return 0; }
#endif /* ENABLE_CPBL */


#if defined(ENABLE_LPAGE)
int lpt_reset(CPUArchState *env)
{
    if (env->opt_link == nullptr)
        return 0;
    LargePageTable &lpt = cpu_get_lpt(env);
    lpt.reset();
    return 1;
}
/*
 * lpt_add_page()
 *  Add a large page to LPT.
 */
int lpt_add_page(CPUArchState *env, target_ulong addr, target_ulong size)
{
    LargePageTable &lpt = cpu_get_lpt(env);
    lpt.insert(addr, size);
    return 1;
}

/*
 * lpt_search_page()
 *  Given an address, return 1 if this address overlaps with any tracked
 *  large page and return 0 otherwise. The large page record is NOT removed
 *  if it is found.
 */
int lpt_search_page(CPUArchState *env, target_ulong addr, target_ulong *addrp,
                    target_ulong *sizep)
{
    LargePageTable &lpt = cpu_get_lpt(env);
    return lpt.search(addr, LargePageTable::SEARCH, addrp, sizep);
}

/*
 * lpt_flush_page()
 *  Given an address, return the pte index if this address overlaps with
 *  any tracked large page and return -1 otherwise. If a large page is found,
 *  remove it from the list.
 */
int lpt_flush_page(CPUArchState *env, target_ulong addr, target_ulong *addrp,
                   target_ulong *sizep)
{
    LargePageTable &lpt = cpu_get_lpt(env);
    PROFILE( lpt.incTotal() );
    if (lpt.search(addr, LargePageTable::FLUSH, addrp, sizep))
        return 1;
    PROFILE( lpt.incMiss() );
    return 0;
}
#else
int lpt_reset(CPUArchState *env) { return 0; }
int lpt_add_page(CPUArchState *env, target_ulong addr, target_ulong size) { return 0; }
int lpt_search_page(CPUArchState *env, target_ulong addr,
                    target_ulong *addrp, target_ulong *sizep) { return 0; }
int lpt_flush_page(CPUArchState *env, target_ulong addr,
                   target_ulong *addrp, target_ulong *sizep) { return 0; }
#endif

/*
 * optimization_init()
 *  Initialize the optimization schemes.
 */
int optimization_init(CPUArchState *env)
{
    CPUState *cpu = ENV_GET_CPU(env);
    if (cpu->cpu_index == 0) {
        tbs = tcg_ctx.tb_ctx->tbs;
        if (!tbs) {
            std::cerr << __func__ << ": fatal error.\n";
            exit(0);
        }
        if (get_cpu_size() != sizeof(CPUArchState)) {
            std::cerr << "Inconsistent CPUArchState size in C and C++.\n"
                         "This may be because sizeof empty struct in C is "
                         "different with C++. Please fix this.\n";
            exit(0);
        }
    }

    /* Create a processor tracer for each env. */
    BaseTracer *Tracer = BaseTracer::CreateTracer(env);

    /* Create optimization facilities. */
    CPUOptimization *Opt = new CPUOptimization(cpu, Tracer);

    /* Make an uplink to the optimizaiton facility object. */
    env->opt_link = Opt;
    return 1;
}

/*
 * optimization_finalize()
 *  Finalize the optimization schemes.
 */
int optimization_finalize(CPUArchState *env)
{
    if (env->opt_link == nullptr)
        return 0;

    PROFILE( cpu_get_ibtc(env).dump() );
#if defined(CONFIG_SOFTMMU)
    PROFILE( cpu_get_cpbl(env).dump() );
    PROFILE( cpu_get_lpt(env).dump() );
#endif

    BaseTracer::DeleteTracer(env);
    delete (CPUOptimization *)env->opt_link;
    return 1;
}

/*
 * optimization_reset()
 *  Reset to default values of the optimizatiion schemes.
 */
int optimization_reset(CPUArchState *env, int force_flush)
{
    if (env->opt_link == nullptr)
        return 0;

    ITLB &itlb = cpu_get_itlb(env);
    IBTC &ibtc = cpu_get_ibtc(env);

    itlb.reset();
    if (force_flush)
        ibtc.reset();

    BaseTracer::ResetTracer(env);
    return 1;
}

int optimization_remove_entry(CPUArchState *env, TranslationBlock *tb)
{
    IBTC &ibtc = cpu_get_ibtc(env);
    ibtc.remove(tb);
    return 1;
}

int optimization_flush_page(CPUArchState *env, target_ulong pc)
{
#if defined(CONFIG_SOFTMMU)
    ITLB &itlb = cpu_get_itlb(env);
    itlb.flush(pc);
#else
    IBTC &ibtc = cpu_get_ibtc(env);
    ibtc.reset();
#endif
    return 1;
}

int optimization_init_tb(TranslationBlock *tb, int id)
{
    tb->id = id;
    tb->tid = -1;
    tb->mode = BLOCK_NONE;
    tb->opt_ptr = nullptr;
    tb->exec_count = 0;
    tb->patch_jmp = 0;
    tb->patch_next = 0;
    tb->jmp_pc[0] = tb->jmp_pc[1] = (target_ulong)-1;
    tb->image = nullptr;
    tb->state = nullptr;
    tb->chain = nullptr;
    return 1;
}

}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

