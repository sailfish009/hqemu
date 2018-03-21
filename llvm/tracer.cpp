/*
 *  (C) 2016 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 *
 *   This file implements the trace/region formation algorithm.
 */


#include "utils.h"
#include "tracer.h"
#include "llvm-state.h"


ControlFlowGraph GlobalCFG;

/* The following implements routines of the C interfaces for QEMU. */
extern "C" {

/*
 * Default tracer structure.
 */
tracer_t tracer = {
    .enabled = false,
    .mode = TRANS_MODE_NONE,
    .gen_prolog = nullptr,
    .gen_epilog = nullptr,
    .gen_block  = nullptr,
    .exec_block = nullptr,
    .interrupt  = nullptr,
};

void tcg_gen_op1(TCGContext *, TCGOpcode, TCGArg);
void tcg_gen_op2(TCGContext *, TCGOpcode, TCGArg, TCGArg);

/*
 * tracer_gen_prolog()
 *  Generate the tracing prolog stub at the beginning of a basic block.
 *  Stubs of profiling and prediction are inserted to detect a NET Trace.
 */
int tracer_gen_prolog(CPUArchState *env, TranslationBlock *tb)
{
    /* Format: trace_prolog op1(usermode?), op2(emitting helper?) */
    int Mode = getTransMode();
    bool EmitHelper = Mode == TRANS_MODE_HYBRIDS || Mode == TRANS_MODE_HYBRIDM;
    tcg_gen_op2(&tcg_ctx, INDEX_op_trace_prolog, isUserTB(tb), EmitHelper);
    return 0;
}

/*
 * tracer_gen_epilog()
 *  Generate the tracing epilog stub at the end of a basic block.
 */
int tracer_gen_epilog(CPUArchState *env, TranslationBlock *tb)
{
    /* Format: trace_epilog op1(never used) */
    tcg_gen_op1(&tcg_ctx, INDEX_op_trace_epilog, 0);
    return 0;
}

int tracer_interrupt(CPUArchState *env)
{
    BaseTracer::ResetTracer(env);
    return 0;
}

/*
 * tracer_gen_block()
 *  This routine is called to optimize a single 'block' when running with the
 *  block-only mode.
 */
int tracer_gen_block(CPUArchState *env, TranslationBlock *tb)
{
    SingleBlockTracer &Tracer = getSingleBlockTracer(env);
    Tracer.record(tb);
    return 0;
}

static inline void copy_image(CPUArchState *env, TranslationBlock *tb)
{
#if defined(CONFIG_LLVM) && defined(CONFIG_SOFTMMU)
    char *p = new char[tb->size];
    for (int i = 0, e = tb->size; i != e; ++i)
        p[i] = cpu_ldub_code(env, tb->pc + i);
    tb->image = (void *)p;
#endif
}

/*
 * tracer_exec_block()
 *  This routine is called when QEMU tries to conduct block linking.
 */
int tracer_exec_block(CPUArchState *env, uintptr_t *next_tb, TranslationBlock *tb)
{
    if (*next_tb != 0) {
        /* Two linking blocks are probed. Add this link in the global CFG. */
        TranslationBlock *pred = (TranslationBlock *)(*next_tb & ~TB_EXIT_MASK);
        GlobalCFG.insertLink(pred, tb);
    }

    if (update_tb_mode(tb, BLOCK_NONE, BLOCK_ACTIVE)) {
        /* If the basic block is marked as BLOCK_NONE (i.e., just translated
         * by TCG), this is the 'first' time the tracer sees this block. 
         * Set this block to BLOCK_ACTIVE. */
        tcg_save_state(env, tb);
        copy_image(env, tb);
    } else if (update_tb_mode(tb, BLOCK_ACTIVE, BLOCK_TRACEHEAD)) {
        /* This is the 'second' time we see this block--a possible cyclic path
         * is executed. Start trace formation for NET tracers. */
        if (tracer.mode == TRANS_MODE_HYBRIDS || tracer.mode == TRANS_MODE_HYBRIDM)
            start_trace_profiling(tb);
    }

    return 0;
}

void start_trace_profiling(TranslationBlock *tb)
{
    /* Turn on trace profiling by jumping to the next instruction. */
    uintptr_t jmp_addr = tb_get_jmp_entry(tb);
#if defined(TCG_TARGET_I386)
    patch_jmp(jmp_addr, jmp_addr + 5);
#elif defined(TCG_TARGET_ARM)
    patch_jmp(jmp_addr, jmp_addr + 4);
#endif
}


/*
 * helper_NET_profile()
 *  Helper function to perform trace profiling.
 */
void helper_NET_profile(CPUArchState *env, int id)
{
    TranslationBlock *tb = &tbs[id];
    if (Atomic<int>::inc_return(&tb->exec_count) != PROFILE_THRESHOLD)
        return;

    /* We reach a profile threshold, stop trace profiling and start trace tail
     * prediction. The profiling is disabled by setting the jump directly to 
     * trace prediction stub. */
    patch_jmp(tb_get_jmp_entry(tb), tb_get_jmp_next(tb));
    env->start_trace_prediction = 1;
}

/*
 * helper_NET_predict()
 *  Helper function to perform trace prediction.
 */
void helper_NET_predict(CPUArchState *env, int id)
{
    NETTracer &Tracer = getNETTracer(env);
    Tracer.record(&tbs[id]);
}

} /* extern "C" */


#if defined(CONFIG_LLVM)
#include "llvm.h"
static inline void OptimizeBlock(CPUArchState *env, TranslationBlock *TB)
{
    auto Request = OptimizationInfo::CreateRequest(TB);
    LLVMEnv::OptimizeBlock(env, std::move(Request));
}
static inline void OptimizeTrace(CPUArchState *env, NETTracer::TBVec &TBs,
                                 int LoopHeadIdx)
{
    auto Request = OptimizationInfo::CreateRequest(TBs, LoopHeadIdx);
    LLVMEnv::OptimizeTrace(env, std::move(Request));
}
#else
static inline void OptimizeBlock(CPUArchState *, TranslationBlock *) {}
static inline void OptimizeTrace(CPUArchState *, NETTracer::TBVec &, int) {}
#endif


/*
 * BaseTracer
 */
BaseTracer *BaseTracer::CreateTracer(CPUArchState *env)
{
#if defined(CONFIG_LLVM)
    int Mode = getTransMode();
    switch (Mode) {
        case TRANS_MODE_NONE:
            return new BaseTracer(env);
        case TRANS_MODE_BLOCK:
            return new SingleBlockTracer(env);
        case TRANS_MODE_HYBRIDS:
            return new NETTracer(env, TRANS_MODE_HYBRIDS);
        case TRANS_MODE_HYBRIDM:
            return new NETTracer(env, TRANS_MODE_HYBRIDM);
        default:
            break;
    }
#endif
    return new BaseTracer(env);
}

void BaseTracer::DeleteTracer(CPUArchState *env)
{
    auto Tracer = cpu_get_tracer(env);
    if (Tracer)
        delete Tracer;
}

void BaseTracer::ResetTracer(CPUArchState *env)
{
    cpu_get_tracer(env)->reset();
}


/*
 * SingleBlockTracer
 */
SingleBlockTracer::SingleBlockTracer(CPUArchState *env) : BaseTracer(env)
{
    if (!tracer.enabled) {
        tracer.enabled = true;
        tracer.mode = TRANS_MODE_BLOCK;
        tracer.gen_block = tracer_gen_block;
        tracer.gen_prolog = tracer_gen_prolog;
        tracer.gen_epilog = tracer_gen_epilog;
    }
}

void SingleBlockTracer::record(TranslationBlock *tb)
{
    TB = tb;
    OptimizeBlock(Env, tb);
}


/*
 * NETTracer
 */
NETTracer::NETTracer(CPUArchState *env, int Mode) : BaseTracer(env)
{
    if (!tracer.enabled) {
        tracer.enabled = true;
        tracer.mode = Mode;
        tracer.gen_prolog = tracer_gen_prolog;
        tracer.gen_epilog = tracer_gen_epilog;
        tracer.exec_block = tracer_exec_block;
        tracer.interrupt = tracer_interrupt;
    }
}

NETTracer::~NETTracer()
{
    if (tracer.enabled)
        tracer.enabled = false;
}

void NETTracer::reset()
{
    TBs.clear();
    Env->start_trace_prediction = 0;
}

void NETTracer::record(TranslationBlock *tb)
{
#if defined(CONFIG_LLVM)
    /* Skip this trace if the next block is an annotated loop head and
     * is going to be included in the middle of a trace. */
    if (!TBs.empty() && TBs[0] != tb &&
        llvm_has_annotation(tb->pc, ANNOTATION_LOOP)) {
        reset();
        return;
    }
#endif

    /* The trace prediction will terminate if a cyclic path is detected.
     * (i.e., current tb has existed in the tracing butter either in the
     * head or middle of the buffer.) */
    int LoopHeadIdx = -1;

#ifdef USE_TRACETREE_ONLY
    /* We would like to have a straight-line or O-shape trace.
     * (the 6-shape trace is excluded) */
    if (!TBs.empty() && tb == TBs[0]) {
        LoopHeadIdx = 0;
        goto trace_building;
    }
#else
    for (int i = 0, e = TBs.size(); i != e; ++i) {
        if (tb == TBs[i]) {
            LoopHeadIdx = i;
            goto trace_building;
        }
    }
#endif

    TBs.push_back(tb);

    /* Stop if the maximum prediction length is reached. */
    if (TBs.size() == PREDICT_THRESHOLD)
        goto trace_building;

    return;

trace_building:
    /* If the trace is a loop with a branch to the middle of the loop body,
     * we forms two sub-traces: (1) the loop starting from the loopback to
     * the end of the trace and (2) the original trace. */
    /* NOTE: We want to find more traces so the original trace is included. */

    if (LoopHeadIdx > 0) {
        /* Loopback at the middle. The sub-trace (1) is optimized first. */
        TBVec Loop(TBs.begin() + LoopHeadIdx, TBs.end());
        if (Loop[0]->mode == BLOCK_ACTIVE)
            OptimizeTrace(Env, Loop, 0);
    }
    OptimizeTrace(Env, TBs, LoopHeadIdx);

    reset();
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
