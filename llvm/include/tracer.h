/*
 *  (C) 2016 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __TRACE_H
#define __TRACE_H

#include <vector>
#include <iostream>
#include "qemu-types.h"
#include "optimization.h"
#include "utils.h"


/* 
 * Base processor tracer
 */
class BaseTracer {
public:
    CPUArchState *Env;

    BaseTracer(CPUArchState *env) : Env(env) {}
    virtual ~BaseTracer() {}
    virtual void reset() {}
    virtual void record(TranslationBlock *tb) {}
    virtual void dump() {}

    /* Create and return the tracer object based on LLVM_MODE. */
    static BaseTracer *CreateTracer(CPUArchState *env);

    /* Release the trace resources. */
    static void DeleteTracer(CPUArchState *env);

    /* Reset the trace resources. */
    static void ResetTracer(CPUArchState *env);
};


/*
 * Trace of a single basic block
 */
class SingleBlockTracer : public BaseTracer {
    TranslationBlock *TB;

public:
    SingleBlockTracer(CPUArchState *env);

    void record(TranslationBlock *tb) override;
};


/*
 * Trace with NET trace formation algorithm
 */
#define PROFILE_THRESHOLD 50
#define PREDICT_THRESHOLD 16
class NETTracer : public BaseTracer {
public:
    typedef std::vector<TranslationBlock *> TBVec;
    TBVec TBs;

    NETTracer(CPUArchState *env, int Mode);
    ~NETTracer();

    void reset() override;
    void record(TranslationBlock *tb) override;
    void dump() override {
        std::cerr << "Profile Threshold = " <<  PROFILE_THRESHOLD << "\n"
                  << "Predict Threshold = " <<  PREDICT_THRESHOLD << "\n";
    }
};

/* tb_get_jmp_entry - Return the address of the trampoline. */
static inline uintptr_t tb_get_jmp_entry(TranslationBlock *tb) {
    return (uintptr_t)tb->tc_ptr + tb->patch_jmp;
}
/* tb_get_jmp_next - Return the address to which the trampoline initially jumps. */
static inline uintptr_t tb_get_jmp_next(TranslationBlock *tb) {
    return (uintptr_t)tb->tc_ptr + tb->patch_next;
}
static inline SingleBlockTracer &getSingleBlockTracer(CPUArchState *env) {
    return *static_cast<SingleBlockTracer *>(cpu_get_tracer(env));
}
static inline NETTracer &getNETTracer(CPUArchState *env) {
    return *static_cast<NETTracer *>(cpu_get_tracer(env));
}

static inline void delete_image(TranslationBlock *tb)
{
#if defined(CONFIG_LLVM) && defined(CONFIG_SOFTMMU)
    delete (char *)tb->image;
    tb->image = nullptr;
#endif
}

static inline bool update_tb_mode(TranslationBlock *tb, int from, int to) {
    return Atomic<int>::testandset(&tb->mode, from, to);
}

extern "C" {
int tracer_gen_prolog(CPUArchState *env, TranslationBlock *tb);
int tracer_gen_epilog(CPUArchState *env, TranslationBlock *tb);
int tracer_gen_block(CPUArchState *env, TranslationBlock *tb);
int tracer_exec_block(CPUArchState *env, uintptr_t *next_tb, TranslationBlock *tb);
int tracer_interrupt(CPUArchState *env);
void start_trace_profiling(TranslationBlock *tb);
}

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

