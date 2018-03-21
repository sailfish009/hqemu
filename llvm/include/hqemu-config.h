/*
 *  (C) 2016 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __HQEMU_CONFIG_H
#define __HQEMU_CONFIG_H


#define PACKAGE_NAME "HQEMU"
#define PACKAGE_VERSION_MAJOR "2.5"
#define PACKAGE_VERSION_MINOR "1"

#define ENABLE_IBTC
#define ENABLE_CPBL
#define ENABLE_LPAGE
#define ENABLE_PASSES
#define ENABLE_MCJIT
//#define ENABLE_TLBVERSION
//#define ENALBE_CPU_PROFILE
//#define USE_TRACETREE_ONLY


#if defined(CONFIG_USER_ONLY)
#  define ENABLE_TCG_VECTOR
#  define GUEST_BASE guest_base
#else
#  define GUEST_BASE (0UL)
#endif

#if defined(ENABLE_TLBVERSION)
#  if defined(ALIGNED_ONLY)
#    undef ENABLE_TLBVERSION
#  elif HOST_LONG_BITS == 64 && TARGET_LONG_BITS == 32 && defined(HOST_X86_64)
#    define ENABLE_TLBVERSION_EXT
#  endif
#endif

#ifndef ENABLE_TLBVERSION
#  define TLB_INVALID_SHIFT   3
#  define TLB_NOTDIRTY_SHIFT  4
#  define TLB_MMIO_SHIFT      5
#  define TLB_VERSION_BITS    0
#  define TLB_VERSION_MASK    0
#  define TLB_VERSION_SHIFT   (0)
#  define tlb_version(__env)  0
typedef target_ulong tlbaddr_t;
#elif defined(ENABLE_TLBVERSION_EXT)
#  define TLB_INVALID_SHIFT   3
#  define TLB_NOTDIRTY_SHIFT  4
#  define TLB_MMIO_SHIFT      5
#  define TLB_VERSION_BITS    32
#  define TLB_VERSION_SIZE    (1UL << TLB_VERSION_BITS)
#  define TLB_VERSION_MASK    (0xFFFFFFFF00000000UL)
#  define TLB_VERSION_SHIFT   (32)
#  define tlb_version(__env)  (__env->tlb_version)
typedef unsigned long tlbaddr_t;
#else
#  define TLB_INVALID_SHIFT   (TARGET_PAGE_BITS - 3)
#  define TLB_NOTDIRTY_SHIFT  (TARGET_PAGE_BITS - 2)
#  define TLB_MMIO_SHIFT      (TARGET_PAGE_BITS - 1)
#  define TLB_VERSION_BITS    (TARGET_PAGE_BITS - 3)
#  define TLB_VERSION_SIZE    (1 << TLB_VERSION_BITS)
#  define TLB_VERSION_MASK    (TLB_VERSION_SIZE - 1)
#  define TLB_VERSION_SHIFT   (0)
#  define tlb_version(__env)  (__env->tlb_version)
typedef target_ulong tlbaddr_t;
#endif


typedef int BlockID;
typedef int TraceID;
#define BUILD_NONE  ((uint16_t)0)
#define BUILD_TCG   ((uint16_t)1 << 0)
#define BUILD_LLVM  ((uint16_t)1 << 1)

#define CPU_OPTIMIZATION_COMMON \
    unsigned long sp;           \
    void *opt_link;             \
    uint16_t build_mode;        \
    int start_trace_prediction; \
    uintptr_t image_base;       \
    uint32_t restore_val;       \


#define TB_OPTIMIZATION_COMMON                                     \
    BlockID id;                                                    \
    TraceID tid;            /* trace id */                         \
    int mode;               /* current state */                    \
    void *opt_ptr;          /* pointer to the optimized code */    \
    int exec_count;         /* trace profile execution count */    \
    uint16_t patch_jmp;     /* offset of trace trampoline */       \
    uint16_t patch_next;    /* offset of trace prediction stub */  \
    target_ulong jmp_pc[2]; /* pc of the succeeding blocks */      \
    void *image;                                                   \
    void *state;                                                   \
    void *chain;


enum {
    BLOCK_NONE = 0,
    BLOCK_ACTIVE,
    BLOCK_TRACEHEAD,
    BLOCK_INPROGRESS,
    BLOCK_OPTIMIZED,
    BLOCK_INVALID,
};

enum {
    TRANS_MODE_NONE = 0,
    TRANS_MODE_BLOCK,
    TRANS_MODE_HYBRIDS,
    TRANS_MODE_HYBRIDM,
    TRANS_MODE_INVALID,
};

/* getTransMode - Parse translation mode from env-variable LLVM_MODE. */
static inline int getTransMode(void) {
    char *p = getenv("LLVM_MODE");
    if (p == NULL)             return TRANS_MODE_HYBRIDM;
    if (!strcmp(p, "hybridm")) return TRANS_MODE_HYBRIDM;
    if (!strcmp(p, "hybrids")) return TRANS_MODE_HYBRIDS;
    if (!strcmp(p, "block"))   return TRANS_MODE_BLOCK;
    if (!strcmp(p, "none"))    return TRANS_MODE_NONE;
    return TRANS_MODE_INVALID;
}

/* Annotation/attribute for traces. */
enum {
    A_None   = ((uint32_t)0),
    A_SetCC  = ((uint32_t)1 << 0),
    A_NoSIMDization = ((uint32_t)1 << 1),
};

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

