/*
 *  (C) 2015 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __OPTIMIZATION_H
#define __OPTIMIZATION_H

#include <iostream>
#include <list>
#include "qemu-types.h"


extern "C" TranslationBlock *tbs;

/*
 * Instruction TLB (iTLB)
 */
#define ITLB_CACHE_BITS     (10)
#define ITLB_CACHE_SIZE     (1U << ITLB_CACHE_BITS)
#define ITLB_CACHE_MASK     (ITLB_CACHE_SIZE - 1)

class ITLB {
    struct itlb_t { tb_page_addr_t paddr; };
    itlb_t Cache[ITLB_CACHE_SIZE];

public:
    ITLB() { reset(); }
    ~ITLB() {}

    inline itlb_t &cache(target_ulong vaddr) {
        return Cache[(vaddr >> TARGET_PAGE_BITS) & ITLB_CACHE_MASK];
    }
    void reset() {
        for (unsigned i = 0; i < ITLB_CACHE_SIZE; ++i)
            Cache[i].paddr = (tb_page_addr_t)-1;
    }
    void flush(target_ulong vaddr) {
        cache(vaddr).paddr = (tb_page_addr_t)-1;
    }
    void insert(target_ulong vaddr, tb_page_addr_t paddr) {
        cache(vaddr).paddr = paddr;
    }
    tb_page_addr_t get(target_ulong vaddr) {
        return cache(vaddr).paddr;
    }
};


/*
 * Indirect Branch Target Cache (IBTC)
 */
#define IBTC_CACHE_BITS     (16)
#define IBTC_CACHE_SIZE     (1U << IBTC_CACHE_BITS)
#define IBTC_CACHE_MASK     (IBTC_CACHE_SIZE - 1)

class IBTC {
    typedef std::pair<target_ulong, TranslationBlock *> ibtc_t;
    ibtc_t Cache[IBTC_CACHE_SIZE];
    bool NeedUpdate;
    uint64_t Total;         /* Total access count */
    uint64_t Miss;          /* Miss count */

public:
    IBTC() : NeedUpdate(false), Total(0), Miss(0) { reset(); }
    ~IBTC() {}

    inline ibtc_t &cache(target_ulong pc) {
        return Cache[(pc >> 2) & IBTC_CACHE_MASK];
    }
    void reset() {
        for (unsigned i = 0; i < IBTC_CACHE_SIZE; ++i)
            Cache[i].first = (target_ulong)-1;
    }
    void remove(TranslationBlock *tb) {
        ibtc_t &c = cache(tb->pc);
        if (c.first == tb->pc)
            c.first = (target_ulong)-1;
    }
    void insert(target_ulong pc, TranslationBlock *tb) {
        cache(pc) = std::make_pair(pc, tb);
    }
    TranslationBlock *get(target_ulong pc) {
        ibtc_t &c = cache(pc);
        return (c.first == pc) ? c.second : nullptr;
    }
    void setUpdate()   { NeedUpdate = true;  }
    void resetUpdate() { NeedUpdate = false; }
    bool needUpdate()  { return NeedUpdate;  }
    inline void incTotal() { Total++; }
    inline void incMiss()  { Miss++;  }
    void dump() {
        double HitRate = (double)(Total - Miss) * 100 / Total;
        std::cerr << "\nibtc.miss = " << Miss << "/" << Total <<
                     "  (hit rate=" << HitRate << "%)\n";
    }
};

/*
 * Cross-Page Block Linking (CPBL)
 */
class CPBL {
    uint64_t Total;             /* Total access count */
    uint64_t Miss;              /* Miss count */
    uint64_t ValidateTotal;     /* Total validation count  */
    uint64_t ValidateMiss;      /* Miss validation count */
public:
    CPBL() : Total(0), Miss(0), ValidateTotal(0), ValidateMiss(0) {}

    inline void incTotal()  { Total++; }
    inline void incMiss()   { Miss++; }
    inline void incValidateTotal() { ValidateTotal++; }
    inline void incValidateMiss()  { ValidateMiss++;  }
    void dump() {
        double HitRate = (double)(Total - Miss) * 100 / Total;
        double HitRate2 = (double)(ValidateTotal - ValidateMiss) * 100 / Total;
        std::cerr << "cpbl.miss = " << Miss << "/" << Total << 
                     "  (hit rate=" << HitRate << "%)\n" <<
                     "validate.miss = " << ValidateMiss << "/" << ValidateTotal <<
                     "  (hit rate=" << HitRate2 << "%)\n";
    }
};

/*
 * Large Page Table
 *
 * This handling is to track every large page created by the guest system.
 * Once a `possibly' large page is invalidated, do a search with the tracked
 * pages to determine if it is really a large page invalidation. If it cannot
 * be found, this is a false alert and we can fall back to the default-size
 * page flushing. Otherwise, SoftTLB, IBTC/CPBL optimization, etc. are
 * partial or full cleanup due to the true large page flushing.
 */
#define MAX_NUM_LARGEPAGE   (1024)

class LargePageTable {
    typedef std::pair<target_ulong, target_ulong> PTE;
    typedef std::list<PTE> PTEList;
    PTEList Used;
    PTEList Free;
    CPUState *CS;
    uint64_t Total;
    uint64_t Miss;

public:
    LargePageTable(CPUState *cpu) : Total(0), Miss(0) {
        CS = cpu;
        Used.clear();
        Free.resize(MAX_NUM_LARGEPAGE);
    }
    ~LargePageTable() {}

    enum {
        SEARCH = 0,
        FLUSH,
    };

    void reset() {
        Free.splice(Free.end(), Used);
    }
    void remove(PTEList::iterator I) {
        Free.splice(Free.begin(), Used, I);
    }
    void allocate(PTE pte) {
        /* If the free list is empty, we need to clear softtlb by calling
         * tlb_flush() which will then invoke LTP::reset() to clear LPT. */
        if (Free.empty())
            tlb_flush(CS, 0);
        Free.front() = pte;
        Used.splice(Used.begin(), Free, Free.begin());
    }
    void insert(target_ulong addr, target_ulong size) {
        for (PTEList::iterator I = Used.begin(), E = Used.end(); I != E; ++I) {
            if (I->first == (addr & I->second)) {
                Used.splice(Used.begin(), Used, I);
                return;
            }
        }
        target_ulong mask = ~(size - 1);
        allocate(PTE(addr & mask, mask));
    }
    bool search(target_ulong addr, bool mode, target_ulong *addrp,
                target_ulong *sizep) {
        for (PTEList::iterator I = Used.begin(), E = Used.end(); I != E; ++I) {
            if (I->first != (addr & I->second))
                continue;
            *addrp = I->first;
            *sizep = ~I->second + 1;
            if (mode == FLUSH)
                remove(I);
            return true;
        }
        return false;
    }
    void incTotal() { Total++; }
    void incMiss()  { Miss++;  }
    void dump() {
        double Rate = (double)(Total - Miss) * 100 / Total;
        std::cerr << "lpt.miss = " << Miss << "/" << Total << 
                     " (false flushing=" << Rate << "% #pages=" <<
                     Used.size() << ")\n";
    }
};


class BaseTracer;

struct CPUOptimization {
    CPUOptimization(CPUState *cpu, BaseTracer *tracer)
        : lpt(LargePageTable(cpu)), pt(tracer) {}

    ITLB itlb;          /* instruction TLB */
    IBTC ibtc;          /* indirect branch target cache */
    CPBL cpbl;          /* cross-page block linking */
    LargePageTable lpt; /* large page handling */
    BaseTracer *pt;     /* processor tracer */
};


static inline int isUserTB(TranslationBlock *tb) {
    int is_user = 1;
#if defined(CONFIG_SOFTMMU)
#if defined(TARGET_ALPHA)
    is_user = (tb->flags & TB_FLAGS_USER_MODE);
#elif defined(TARGET_AARCH64)
    is_user = (ARM_TBFLAG_AA64_EL(tb->flags) == 0);
#elif defined(TARGET_ARM)
    is_user = (ARM_TBFLAG_PRIV(tb->flags) == 0);
#elif defined(TARGET_I386)
    is_user = ((tb->flags >> HF_CPL_SHIFT) & 3) == 3;
#elif defined(TARGET_MIPS)
    is_user = (tb->flags & MIPS_HFLAG_UM);
#elif defined(TARGET_PPC)
    is_user = ((tb->flags >> MSR_PR) & 1);
#else
#error "unsupported processor type"
#endif
#endif
    return is_user;
}

static inline ITLB &cpu_get_itlb(CPUArchState *env) {
    return ((CPUOptimization *)env->opt_link)->itlb;
}
static inline IBTC &cpu_get_ibtc(CPUArchState *env) {
    return ((CPUOptimization *)env->opt_link)->ibtc;
}
static inline CPBL &cpu_get_cpbl(CPUArchState *env) {
    return ((CPUOptimization *)env->opt_link)->cpbl;
}
static inline LargePageTable &cpu_get_lpt(CPUArchState *env) {
    return ((CPUOptimization *)env->opt_link)->lpt;
}
static inline BaseTracer *cpu_get_tracer(CPUArchState *env) {
    return ((CPUOptimization *)env->opt_link)->pt;
}

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

