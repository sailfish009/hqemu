/*
 *  Software MMU support for LLVM
 */

#if DATA_SIZE == 1
# define llvm_le_ld_name  glue(glue(llvm_ret_ld, USUFFIX), MMUSUFFIX)
# define llvm_be_ld_name  llvm_le_ld_name
# define llvm_le_lds_name glue(glue(llvm_ret_ld, SSUFFIX), MMUSUFFIX)
# define llvm_be_lds_name llvm_le_lds_name
# define llvm_le_st_name  glue(glue(llvm_ret_st, SUFFIX), MMUSUFFIX)
# define llvm_be_st_name  llvm_le_st_name
#else
# define llvm_le_ld_name  glue(glue(llvm_le_ld, USUFFIX), MMUSUFFIX)
# define llvm_be_ld_name  glue(glue(llvm_be_ld, USUFFIX), MMUSUFFIX)
# define llvm_le_lds_name glue(glue(llvm_le_ld, SSUFFIX), MMUSUFFIX)
# define llvm_be_lds_name glue(glue(llvm_be_ld, SSUFFIX), MMUSUFFIX)
# define llvm_le_st_name  glue(glue(llvm_le_st, SUFFIX), MMUSUFFIX)
# define llvm_be_st_name  glue(glue(llvm_be_st, SUFFIX), MMUSUFFIX)
#endif

#ifdef TARGET_WORDS_BIGENDIAN
# define llvm_te_ld_name  llvm_be_ld_name
# define llvm_te_st_name  llvm_be_st_name
#else
# define llvm_te_ld_name  llvm_le_ld_name
# define llvm_te_st_name  llvm_le_st_name
#endif


#ifndef SOFTMMU_CODE_ACCESS
WORD_TYPE llvm_le_ld_name(CPUArchState *env, target_ulong addr, TCGMemOpIdx oi)
{
    unsigned mmu_idx = get_mmuidx((uint16_t)oi);
    int index = (addr >> TARGET_PAGE_BITS) & (CPU_TLB_SIZE - 1);
    tlbaddr_t tlb_addr = env->tlb_table[mmu_idx][index].ADDR_READ;
    uintptr_t haddr;
    DATA_TYPE res;
    uintptr_t retaddr;

    env->restore_val = oi >> 16;

    /* Adjust the given return address.  */
    retaddr = GETPC();

    /* If the TLB entry is for a different page, reload and try again.  */
    if (page_val(addr, env) != (tlb_addr & TLB_NONIO_MASK)) {
        if ((addr & (DATA_SIZE - 1)) != 0
            && (get_memop(oi) & MO_AMASK) == MO_ALIGN) {
            cpu_unaligned_access(ENV_GET_CPU(env), addr, READ_ACCESS_TYPE,
                                 mmu_idx, retaddr);
        }
        if (!VICTIM_TLB_HIT(ADDR_READ)) {
            tlb_fill(ENV_GET_CPU(env), addr, READ_ACCESS_TYPE,
                     mmu_idx, retaddr);
        }
        tlb_addr = env->tlb_table[mmu_idx][index].ADDR_READ;
    }

    /* Handle an IO access.  */
    if (unlikely(tlb_addr & TLB_IO_MASK)) {
        CPUIOTLBEntry *iotlbentry;
        if ((addr & (DATA_SIZE - 1)) != 0) {
            goto do_unaligned_access;
        }
        iotlbentry = &env->iotlb[mmu_idx][index];

        /* ??? Note that the io helpers always read data in the target
           byte ordering.  We should push the LE/BE request down into io.  */
        res = glue(io_read, SUFFIX)(env, iotlbentry, addr, retaddr);
        res = TGT_LE(res);
        return res;
    }

    /* Handle slow unaligned access (it spans two pages or IO).  */
    if (DATA_SIZE > 1
        && unlikely((addr & ~TARGET_PAGE_MASK) + DATA_SIZE - 1
                    >= TARGET_PAGE_SIZE)) {
        target_ulong addr1, addr2;
        DATA_TYPE res1, res2;
        unsigned shift;
    do_unaligned_access:
        if ((get_memop(oi) & MO_AMASK) == MO_ALIGN) {
            cpu_unaligned_access(ENV_GET_CPU(env), addr, READ_ACCESS_TYPE,
                                 mmu_idx, retaddr);
        }
        addr1 = addr & ~(DATA_SIZE - 1);
        addr2 = addr1 + DATA_SIZE;
        /* Note the adjustment at the beginning of the function.
           Undo that for the recursion.  */
        res1 = helper_le_ld_name(env, addr1, oi, retaddr + GETPC_ADJ);
        res2 = helper_le_ld_name(env, addr2, oi, retaddr + GETPC_ADJ);
        shift = (addr & (DATA_SIZE - 1)) * 8;

        /* Little-endian combine.  */
        res = (res1 >> shift) | (res2 << ((DATA_SIZE * 8) - shift));
        return res;
    }

    /* Handle aligned access or unaligned access in the same page.  */
    if ((addr & (DATA_SIZE - 1)) != 0
        && (get_memop(oi) & MO_AMASK) == MO_ALIGN) {
        cpu_unaligned_access(ENV_GET_CPU(env), addr, READ_ACCESS_TYPE,
                             mmu_idx, retaddr);
    }

    haddr = addr + env->tlb_table[mmu_idx][index].addend;
#if DATA_SIZE == 1
    res = glue(glue(ld, LSUFFIX), _p)((uint8_t *)haddr);
#else
    res = glue(glue(ld, LSUFFIX), _le_p)((uint8_t *)haddr);
#endif
    return res;
}

#if DATA_SIZE > 1
WORD_TYPE llvm_be_ld_name(CPUArchState *env, target_ulong addr, TCGMemOpIdx oi)
{
    unsigned mmu_idx = get_mmuidx((uint16_t)oi);
    int index = (addr >> TARGET_PAGE_BITS) & (CPU_TLB_SIZE - 1);
    tlbaddr_t tlb_addr = env->tlb_table[mmu_idx][index].ADDR_READ;
    uintptr_t haddr;
    DATA_TYPE res;
    uintptr_t retaddr;

    env->restore_val = oi >> 16;

    /* Adjust the given return address.  */
    retaddr = GETPC();

    /* If the TLB entry is for a different page, reload and try again.  */
    if (page_val(addr, env) != (tlb_addr & TLB_NONIO_MASK)) {
        if ((addr & (DATA_SIZE - 1)) != 0
            && (get_memop(oi) & MO_AMASK) == MO_ALIGN) {
            cpu_unaligned_access(ENV_GET_CPU(env), addr, READ_ACCESS_TYPE,
                                 mmu_idx, retaddr);
        }
        if (!VICTIM_TLB_HIT(ADDR_READ)) {
            tlb_fill(ENV_GET_CPU(env), addr, READ_ACCESS_TYPE,
                     mmu_idx, retaddr);
        }
        tlb_addr = env->tlb_table[mmu_idx][index].ADDR_READ;
    }

    /* Handle an IO access.  */
    if (unlikely(tlb_addr & TLB_IO_MASK)) {
        CPUIOTLBEntry *iotlbentry;
        if ((addr & (DATA_SIZE - 1)) != 0) {
            goto do_unaligned_access;
        }
        iotlbentry = &env->iotlb[mmu_idx][index];

        /* ??? Note that the io helpers always read data in the target
           byte ordering.  We should push the LE/BE request down into io.  */
        res = glue(io_read, SUFFIX)(env, iotlbentry, addr, retaddr);
        res = TGT_BE(res);
        return res;
    }

    /* Handle slow unaligned access (it spans two pages or IO).  */
    if (DATA_SIZE > 1
        && unlikely((addr & ~TARGET_PAGE_MASK) + DATA_SIZE - 1
                    >= TARGET_PAGE_SIZE)) {
        target_ulong addr1, addr2;
        DATA_TYPE res1, res2;
        unsigned shift;
    do_unaligned_access:
        if ((get_memop(oi) & MO_AMASK) == MO_ALIGN) {
            cpu_unaligned_access(ENV_GET_CPU(env), addr, READ_ACCESS_TYPE,
                                 mmu_idx, retaddr);
        }
        addr1 = addr & ~(DATA_SIZE - 1);
        addr2 = addr1 + DATA_SIZE;
        /* Note the adjustment at the beginning of the function.
           Undo that for the recursion.  */
        res1 = helper_be_ld_name(env, addr1, oi, retaddr + GETPC_ADJ);
        res2 = helper_be_ld_name(env, addr2, oi, retaddr + GETPC_ADJ);
        shift = (addr & (DATA_SIZE - 1)) * 8;

        /* Big-endian combine.  */
        res = (res1 << shift) | (res2 >> ((DATA_SIZE * 8) - shift));
        return res;
    }

    /* Handle aligned access or unaligned access in the same page.  */
    if ((addr & (DATA_SIZE - 1)) != 0
        && (get_memop(oi) & MO_AMASK) == MO_ALIGN) {
        cpu_unaligned_access(ENV_GET_CPU(env), addr, READ_ACCESS_TYPE,
                             mmu_idx, retaddr);
    }

    haddr = addr + env->tlb_table[mmu_idx][index].addend;
    res = glue(glue(ld, LSUFFIX), _be_p)((uint8_t *)haddr);
    return res;
}
#endif /* DATA_SIZE > 1 */

/* Provide signed versions of the load routines as well.  We can of course
   avoid this for 64-bit data, or for 32-bit data on 32-bit host.  */
#if DATA_SIZE * 8 < TCG_TARGET_REG_BITS
WORD_TYPE llvm_le_lds_name(CPUArchState *env, target_ulong addr, TCGMemOpIdx oi)
{
    env->restore_val = oi >> 16;
    return (SDATA_TYPE)helper_le_ld_name(env, addr, (uint16_t)oi, GETRA());
}

# if DATA_SIZE > 1
WORD_TYPE llvm_be_lds_name(CPUArchState *env, target_ulong addr, TCGMemOpIdx oi)
{
    env->restore_val = oi >> 16;
    return (SDATA_TYPE)helper_be_ld_name(env, addr, (uint16_t)oi, GETRA());
}
# endif
#endif

void llvm_le_st_name(CPUArchState *env, target_ulong addr, DATA_TYPE val,
                     TCGMemOpIdx oi)
{
    unsigned mmu_idx = get_mmuidx((uint16_t)oi);
    int index = (addr >> TARGET_PAGE_BITS) & (CPU_TLB_SIZE - 1);
    tlbaddr_t tlb_addr = env->tlb_table[mmu_idx][index].addr_write;
    uintptr_t haddr;
    uintptr_t retaddr;

    env->restore_val = oi >> 16;

    /* Adjust the given return address.  */
    retaddr = GETPC();

    /* If the TLB entry is for a different page, reload and try again.  */
    if (page_val(addr, env) != (tlb_addr & TLB_NONIO_MASK)) {
        if ((addr & (DATA_SIZE - 1)) != 0
            && (get_memop(oi) & MO_AMASK) == MO_ALIGN) {
            cpu_unaligned_access(ENV_GET_CPU(env), addr, MMU_DATA_STORE,
                                 mmu_idx, retaddr);
        }
        if (!VICTIM_TLB_HIT(addr_write)) {
            tlb_fill(ENV_GET_CPU(env), addr, MMU_DATA_STORE, mmu_idx, retaddr);
        }
        tlb_addr = env->tlb_table[mmu_idx][index].addr_write;
    }

    /* Handle an IO access.  */
    if (unlikely(tlb_addr & TLB_IO_MASK)) {
        CPUIOTLBEntry *iotlbentry;
        if ((addr & (DATA_SIZE - 1)) != 0) {
            goto do_unaligned_access;
        }
        iotlbentry = &env->iotlb[mmu_idx][index];

        /* ??? Note that the io helpers always read data in the target
           byte ordering.  We should push the LE/BE request down into io.  */
        val = TGT_LE(val);
        glue(io_write, SUFFIX)(env, iotlbentry, val, addr, retaddr);
        return;
    }

    /* Handle slow unaligned access (it spans two pages or IO).  */
    if (DATA_SIZE > 1
        && unlikely((addr & ~TARGET_PAGE_MASK) + DATA_SIZE - 1
                     >= TARGET_PAGE_SIZE)) {
        int i;
    do_unaligned_access:
        if ((get_memop(oi) & MO_AMASK) == MO_ALIGN) {
            cpu_unaligned_access(ENV_GET_CPU(env), addr, MMU_DATA_STORE,
                                 mmu_idx, retaddr);
        }
        /* XXX: not efficient, but simple */
        /* Note: relies on the fact that tlb_fill() does not remove the
         * previous page from the TLB cache.  */
        for (i = DATA_SIZE - 1; i >= 0; i--) {
            /* Little-endian extract.  */
            uint8_t val8 = val >> (i * 8);
            /* Note the adjustment at the beginning of the function.
               Undo that for the recursion.  */
            glue(helper_ret_stb, MMUSUFFIX)(env, addr + i, val8,
                                            oi, retaddr + GETPC_ADJ);
        }
        return;
    }

    /* Handle aligned access or unaligned access in the same page.  */
    if ((addr & (DATA_SIZE - 1)) != 0
        && (get_memop(oi) & MO_AMASK) == MO_ALIGN) {
        cpu_unaligned_access(ENV_GET_CPU(env), addr, MMU_DATA_STORE,
                             mmu_idx, retaddr);
    }

    haddr = addr + env->tlb_table[mmu_idx][index].addend;
#if DATA_SIZE == 1
    glue(glue(st, SUFFIX), _p)((uint8_t *)haddr, val);
#else
    glue(glue(st, SUFFIX), _le_p)((uint8_t *)haddr, val);
#endif
}

#if DATA_SIZE > 1
void llvm_be_st_name(CPUArchState *env, target_ulong addr, DATA_TYPE val,
                     TCGMemOpIdx oi)
{
    unsigned mmu_idx = get_mmuidx((uint16_t)oi);
    int index = (addr >> TARGET_PAGE_BITS) & (CPU_TLB_SIZE - 1);
    tlbaddr_t tlb_addr = env->tlb_table[mmu_idx][index].addr_write;
    uintptr_t haddr;
    uintptr_t retaddr;

    env->restore_val = oi >> 16;

    /* Adjust the given return address.  */
    retaddr = GETPC();

    /* If the TLB entry is for a different page, reload and try again.  */
    if (page_val(addr, env) != (tlb_addr & TLB_NONIO_MASK)) {
        if ((addr & (DATA_SIZE - 1)) != 0
            && (get_memop(oi) & MO_AMASK) == MO_ALIGN) {
            cpu_unaligned_access(ENV_GET_CPU(env), addr, MMU_DATA_STORE,
                                 mmu_idx, retaddr);
        }
        if (!VICTIM_TLB_HIT(addr_write)) {
            tlb_fill(ENV_GET_CPU(env), addr, MMU_DATA_STORE, mmu_idx, retaddr);
        }
        tlb_addr = env->tlb_table[mmu_idx][index].addr_write;
    }

    /* Handle an IO access.  */
    if (unlikely(tlb_addr & TLB_IO_MASK)) {
        CPUIOTLBEntry *iotlbentry;
        if ((addr & (DATA_SIZE - 1)) != 0) {
            goto do_unaligned_access;
        }
        iotlbentry = &env->iotlb[mmu_idx][index];

        /* ??? Note that the io helpers always read data in the target
           byte ordering.  We should push the LE/BE request down into io.  */
        val = TGT_BE(val);
        glue(io_write, SUFFIX)(env, iotlbentry, val, addr, retaddr);
        return;
    }

    /* Handle slow unaligned access (it spans two pages or IO).  */
    if (DATA_SIZE > 1
        && unlikely((addr & ~TARGET_PAGE_MASK) + DATA_SIZE - 1
                     >= TARGET_PAGE_SIZE)) {
        int i;
    do_unaligned_access:
        if ((get_memop(oi) & MO_AMASK) == MO_ALIGN) {
            cpu_unaligned_access(ENV_GET_CPU(env), addr, MMU_DATA_STORE,
                                 mmu_idx, retaddr);
        }
        /* XXX: not efficient, but simple */
        /* Note: relies on the fact that tlb_fill() does not remove the
         * previous page from the TLB cache.  */
        for (i = DATA_SIZE - 1; i >= 0; i--) {
            /* Big-endian extract.  */
            uint8_t val8 = val >> (((DATA_SIZE - 1) * 8) - (i * 8));
            /* Note the adjustment at the beginning of the function.
               Undo that for the recursion.  */
            glue(helper_ret_stb, MMUSUFFIX)(env, addr + i, val8,
                                            oi, retaddr + GETPC_ADJ);
        }
        return;
    }

    /* Handle aligned access or unaligned access in the same page.  */
    if ((addr & (DATA_SIZE - 1)) != 0
        && (get_memop(oi) & MO_AMASK) == MO_ALIGN) {
        cpu_unaligned_access(ENV_GET_CPU(env), addr, MMU_DATA_STORE,
                             mmu_idx, retaddr);
    }

    haddr = addr + env->tlb_table[mmu_idx][index].addend;
    glue(glue(st, SUFFIX), _be_p)((uint8_t *)haddr, val);
}
#endif /* DATA_SIZE > 1 */

#endif /* !defined(SOFTMMU_CODE_ACCESS) */

#undef llvm_le_ld_name
#undef llvm_be_ld_name
#undef llvm_le_lds_name
#undef llvm_be_lds_name
#undef llvm_le_st_name
#undef llvm_be_st_name
#undef llvm_te_ld_name
#undef llvm_te_st_name
