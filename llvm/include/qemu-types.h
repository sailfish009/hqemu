/*
 *  (C) 2016 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __QEMU_TYPES_H
#define __QEMU_TYPES_H

extern "C" {
#include "cpu.h"
#include "exec/tb-hash.h"
#include "exec/exec-all.h"
#include "exec/helper-proto.h"
#include "exec/cpu_ldst.h"
#include "tcg/tcg.h"
#include "qemu/atomic.h"
#include "hqemu.h"

extern uint8_t *tb_ret_addr;
extern uint8_t *ibtc_ret_addr;

}

#ifdef inline
#undef inline
#endif

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

