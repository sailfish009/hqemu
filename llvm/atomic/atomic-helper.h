/*
 * Copyright (C) 2010 Parallel Processing Institute (PPI), Fudan Univ.
 *  <http://ppi.fudan.edu.cn/system_research_group>
 *
 * Authors:
 *  Zhaoguo Wang    <zgwang@fudan.edu.cn>
 *  Yufei Chen      <chenyufei@fudan.edu.cn>
 *  Ran Liu         <naruilone@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "config-target.h"

#ifdef CONFIG_COREMU

#if defined(TARGET_I386)
#define __GEN_HEADER(type) \
DEF_HELPER_3(atomic_inc##type, void, env, tl, int)                \
DEF_HELPER_4(xchg##type, void, env, tl, int, int)                 \
DEF_HELPER_4(atomic_op##type, void, env, tl, tl, int)             \
DEF_HELPER_4(atomic_xadd##type, void, env, tl, int, int)          \
DEF_HELPER_4(atomic_cmpxchg##type, void, env, tl, int, int)       \
DEF_HELPER_2(atomic_not##type, void, env, tl)                     \
DEF_HELPER_2(atomic_neg##type, void, env, tl)

__GEN_HEADER(b)
__GEN_HEADER(w)
__GEN_HEADER(l)
#ifdef TARGET_X86_64
__GEN_HEADER(q)
#endif

DEF_HELPER_2(atomic_cmpxchg8b, void, env, tl)
DEF_HELPER_2(atomic_cmpxchg16b, void, env, tl)

DEF_HELPER_4(atomic_bts, void, env, tl, tl, int)
DEF_HELPER_4(atomic_btr, void, env, tl, tl, int)
DEF_HELPER_4(atomic_btc, void, env, tl, tl, int)

/* fence */
DEF_HELPER_1(fence, void, env)

#elif defined(TARGET_ARM)
#define __GEN_HEADER(type) \
DEF_HELPER_3(load_exclusive##type, void, env, i32, i32)           \
DEF_HELPER_4(store_exclusive##type, void, env, i32, i32, i32)

__GEN_HEADER(b)
__GEN_HEADER(w)
__GEN_HEADER(l)
__GEN_HEADER(q)

DEF_HELPER_1(clear_exclusive, void, env)

DEF_HELPER_4(swpb, void, env, i32, i32, i32)
DEF_HELPER_4(swp, void, env, i32, i32, i32)
#else
#error "unsupported processor type"
#endif

#endif

