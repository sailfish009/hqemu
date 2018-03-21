/*
 * QEMU float support
 *
 * Derived from SoftFloat.
 */

/*============================================================================

This C header file is part of the SoftFloat IEC/IEEE Floating-point Arithmetic
Package, Release 2b.

Written by John R. Hauser.  This work was made possible in part by the
International Computer Science Institute, located at Suite 600, 1947 Center
Street, Berkeley, California 94704.  Funding was partially provided by the
National Science Foundation under grant MIP-9311980.  The original version
of this code was written as part of a project to build a fixed-point vector
processor in collaboration with the University of California at Berkeley,
overseen by Profs. Nelson Morgan and John Wawrzynek.  More information
is available through the Web page `http://www.cs.berkeley.edu/~jhauser/
arithmetic/SoftFloat.html'.

THIS SOFTWARE IS DISTRIBUTED AS IS, FOR FREE.  Although reasonable effort has
been made to avoid it, THIS SOFTWARE MAY CONTAIN FAULTS THAT WILL AT TIMES
RESULT IN INCORRECT BEHAVIOR.  USE OF THIS SOFTWARE IS RESTRICTED TO PERSONS
AND ORGANIZATIONS WHO CAN AND WILL TAKE FULL RESPONSIBILITY FOR ALL LOSSES,
COSTS, OR OTHER PROBLEMS THEY INCUR DUE TO THE SOFTWARE, AND WHO FURTHERMORE
EFFECTIVELY INDEMNIFY JOHN HAUSER AND THE INTERNATIONAL COMPUTER SCIENCE
INSTITUTE (possibly via similar legal warning) AGAINST ALL LOSSES, COSTS, OR
OTHER PROBLEMS INCURRED BY THEIR CUSTOMERS AND CLIENTS DUE TO THE SOFTWARE.

Derivative works are acceptable, even for commercial purposes, so long as
(1) the source code for the derivative work includes prominent notice that
the work is derivative, and (2) the source code includes prominent notice with
these four paragraphs for those parts of this code that are retained.

=============================================================================*/

#ifndef SOFTFLOAT_NATIVE_H
#define SOFTFLOAT_NATIVE_H

#include <math.h>
#include "fpu/softfloat-native-def.h"

typedef union {
   float32 f;
   int32_t i;
   uint32_t u;
   float s;
} llvm_float32;

typedef union {
   float64 f;
   int64_t i;
   uint64_t u;
   double d;
} llvm_float64;

#ifdef float32_val
#undef float32_val
#endif
#ifdef float64_val
#undef float64_val
#endif

#define float32_val(x)  ((llvm_float32)(x)).f
#define float64_val(x)  ((llvm_float64)(x)).f
#define lfloat(x)       ((llvm_float32)(x)).s
#define ldouble(x)      ((llvm_float64)(x)).d

#define DEF_HELPER(name) { (void *)llvm_##name, "llvm_"#name }
static TCGHelperInfo native_fpu_helpers[] = {
    DEF_HELPER(int32_to_float32),
    DEF_HELPER(int32_to_float64),
    DEF_HELPER(uint32_to_float32),
    DEF_HELPER(uint32_to_float64),
    DEF_HELPER(int64_to_float32),
    DEF_HELPER(uint64_to_float32),
    DEF_HELPER(int64_to_float64),
    DEF_HELPER(uint64_to_float64),
    DEF_HELPER(float32_to_int32),
    DEF_HELPER(float32_to_int64),
    DEF_HELPER(float32_to_float64),
    DEF_HELPER(float32_add),
    DEF_HELPER(float32_sub),
    DEF_HELPER(float32_mul),
    DEF_HELPER(float32_div),
    DEF_HELPER(float32_rem),
    DEF_HELPER(float32_sqrt),
    DEF_HELPER(float32_abs),
    DEF_HELPER(float32_chs),
    DEF_HELPER(float64_to_int32),
    DEF_HELPER(float64_to_int64),
    DEF_HELPER(float64_to_float32),
    DEF_HELPER(float64_add),
    DEF_HELPER(float64_sub),
    DEF_HELPER(float64_mul),
    DEF_HELPER(float64_div),
    DEF_HELPER(float64_rem),
    DEF_HELPER(float64_sqrt),
    DEF_HELPER(float64_abs),
    DEF_HELPER(float64_chs),

    DEF_HELPER(float32_muladd),
#if 0
    DEF_HELPER(float32_to_int32_round_to_zero),
    DEF_HELPER(float32_to_int64_round_to_zero),
    DEF_HELPER(float32_round_to_int),
    DEF_HELPER(float32_eq),
    DEF_HELPER(float32_le),
    DEF_HELPER(float32_lt),
    DEF_HELPER(float32_unordered),
    DEF_HELPER(float64_to_int32_round_to_zero),
    DEF_HELPER(float64_to_int64_round_to_zero),
    DEF_HELPER(float64_round_to_int),
    DEF_HELPER(float64_trunc_to_int),
    DEF_HELPER(float64_eq),
    DEF_HELPER(float64_le),
    DEF_HELPER(float64_lt),
    DEF_HELPER(float64_unordered),
#endif
};
#undef DEF_HELPER

int num_native_fpu_helpers(void)
{
    return ARRAY_SIZE(native_fpu_helpers);
}

void *get_native_fpu_helpers(void)
{
    return native_fpu_helpers;
}

/* XXX: this code implements the x86 behaviour, not the IEEE one.  */
#if TCG_TARGET_REG_BITS == 32
static inline int32 long_to_int32(long a)
{
    return a;
}
#else
static inline int32 long_to_int32(long a)
{
    if (a != (int32_t)a)
        a = 0x80000000;
    return a;
}
#endif

/*----------------------------------------------------------------------------
| Software IEC/IEEE integer-to-floating-point conversion routines.
*----------------------------------------------------------------------------*/
float32 llvm_int32_to_float32(int32_t v)   { return float32_val((float)v);  }
float64 llvm_int32_to_float64(int32_t v)   { return float64_val((double)v); }
float32 llvm_uint32_to_float32(uint32_t v) { return float32_val((float)v);  }
float64 llvm_uint32_to_float64(uint32_t v) { return float64_val((double)v); }
float32 llvm_int64_to_float32(int64_t v)   { return float32_val((float)v);  }
float32 llvm_uint64_to_float32(uint64_t v) { return float32_val((float)v);  }
float64 llvm_int64_to_float64(int64_t v)   { return float64_val((double)v); }
float64 llvm_uint64_to_float64(uint64_t v) { return float64_val((double)v); }

/*----------------------------------------------------------------------------
| Software IEC/IEEE single-precision conversion routines.
*----------------------------------------------------------------------------*/
int32 llvm_float32_to_int32( float32 a ) { return long_to_int32(lrintf(lfloat(a))); }
int32 llvm_float32_to_int32_round_to_zero( float32 a ) { return (int32)lfloat(a); }
int64 llvm_float32_to_int64( float32 a ) { return llrintf(lfloat(a)); }
int64 llvm_float32_to_int64_round_to_zero( float32 a ) { return (int64)lfloat(a); }
float64 llvm_float32_to_float64( float32 a ) { return float64_val((double)lfloat(a)); }

/*----------------------------------------------------------------------------
| Software IEC/IEEE single-precision operations.
*----------------------------------------------------------------------------*/
float32 llvm_float32_round_to_int( float32 a ) { return float32_val(rintf(lfloat(a))); }
float32 llvm_float32_add( float32 a, float32 b ) { return float32_val(lfloat(a) + lfloat(b)); }
float32 llvm_float32_sub( float32 a, float32 b ) { return float32_val(lfloat(a) - lfloat(b)); }
float32 llvm_float32_mul( float32 a, float32 b ) { return float32_val(lfloat(a) * lfloat(b)); }
float32 llvm_float32_div( float32 a, float32 b ) { return float32_val(lfloat(a) / lfloat(b)); }
float32 llvm_float32_rem( float32 a, float32 b ) { return float32_val(remainderf(lfloat(a), lfloat(b))); }
float32 llvm_float32_sqrt( float32 a ) { return float32_val(sqrtf(lfloat(a))); }
int llvm_float32_eq( float32 a, float32 b ) { return lfloat(a) == lfloat(b); }
int llvm_float32_le( float32 a, float32 b ) { return lfloat(a) <= lfloat(b); }
int llvm_float32_lt( float32 a, float32 b ) { return lfloat(a) < lfloat(b); }
int llvm_float32_unordered( float32 a, float32 b ) { return isunordered(lfloat(a), lfloat(b)); }
float32 llvm_float32_abs(float32 a) { return float32_val(fabsf(lfloat(a))); }
float32 llvm_float32_chs(float32 a) { return float32_val(-lfloat(a)); }

float32 llvm_float32_muladd( float32 a, float32 b, float32 c ) { return float32_val(lfloat(a) * lfloat(b) + lfloat(c)); }

/*----------------------------------------------------------------------------
| Software IEC/IEEE double-precision conversion routines.
*----------------------------------------------------------------------------*/
int32 llvm_float64_to_int32( float64 a ) { return long_to_int32(lrint(ldouble(a))); }
int32 llvm_float64_to_int32_round_to_zero( float64 a ) { return (int32)ldouble(a); }
int64 llvm_float64_to_int64( float64 a ) { return llrint(ldouble(a)); }
int64 llvm_float64_to_int64_round_to_zero( float64 a ) { return (int64)ldouble(a); }
float32 llvm_float64_to_float32( float64 a ) { return float32_val((float)ldouble(a)); }

/*----------------------------------------------------------------------------
| Software IEC/IEEE double-precision operations.
*----------------------------------------------------------------------------*/
float64 llvm_float64_round_to_int( float64 a ) { return float64_val(rint(ldouble(a))); }
float64 llvm_float64_trunc_to_int( float64 a ) { return float64_val(trunc(ldouble(a))); }
float64 llvm_float64_add( float64 a, float64 b ) { return float64_val(ldouble(a) + ldouble(b)); }
float64 llvm_float64_sub( float64 a, float64 b ) { return float64_val(ldouble(a) - ldouble(b)); }
float64 llvm_float64_mul( float64 a, float64 b ) { return float64_val(ldouble(a) * ldouble(b)); }
float64 llvm_float64_div( float64 a, float64 b ) { return float64_val(ldouble(a) / ldouble(b)); }
float64 llvm_float64_rem( float64 a, float64 b ) { return float64_val(remainder(ldouble(a), ldouble(b))); }
float64 llvm_float64_sqrt( float64 a ) { return float64_val(sqrt(ldouble(a))); }
int llvm_float64_eq( float64 a, float64 b ) { return ldouble(a) == ldouble(b); }
int llvm_float64_le( float64 a, float64 b ) { return ldouble(a) <= ldouble(b); }
int llvm_float64_lt( float64 a, float64 b ) { return ldouble(a) < ldouble(b); }
int llvm_float64_unordered( float64 a, float64 b ) { return isunordered(ldouble(a), ldouble(b)); }
float64 llvm_float64_abs(float64 a) { return float64_val(fabs(ldouble(a))); }
float64 llvm_float64_chs(float64 a) { return float64_val(-ldouble(a)); }

#undef float32_val
#undef float64_val
#undef lfloat
#undef ldouble

#endif /* !SOFTFLOAT_NATIVE_H */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
