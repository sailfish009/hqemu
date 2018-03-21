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

#ifndef SOFTFLOAT_NATIVE_DEF_H
#define SOFTFLOAT_NATIVE_DEF_H

#ifdef __cplusplus
extern "C"
{
#endif

#include "fpu/softfloat.h"

int num_native_fpu_helpers(void);
void *get_native_fpu_helpers(void);

/*----------------------------------------------------------------------------
| Software IEC/IEEE integer-to-floating-point conversion routines.
*----------------------------------------------------------------------------*/
float32 llvm_int32_to_float32(int32_t v);
float64 llvm_int32_to_float64(int32_t v);
float32 llvm_uint32_to_float32(uint32_t v);
float64 llvm_uint32_to_float64(uint32_t v);
float32 llvm_int64_to_float32(int64_t v);
float32 llvm_uint64_to_float32(uint64_t v);
float64 llvm_int64_to_float64(int64_t v);
float64 llvm_uint64_to_float64(uint64_t v);

/*----------------------------------------------------------------------------
| Software IEC/IEEE single-precision conversion routines.
*----------------------------------------------------------------------------*/
int32 llvm_float32_to_int32( float32 a );
int32 llvm_float32_to_int32_round_to_zero( float32 a );
int64 llvm_float32_to_int64( float32 a );
int64 llvm_float32_to_int64_round_to_zero( float32 a );
float64 llvm_float32_to_float64( float32 a );

/*----------------------------------------------------------------------------
| Software IEC/IEEE single-precision operations.
*----------------------------------------------------------------------------*/
float32 llvm_float32_round_to_int( float32 a );
float32 llvm_float32_add( float32 a, float32 b );
float32 llvm_float32_sub( float32 a, float32 b );
float32 llvm_float32_mul( float32 a, float32 b );
float32 llvm_float32_div( float32 a, float32 b );
float32 llvm_float32_rem( float32 a, float32 b );
float32 llvm_float32_sqrt( float32 a );
int llvm_float32_eq( float32 a, float32 b );
int llvm_float32_le( float32 a, float32 b );
int llvm_float32_lt( float32 a, float32 b );
int llvm_float32_unordered( float32 a, float32 b );
float32 llvm_float32_abs(float32 a);
float32 llvm_float32_chs(float32 a);

float32 llvm_float32_muladd( float32 a, float32 b, float32 c );

/*----------------------------------------------------------------------------
| Software IEC/IEEE double-precision conversion routines.
*----------------------------------------------------------------------------*/
int32 llvm_float64_to_int32( float64 a );
int32 llvm_float64_to_int32_round_to_zero( float64 a );
int64 llvm_float64_to_int64( float64 a );
int64 llvm_float64_to_int64_round_to_zero( float64 a );
float32 llvm_float64_to_float32( float64 a );

/*----------------------------------------------------------------------------
| Software IEC/IEEE double-precision operations.
*----------------------------------------------------------------------------*/
float64 llvm_float64_round_to_int( float64 a );
float64 llvm_float64_trunc_to_int( float64 a );
float64 llvm_float64_add( float64 a, float64 b );
float64 llvm_float64_sub( float64 a, float64 b );
float64 llvm_float64_mul( float64 a, float64 b );
float64 llvm_float64_div( float64 a, float64 b );
float64 llvm_float64_rem( float64 a, float64 b );
float64 llvm_float64_sqrt( float64 a );
int llvm_float64_eq( float64 a, float64 b );
int llvm_float64_le( float64 a, float64 b );
int llvm_float64_lt( float64 a, float64 b );
int llvm_float64_unordered( float64 a, float64 b );
float64 llvm_float64_abs(float64 a);
float64 llvm_float64_chs(float64 a);

#ifdef __cplusplus
}
#endif

#endif /* !SOFTFLOAT_NATIVE_DEF_H */
