/* Test the `vreinterpretQu16_u64' ARM Neon intrinsic.  */
/* This file was autogenerated by neon-testgen.  */

/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-save-temps -O0" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"

void test_vreinterpretQu16_u64 (void)
{
  uint16x8_t out_uint16x8_t;
  uint64x2_t arg0_uint64x2_t;

  out_uint16x8_t = vreinterpretq_u16_u64 (arg0_uint64x2_t);
}

