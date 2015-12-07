/**
 * $Id: pragma_omp_simd.cxx 792 2015-04-17 14:07:44Z jrobcary $
 *
 * Code stub for determining whether the compiler supports simd pragmas
 * from OpenMP 4.
 *
 * Copyright &copy; 2013-2015, Tech-X Corporation, Boulder, CO.
 * See LICENSE file (EclipseLicense.txt) for conditions of use.
 */
#include <omp.h>

int main(int argc, char** argv) {
  float a[8] = {
    0.0
  }, b[8] = {
    0.0
  };
#pragma omp for simd
  for (int i = 0; i < 8; ++i) {
    a[i] += b[i];
  }
  return 0;
}

