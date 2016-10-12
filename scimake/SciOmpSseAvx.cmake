######################################################################
#
# SciSseAvx: Determine sse and avx capabilities to processor and add
#            to appropriate flags.
#
# $Id: SciOmpSseAvx.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

SciPrintString(" Optimization checking ")
message("")
message(STATUS "--------- Analyzing vector capabilities ---------")

######################################################################
# Determine the processor
######################################################################

if (EXISTS /proc/cpuinfo)
  message(STATUS "Working on LINUX.")
  if (DISABLE_CPUCHECK) # For BGP
    message(STATUS "CPU check disabled.")
  else ()
    execute_process(COMMAND cat /proc/cpuinfo
        COMMAND grep "model name"
        COMMAND head -1
        OUTPUT_VARIABLE SCIC_CPU
        OUTPUT_STRIP_TRAILING_WHITESPACE)
# For Blue Gene
    if (SCIC_CPU)
      string(REGEX REPLACE "^.*: " "" SCIC_CPU ${SCIC_CPU})
      execute_process(COMMAND cat /proc/cpuinfo
          COMMAND grep "flags"
          COMMAND head -1
          OUTPUT_VARIABLE CPU_CAPABILITIES
          OUTPUT_STRIP_TRAILING_WHITESPACE)
    else ()
      execute_process(COMMAND cat /proc/cpuinfo
          COMMAND grep "^cpu"
          COMMAND head -1
          OUTPUT_VARIABLE SCIC_CPU
          OUTPUT_STRIP_TRAILING_WHITESPACE)
      if (SCIC_CPU)
        string(REGEX REPLACE "^.*: " "" SCIC_CPU ${SCIC_CPU})
        string(REGEX REPLACE "^.*, *" "" CPU_CAPABILITIES ${SCIC_CPU})
        string(REGEX REPLACE ",.*$" "" SCIC_CPU ${SCIC_CPU})
      endif ()
    endif ()
  endif ()
elseif (APPLE)
  execute_process(COMMAND sysctl -a machdep.cpu.brand_string
      OUTPUT_VARIABLE SCIC_CPU
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  string(REGEX REPLACE "^.*: " "" SCIC_CPU "${SCIC_CPU}")
  execute_process(COMMAND sysctl -a machdep.cpu.features
      COMMAND head -1
      OUTPUT_VARIABLE CPU_CAPABILITIES
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  string(REGEX REPLACE "^.*: *" "" CPU_CAPABILITIES "${CPU_CAPABILITIES}")
  # string(REGEX REPLACE "SSE" "sse" CPU_CAPABILITIES "${CPU_CAPABILITIES}")
  string(TOLOWER "${CPU_CAPABILITIES}" CPU_CAPABILITIES)
endif ()
message(STATUS "CPU = ${SCIC_CPU}.")
message(STATUS "CPU_CAPABILITIES = ${CPU_CAPABILITIES}.")

######################################################################
# Sort into sse or avx
######################################################################

if (CPU_CAPABILITIES)
  separate_arguments(CPU_CAPABILITIES)
  # message(STATUS "CPU capabilities are ${CPU_CAPABILITIES}")
  foreach (cap ${CPU_CAPABILITIES})
    # MESSAGE("Examining ${cap}")
    if (${cap} MATCHES "^sse")
      list(APPEND SSE_CAPABILITIES ${cap})
    elseif (${cap} MATCHES "^avx")
      list(APPEND AVX_CAPABILITIES ${cap})
    endif ()
  endforeach ()
  foreach (cap SSE AVX)
    if (${cap}_CAPABILITIES)
      list(SORT ${cap}_CAPABILITIES)
      list(REVERSE ${cap}_CAPABILITIES)
      list(GET ${cap}_CAPABILITIES 0 ${cap}_CAPABILITY)
      string(REPLACE "_" "." ${cap}_CAPABILITY "${${cap}_CAPABILITY}")
    endif ()
  endforeach ()
endif ()

foreach (cap SSE AVX)
  message(STATUS "${cap} capabilities are ${${cap}_CAPABILITIES}")
  message(STATUS "${cap} capability is ${${cap}_CAPABILITY}")
endforeach ()

######################################################################
# Check whether compilers support SSE2 or AVX if CPU supports it
######################################################################

# Handy
include(CheckCSourceCompiles)
include(CheckCSourceRuns)
include(CheckCXXSourceCompiles)
include(CheckCXXSourceRuns)

message(STATUS "Checking vector capabilities.  CMAKE_REQUIRED_FLAGS = ${CMAKE_REQUIRED_FLAGS}.")

# message(STATUS "Checking sse2 capabilities.")
set(CMAKE_REQUIRED_FLAGS_SAV "${CMAKE_REQUIRED_FLAGS}")
set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS} ${SSE2_FLAG}")
# message(STATUS "SSE2_FLAG = ${SSE2_FLAG}.")
check_c_source_compiles(
"
#include <emmintrin.h>
int main(int argc, char** argv) {
  double a[2] = {1.0, 2.0};
  __m128d t = _mm_loadu_pd(a);
  return 0;
}
"
SSE2_COMPILES
)
SciPrintVar(SSE2_COMPILES)
if (SSE2_COMPILES)
  check_c_source_runs(
"
#include <emmintrin.h>
int main(int argc, char** argv) {
  double a[2] = {1.0, 2.0};
  __m128d t = _mm_loadu_pd(a);
  return 0;
}
"
  SSE2_RUNS
  )
endif ()
set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAV}")
SciPrintVar(SSE2_RUNS)

# Check whether have avx.
# message(STATUS "Checking avx capabilities.")
set(CMAKE_REQUIRED_FLAGS_SAV "${CMAKE_REQUIRED_FLAGS}")
set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS} ${AVX_FLAG}")
# message(STATUS "AVX_FLAG = ${AVX_FLAG}.")
check_c_source_compiles(
"
#include <immintrin.h>
int main(int argc, char** argv) {
  double a[4] = {1.0, 2.0, 3.0, 4.0};
  __m256d t = _mm256_loadu_pd(a);
  return 0;
}
"
AVX_COMPILES
)
SciPrintVar(AVX_COMPILES)
if (AVX_COMPILES)
  check_c_source_runs(
"
#include <immintrin.h>
int main(int argc, char** argv) {
  double a[4] = {1.0, 2.0, 3.0, 4.0};
  __m256d t = _mm256_loadu_pd(a);
  return 0;
}
"
  AVX_RUNS
  )
endif ()
set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAV}")
SciPrintVar(AVX_RUNS)

# Check whether have avx2.
# message(STATUS "Checking avx2 capabilities.")
set(CMAKE_REQUIRED_FLAGS_SAV "${CMAKE_REQUIRED_FLAGS}")
set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS} ${AVX_FLAG} ${AVX2_FLAG}")
# message(STATUS "AVX2_FLAG = ${AVX2_FLAG}.")
check_cxx_source_compiles(
"
#include <immintrin.h>
int main(int argc, char** argv) {
  double a[4] = {1.0, 2.0, 3.0, 4.0};
  __m128i vindex = _mm_set_epi32(0, 2, 1, 3);
  __m256d t = _mm256_i32gather_pd(a, vindex, 8);
  return 0;
}
"
AVX2_COMPILES
)
SciPrintVar(AVX2_COMPILES)
if (AVX2_COMPILES)
  check_cxx_source_runs(
"
#include <immintrin.h>
int main(int argc, char** argv) {
  double a[4] = {1.0, 2.0, 3.0, 4.0};
  __m128i vindex = _mm_set_epi32(0, 2, 1, 3);
  __m256d t = _mm256_i32gather_pd(a, vindex, 8);
  return 0;
}
"
  AVX2_RUNS
  )
endif ()
set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAV}")
SciPrintVar(AVX2_RUNS)

# Check whether have avx512.
# message(STATUS "Checking avx512 capabilities.")
set(CMAKE_REQUIRED_FLAGS_SAV "${CMAKE_REQUIRED_FLAGS}")
set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS} ${AVX_FLAG} ${AVX512_FLAG}")
# message(STATUS "AVX512_FLAG = ${AVX512_FLAG}.")
check_cxx_source_compiles(
"
#include <immintrin.h>
int main(int argc, char** argv) {
  double a[8] = {1., 2., 3., 4., 5., 6., 7., 8.};
  __m512d t = _mm512_load_pd(a);
  return 0;
}
"
AVX512_COMPILES
)
SciPrintVar(AVX512_COMPILES)
if (AVX512_COMPILES)
  check_cxx_source_runs(
"
#include <immintrin.h>
int main(int argc, char** argv) {
  double a[8] = {1., 2., 3., 4., 5., 6., 7., 8.};
  __m512d t = _mm512_load_pd(a);
  return 0;
}
"
  AVX512_RUNS
  )
endif ()
set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAV}")
SciPrintVar(AVX512_RUNS)

######################################################################
# Now handle the flags for sse2 and avx
# If we do runtime detection, we can add these flags more liberally
######################################################################

if (SSE2_COMPILES)
  set(SSE2_BUILDS FULL RELEASE RELWITHDEBINFO MINSIZEREL)
  if (ALLOW_SSE2)
    set(SSE2_BUILDS ${SSE2_BUILDS} ${CMAKE_BUILD_TYPE_UC})
  endif ()
  list(REMOVE_DUPLICATES SSE2_BUILDS)
  list(FIND SSE2_BUILDS ${CMAKE_BUILD_TYPE_UC} sse2found)
  if (NOT ${sse2found} EQUAL -1)
    set(HAVE_SSE2 TRUE)
  endif ()
  foreach (cmp C CXX)
    foreach (bld ${SSE2_BUILDS})
      set(CMAKE_${cmp}_FLAGS_${bld} "${CMAKE_${cmp}_FLAGS_${bld}} ${SSE2_FLAG}")
    endforeach ()
  endforeach ()
endif ()

foreach (INSTSET AVX AVX2 AVX512)
  if (${INSTSET}_RUNS)
    set(${INSTSET}_BUILDS FULL)
    if (ALLOW_${INSTSET})
      set(${INSTSET}_BUILDS ${${INSTSET}_BUILDS} ${CMAKE_BUILD_TYPE_UC})
    endif ()
    list(REMOVE_DUPLICATES ${INSTSET}_BUILDS)
    list(FIND ${INSTSET}_BUILDS ${CMAKE_BUILD_TYPE_UC} avxfound)
    if (NOT ${avxfound} EQUAL -1)
      set(HAVE_${INSTSET} TRUE)
    endif ()
    foreach (cmp C CXX)
      foreach (bld ${${INSTSET}_BUILDS})
        set(CMAKE_${cmp}_FLAGS_${bld} "${CMAKE_${cmp}_FLAGS_${bld}} ${${INSTSET}_FLAG}")
      endforeach ()
    endforeach ()
  endif ()
endforeach ()

######################################################################
# OpenMP detection
######################################################################

if (USE_OPENMP)
  message(STATUS "OpenMP requested.")
  if (OPENMP_FLAGS)
    message(STATUS "OpenMP flag defined.")
    set(HAVE_OPENMP TRUE)
  else ()
    message(STATUS "Seeking OpenMP.")
    find_package(OpenMP)
    if (OPENMP_FOUND)
      message(STATUS "OpenMP found.")
      set(HAVE_OPENMP TRUE)
      set(OPENMP_FLAGS ${OpenMP_C_FLAGS})
    else ()
      message(WARNING "OpenMP requested but flags not known.")
    endif ()
  endif ()
  if (HAVE_OPENMP)
    message(STATUS "OPENMP_FLAGS = ${OPENMP_FLAGS}.")
# To test for openmp4, need to add openmp flags for compilation
    set(CMAKE_CXX_FLAGS_SAV "${CMAKE_CXX_FLAGS}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OPENMP_FLAGS}")
    try_compile(HAVE_PRAGMA_OMP_SIMD ${PROJECT_BINARY_DIR}/scimake
      ${SCIMAKE_DIR}/trycompile/pragma_omp_simd.cxx
      # OUTPUT_VARIABLE BUILD_OUT
    )
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS_SAV}")
    # message(STATUS "Build result = \n ${BUILD_OUT}")
    if (HAVE_PRAGMA_OMP_SIMD)
      message(STATUS "OpenMP 4 pragma omp simd available")
    else ()
      message(STATUS "OpenMP 4 pragma omp simd NOT available")
    endif ()
    foreach (cmp C CXX)
      foreach (bld FULL RELEASE RELWITHDEBINFO MINSIZEREL DEBUG)
        set(CMAKE_${cmp}_FLAGS_${bld} "${CMAKE_${cmp}_FLAGS_${bld}} ${OPENMP_FLAGS}")
      endforeach ()
    endforeach ()
  endif ()
endif ()

######################################################################
# Print results
######################################################################

SciPrintString("After analyzing vector and thread capabilities:")
foreach (cmp C CXX)
  foreach (bld FULL RELEASE RELWITHDEBINFO MINSIZEREL DEBUG)
    SciPrintVar(CMAKE_${cmp}_FLAGS_${bld})
  endforeach ()
  SciPrintVar(CMAKE_${cmp}_FLAGS)
endforeach ()
SciPrintString("")

