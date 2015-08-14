######################################################################
#
# SciCChecks: check various C capabilities
#
# $Id: SciCChecks.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# Determine the compiler id
set(C_COMPILER ${CMAKE_C_COMPILER})
set(C_COMPILER_ID ${CMAKE_C_COMPILER_ID})
message(STATUS "C_COMPILER_ID = ${C_COMPILER_ID}.")
SciPrintVar(C_COMPILER)
SciPrintVar(C_COMPILER_ID)

# Type checks
include(CheckTypeSize)

# Check for ssize_t
check_type_size(ssize_t SIZE_OF_SSIZE_T)
if (HAVE_SIZE_OF_SSIZE_T)
  message(STATUS "ssize_t available.")
else ()
  message(STATUS "ssize_t not available.")
  set(ssize_t SSIZE_T)
endif ()

# Check whether time and sys/time can both be included
include(CheckCSourceCompiles)
check_c_source_compiles(
"
#include <sys/time.h>
#include <time.h>
int main(int argc, char** argv) {return 0;}
"
TIME_WITH_SYS_TIME
)
if (TIME_WITH_SYS_TIME)
  if (DEBUG_CMAKE)
    message("time.h and sys/time.h are compatible.")
  endif ()
  set(TIME_WITH_SYS_TIME 1 CACHE BOOL "Whether time and sys/time are compatible")
else ()
  if (DEBUG_CMAKE)
    message("time.h and sys/time.h are NOT compatible.")
  endif ()
endif ()

# Check whether struct tm is in sys/time
include(CheckCSourceCompiles)
check_c_source_compiles(
"#include <sys/time.h>\nint main(int argc, char** argv) {struct tm tm;int *p = &tm.tm_sec;return !p;}"
TM_IN_SYS_TIME
)
if (TM_IN_SYS_TIME)
  if (DEBUG_CMAKE)
    message("struct tm is in time.h.")
  endif ()
  set(TM_IN_SYS_TIME 1 CACHE BOOL "Whether struct tm is in sys/time.")
else ()
  if (DEBUG_CMAKE)
    message("struct tm is NOT in time.h.")
  endif ()
endif ()

# Check variable sizes
check_c_source_compiles(
"#include <stdio.h>
void f(unsigned int i){}
void f(size_t i){}
int main(int argc, char** argv) {return 0;}"
UINT_IS_NOT_SIZE_T
)
if (UINT_IS_NOT_SIZE_T)
  if (DEBUG_CMAKE)
    message("uint and size_t are not the same.")
  endif ()
else ()
  if (DEBUG_CMAKE)
    message("uint and size_t are the same.")
  endif ()
  set(UINT_IS_SIZE_T 1 CACHE BOOL "Whether uint is the same as size_t")
endif ()

check_c_source_compiles(
"#ifdef _WIN32
 #include <BaseTsd.h>
 void f(int i){}
 void f(SSIZE_T i){}
#else
 #include <unistd.h>
 void f(int i){}
 void f(ssize_t i){}
#endif
int main(int argc, char** argv) {return 0;}"
INT_IS_NOT_SSIZE_T
)
if (INT_IS_NOT_SSIZE_T)
  if (DEBUG_CMAKE)
    message("int and ssize_t are not the same size.")
  endif ()
else ()
  if (DEBUG_CMAKE)
    message("int and ssize_t are the same size.")
  endif ()
  set(INT_IS_SSIZE_T 1 CACHE BOOL "Whether int is the same as ssize_t")
endif ()

# Get math into C for Windows
if (WIN32)
  set(_USE_MATH_DEFINES 1 CACHE BOOL "To bring in math defines on Windows.")
endif ()

#
# Tech-X Builde type: FULL, meaning as optimized as possible
# for this specific processor
#

#
# Determine flags by compiler

set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_RELEASE}")
if (${C_COMPILER_ID} STREQUAL GNU)

  set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_FULL} -ffast-math")
  if (SSE_CAPABILITY)
    set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_FULL} -m${SSE_CAPABILITY}")
  endif ()
  set(SSE2_FLAG "-msse2")
  set(AVX_FLAG "-mavx")
  if (APPLE)
# On OS X direct to use clang assembler
    set(AVX_FLAG "${AVX_FLAG} -Wa,-q")
  endif ()
  set(AVX2_FLAG "-mavx2")
  set(OPENMP_FLAGS -fopenmp)

elseif (${C_COMPILER_ID} STREQUAL Clang)

  if (SSE_CAPABILITY)
    set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_FULL} -m${SSE_CAPABILITY}")
  endif ()
  set(SSE2_FLAG "-msse2")
  set(AVX_FLAG "-mavx")
  set(AVX2_FLAG "-mavx2")

elseif (${C_COMPILER_ID} STREQUAL Cray)

  set(OPENMP_FLAGS "-h omp")

elseif (${C_COMPILER_ID} STREQUAL Intel)

  set(SSE2_FLAG "-msse2")
  set(AVX_FLAG "-mavx")
  if (APPLE)
# On OS X direct to use clang assembler.  Needs testing.
    set(AVX_FLAG "${AVX_FLAG} -Wa,-q")
  endif ()
  set(AVX2_FLAG "-march=core-avx2")
  set(OPENMP_FLAGS "-openmp")

elseif (${C_COMPILER_ID} STREQUAL MSVC)

elseif (${C_COMPILER_ID} STREQUAL PathScale)

elseif (${C_COMPILER_ID} STREQUAL PGI)

# Compiler optimization flags set based on "ultra" optimization in
# flags.m4.  Overrides scimake default, since that had -Mipa=fast
# (no inline).
  set(CMAKE_C_FLAGS_FULL
      "-fast -O3 -DNDEBUG -Munroll -Minline=levels:5 -Mipa=fast,inline -Mmovnt")
  set(SSE2_FLAG "-Mvect=simd:128")
  set(AVX_FLAG "-Mvect=simd:256")
  # set(AVX_FLAG -h intrinsics)
  set(OPENMP_FLAGS "-mp")

elseif (${C_COMPILER_ID} STREQUAL XL)

  set(OPENMP_FLAGS "-qsmp=omp -qsmp=stackcheck")

else ()
  message(STATUS "FULL flags not known for ${C_COMPILER_ID}")
endif ()

# Print the performance flags
message(STATUS "Performance flags:")
SciPrintVar(SSE2_FLAG)
SciPrintVar(AVX_FLAG)
SciPrintVar(AVX2_FLAG)
SciPrintVar(AVX512_FLAG)
SciPrintVar(OPENMP_FLAGS)

# Remove /MD etc for static builds on Windows, Add /bigobj.
if (WIN32 AND NOT MINGW)
  foreach (bldtype FULL RELEASE RELWITHDEBINFO MINSIZEREL DEBUG)
    SciRplCompilerFlags(C ${bldtype})
  endforeach ()
endif ()

# Print results
message(STATUS "C compiler options:")
foreach (bld FULL RELEASE RELWITHDEBINFO MINSIZEREL DEBUG)
  SciPrintVar(CMAKE_C_FLAGS_${bld})
endforeach ()
SciPrintVar(CMAKE_C_FLAGS)

