######################################################################
#
# SciCChecks: check various C capabilities
#
# $Id: SciCChecks.cmake 1081 2016-09-10 15:44:42Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

# Determine compiler version
message("")
include(${SCIMAKE_DIR}/SciFindCompilerVersion.cmake)
SciFindCompilerVersion(C)
if (NOT C_VERSION)
  message(FATAL_ERROR "Could not determine compiler version.")
endif ()

# Type checks
include(CheckTypeSize)

# Print some sizes
check_type_size(int SCI_SIZEOF_INT)
message(STATUS "SCI_SIZEOF_INT = ${SCI_SIZEOF_INT}.")
check_type_size("unsigned int" SCI_SIZEOF_UINT)
message(STATUS "SCI_SIZEOF_UINT = ${SCI_SIZEOF_UINT}.")

# Check for size_t
set(CMAKE_REQUIRED_INCLUDES_SAV ${CMAKE_REQUIRED_INCLUDES})
set(CMAKE_REQUIRED_INCLUDES stdio.h)
check_type_size(size_t SCI_SIZEOF_SIZE_T)
set(CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES_SAV})
message(STATUS "SCI_SIZEOF_SIZE_T = ${SCI_SIZEOF_SIZE_T}.")
if (HAVE_SCI_SIZEOF_SIZE_T)
  message(STATUS "size_t defined.")
  if ("${SCI_SIZEOF_UINT}" EQUAL "${SCI_SIZEOF_SIZE_T}")
    set(UINT_IS_NOT_SIZE_T FALSE)
    set(UINT_IS_SIZE_T TRUE)
    set(INT_IS_NOT_SSIZE_T FALSE)
    set(INT_IS_SSIZE_T TRUE)
  else ()
    set(UINT_IS_NOT_SIZE_T TRUE)
    set(UINT_IS_SIZE_T FALSE)
    set(INT_IS_NOT_SSIZE_T TRUE)
    set(INT_IS_SSIZE_T FALSE)
  endif ()
else ()
  message(STATUS "size_t not defined.")
endif ()

# Check for ssize_t.  We know Windows has SSIZE_T.
# Some compilers (Intel) even on Unix may not have ssize_t.
set(CMAKE_REQUIRED_INCLUDES_SAV ${CMAKE_REQUIRED_INCLUDES})
set(CMAKE_REQUIRED_INCLUDES unistd.h)
check_type_size(ssize_t SCI_SIZEOF_SSIZE_T)
if (HAVE_SCI_SIZEOF_SSIZE_T)
  message(STATUS "SCI_SIZEOF_SSIZE_T = ${SCI_SIZEOF_SSIZE_T}.")
  message(STATUS "ssize_t defined.")
  set(SCI_HAVE_SSIZE_T TRUE)
else ()
  message(STATUS "ssize_t not defined.")
  set(SCI_HAVE_SSIZE_T FALSE)
endif ()
set(CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES_SAV})

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

# Get math into C for Windows
if (WIN32)
  set(_USE_MATH_DEFINES 1 CACHE BOOL "To bring in math defines on Windows.")
endif ()

#
# Tech-X Builde type: FULL, meaning as optimized as possible
# for this specific processor
#

set(Generic_FLAG " ")
# Initialize the following flags to bogus values so we don't
# get ISA_COMPILES and ISA_RUNS for ISAs that arent' really supported.
set(SSE2_FLAG "compiler flags for this ISA not known")
set(AVX_FLAG "compiler flags for this ISA not known")
set(AVX2_FLAG "compiler flags for this ISA not known")
set(AVX512_FLAG "compiler flags for this ISA not known")
#
# Determine flags by compiler
#
set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_RELEASE}")
if (${C_COMPILER_ID} STREQUAL GNU)

  set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_FULL} -ffast-math")
  set(SSE2_FLAG "-msse2")
  set(AVX_FLAG "-mavx")
  if (APPLE)
# On OS X direct to use clang assembler
    set(AVX_FLAG "${AVX_FLAG} -Wa,-q")
  endif ()
  set(AVX2_FLAG "-mavx2")
  set(AVX512_FLAG "-mavx512f")
  set(OPENMP_FLAGS -fopenmp)

elseif (${C_COMPILER_ID} STREQUAL Clang)

  set(SSE2_FLAG "-msse2")
  set(AVX_FLAG "-mavx")
  set(AVX2_FLAG "-mavx2")

elseif (${C_COMPILER_ID} STREQUAL Cray)

  set(OPENMP_FLAGS "-h omp")

elseif (${C_COMPILER_ID} STREQUAL Intel)

  set(SSE2_FLAG "-msse2")
  set(AVX_FLAG "-march=corei7-avx")
  if (APPLE)
# On OS X direct to use clang assembler.  Needs testing.
    set(AVX_FLAG "${AVX_FLAG} -Wa,-q")
  endif ()
  set(AVX2_FLAG "-march=core-avx2")
  set(OPENMP_FLAGS "-openmp")

elseif (${C_COMPILER_ID} STREQUAL MSVC)

  set(SSE2_FLAG "")
  set(AVX_FLAG "/arch:AVX")
  set(AVX2_FLAG "/arch:AVX2")
  set(AVX512_FLAG "unknown architecture flags")
  set(OPENMP_FLAGS "/openmp")

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

# CMake default XL compiler flags are very poor
  set(CMAKE_C_FLAGS_RELEASE "-O3 -qnooptdebug")
  set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_RELEASE}")
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O2 -qoptdebug")
  set(CMAKE_C_FLAGS_DEBUG "-g -qnoopt -O0 -qcheck=all")
  set(OPENMP_FLAGS "-qsmp=omp -qsmp=stackcheck")

else ()
  message(STATUS "FULL flags not known for ${C_COMPILER_ID}")
endif ()

SciPrintString("")
SciPrintString("  CMake detected C implicit libraries:")
SciPrintVar(CMAKE_C_IMPLICIT_LINK_LIBRARIES)
SciPrintVar(CMAKE_C_IMPLICIT_LINK_DIRECTORIES)

# Print the performance flags
message(STATUS "Performance flags:")
SciPrintVar(Generic_FLAG)
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

