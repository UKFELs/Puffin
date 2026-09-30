###
# Copyright (c) 2012-2018, University of Strathclyde
# License: BSD-3-Clause
# 25.6.2015 Jonathan Smith - CMake lists for PUFFIN Project
###

cmake_minimum_required (VERSION 3.20)
project (puffin)

enable_language (Fortran)

# The version number.
set (Puffin_VERSION_MAJOR 2)
set (Puffin_VERSION_MINOR 1)
set (Puffin_VERSION_PATCH 0a)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules")

# ENABLE_PARALLEL=ON  builds against a real MPI, FFTW3-MPI and parallel HDF5.
# ENABLE_PARALLEL=OFF builds a serial executable that needs none of them: the
# sources in puffin/lib/backends/serial stand in for the MPI module, the
# fftw_mpi_* planner and the parallel-HDF5 property calls, all with one-rank
# semantics. No call site elsewhere in Puffin changes between the two modes.
# See doc/SERIAL_BUILD.md.
option(ENABLE_PARALLEL "whether to build parallel PUFFIN" ON)
option(USE_MKL "Use Intel MKL" OFF)
option(ENABLE_TESTING "Enable testing" OFF)
option(ENABLE_BENCHMARKS "Enable performance benchmarks (see benchmark/README.md)" OFF)
option(ENABLE_LINT "Enable extra compiler warnings (-Wall -Wextra)" OFF)

######################################################################
#
# Find packages
#
######################################################################

if(ENABLE_PARALLEL)
  find_package(MPI REQUIRED)
  set(USE_MPI 1 CACHE BOOL "setting USE_MPI to 1" FORCE)
  message(STATUS "Building parallel Puffin (MPI enabled)")
else()
  set(USE_MPI 0 CACHE BOOL "setting USE_MPI to 0" FORCE)
  message(STATUS "Building serial Puffin (MPI disabled) - run the executable directly, not under mpirun")
endif()

if (USE_MKL)
  # User is expected to provide MKLROOT or MKL_LIBRARY_DIRS
  if(NOT DEFINED MKL_LIBRARY_DIRS AND DEFINED ENV{MKLROOT})
    set(MKL_LIBRARY_DIRS "$ENV{MKLROOT}/lib/intel64")
    set(MKL_INCLUDE_DIRS "$ENV{MKLROOT}/include")
  endif()
  message(STATUS "Using MKL. MKL_LIBRARY_DIRS: ${MKL_LIBRARY_DIRS}")
elseif (ENABLE_PARALLEL)
  find_package(FFTW3 REQUIRED COMPONENTS MPI)
else()
  # Serial build: plain FFTW3 only. backends/serial/puffin_fftw3.f90 supplies
  # the fftw_mpi_* entry points on top of it, so libfftw3_mpi is not linked.
  find_package(FFTW3 REQUIRED)
endif()

find_package(HDF5 COMPONENTS Fortran HL REQUIRED)

# A serial build never calls the MPI-IO driver, but if the HDF5 that was found
# is itself a parallel build then linking it still drags libmpi in. That is
# harmless - the executable is still launched directly, not under mpirun - but
# it does mean the binary is not MPI-free. Point at a serial HDF5 with
# HDF5_ROOT if that matters.
if (NOT ENABLE_PARALLEL AND HDF5_IS_PARALLEL)
  message(STATUS
    "Serial build linked against a parallel HDF5, so libmpi comes in transitively. "
    "Set HDF5_ROOT to a serial HDF5 for a build with no MPI dependency at all.")
endif()

find_package(OpenMP REQUIRED)
find_package(Doxygen)

if (DOXYGEN_FOUND)
    message(STATUS "Doxygen FOUND!!")
    message(STATUS "DOXYGEN IS : ${DOXYGEN_EXECUTABLE}")
endif()

if (OpenMP_FOUND)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
endif()

if (ENABLE_LINT)
  if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    # -Wno-unused-dummy-argument: many routines share a common call signature
    # (lattice element interfaces, callback-style routines) where a given
    # implementation legitimately ignores some arguments.
    # -Wno-compare-reals: exact REAL(8) ==/ /= comparisons are used
    # deliberately (sentinel values, zero guards) in several places; see
    # doc/LINT_NOTES.md for the full list. Rewriting them risks changing
    # numerical output in a codebase with a bit-exact E2E test.
    # -Wno-aliasing: transforms.f90 passes the same buffer as FFTW's `in`
    # and `out` arguments deliberately, for an in-place transform; see
    # doc/LINT_NOTES.md.
    # -Wno-intrinsic-shadow: error_fn.f90 deliberately defines its own
    # erf/erfc, shadowing the Fortran 2008 intrinsics of the same name;
    # see doc/LINT_NOTES.md.
    # -Wno-conversion: legacy numerical/HDF5 code relies on implicit
    # narrowing (REAL->INTEGER truncation, INTEGER(8)->INTEGER(4)) as a
    # deliberate idiom throughout; see doc/LINT_NOTES.md.
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -Wno-unused-dummy-argument -Wno-compare-reals -Wno-aliasing -Wno-intrinsic-shadow -Wno-conversion")
  elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -warn all")
  else()
    message(WARNING "ENABLE_LINT requested but no warning flags known for Fortran compiler '${CMAKE_Fortran_COMPILER_ID}'")
  endif()
endif()

execute_process(COMMAND git --work-tree=${CMAKE_SOURCE_DIR} rev-parse --abbrev-ref HEAD
  RESULT_VARIABLE GIT_RESULT
  OUTPUT_VARIABLE GIT_BRANCH
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )

if (NOT GIT_BRANCH)
  set(GIT_BRANCH "unknown branch, not in git")
endif()

execute_process(COMMAND git --work-tree=${CMAKE_SOURCE_DIR} rev-parse HEAD
  RESULT_VARIABLE GIT_RESULT
  OUTPUT_VARIABLE GIT_REVISION
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )

if (NOT GIT_REVISION)
  set(GIT_REVISION "unknown revision, not in git")
endif()

message(STATUS "git branch : ${GIT_BRANCH} ${GIT_REVISION}")
string(TIMESTAMP CONFIGURE_TIME_STAMP "%Y-%m-%d %H:%M")

######################################################################
#
# Variables for inclusion in config.h
#
######################################################################

set(CXX ${CMAKE_CXX_COMPILER})
set(CFLAGS "${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_${CBT_UC}}")
set(CXXFLAGS "${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_${CBT_UC}}")
set(HAVE_CMAKE 1 CACHE STRING "Whether built with CMake")

if(ENABLE_TESTING OR ENABLE_BENCHMARKS)
  enable_testing()
endif()

######################################################################
#
# Create the configure file
#
######################################################################

configure_file(config-cmake.h.in config.h)

configure_file(puffin/lib/common/puffProvenance.f90.in puffin/lib/common/puffProvenance.f90)
# configure_file(doc/fortran.dxg.in doc/fortran.dxg)

add_subdirectory(puffin)

if (ENABLE_BENCHMARKS)
  add_subdirectory(benchmark)
endif (ENABLE_BENCHMARKS)

if (ENABLE_TESTING)
  add_subdirectory(test)
endif (ENABLE_TESTING)

if (DOXYGEN_FOUND)
    add_custom_target(doc
        ${DOXYGEN_EXECUTABLE} ${PROJECT_BINARY_DIR}/doc/fortran.dxg
        WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/doc
        COMMENT "Generating API documentation with Doxygen" VERBATIM
        )
endif()

######################################################################
#
# Static lint target (opt-in, not part of the default build)
#
######################################################################

find_program(FORTITUDE_EXECUTABLE NAMES fortitude)

if (FORTITUDE_EXECUTABLE)
    add_custom_target(lint
        ${FORTITUDE_EXECUTABLE} check ${CMAKE_SOURCE_DIR}/puffin
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMENT "Running fortitude static analysis on puffin/" VERBATIM
        )
else()
    add_custom_target(lint
        ${CMAKE_COMMAND} -E echo
            "fortitude not found. Install it (e.g. 'pip install fortitude-lint' or 'cargo install fortitude-lint'), re-run cmake, then 'make lint'."
        COMMENT "fortitude not found" VERBATIM
        )
endif()

install (DIRECTORY utilities/post DESTINATION bin)
install (DIRECTORY utilities/pyPlotting DESTINATION bin)
install (DIRECTORY utilities/setup DESTINATION bin)
install (DIRECTORY utilities/visit-scripts DESTINATION bin)

install (DIRECTORY inputs/ DESTINATION examples)
# install (DIRECTORY inputs/gen/ DESTINATION python)

install (DIRECTORY scripts/ DESTINATION examples/job-submission-scripts)

######################################################################
#
# Finalize (installation, packaging)
#
######################################################################

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Puffin")

# build a CPack driven installer package

include (InstallRequiredSystemLibraries)
set (CPACK_RESOURCE_FILE_LICENSE  
     "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt")
set (CPACK_PACKAGE_VERSION_MAJOR "${Puffin_VERSION_MAJOR}")
set (CPACK_PACKAGE_VERSION_MINOR "${Puffin_VERSION_MINOR}")
set (CPACK_PACKAGE_VERSION_PATCH "${Puffin_VERSION_PATCH}")
#include (CPack)
