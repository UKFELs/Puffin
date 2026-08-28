###
# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Jonathan Smith
# License: BSD-3-Clause
###

enable_language(Fortran)
#set (C_SOURCES "AIO.c")
FILE(GLOB F_LIB_SOURCES lib/*.f90 lib/*/*.f90 ${PROJECT_BINARY_DIR}/puffin/lib/common/puffProvenance.f90)
FILE(GLOB F_EXE_SOURCES app/*.f90)

#
# USE_MKL to be set by user, but actually a flag not just for MKL but for other openMP options
# Should probably break into USE_MKL and USE_OPENMP
# Also need to figure out if we gain from ilp64 lib instead of lp64, (also would be ilp64 blacs...)
#

if (USE_MKL)
  if (USE_OPENMP)
    set (MKL_LIBS mkl_intel_lp64 mkl_core mkl_intel_thread pthread)
  else()
    set (MKL_LIBS mkl_intel_lp64 mkl_core mkl_sequential )
  endif()
endif()

set (BASE_LINK_LIBS dl m)

if (USE_MKL)
  include_directories(${MKL_INCLUDE_DIRS} ${MPI_Fortran_INCLUDE_PATH})
  link_directories(${MKL_LIBRARY_DIRS})
else()
  include_directories(${FFTW3_INCLUDE_DIRS} ${HDF5_INCLUDE_DIRS} ${MPI_Fortran_INCLUDE_PATH})
endif ()


if (USE_MKL)
  add_definitions(${MPI_C_COMPILE_FLAGS} ${OpenMP_C_FLAGS})
else()
  add_definitions(${MPI_C_COMPILE_FLAGS} ${OpenMP_C_FLAGS})
endif()

#C definitions are not needed for Fortran apps.
#remove_definitions(${MPI_C_COMPILE_FLAGS} ${OpenMP_C_FLAGS})

if (USE_MKL)
  add_definitions(${OpenMP_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS})
else()
  add_definitions(${OpenMP_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS})
endif()

add_library(puffin_mpi ${F_LIB_SOURCES})

#
# Disable F2003 reallocation-on-assignment for the integration hot path.
#
# gfortran cannot parallelise an !$OMP WORKSHARE array assignment whose
# left-hand side is an `allocatable`: the realloc-on-assign test it has to emit
# is not something it can spread across a team, so it falls back to
# `#pragma omp single` and the whole block runs on one thread.  Every WORKSHARE
# target in rk4par is a module allocatable, so without this all five parallel
# regions there are silently serial.  Verified with -fdump-tree-original:
# rk4par goes from 5 `omp single` / 0 `omp for` to 40 `omp for`.
#
# This is scoped to these three files ON PURPOSE.  Applied globally the flag
# breaks the 3D and 1D-taper E2E tests (reproducible SIGSEGV on a clean build):
# code elsewhere -- setup, IO, the parallel field redistribution -- does rely on
# assignment auto-allocating its left-hand side.  Here the targets are all
# explicitly allocated first (allact_rk4_arrs, and the allocate() at the top of
# getrhs), so dropping the realloc semantics is a no-op for correctness.
#
# Adding a new file to this list therefore means checking every array
# assignment in it, not just that it happens to compile.
#
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(PUFFIN_NO_REALLOC_LHS_FLAG "-fno-realloc-lhs")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  set(PUFFIN_NO_REALLOC_LHS_FLAG "-assume;norealloc_lhs")
endif()

if (PUFFIN_NO_REALLOC_LHS_FLAG)
  set_source_files_properties(
    ${CMAKE_CURRENT_SOURCE_DIR}/lib/interaction/puffin_mpi_RK4.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/lib/interaction/rhs.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/lib/interaction/system_interpolation_1D.f90
    TARGET_DIRECTORY puffin_mpi
    PROPERTIES COMPILE_OPTIONS "${PUFFIN_NO_REALLOC_LHS_FLAG}")
endif()
add_executable(puffin ${F_EXE_SOURCES})
#add_dependencies(puffin AIO)

if (USE_MKL)
#  target_link_libraries(puffin AIO ${MKL_LIBS} ${MPI_Fortran_LIBRARIES} ${BASE_LINK_LIBS})
  target_link_libraries(puffin ${MKL_LIBS} ${MPI_Fortran_LIBRARIES} ${BASE_LINK_LIBS})
else()
#  target_link_libraries(puffin AIO ${FFTW_LIBS} ${Hdf5_LIBRARIES} ${MPI_Fortran_LIBRARIES} ${BASE_LINK_LIBS})
  target_link_libraries(puffin_mpi ${FFTW3_LIBRARIES} ${HDF5_LIBRARIES} ${MPI_Fortran_LIBRARIES} ${BASE_LINK_LIBS})
  target_link_libraries(puffin puffin_mpi)
endif()

#target_link_libraries(puffin puffin_mpi)
install (TARGETS puffin DESTINATION bin)
install (TARGETS puffin_mpi DESTINATION lib)
