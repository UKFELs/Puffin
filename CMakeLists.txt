###
# (c) Strathclyde University
#
# 25.6.2015 Jonathan Smith - CMake lists for PUFFIN Project
###
cmake_minimum_required (VERSION 2.8.10)
project (puffin)
enable_language (Fortran)
# The version number.
set (Puffin_VERSION_MAJOR 1)
set (Puffin_VERSION_MINOR 4)
# And because I don't know better
set (Puffin_VERSION_PATCH 1)
# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
elseif (Fortran_COMPILER_NAME MATCHES "g77")
  # g77
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")
# I think this is unsuccessful in just about all cases, but it doesn't harm
# Todo: use scimake to determine FFTW (and potentially also FORTRAN/MPI/openMP options)
find_library(FFTW NAMES fftw_mpi fftw)
find_package(MPI REQUIRED)

add_subdirectory(source)

## todo install example files in a subdirectory of the installation
#add_subdirectory(inputs)

# TODO install a license file, eg openBSD license.

