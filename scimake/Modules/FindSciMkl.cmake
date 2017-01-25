# - FindSciMkl: Module to find include directories and
#   libraries for Mkl.
#
# $Id: FindSciMkl.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
# Module usage:
#   find_package(SciMkl ...)
#
# This module will define the following variables:
#  HAVE_MKL, MKL_FOUND = Whether libraries and includes are found
#  Mkl_INCLUDE_DIRS = Location of Mkl includes
#  Mkl_LIBRARY_DIRS = Location of Mkl libraries
#  Mkl_LIBRARIES    = Required libraries
#  Mkl_STLIB        = Static libraries
#  Iomp5_LIBRARIES  = Openmp intel libraries
#
######################################################################
#
#  Order of precedence: Command-line, environment, hard-code try
#
if ("${Mkl_ROOT_DIR}" STREQUAL "")
  if (NOT "$ENV{MKLROOT}" STREQUAL "")
     set(Mkl_ROOT_DIR "$ENV{MKLROOT}")
  endif ()
endif ()

# Try hard-code directory if not specified already
if ("${Mkl_ROOT_DIR}" STREQUAL "")
  if (WIN32)
    set(Mkl_ROOT_DIR "C:/Program Files (x86)/Intel/Composer XE/mkl")
  else (WIN32)
    set(Mkl_ROOT_DIR "/usr/local/intel/mkl")
  endif (WIN32)
endif ()

#
#  Allow architecture modification: intel64 or mic
#
if ("${Mkl_ARCH}" STREQUAL "")
  set(Mkl_ARCH "intel64")
endif ()

#
#  By default, just use the blas and lapack, but some may want
#  to use scalapack and pardiso as well
#
if ("${Mkl_SEARCH_LIBS}" STREQUAL "")
  if (ENABLE_MKL_SCALAPACK)
    set(Mkl_SEARCH_LIBS "mkl_scalapack_lp64;mkl_intel_lp64;mkl_core;mkl_intel_thread;mkl_blacs_intelmpi_lp64")
  else ()
    set(Mkl_SEARCH_LIBS "mkl_intel_lp64;mkl_intel_thread;mkl_core")
  endif ()
endif ()

#
#  Now start the searching
#
SciFindPackage(PACKAGE "Mkl"
              LIBRARIES ${Mkl_SEARCH_LIBS}
              INCLUDE_SUBDIRS "include"
              LIBRARY_SUBDIRS "lib/${Mkl_ARCH}"
              )

if (NOT MKL_FOUND)
  message(STATUS "Did not find Mkl.  Use -DMkl_ROOT_DIR and, if using OpenMP, Iomp5_ROOT_DIR to specify the installation directory.")
  if (SciMkl_FIND_REQUIRED)
    message(FATAL_ERROR "Finding MKL failed.")
  endif ()
endif ()
if (MKL_FOUND)
  message(STATUS "Mkl found.")
  set(HAVE_MKL 1 CACHE BOOL "Whether have Mkl")
endif ()

#
#  IOMP5 is sometimes needed to get link to work.
#  Go ahead and find it to be available.
#
#  Set iomp_dir
get_filename_component(Iomp5_ROOT_DIR ${Mkl_ROOT_DIR}/../compiler/lib/${Mkl_ARCH} REALPATH)

# Not quite sure about this -- this comes from Rood
if (WIN32)
  SciFindPackage(PACKAGE "Iomp5" LIBRARIES "libiomp5md")
else ()
  SciFindPackage(PACKAGE "Iomp5" LIBRARIES "iomp5")
endif ()

