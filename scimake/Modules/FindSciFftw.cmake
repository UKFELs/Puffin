# - FindSciFftw: Module to find include directories and
#   libraries for Fftw.
#
# Module usage:
#   find_package(SciFftw ...)
#
# This module will define the following variables:
#  HAVE_FFTW, FFTW_FOUND = Whether libraries and includes are found
#  Fftw_INCLUDE_DIRS       = Location of Fftw includes
#  Fftw_LIBRARY_DIRS       = Location of Fftw libraries
#  Fftw_LIBRARIES          = Required libraries

######################################################################
#
# SciFindFftw: find includes and libraries for txbase
#
# $Id: FindSciFftw.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (ENABLE_PARALLEL)
  message(STATUS "Looking for parallel FFTW")
  SciFindPackage(PACKAGE "Fftw"
                INSTALL_DIR "fftw-par"
                HEADERS "fftw.h;rfftw.h;fftw_mpi.h;rfftw_mpi.h"
                LIBRARIES "fftw;rfftw;fftw_mpi;rfftw_mpi"
                )
else ()
  message(STATUS "Looking for serial FFTW")
  SciFindPackage(PACKAGE "Fftw"
                INSTALL_DIR "fftw"
                HEADERS "fftw.h;rfftw.h"
                LIBRARIES "fftw;rfftw"
                )
endif ()

if (FFTW_FOUND)
  message(STATUS "Found Fftw")
  set(HAVE_FFTW 1 CACHE BOOL "Whether have the FFTW library")
else ()
  message(STATUS "Did not find Fftw.  Use -DFFTW_DIR to specify the installation directory.")
  if (SciFftw_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

