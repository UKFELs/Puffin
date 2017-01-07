# - FindSciFftw3: Module to find include directories and
#   libraries for Fftw3
#
# Module usage:
#   find_package(SciFftw3 ...)
#
# This module will define the following variables:
#  HAVE_FFTW3, FFTW3_FOUND = Whether libraries and includes are found
#  Fftw3_INCLUDE_DIRS       = Location of Polyswift includes
#  Fftw3_LIBRARY_DIRS       = Location of Polyswift libraries
#  Fftw3_LIBRARIES          = Required libraries

######################################################################
#
# FindSciFftw3: find includes and libraries for Fftw3
#
# $Id: FindSciFftw3.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (DEFINED Fftw3_FIND_VERSION)
  set(Fftw3_SEARCH "fftw3${Fftw3_FIND_VERSION}")
elseif (ENABLE_PARALLEL)
  set(Fftw3_SEARCH "fftw3-par")
else ()
  set(Fftw3_SEARCH "fftw3")
endif ()

if (ENABLE_PARALLEL)
  message(STATUS "Looking for parallel FFTW3")
  SciFindPackage(PACKAGE "Fftw3"
                INSTALL_DIR "${Fftw3_SEARCH}"
                HEADERS "fftw3.h;fftw3.f;fftw3-mpi.h"
                LIBRARIES "fftw3;fftw3_mpi"
                )
else ()
  message(STATUS "Looking for serial FFTW3")
  SciFindPackage(PACKAGE "Fftw3"
                INSTALL_DIR "${Fftw3_SEARCH}"
                HEADERS "fftw3.h;fftw3.f"
                LIBRARIES "fftw3"
                )
endif ()

if (FFTW3_FOUND)
  message(STATUS "Found Fftw3")
  set(HAVE_FFTW3 1 CACHE BOOL "Whether have the FFTW3 library")
else ()
  message(STATUS "Did not find Fftw3.  Use -DFFTW3_DIR to specify the installation directory.")
  if (SciFftw3_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()
