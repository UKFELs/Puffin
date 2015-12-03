# - FindSciSz: Module to find include directories and
#   libraries for Sz.
#
# Module usage:
#   find_package(SciSz ...)
#
# This module will define the following variables:
#  HAVE_SZ, SZ_FOUND = Whether libraries and includes are found
#  Sz_INCLUDE_DIRS       = Location of Sz includes
#  Sz_LIBRARY_DIRS       = Location of Sz libraries
#  Sz_LIBRARIES          = Required libraries

######################################################################
#
# FindSciSz: find includes and libraries for sz(compression)
#
# $Id: FindSciSz.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (CMAKE_Fortran_COMPILER_WORKS)
  set(desiredmods sz)
else ()
  set(desiredmods)
endif ()

SciFindPackage(PACKAGE "Sz"
  HEADERS "szlib.h"
  LIBRARIES "sz"
  MODULES ${desiredmods}
)

if (SZ_FOUND)
  message(STATUS "Found SZ")
  set(HAVE_SZ 1 CACHE BOOL "Whether have the sz(compression) library")
else ()
  if (SciSz_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

