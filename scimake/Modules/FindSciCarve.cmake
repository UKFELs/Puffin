# - FindSciCarve: Module to find include directories and
#   libraries for Carve.
#
# Module usage:
#   find_package(SciCarve ...)
#
# This module will define the following variables:
#  HAVE_CARVE, CARVE_FOUND = Whether libraries and includes are found
#  Carve_INCLUDE_DIRS      = Location of Carve includes
#  Carve_LIBRARY_DIRS      = Location of Carve libraries
#  Carve_LIBRARIES         = Required libraries

##################################################################
#
# Find module for CARVE
#
# $Id: FindSciCarve.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
##################################################################

set(Carve_LIBRARY_LIST
  carve
)

SciFindPackage(
  PACKAGE Carve
  HEADERS carve.hpp
  INCLUDE_SUBDIRS include/carve include
  LIBRARIES ${Carve_LIBRARY_LIST}
  PROGRAMS convert intersect slice triangulate view
)

if (CARVE_FOUND)
  message(STATUS "[FindSciCarve.cmake] - Found CARVE")
  message(STATUS "[FindSciCarve.cmake] - Carve_INCLUDE_DIRS = ${Carve_INCLUDE_DIRS}")
  message(STATUS "[FindSciCarve.cmake] - Carve_LIBRARIES = ${Carve_LIBRARIES}")
  set(HAVE_CARVE 1 CACHE BOOL "Whether have Carve.")
else ()
  message(STATUS "[FindSciCarve.cmake] - Did not find CARVE, use -DCARVE_DIR to supply the CARVE installation directory.")
  if (SciCarve_FIND_REQUIRED)
    message(FATAL_ERROR "[FindSciCarve.cmake] - Failing.")
  endif ()
endif ()

