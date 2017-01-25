# - FindSciPatoh: Module to find include directories and
#   libraries for Patoh.
#
# Module usage:
#   find_package(SciPatoh ...)
#
# This module will define the following variables:
#  HAVE_PATOH, PATOH_FOUND = Whether libraries and includes are found
#  Patoh_INCLUDE_DIRS       = Location of Patoh includes
#  Patoh_LIBRARY_DIRS       = Location of Patoh libraries
#  Patoh_LIBRARIES          = Required libraries

######################################################################
#
# FindMuparser: find includes and libraries for muparser
#
# $Id: FindSciPatoh.cmake 1081 2016-09-10 15:44:42Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

SciFindPackage(PACKAGE "Patoh"
              INSTALL_DIR "patoh"
              PROGRAMS ""
              HEADERS "patoh.h"
              LIBRARIES "patoh"
              )

if (PATOH_FOUND)
  # message(STATUS "Found Patoh")
  set(HAVE_PATOH 1 CACHE BOOL "Whether have the Patoh library")

else ()
   message(STATUS "Did not find Patoh.  Use -DPATOH_DIR to specify the installation directory.")

   if (SciPatoh_FIND_REQUIRED)
       message(FATAL_ERROR "Failing.")
   endif ()

endif ()

