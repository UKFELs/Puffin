# - FindSciMdsplus: Module to find include directories and
#   libraries for Mdsplus.
#
# Module usage:
#   find_package(SciMdsplus ...)
#
# This module will define the following variables:
#  HAVE_MDSPLUS, MDSPLUS_FOUND = Whether libraries and includes are found
#  Mdsplus_INCLUDE_DIRS        = Location of Mdsplus includes
#  Mdsplus_LIBRARY_DIRS        = Location of Mdsplus libraries
#  Mdsplus_LIBRARIES           = Required libraries

######################################################################
#
# FindMdsplus: find includes and libraries for mdsplus
#
# $Id: FindSciMdsplus.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

SciFindPackage(PACKAGE "Mdsplus"
              INSTALL_DIR "mdsplus"
              HEADERS "mdsdescrip.h" "mdslib.h"
              LIBRARIES "MdsLib"
              INCLUDE_SUBDIRS "include"
              LIBRARY_SUBDIRS "lib"
              )

if (MDSPLUS_FOUND)
  message(STATUS "Found Mdsplus")
  set(HAVE_MDSPLUS 1 CACHE BOOL "Whether have the mdsplus library")
else ()
   message(STATUS "Did not find Mdsplus.  Use -DMdsplus_ROOT_DIR to specify the installation directory.")
   if (SciMdsplus_FIND_REQUIRED)
       message(FATAL_ERROR "Failing.")
   endif ()
endif ()

