# - FindSciMetis: Module to find include directories and libraries
#   for Metis. This module was implemented as there is no stock
#   CMake module for Metis. This is currently being used by QuIDS
#   project.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciMetis REQUIRED)
#
# This module will define the following variables:
#  HAVE_METIS         = Whether have the Metis library
#  Metis_INCLUDE_DIRS = Location of Metis includes
#  Metis_LIBRARY_DIRS = Location of Metis libraries
#  Metis_LIBRARIES    = Required libraries
#  Metis_STLIBS       = Location of Metis static library

######################################################################
#
# SciFindMetis: find includes and libraries for Metis.
#
# $Id: FindSciMetis.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################
set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH})

SciFindPackage(PACKAGE "Metis"
              INSTALL_DIR "metis"
              HEADERS "metis.h"
              LIBRARIES "metis"
              )

if (METIS_FOUND)
  message(STATUS "Found Metis")
  set(HAVE_METIS 1 CACHE BOOL "Whether have the METIS library")
else ()
  message(STATUS "Did not find Metis.  Use -DMETIS_DIR to specify the installation directory.")
  if (SciMetis_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

