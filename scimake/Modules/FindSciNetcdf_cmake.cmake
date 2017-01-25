# - FindSciNetcdf_cmake: Module to find include directories and libraries
#   for Netcdf. This module was implemented as there is no stock
#   CMake module for Netcdf.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciNetcdf_cmake REQUIRED)
#
# This module will define the following variables:
#  HAVE_NETCDF_CMAKE   = Whether have the Netcdf library
#  Netcdf_cmake_INCLUDE_DIRS = Location of Netcdf_cmake includes
#  Netcdf_cmake_LIBRARY_DIRS = Location of Netcdf_cmake libraries
#  Netcdf_cmake_LIBRARIES    = Required libraries
#  Netcdf_cmake_STLIBS       = Location of Netcdf_cmake static library

######################################################################
#
# SciFindNetcdf_cmake: find includes and libraries for Netcdf_cmake.
#
# $Id: FindSciNetcdf_cmake.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(instdirs netcdf_cmake)

set(desiredlibs netcdf)

SciFindPackage(PACKAGE "Netcdf_cmake"
  INSTALL_DIR ${instdirs}
  HEADERS "netcdf.h"
  LIBRARIES ${desiredlibs}
)

if (NETCDF_CMAKE_FOUND)
  message(STATUS "Found Netcdf_cmake")
  set(HAVE_NETCDF_CMAKE 1 CACHE BOOL "Whether have the NETCDF_CMAKE library")
else ()
  message(STATUS "Did not find Netcdf_cmake.  Use -DNETCDF_CMAKE_DIR to specify the installation directory.")
  if (SciNetcdf_cmake_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

