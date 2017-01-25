# - FindSciNetcdff: Module to find include directories and libraries
#   for Netcdf-fortran. This module was implemented as there is no stock
#   CMake module for Netcdf fortran.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciNetcdf REQUIRED)
#
# This module will define the following variables:
#  HAVE_NETCDF         = Whether have the Netcdf library
#  Netcdff_INCLUDE_DIRS = Location of Netcdf includes
#  Netcdff_LIBRARY_DIRS = Location of Netcdf libraries
#  Netcdff_LIBRARIES    = Required libraries
#  Netcdff_STLIBS       = Location of Netcdf static library

######################################################################
#
# SciFindNetcdf: find includes and libraries for Netcdf.
#
# $Id: FindSciNetcdff.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (NETCDF_BUILDS)
  set(instdirs ${NETCDF_BUILDS})
else ()
if (ENABLE_PARALLEL)
  set(instdirs netcdf-par)
else ()
  set(instdirs netcdf)
endif ()
endif ()

set(desiredlibs netcdff netcdf)

SciFindPackage(PACKAGE "Netcdf"
  INSTALL_DIR ${instdirs}
  HEADERS "netcdf.h"
  LIBRARIES ${desiredlibs}
  MODULES "netcdf"
)

if (NETCDF_FOUND)
  message(STATUS "Found Netcdff")
  set(HAVE_NETCDF 1 CACHE BOOL "Whether have the NETCDFF library")
else ()
  message(STATUS "Did not find Netcdff.  Use -DNETCDF_DIR to specify the installation directory.")
  if (SciNetcdf_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

