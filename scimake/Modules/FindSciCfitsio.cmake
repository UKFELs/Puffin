# - FindSciCfitsion: Module to find include directories and libraries
#   for Cfitsio. This module was implemented as there is no stock
#   CMake module for Cfitsio. This is currently being used by QuIDS
#   project.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciCfitsio REQUIRED)
#
# This module will define the following variables:
#  HAVE_CFITSIO         = Whether have the Cfitsio library
#  Cfitsio_INCLUDE_DIRS = Location of Cfitsio includes
#  Cfitsio_LIBRARY_DIRS = Location of Cfitsio libraries
#  Cfitsio_LIBRARIES    = Required libraries
#  Cfitsio_STLIBS       = Location of Cfitsio static library

######################################################################
#
# SciFindCfitsio: find includes and libraries for Cfitsio.
#
# $Id: FindSciCfitsio.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################
set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH})

SciFindPackage(PACKAGE "Cfitsio"
              INSTALL_DIR "cfitsio"
              HEADERS "fitsio.h"
              LIBRARIES "cfitsio"
              )

if (CFITSIO_FOUND)
  message(STATUS "Found Cfitsio")
  set(HAVE_CFITSIO 1 CACHE BOOL "Whether have the CFITSIO library")
else ()
  message(STATUS "Did not find Cfitsio.  Use -DCFITSIO_DIR to specify the installation directory.")
  if (SciCfitsio_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

