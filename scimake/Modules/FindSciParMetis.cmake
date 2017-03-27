# - FindSciParMetis: Module to find include directories and libraries
#   for ParMetis. This module was implemented as there is no stock
#   CMake module for ParMetis.
#   It also looks for the corresponding libmetis.a
#
# This module can be included in CMake builds in find_package:
#   find_package(SciParMetis REQUIRED)
#
# This module will define the following variables:
#  HAVE_PARMETIS         = Whether have the ParMetis library
#  ParMetis_INCLUDE_DIRS = Location of ParMetis includes
#  ParMetis_LIBRARY_DIRS = Location of ParMetis libraries
#  ParMetis_LIBRARIES    = Required libraries
#  ParMetis_STLIBS       = Location of ParMetis static library

######################################################################
#
# SciFindParMetis: find includes and libraries for ParMetis.
#
# $Id: FindSciParMetis.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################
set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH})

# ParMetis version > 4 requires gklib (distributed with package)
if (FIND_ParMetis_GKLIB)
  set(ParMetis_SEARCH_LIBS "parmetis;metis;GKlib")
else ()
  set(ParMetis_SEARCH_LIBS "parmetis;metis")
endif ()

SciFindPackage(PACKAGE "ParMetis"
              INSTALL_DIR "parmetis-par"
              HEADERS "parmetis.h"
              LIBRARIES "${ParMetis_SEARCH_LIBS}"
              )

if (PARMETIS_FOUND)
  message(STATUS "Found ParMetis")
  set(HAVE_PARMETIS 1 CACHE BOOL "Whether have the PARMETIS library")
else ()
  message(STATUS "Did not find ParMetis.  Use -DPARMETIS_DIR to specify the installation directory.")
  if (SciParMetis_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

