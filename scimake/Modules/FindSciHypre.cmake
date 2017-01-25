# - FindSciHypre: Module to find include directories and libraries
#   for Hypre. This module was implemented as there is no stock
#   CMake module for Hypre.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciHypre REQUIRED)
#
# This module will define the following variables:
#  HAVE_HYPRE         = Whether have the Hypre library
#  Hypre_INCLUDE_DIRS = Location of Hypre includes
#  Hypre_LIBRARY_DIRS = Location of Hypre libraries
#  Hypre_LIBRARIES    = Required libraries
#  Hypre_STLIBS       = Location of Hypre static library

######################################################################
#
# SciFindHypre: find includes and libraries for Hypre.
#
# $Id: FindSciHypre.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################
set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH})

if (WIN32)
  set(HYPRE_LIB_PREFIX "")
else (WIN32)
  set(HYPRE_LIB_PREFIX "lib")
endif (WIN32)

if (WIN32)
  set(HYPRE_LIB_SUFFIX "lib")
else (WIN32)
  set(HYPRE_LIB_SUFFIX "a")
endif (WIN32)

if (NOT DEFINED Hypre_SEARCH)
  set(Hypre_SEARCH "hypre")
endif ()
if (NOT DEFINED Hypre_SEARCH_HEADERS)
  set(Hypre_SEARCH_HEADERS "HYPRE.h")
endif ()
if (NOT DEFINED Hypre_SEARCH_LIBS)
  set(Hypre_SEARCH_LIBS "${HYPRE_LIB_PREFIX}HYPRE.${HYPRE_LIB_SUFFIX}")
endif ()

SciFindPackage(PACKAGE "Hypre"
              INSTALL_DIR ${Hypre_SEARCH}
              HEADERS ${Hypre_SEARCH_HEADERS}
              LIBRARIES ${Hypre_SEARCH_LIBS}
              )

if (HYPRE_FOUND)
  message(STATUS "Found HYPRE")
  set(HAVE_HYPRE 1 CACHE BOOL "Whether have the HYPRE library")
else ()
  message(STATUS "Did not find HYPRE. Use -DHYPRE_DIR to specify the installation directory.")
  if (SciHypre_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

