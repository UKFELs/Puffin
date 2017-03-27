# - FindSciUedge: Module to find include directories and
#   libraries for Uedge
#
# Module usage:
#   find_package(SciUedge ...)
#
# This module will define the following variables:
#  HAVE_UEDGE, UEDGE_FOUND = Whether libraries and includes are found
#  Uedge_INCLUDE_DIRS       = Location of Polyswift includes
#  Uedge_LIBRARY_DIRS       = Location of Polyswift libraries
#  Uedge_LIBRARIES          = Required libraries

######################################################################
#
# FindSciUedge: find includes and libraries for UEDGE
#
# $Id: FindSciUedge.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (ENABLE_PARALLEL)
  set(instdir uedge-par)
else ()
  set(instdir uedge)
endif ()

# Find shared libs
SciFindPackage(PACKAGE "UedgeShared"
  INSTALL_DIR "${instdir}"
  HEADERS "UedgeIfc.h"
  INCLUDE_SUBDIRS "include;uebase"
  LIBRARIES "uecxxpy;uecxxpycli"
  LIBRARY_SUBDIRS "lib;uecxxpy"
)
if (UEDGESHARED_FOUND)
  set(HAVE_UEDGE_SHARED 1 CACHE BOOL "Whether have the UEDGE_SHARED library")
endif ()

# Find static libs
SciFindPackage(PACKAGE "UedgeStatic"
  INSTALL_DIR "${instdir}"
  HEADERS "UedgeIfc.h"
  INCLUDE_SUBDIRS "include;uebase"
  LIBRARIES "uecxxst;uestat"
  LIBRARY_SUBDIRS "lib;uecxxst;uestat"
)
if (UEDGESTATIC_FOUND)
  set(HAVE_UEDGE_STATIC 1 CACHE BOOL "Whether have the UEDGE_STATIC library")
endif ()
# Make sure these are static
SciGetStaticLibs("${UedgeStatic_LIBRARIES}" UedgeStatic_LIBRARIES)

# Combined parameters
if (UEDGESHARED_FOUND OR UEDGESTATIC_FOUND)
  set(HAVE_ANY_UEDGE 1 CACHE BOOL "Whether have any UEDGE library")
endif ()
if (UEDGESHARED_FOUND AND UEDGESTATIC_FOUND)
  set(HAVE_BOTH_UEDGE 1 CACHE BOOL "Whether have both UEDGE libraries")
endif ()

