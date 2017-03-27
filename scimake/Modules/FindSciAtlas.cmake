# - FindSciAtlas: Module to find include directories and
#   libraries for Atlas.
#
# Module usage:
#   find_package(SciAtlas ...)
#
# This module will define the following variables:
#  HAVE_ATLAS, ATLAS_FOUND = Whether libraries and includes are found
#  Atlas_INCLUDE_DIRS = Location of Atlas includes
#  Atlas_LIBRARY_DIRS = Location of Atlas libraries
#  Atlas_LIBRARIES    = Required libraries

######################################################################
#
# FindSciAtlas: find includes and libraries for txbase
#
# $Id: FindSciAtlas.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# Note from Jon Rood 8/24/2012:
# The library order in this probably isn't ideal, but after trying
# several permutations of linking order, this LIBRARIES line with
# duplicates works for linking Atlas and MAGMA to GPULib and the optional
# ALLOW_LIBRARY_DUPLICATES parameter for SciFindPackage was created
# specifically for doing this. If someone is looking to use SciFindAtlas
# and wants to change anything here, you should consult the GPULib project.

set(ALLOW_LIBRARY_DUPLICATES TRUE)

SciFindPackage(PACKAGE "Atlas"
              INSTALL_DIR "atlas"
              HEADERS "clapack.h;cblas.h"
              LIBRARIES "lapack;atlas;cblas;f77blas;lapack;atlas;lapack"
              ALLOW_LIBRARY_DUPLICATES
              )

if (ATLAS_FOUND)
  message(STATUS "Atlas found.")
  set(HAVE_ATLAS 1 CACHE BOOL "Whether have Atlas")
else ()
  message(STATUS "Did not find Atlas.  Use -DAtlas_ROOT_DIR to specify the installation directory.")
  if (SciAtlas_FIND_REQUIRED)
    message(FATAL_ERROR "Failed")
  endif ()
endif ()

