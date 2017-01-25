# - FindSciTxPLAPACK: Module to find include directories and
#   libraries for TxPLAPACK.
#
# Module usage:
#   find_package(SciTxPLAPACK ...)
#
# This module will define the following variables:
#  HAVE_TXPLAPACK, TXPLAPACK_FOUND = Whether libraries and includes are found
#  TxPLAPACK_INCLUDE_DIRS       = Location of Gpulib includes
#  TxPLAPACK_LIBRARY_DIRS       = Location of Gpulib libraries
#  TxPLAPACK_LIBRARIES          = Required libraries

######################################################################
#
# FindSciTxPLAPACK.cmake: Find the TxPLAPACK libraries
#
# $Id: FindSciTxPLAPACK.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (BUILD_WITH_SHARED_RUNTIME)
  set(instdirs txplapack-parsh)
else ()
  set(instdirs txplapack-par)
endif ()

SciFindPackage(
           PACKAGE "TxPLAPACK"
           INSTALL_DIR ${instdirs}
           HEADERS "txplapack_version.h"
           INCLUDE_SUBDIRS include
           LIBRARIES "txplapack"
           LIBRARY_SUBDIRS "lib/${CXX_COMP_LIB_SUBDIR};lib"
           )

if (TXPLAPACK_FOUND)
  message(STATUS "Found txplapack")
  set(HAVE_TXPLAPACK 1 CACHE BOOL "Whether have txplapack")
else ()
  message(STATUS "Did not find txplapack.  Use -DTXPLAPACK_DIR to specify the installation directory.")
  if (TxPLAPACK_FIND_REQUIRED)
    message(FATAL_ERROR "Failed")
  endif ()
endif ()

