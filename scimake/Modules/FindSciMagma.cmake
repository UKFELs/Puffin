# - FindSciMagma: Module to find the MAGMA library.
#
# Module usage:
#   find_package(SciMagma ...)
#
# Should probably be modified to use SciFindPackage...

######################################################################
#
# FindSciMagma.cmake: Find the MAGMA library.
#
# $Id: FindSciMagma.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (WIN32)
  set(MAGMA_LIB_PREFIX "lib")
  set(MAGMA_LIB_SUFFIX ".lib")
else ()
  set(MAGMA_LIB_PREFIX "lib")
  set(MAGMA_LIB_SUFFIX ".a")
endif ()

SciFindPackage(PACKAGE "magma"
  INSTALL_DIR "${magma_ROOT_DIR}"
  LIBRARIES "${MAGMA_LIB_PREFIX}magma${MAGMA_LIB_SUFFIX}"
  HEADERS "magma.h"
  INCLUDE_SUBDIRS "include"
  LIBRARY_SUBDIRS "lib"
  ALLOW_LIBRARY_DUPLICATES TRUE
)

if (MAGMA_FOUND)
  message(STATUS "Found MAGMA.")
  set(HAVE_MAGMA 1 CACHE BOOL "Whether have MAGMA")
else ()
   if (SciMagma_FIND_REQUIRED)
     message(FATAL_ERROR "Could not find MAGMA")
   else ()
     if (magma_ROOT_DIR)
       message(STATUS "MAGMA not found in ${magma_ROOT_DIR}")
     else ()
       message(STATUS "Not searching for MAGMA")
     endif ()
   endif ()
endif ()

