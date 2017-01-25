# - FindSciTxPhysics: Module to find include directories and
#   libraries for Physics.
#
# Module usage:
#   find_package(SciTxPhysics ...)
#
# This module will define the following variables:
#  HAVE_TXPHYSICS, TXPHYSICS_FOUND = Whether libraries and includes are found
#  TxPhysics_INCLUDE_DIRS       = Location of Physics includes
#  TxPhysics_LIBRARY_DIRS       = Location of Physics libraries
#  TxPhysics_LIBRARIES          = Required libraries

######################################################################
#
# FindSciTxPhysics: find includes and libraries for txbase
#
# $Id: FindSciTxPhysics.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

option(ENABLE_TXPHYSICS "Whether to enable TxPhysics" ON)
if (WIN32)
  set(txphysics_statlib txphysics.lib)
else ()
  set(txphysics_statlib libtxphysics.a)
endif ()

if (ENABLE_TXPHYSICS)
  if (HAVE_MPI)
    SciFindPackage(PACKAGE "TxPhysics"
      INSTALL_DIRS "txphysics-ben;txphysics"
      HEADERS "txphysics_version.h"
      LIBRARIES "${txphysics_statlib}"
    )
  else ()
    SciFindPackage(PACKAGE "TxPhysics"
      HEADERS "txphysics_version.h"
      LIBRARIES "${txphysics_statlib}"
    )
  endif ()
endif ()

if (TXPHYSICS_FOUND)
  # message(STATUS "Found TxPhysics.")
  set(HAVE_TXPHYSICS 1 CACHE BOOL "Whether have the TxPhysics library")
else ()
  message(WARNING "Did not find TxPhysics.  Use -DTXPHYSICS_DIR to specify the installation directory.")
  if (TxPhysics_FIND_REQUIRED)
    message(FATAL_ERROR "Failing.")
  endif ()
endif ()

