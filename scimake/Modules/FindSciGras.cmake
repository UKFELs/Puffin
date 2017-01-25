# - FindTxGras: Module to find include directories and
#   libraries for Gras.
#
# Module usage:
#   find_package(TxGras ...)
#
# This module will define the following variables:
#  HAVE_GRAS, GRAS_FOUND = Whether libraries and includes are found
#  Gras_INCLUDE_DIRS       = Location of Gras includes
#  Gras_LIBRARY_DIRS       = Location of Gras libraries
#  Gras_LIBRARIES          = Required libraries

###########################################################
#
# Find module for Gras installation
#
# $Id: FindSciGras.cmake 1103 2016-11-16 23:42:38Z mrcopper $
#
# Copyright 2016-2016, Tech-X Corporation, Boulder, CO.
# All rights reserved.
#
###########################################################

if (DEFINED GRAS_DIR)
  message(STATUS "[FindGras.cmake] - GRAS_DIR is ${GRAS_DIR}")
endif ()

SciGetInstSubdirs(Gras-sersh instdirs)

SciFindPackage(
  PACKAGE Gras
  INSTALL_DIRS ${instdirs}
  PROGRAMS gras
)

if (GRAS_FOUND)
  message(STATUS "Found Gras")
  set(HAVE_GRAS 1 CACHE BOOL "Whether have Gras")

  # Derive the base Gras directory
  set(Gras_FOUND_PROGRAM ${Gras_gras})
  if (NOT Gras_FOUND_PROGRAM)
    set(Gras_FOUND_PROGRAM ${Gras_gras})
  endif ()

# This can only happen if there is something wrong with SciFindPackage,
# and so a fatal error is appropriate.
  if (NOT Gras_FOUND_PROGRAM)
    message(FATAL_ERROR "No Gras executable was found?  Failing.")
  endif ()

  if (DEBUG_CMAKE)
    message(STATUS "Using path ${Gras_FOUND_PROGRAM} to derive base Gras directory.")
  endif ()

  get_filename_component(GRAS_DIR ${Gras_FOUND_PROGRAM} PATH)
  # get_filename_component(GRAS_DIR ${GRAS_DIR} PATH)
  get_filename_component(GRAS_DIR ${GRAS_DIR}/.. REALPATH)
  if (DEBUG_CMAKE)
    message(STATUS "GRAS_DIR is ${GRAS_DIR}")
  endif ()

  if (DEBUG_CMAKE)
    message("Trying to run executable ${Gras_FOUND_PROGRAM} to determine Gras version.")
  endif ()

#  include(${TXCMAKE_DIR}/TxEngFindVersion.cmake)
#  TxEngFindVersion(${Gras_FOUND_PROGRAM} EXE_VERSION EXE_REVISION)
#  set(Gras_VERSION ${EXE_VERSION})
#  set(GRAS_VERSION ${EXE_VERSION})
#  set(Gras_REVISION ${EXE_REVISION})
else ()
  message(STATUS "Gras not found. Use -DGRAS_DIR to specify the installation directory.")
  if (SciGras_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
  set(Gras_VERSION "GrasNotFound")
  set(Gras_REVISION "GrasNotFound")
endif ()

