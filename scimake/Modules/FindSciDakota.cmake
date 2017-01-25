# - FindTxDakota: Module to find include directories and
#   libraries for Dakota.
#
# Module usage:
#   find_package(TxDakota ...)
#
# This module will define the following variables:
#  HAVE_DAKOTA, DAKOTA_FOUND = Whether libraries and includes are found
#  Dakota_INCLUDE_DIRS       = Location of Dakota includes
#  Dakota_LIBRARY_DIRS       = Location of Dakota libraries
#  Dakota_LIBRARIES          = Required libraries

###########################################################
#
# Find module for Dakota installation
#
# $Id: FindSciDakota.cmake 1099 2016-10-19 20:12:45Z mrcopper $
#
# Copyright 2016-2016, Tech-X Corporation, Boulder, CO.
# All rights reserved.
#
###########################################################

if (DEFINED DAKOTA_DIR)
  message(STATUS "[FindDakota.cmake] - DAKOTA_DIR is ${DAKOTA_DIR}")
endif ()

SciGetInstSubdirs(Dakota-ser instdirs)

SciFindPackage(
  PACKAGE Dakota
  INSTALL_DIRS ${instdirs}
  PROGRAMS dakota
)

if (DAKOTA_FOUND)
  message(STATUS "Found Dakota")
  set(HAVE_DAKOTA 1 CACHE BOOL "Whether have Dakota")

  # Derive the base Dakota directory
  set(Dakota_FOUND_PROGRAM ${Dakota_dakota})
  if (NOT Dakota_FOUND_PROGRAM)
    set(Dakota_FOUND_PROGRAM ${Dakota_dakota})
  endif ()

# This can only happen if there is something wrong with SciFindPackage,
# and so a fatal error is appropriate.
  if (NOT Dakota_FOUND_PROGRAM)
    message(FATAL_ERROR "Neither Dakota executable was found?  Failing.")
  endif ()

  if (DEBUG_CMAKE)
    message(STATUS "Using path ${Dakota_FOUND_PROGRAM} to derive base Dakota directory.")
  endif ()

  get_filename_component(DAKOTA_DIR ${Dakota_FOUND_PROGRAM} PATH)
  # get_filename_component(DAKOTA_DIR ${DAKOTA_DIR} PATH)
  get_filename_component(DAKOTA_DIR ${DAKOTA_DIR}/.. REALPATH)
  if (DEBUG_CMAKE)
    message(STATUS "DAKOTA_DIR is ${DAKOTA_DIR}")
  endif ()

  if (DEBUG_CMAKE)
    message("Trying to run executable ${Dakota_FOUND_PROGRAM} to determine Dakota version.")
  endif ()

# Dakota_VERSION is required by SciComposerBase.cmake to set the package
# installer name.  It is provided by executing "executable --version",
# and contains version number/revision number.
  include(${TXCMAKE_DIR}/TxEngFindVersion.cmake)
  TxEngFindVersion(${Dakota_FOUND_PROGRAM} EXE_VERSION EXE_REVISION)
  set(Dakota_VERSION ${EXE_VERSION})
  set(DAKOTA_VERSION ${EXE_VERSION})
  set(Dakota_REVISION ${EXE_REVISION})
else ()
  message(STATUS "Dakota not found. Use -DDAKOTA_DIR to specify the installation directory.")
  if (SciDakota_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
  set(Dakota_VERSION "DakotaNotFound")
  set(Dakota_REVISION "DakotaNotFound")
endif ()

