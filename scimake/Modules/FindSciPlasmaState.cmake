# - FindSciPlasmaState: Module to find include directories and libraries
#   for PlasmaState. This module was implemented as there is no stock
#   CMake module for PlasmaState.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciPlasmaState REQUIRED)
#
# This module will define the following variables:
#  HAVE_PLASMASTATE         = Whether have the PlasmaState library
#  PlasmaState_INCLUDE_DIRS = Location of PlasmaState includes
#  PlasmaState_LIBRARY_DIRS = Location of PlasmaState libraries
#  PlasmaState_LIBRARIES    = Required libraries
#  PlasmaState_STLIBS       = Location of PlasmaState static library

######################################################################
#
# FindSciPlasmaState: find includes and libraries for PlasmaState
#
# $Id: FindSciPlasmaState.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (ENABLE_PARALLEL)
  set(instdir plasma_state-ben plasma_state)
else ()
  set(instdir plasma_state)
endif ()
SciFindPackage(PACKAGE "PlasmaState"
  INSTALL_DIRS ${instdir}
  HEADERS state.h
  LIBRARIES PlasmaState
)

set(PLASMASTATE_DIR ${PlasmaState_DIR})
if (PLASMASTATE_FOUND)
  message(STATUS "Found PlasmaState")
  message(STATUS "${PLASMASTATE_DIR} = ${PLASMASTATE_DIR}.")
  set(HAVE_PLASMASTATE 1 CACHE BOOL "Whether have the PlasmaState library")
  set(HAVE_PlasmaState 1 CACHE BOOL "Whether have the PlasmaState library")
# Find gacodes
  include(${SCIMAKE_DIR}/SciGetDepsFromInstall.cmake)
# Get dependencies
  #SciGetDepsFromInstall(NetlibLite ${PLASMASTATE_DIR} NETLIB_LITE)
endif ()

