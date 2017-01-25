# - FindAcisVizExchange: Module to find include directories and
#   libraries for ACIS Viz Exchange Library
#
# Module usage:
#   find_package(AcisVizExchange ...)
#
# This module will define the following variables:
#  HAVE_ACISVIZEXCHANGE, ACISVIZEXCHANGE_FOUND = Whether libraries and includes are found
#  AcisVizExchange_INCLUDE_DIRS    = Location of AcisVizExchange includes
#  AcisVizExchange_LIBRARY_DIRS    = Location of AcisVizExchange libraries
#  AcisVizExchange_LIBRARIES       = Required libraries

######################################################################
#
# FindAcisVizExchange: find includes and libraries for ACIS 3D Interop Libray
#
# $Id: FindSciAcisVizExchange.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(AcisVizExchange_LIBRARY_LIST
  SpaACIS
  SpaHBridge
  SpaHPart
  hoops1918
)

set(instdirs "AcisVizExchange")

SciFindPackage(PACKAGE "AcisVizExchange"
  INSTALL_DIR ${instdirs}
  HEADERS "spa_unlock_state.h"
  LIBRARIES ${AcisVizExchange_LIBRARY_LIST}
  LIBRARY_SUBDIRS "/bin/macos_a64"
)

if (ACISVIZEXCHANGE_FOUND)
  message(STATUS "[FindSciAcisVizExchange.cmake] - Found AcisVizExchange")
  message(STATUS "[FindSciAcisVizExchange.cmake] - AcisVizExchange_INCLUDE_DIRS = ${AcisVizExchange_INCLUDE_DIRS}")
  message(STATUS "[FindSciAcisVizExchange.cmake] - AcisVizExchange_LIBRARIES = ${AcisVizExchange_LIBRARIES}")
  set(HAVE_ACISVIZEXCHANGE 1 CACHE BOOL "Whether have AcisVizExchange.")
else ()
  message(STATUS "[FindSciAcisVizExchange.cmake] - Did not find ACISVIZEXCHANGE, use -DACISVIZEXCHANGE_DIR to supply the ACISVIZEXCHANGE installation directory.")
  if (SciAcisVizExchange_FIND_REQUIRED)
    message(FATAL_ERROR "[FindSciAcisVizExchange.cmake] - Failing.")
  endif ()
endif ()

