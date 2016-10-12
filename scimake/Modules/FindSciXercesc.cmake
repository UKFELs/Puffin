# - FindSciXercesc: Module to find include directories and libraries
#   for Xercesc.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciXercesc REQUIRED)
#
# This module will define the following variables:
#  HAVE_XERCESC         = Whether have the Xerces library
#  Xercesc_INCLUDE_DIRS = Location of Xerces includes
#  Xercesc_LIBRARY_DIRS = Location of Xerces libraries
#  Xercesc_LIBRARIES    = Required libraries
#  Xercesc_STLIBS       = Location of Xerces static library

######################################################################
#
# SciFindXercesc.cmake: find includes and libraries for Xercesc
#
# $Id: FindSciXercesc.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2012-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH})

SciFindPackage(PACKAGE "Xercesc"
              INSTALL_DIR "xercesc"
              HEADERS "xercesc"
              LIBRARIES "xerces-c"
              )
if (XERCESC_FOUND)
  message(STATUS "Found Xercesc")
  set(HAVE_XERCESC 1 CACHE BOOL "Whether have the XERCESC library")
else ()
  message(STATUS "Did not find Xercesc.  Use -DXERCESC_DIR to specify the installation directory.")
  if (SciXercesc_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

