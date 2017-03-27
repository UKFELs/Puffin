# - FindSciNemesis: Module to find include directories and libraries
#   for Nemesis. This module was implemented as there is no stock
#   CMake module for Nemesis.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciNemesis REQUIRED)
#
# This module will define the following variables:
#  HAVE_NEMESIS   = Whether have the Nemesis library
#  Nemesis_INCLUDE_DIRS = Location of Nemesis includes
#  Nemesis_LIBRARY_DIRS = Location of Nemesis libraries
#  Nemesis_LIBRARIES    = Required libraries
#  Nemesis_STLIBS       = Location of Nemesis static library

######################################################################
#
# SciFindNemesis: find includes and libraries for Nemesis.
#
# $Id: FindSciNemesis.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(instdirs nemesis)

set(desiredlibs nemesis)
set(desiredIncs "ne_nemesisI.h")

SciFindPackage(PACKAGE "Nemesis"
  INSTALL_DIR ${instdirs}
  HEADERS     ${desiredIncs}
  LIBRARIES   ${desiredlibs}
)

if (NEMESIS_FOUND)
  message(STATUS "Found Nemesis")
  set(HAVE_NEMESIS 1 CACHE BOOL "Whether have the NEMESIS library")
else ()
  message(STATUS "Did not find Nemesis.  Use -DNEMESIS_DIR to specify the installation directory.")
  if (SciNemesis_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

