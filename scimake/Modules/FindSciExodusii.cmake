# - FindSciExodusii: Module to find include directories and libraries
#   for ExodusII. This module was implemented as there is no stock
#   CMake module for ExodusII.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciExodusii REQUIRED)
#
# This module will define the following variables:
#  HAVE_EXODUSII   = Whether have the ExodusII library
#  Exodusii_INCLUDE_DIRS = Location of ExodusII includes
#  Exodusii_LIBRARY_DIRS = Location of ExodusII libraries
#  Exodusii_LIBRARIES    = Required libraries
#  Exodusii_STLIBS       = Location of ExodusII static library

######################################################################
#
# SciFindExodusii: find includes and libraries for ExodusII.
#
# $Id: FindSciExodusii.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(instdirs exodusii)

set(desiredlibs exodusII)
set(desiredIncs "exodusII.h" "exodusII_int.h")

SciFindPackage(PACKAGE "Exodusii"
  INSTALL_DIR ${instdirs}
  HEADERS     ${desiredIncs}
  LIBRARIES   ${desiredlibs}
)

if (EXODUSII_FOUND)
  message(STATUS "Found Exodusii")
  set(HAVE_EXODUSII 1 CACHE BOOL "Whether have the EXODUSII library")
else ()
  message(STATUS "Did not find Exodusii.  Use -DEXODUSII_DIR to specify the installation directory.")
  if (SciExodusii_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

