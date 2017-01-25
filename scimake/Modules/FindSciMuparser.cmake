# - FindSciMuparser: Module to find include directories and
#   libraries for Muparser.
#
# Module usage:
#   find_package(SciMuparser ...)
#
# This module will define the following variables:
#  HAVE_MUPARSER, MUPARSER_FOUND = Whether libraries and includes are found
#  Muparser_INCLUDE_DIRS       = Location of Muparser includes
#  Muparser_LIBRARY_DIRS       = Location of Muparser libraries
#  Muparser_LIBRARIES          = Required libraries

######################################################################
#
# FindMuparser: find includes and libraries for muparser
#
# $Id: FindSciMuparser.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(ALLOW_SERIAL_WITH_PARALLEL_SAV ${ALLOW_SERIAL_WITH_PARALLEL})
set(ALLOW_SERIAL_WITH_PARALLEL TRUE)
SciGetInstSubdirs(muparser instdirs)
# message(STATUS "instdirs = ${instdirs}.")
SciFindPackage(PACKAGE "Muparser"
  INSTALL_DIRS ${instdirs}
  HEADERS "muParser.h"
  LIBRARIES "muparser"
  INCLUDE_SUBDIRS "include"
  LIBRARY_SUBDIRS "lib"
)
set(ALLOW_SERIAL_WITH_PARALLEL ${ALLOW_SERIAL_WITH_PARALLEL_SAV})

if (MUPARSER_FOUND)
  message(STATUS "Found Muparser")
  set(HAVE_MUPARSER 1 CACHE BOOL "Whether have the MUPARSER library")
else ()
   message(STATUS "Did not find Muparser.  Use -DMUPARSER_DIR to specify the installation directory.")
   if (SciMuparser_FIND_REQUIRED)
       message(FATAL_ERROR "Failing.")
   endif ()
endif ()

