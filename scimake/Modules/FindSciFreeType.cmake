# - FindSciFreeType: Module to find include directories and
#   libraries for FreeType.
#
# Module usage:
#   find_package(SciFreeType ...)
#
# This module will define the following variables:
#  HAVE_FREETYPE, FREETYPE_FOUND = Whether libraries and includes are found
#  FreeType_INCLUDE_DIRS       = Location of FreeType includes
#  FreeType_LIBRARY_DIRS       = Location of FreeType libraries
#  FreeType_LIBRARIES          = Required libraries

######################################################################
#
# SciFindFreeType: find includes and libraries for z(compression)
#
# $Id: FindSciFreeType.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

SciFindPackage(
  PACKAGE   FreeType
  HEADERS   ft2build.h
  INCLUDE_SUBDIRS include include/freetype2
  LIBRARIES freetype
)

if (FREETYPE_FOUND)
  # message(STATUS "Found FreeType")
  set(HAVE_FREETYPE 1 CACHE BOOL "Whether have FreeType")
else ()
  message(STATUS "Did not find FreeType.")
  if (SciFreeType_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

