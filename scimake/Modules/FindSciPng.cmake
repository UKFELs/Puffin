# - FindSciPng: Module to find include directories and
#   libraries for Png.
#
# Module usage:
#   find_package(SciPng ...)
#
# This module will define the following variables:
#  HAVE_PNG, PNG_FOUND = Whether libraries and includes are found
#  Png_INCLUDE_DIRS       = Location of Png includes
#  Png_LIBRARY_DIRS       = Location of Png libraries
#  Png_LIBRARIES          = Required libraries

######################################################################
#
# SciFindPng: find includes and libraries for z(compression)
#
# $Id: FindSciPng.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

SciFindPackage(
  PACKAGE "Png"
  INSTALL_DIRS libpng-sersh
  HEADERS png.h
  LIBRARIES "png"
)

if (PNG_FOUND)
  set(HAVE_PNG 1 CACHE BOOL "Whether have the Png (compression) library")
else ()
  message(STATUS "Did not find Png (compression) library.")
  if (SciPng_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

