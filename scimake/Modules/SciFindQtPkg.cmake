# - SciFindQtPkg: Module to find include directories and
#   libraries for a package installed with Qt.
#
# Module usage:
#   find_package(SciQt3D ...)
#
# This module will define the following variables:
#  Qt3D_INCLUDE_DIRS = Location of Qt3D includes
#  Qt3D_LIBRARY      = The Qt3D library

######################################################################
#
# SciFindQtPkg: find includes and libraries for a qt package
#
# $Id: SciFindQtPkg.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# SciGetStaticLibs
#
# Given a package name, find the associated include directory and library
#
include(CMakeParseArguments)
macro(SciFindQtPkg)

# Parse the arguments
  CMAKE_PARSE_ARGUMENTS(SFQP
    "REQUIRED"
    "PACKAGE"
    "HEADERS;LIBRARIES;FRAMEWORKS"
    ${ARGN}
  )

# Start message
  message("")
  message("--------- SciFindQtPkg looking for ${SFQP_PACKAGE} ---------")

# Construct various names(upper/lower case) for package
  string(REGEX REPLACE "[./-]" "_" qtpkgreg ${SFQP_PACKAGE})
# scipkgreg is the regularized package name
  string(TOUPPER ${qtpkgreg} qtpkguc)

# Assume Qt found
  if (NOT QT_DIR)
    set(QT_${qtpkguc}_FOUND FALSE)
    if (SFQP_REQUIRED)
      message(FATAL_ERROR "QT_DIR not set.  Cannot find ${SFQP_PACKAGE}.")
    else ()
      message(WARNING "QT_DIR not set.  Cannot find ${SFQP_PACKAGE}.")
    endif ()
    return()
  endif ()

# For APPLE should be setting the framework, but less familiar
  message(STATUS "QT_DIR = ${QT_DIR}.")
  message(STATUS "QT_LIBRARY_DIR = ${QT_LIBRARY_DIR}.")
  if (APPLE)
# JRC: Framework variable names should conform to standards
# CMake convention is that the library name is the framework name
    set(QT_${qtpkguc}_FRAMEWORK ${QT_LIBRARY_DIR}/${qtpkgreg}.framework)
    if (EXISTS ${QT_${qtpkguc}_FRAMEWORK})
      get_filename_component(QT_${qtpkguc}_FRAMEWORK "${QT_${qtpkguc}_FRAMEWORK}" REALPATH)
      set(QT_${qtpkguc}_FRAMEWORKS "${QT_${qtpkguc}_FRAMEWORK}")
      message(STATUS "Found ${QT_${qtpkguc}_FRAMEWORK}.")
      get_filename_component(QT_${qtpkguc}_FRAMEWORK_DIRS "${QT_${qtpkguc}_FRAMEWORK}/.." REALPATH)
      get_filename_component(QT_${qtpkguc}_FRAMEWORK_NAMES "${QT_${qtpkguc}_FRAMEWORK}" NAME)
      set(QT_${qtpkguc}_INCLUDE_DIRS ${QT_${qtpkguc}_FRAMEWORK}/Headers)
      set(QT_${qtpkguc}_LIBRARIES ${QT_${qtpkguc}_FRAMEWORKS})
      set(QT_${qtpkguc}_LIBRARY_DIRS ${QT_${qtpkguc}_FRAMEWORK_DIRS})
      set(QT_${qtpkguc}_LIBRARY_NAMES ${QT_${qtpkguc}_FRAMEWORK_NAMES})
    else ()
      message(STATUS "${QT_${qtpkguc}_FRAMEWORK} not found.")
    endif ()
  elseif (WIN32)
    set(QT_${qtpkguc}_INCLUDE_DIRS ${QT_DIR}/include/${qtpkgreg})
    set(QT_${qtpkguc}_LIBRARIES ${QT_DIR}/lib/${qtpkgreg}.lib)
    set(QT_${qtpkguc}_DLLS ${QT_DIR}/lib/${qtpkgreg}.dll)
  else ()
    set(QT_${qtpkguc}_INCLUDE_DIRS ${QT_DIR}/include/${qtpkgreg})
    set(QT_${qtpkguc}_LIBRARIES ${QT_DIR}/lib/lib${qtpkgreg}.so)
  endif ()

# Requires include dirs and libraries to be found
  set(QT_${qtpkguc}_FOUND TRUE)
  foreach (i QT_${qtpkguc}_INCLUDE_DIRS QT_${qtpkguc}_LIBRARIES)
    if (EXISTS ${${i}})
      get_filename_component(${i} "${${i}}" REALPATH)
      set(${i} "${${i}}")
    else ()
      message(STATUS "${${i}} does not exist.")
      set(QT_${qtpkguc}_FOUND FALSE)
    endif ()
  endforeach ()
  message(STATUS "QT_${qtpkguc}_FOUND = ${QT_${qtpkguc}_FOUND}.")
  if (SFQP_REQUIRED AND NOT QT_${qtpkguc}_FOUND)
    message(FATAL_ERROR "QT_${qtpkguc} not found.")
  endif ()

  SciPrintCMakeResults(QT_${qtpkguc})

endmacro()

