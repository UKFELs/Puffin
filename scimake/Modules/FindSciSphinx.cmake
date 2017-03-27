# - FindSciSphinx: This module looks for Sphinx binary
# Sphinx is a documentation generation tool.  Please see
# http://www.sphinx.org
#
# This modules defines the following variables:
#
#   Sphinx_EXECUTABLE     = The path to the sphinx command.
#   SPHINX_FOUND          = Was Sphinx found or not?
#

#################################################################
# Find Sphinx...
#
# $Id: FindSciSphinx.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
#################################################################

if (WIN32)
  set(sfxs Scripts)
else ()
  set(sfxs bin)
endif ()
if (DEBUG_CMAKE)
  message(STATUS "Looking for sphinx-build with SUPRA_SEARCH_PATH = ${SUPRA_SEARCH_PATH} and sfxs = ${sfxs}.")
endif ()
find_program(Sphinx_EXECUTABLE
  sphinx-build
  PATHS ${SUPRA_SEARCH_PATH}
  PATH_SUFFIXES ${sfxs}
)
if (Sphinx_EXECUTABLE)
  set(SPHINX_FOUND 1 CACHE BOOL "Found Sphinx binary")
  message(STATUS "Sphinx_EXECUTABLE found.")
  message(STATUS "Sphinx_EXECUTABLE  = ${Sphinx_EXECUTABLE}")
else ()
  message(STATUS "Sphinx_EXECUTABLE NOT found.")
endif ()

# Enable setting an option
# The -d flag sets the cache directory to be non-hidden
set(Sphinx_OPTS      "" CACHE STRING "Options to be passed to the sphinx executable")

