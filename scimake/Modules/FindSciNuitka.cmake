# - FindSciNuitka: This module looks for Nuitka binary
# Nuitka is a documentation generation tool.  Please see
# http://www.sphinx.org
#
# This modules defines the following variables:
#
#   Nuitka_EXECUTABLE     = The path to the sphinx command.
#   NUITKA_FOUND          = Was Nuitka found or not?
#

#################################################################
# Find Nuitka...
#
# $Id: FindSciNuitka.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2016-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#################################################################

if (WIN32)
  set(sfxs Scripts)
else ()
  set(sfxs bin)
endif ()
if (DEBUG_CMAKE)
  message(STATUS "Looking for nuitka with SUPRA_SEARCH_PATH = ${SUPRA_SEARCH_PATH} and sfxs = ${sfxs}.")
endif ()
find_program(Nuitka_EXECUTABLE
  nuitka
  PATHS ${SUPRA_SEARCH_PATH}
  PATH_SUFFIXES ${sfxs}
)
if (Nuitka_EXECUTABLE)
  set(NUITKA_FOUND 1 CACHE BOOL "Found Nuitka binary")
  message(STATUS "Nuitka_EXECUTABLE found.")
  message(STATUS "Nuitka_EXECUTABLE  = ${Nuitka_EXECUTABLE}")
else ()
  message(STATUS "Nuitka_EXECUTABLE NOT found.")
endif ()

# Enable setting an option
# The -d flag sets the cache directory to be non-hidden
set(Nuitka_OPTS      "" CACHE STRING "Options to be passed to the nuitka executable")

