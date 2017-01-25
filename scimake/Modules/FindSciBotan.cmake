# - FindSciBotann: Module to find include directories and libraries
#   for Botan. This module was implemented as there is no stock
#   CMake module for Botan. This is currently being used by QuIDS
#   project.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciBotan REQUIRED)
#
# This module will define the following variables:
#  HAVE_BOTAN         = Whether have the Botan library
#  Botan_INCLUDE_DIRS = Location of Botan includes
#  Botan_LIBRARY_DIRS = Location of Botan libraries
#  Botan_LIBRARIES    = Required libraries
#  Botan_STLIBS       = Static libraries (if found in same directory)
#  Botan_DLLS         = Windows DLLs (if found in same dir or sister bin dir)
#
# ========= ========= ========= ========= ========= =============== ==========
#
# Variables used by this module, which can be set before calling find_package
# to influence default behavior
#
# Botan_ROOT_DIR           Specifies the root dir of the Botan installation
#
# BUILD_WITH_PYCSH_RUNTIME Specifies to look for installation dirs,
#                          botan-pycsh or botan-sersh
# ENABLE_SHARED OR BUILD_WITH_SHARED_RUNTIME OR BUILD_SHARED_LIBS
#                          operative if BUILD_WITH_PYCSH_RUNTIME not set
#                          Specify to look for installation dir, botan-sersh.

######################################################################
#
# SciFindBotan: find includes and libraries for Botan.
#
# $Id: FindSciBotan.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH})

if (BUILD_WITH_PYCSH_RUNTIME)
  set(instdirs botan-pycsh botan-sersh)
elseif (ENABLE_SHARED OR BUILD_WITH_SHARED_RUNTIME OR BUILD_SHARED_LIBS)
  set(instdirs botan-sersh)
else ()
  set(instdirs botan)
endif ()

SciFindPackage(
  PACKAGE Botan
  INSTALL_DIRS ${instdirs}
  HEADERS botan/botan.h
  LIBRARIES botan
)

if (BOTAN_FOUND)
  message(STATUS "Found Botan")
  set(HAVE_BOTAN 1 CACHE BOOL "Whether have the BOTAN library")
  if (WIN32 AND Botan_DLLS)
# Botan sets this in its installed include files
    # set(Botan_DEFINITIONS {$Botan_DEFINITIONS})
  endif ()
  message(STATUS "Botan_DEFINITIONS = ${Botan_DEFINITIONS}.")
else ()
  message(STATUS "Did not find Botan.  Use -DBOTAN_DIR to specify the installation directory.")
  if (SciBotan_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

