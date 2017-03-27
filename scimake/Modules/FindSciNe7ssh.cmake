# - FindSciNe7sshn: Module to find include directories and libraries
#   for Ne7ssh. This module was implemented as there is no stock
#   CMake module for Ne7ssh. This is currently being used by QuIDS
#   project.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciNe7ssh REQUIRED)
#
# This module will define the following variables:
#  HAVE_NE7SSH         = Whether have the Ne7ssh library
#  Ne7ssh_INCLUDE_DIRS = Location of Ne7ssh includes
#  Ne7ssh_LIBRARY_DIRS = Location of Ne7ssh libraries
#  Ne7ssh_LIBRARIES    = Required libraries
#  Ne7ssh_STLIBS       = Location of Ne7ssh static library

######################################################################
#
# SciFindNe7ssh: find includes and libraries for Ne7ssh.
#
# $Id: FindSciNe7ssh.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH})

if (BUILD_WITH_PYCSH_RUNTIME OR BUILD_WITH_SHARED_RUNTIME)
  set(instdirs ne7ssh-pycsh ne7ssh-sersh)
else ()
  set(instdirs ne7ssh)
endif ()

SciFindPackage(
  PACKAGE "Ne7ssh"
  INSTALL_DIRS ${instdirs}
  HEADERS "ne7ssh.h"
  LIBRARIES "net7ssh"
)

if (NE7SSH_FOUND)
  message(STATUS "Found Ne7ssh")
  set(HAVE_NE7SSH 1 CACHE BOOL "Whether have the NE7SSH library")
  if (WIN32 AND Ne7ssh_DLLS)
    set(Ne7ssh_DEFINITIONS ${Ne7ssh_DEFINITIONS} -D_WINDLL)
  endif ()
  message(STATUS "Ne7ssh_DEFINITIONS = ${Ne7ssh_DEFINITIONS}.")
else ()
  message(STATUS "Did not find Ne7ssh.  Use -DNE7SSH_DIR to specify the installation directory.")
  if (SciNe7ssh_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

