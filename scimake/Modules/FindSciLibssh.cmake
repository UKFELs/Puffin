# - FindSciLibsshn: Module to find include directories and libraries
#   for Libssh. This module was implemented as there is no stock
#   CMake module for Libssh. This is currently being used by QuIDS
#   project.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciLibssh REQUIRED)
#
# Input variables
# ENABLE_SHARED if true, will look for shared installation of libssh
# USE_SHARED_LIBS same as above.
#
# This module will define the following variables:
#  HAVE_LIBSSH         = Whether have the Libssh library
#  Libssh_INCLUDE_DIRS = Location of Libssh includes
#  Libssh_LIBRARY_DIRS = Location of Libssh libraries
#  Libssh_LIBRARIES    = Required libraries
#  Libssh_STLIBS       = Location of Libssh static library

######################################################################
#
# SciFindLibssh: find includes and libraries for Libssh.
#
# $Id: FindSciLibssh.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(CMAKE_FIND_LIBRARY_SUFFIXES_SAV ${CMAKE_FIND_LIBRARY_SUFFIXES})
set(libsubdirs lib)
if (USE_PYC_LIBS)
# Shared libs in ser for libssh
  set(instdirs libssh-pycsh libssh)
else ()
  set(instdirs libssh)
endif ()
if (NOT (USE_SHARED_LIBS OR BUILD_SHARED_LIBS OR
    (WIN32 AND BUILD_WITH_SHARED_RUNTIME)))
  set(libsubdirs lib/static ${libsubdirs})
  if (WIN32)
    set(CMAKE_FIND_LIBRARY_SUFFIXES .lib)
  else ()
    set(CMAKE_FIND_LIBRARY_SUFFIXES .a)
  endif ()
endif ()
# message(STATUS "libsubdirs = ${libsubdirs}.")

SciFindPackage(
  PACKAGE "Libssh"
  INSTALL_DIRS ${instdirs}
  HEADERS "libssh/libssh.h"
  LIBRARY_SUBDIRS ${libsubdirs}
  LIBRARIES "ssh"
)
set(CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES_SAV})

if (LIBSSH_FOUND)
  message(STATUS "Found Libssh")
  set(HAVE_LIBSSH 1 CACHE BOOL "Whether have the LIBSSH library")
  if (WIN32 AND Libssh_DLLS)
    set(Libssh_DEFINITIONS ${Libssh_DEFINITIONS} -ULIBSSH_STATIC)
  else ()
    set(Libssh_DEFINITIONS ${Libssh_DEFINITIONS} -DLIBSSH_STATIC)
  endif ()
else ()
  message(STATUS "Did not find Libssh.  Use -DLibssh_ROOT_DIR to specify the installation directory.")
  if (SciLibssh_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

