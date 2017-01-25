# - FindSciMumps: Module to find include directories and libraries
#   for Mumps. This module was implemented as there is no stock
#   CMake module for Mumps.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciMumps REQUIRED)
#
# This module will define the following variables:
#  HAVE_MUMPS         = Whether have the Mumps library
#  Mumps_INCLUDE_DIRS = Location of Mumps includes
#  Mumps_LIBRARY_DIRS = Location of Mumps libraries
#  Mumps_LIBRARIES    = Required libraries
#  Mumps_STLIBS       = Location of Mumps static library

######################################################################
#
# SciFindMumps: find includes and libraries for Mumps.
#
# $Id: FindSciMumps.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2014-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################
set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH})

if (WIN32)
  set(MUMPS_LIB_PREFIX "")
else (WIN32)
  set(MUMPS_LIB_PREFIX "lib")
endif (WIN32)

if (WIN32)
  set(MUMPS_LIB_SUFFIX "lib")
else (WIN32)
  set(MUMPS_LIB_SUFFIX "a")
endif (WIN32)

if (DEFINED MUMPS_FIND_VERSION)
  if (ENABLE_PARALLEL)
    set(Mumps_SEARCH "mumps${MUMPS_FIND_VERSION}")
  else ()
    set(Mumps_SEARCH "mumps${MUMPS_FIND_VERSION}")
  endif ()
else ()
  if (ENABLE_PARALLEL)
    set(Mumps_SEARCH "mumps-par")
  else ()
    set(Mumps_SEARCH "mumps")
  endif ()
endif ()

#
#  Define what to search for
#
if (NOT DEFINED Mumps_SEARCH_HEADERS)
  set(Mumps_SEARCH_HEADERS "cmumps_c.h;cmumps_root.h;cmumps_struc.h;dmumps_c.h;dmumps_root.h;dmumps_struc.h;mumps_compat.h;mumps_c_types.h;smumps_c.h;smumps_root.h;smumps_struc.h;zmumps_c.h;zmumps_root.h;zmumps_struc.h;mumps_common.h;mumps_headers.h;mumps_io.h;mumps_io_basic.h;mumps_io_err.h;mumps_io_thread.h;mumps_orderings.h;mumps_size.h;mumps_tags.h;")
endif ()
if (NOT DEFINED Mumps_SEARCH_LIBS)
  set(Mumps_SEARCH_LIBS "${MUMPS_LIB_PREFIX}cmumps.${MUMPS_LIB_SUFFIX};${MUMPS_LIB_PREFIX}dmumps.${MUMPS_LIB_SUFFIX};${MUMPS_LIB_PREFIX}smumps.${MUMPS_LIB_SUFFIX};${MUMPS_LIB_PREFIX}zmumps.${MUMPS_LIB_SUFFIX};${MUMPS_LIB_PREFIX}mumps_common.${MUMPS_LIB_SUFFIX};${MUMPS_LIB_PREFIX}pord.${MUMPS_LIB_SUFFIX}")
  if (USE_Mumps_Scalapack)
    set(Mumps_SEARCH_LIBS "${Mumps_SEARCH_LIBS};${MUMPS_LIB_PREFIX}scalapack.${MUMPS_LIB_SUFFIX}")
  endif ()
endif ()

SciFindPackage(PACKAGE "Mumps"
              INSTALL_DIR ${Mumps_SEARCH}
              HEADERS  ${Mumps_SEARCH_HEADERS}
              LIBRARIES ${Mumps_SEARCH_LIBS}
              )

if (MUMPS_FOUND)
  message(STATUS "Found Mumps")
  set(HAVE_MUMPS 1 CACHE BOOL "Whether have the Mumps library")
else ()
  message(STATUS "Did not find Mumps.  Use -DMumps_ROOT_DIR to specify the installation directory.")
  if (SciMumps_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

