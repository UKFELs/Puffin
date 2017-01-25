# - FindSciCLapackscimake: Module to find include directories and
#   libraries for CLapackscimake.
#
# Module usage:
#   find_package(SciCLapackscimake ...)
#
# This module will define the following variables:
#  HAVE_CLAPACKCMAKE, CLAPACKCMAKE_FOUND = Whether libraries and includes are found
#  CLapackscimake_INCLUDE_DIRS = Location of CLapackscimake includes
#  CLapackscimake_LIBRARY_DIRS = Location of CLapackscimake libraries
#  CLapackscimake_LIBRARIES    = Required libraries

######################################################################
#
# FindCLapackscimake: find includes and libraries for txbase
#
# $Id: FindSciCLapackCMake.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(clapack_libs "lapack;blas;")
if (BUILD_WITH_SHARED_RUNTIME)
  set(clapack_libs "${clapack_libs}libf2c")
else ()
  set(clapack_libs "${clapack_libs}f2c")
endif ()

  SciFindPackage(PACKAGE "CLapackscimake"
                INSTALL_DIR "clapack_cmake"
                HEADERS "clapack.h;f2c.h;blaswrap.h"
                LIBRARIES ${clapack_libs}
                )

if (CLAPACKSCIMAKE_FOUND)
  message(STATUS "CLapackscimake found.")
  set(HAVE_CLAPACKCMAKE 1 CACHE BOOL "Whether have CLapackscimake")
else ()
  message(STATUS "Did not find CLapackscimake.  Use -DCLapackscimake_ROOT_DIR to specify the installation directory.")
  if (SciCLapackscimake_FIND_REQUIRED)
    message(FATAL_ERROR "Failed")
  endif ()
endif ()

