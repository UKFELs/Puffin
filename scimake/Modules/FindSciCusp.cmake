# - FindSciCusp: Module to find include directories for cusp.
#
# Module usage:
#   find_package(SciCusp ...)
#
# Variables used by this module, which can be set before calling find_package
# to influence default behavior
# Eigen3_ROOT_DIR          Specifies the root dir of the eigen3 installation
#
# This module will define the following variables:
#  HAVE_CUSP,CUSP_FOUND = Whether libraries and includes are found
#  Cusp_INCLUDE_DIRS       = Location of cusp includes

######################################################################
#
# FindSciCusp: find includes for cusp
#
# $Id: FindSciCusp.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# Try to find an installation of eigen3 in the system include directory.
SciFindPackage(PACKAGE "Cusp"
              INSTALL_DIR "cusp"
              HEADERS "version.h"
                  INCLUDE_SUBDIRS include include/cusp
              )

if (CUSP_FOUND)
  message(STATUS "Found Cusp")
  set(HAVE_CUSP 1 CACHE BOOL "Whether have Cusp")
else ()
  message(STATUS "Did not find Cusp.  Use -DCUSP_DIR to specify the installation directory.")
  if (SciCusp_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

