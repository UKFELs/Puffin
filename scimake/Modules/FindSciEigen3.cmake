# - FindSciEigen3: Module to find include directories for Eigen3.
#
# Module usage:
#   find_package(SciEigen3 ...)
#
# Variables used by this module, which can be set before calling find_package
# to influence default behavior
# Eigen3_ROOT_DIR          Specifies the root dir of the eigen3 installation
#
# This module will define the following variables:
#  HAVE_EIGEN3,EIGEN3_FOUND = Whether libraries and includes are found
#  Eigen3_INCLUDE_DIRS       = Location of Gsl includes

######################################################################
#
# FindEigen3: find includes for Eigen3
#
# $Id: FindSciEigen3.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# Try to find an installation of eigen3 in the system include directory.
find_path(Eigen3_SYS_DIR
    signature_of_eigen3_matrix_library
    PATHS "/usr/local/include/eigen3")
if (NOT(${Eigen3_SYS_DIR} MATCHES Eigen3_SYS_DIR-NOTFOUND))
  set(Eigen3_ROOT_DIR ${Eigen3_ROOT_DIR} ${Eigen3_SYS_DIR})
endif ()

SciFindPackage(PACKAGE "Eigen3"
              INSTALL_DIR "eigen3"
              INCLUDE_SUBDIRS include/eigen3
              HEADERS "Eigen/Core"
             )

if (EIGEN3_FOUND)
  message(STATUS "Found Eigen3")
  set(HAVE_EIGEN3 1 CACHE BOOL "Whether have Eigen3")
else ()
  message(STATUS "Did not find Eigen3.  Use -DEIGEN3_DIR to specify the installation directory.")
  if (SciEigen3_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

