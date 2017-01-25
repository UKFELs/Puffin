######################################################################
#
# @file    FindSciFacetsIfc.cmake
#
# @brief   For finding libraries and include directories for FacetsIfc
#
# @version $Id: FindSciFacetsIfc.cmake 1081 2016-09-10 15:44:42Z cary $
#
# Copyright &copy; 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

# - FindSciFacetsIfc: Module to find include directories and
#   libraries for FacetsIfc.
#
# Module usage:
#   find_package(SciFacetsIfc ...)
#
# This module will define the following variables:
#  HAVE_FACETSIFC, FACETSIFC_FOUND = Whether libraries and includes are found
#  FacetsIfc_INCLUDE_DIRS       = Location of FacetsIfc includes
#  FacetsIfc_LIBRARY_DIRS       = Location of FacetsIfc libraries
#  FacetsIfc_LIBRARIES          = Required libraries

SciFindPackage(PACKAGE "FacetsIfc"
              HEADERS "FacetsIfc.h"
              )

if (FACETSIFC_FOUND)
  message(STATUS "Found FacetsIfc")
  set(HAVE_FACETSIFC 1 CACHE BOOL "Whether have FacetsIfc")
else ()
  message(STATUS "Did not find FacetsIfc.  Use -DFacetsIfc_ROOT_DIR to specify the installation directory.")
  if (SciFacetsIfc_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

