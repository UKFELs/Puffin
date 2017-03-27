# - FindSciCgm: Module to find include directories and
#   libraries for Cgm.
#
# Module usage:
#   find_package(SciCgm ...)
#
# This module will define the following variables:
#  HAVE_CGM, CGM_FOUND = Whether libraries and includes are found
#  Cgm_INCLUDE_DIRS       = Location of Cgm includes
#  Cgm_LIBRARY_DIRS       = Location of Cgm libraries
#  Cgm_LIBRARIES          = Required libraries
#  Cgm_DLLS               =

######################################################################
#
# FindCgm: find includes and libraries of cgm
#
# $Id: FindSciCgm.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2014-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (NOT DEFINED CGM_COMPONENTS)
  set(cgmfindlibs cgm)
else ()
  set(cgmfindlibs ${CGM_COMPONENTS})
endif ()

set(USE_PYC_LIBS TRUE)
if (NOT DEFINED USE_SHARED_LIBS)
  set(USE_SHARED_LIBS TRUE)
endif ()
SciGetInstSubdirs(cgm instdirs)

SciFindPackage(PACKAGE "Cgm"
  INSTALL_DIRS ${instdirs}
  HEADERS "cgm_version.h"
  LIBRARIES "${cgmfindlibs}"
  LIBRARY_SUBDIRS lib/${CXX_COMP_LIB_SUBDIR} lib
)

if (CGM_FOUND)
  message(STATUS "Found Cgm")
else ()
  message(STATUS "Did not find Cgm.  Use -DCgm_ROOT_DIR to specify the installation directory.")
endif ()

