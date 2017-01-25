# - FindSciVsrw: Module to find include directories and libraries
#   for Vsrw, VizSchema read-write library. This module was
#   implemented as there is no stock CMake module for Vsrw.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciVsrw REQUIRED)
#
# This module will define the following variables:
#  HAVE_VSRW         = Whether have the Vsrw library
#  Vsrw_INCLUDE_DIRS = Location of Vsrw includes
#  Vsrw_LIBRARY_DIRS = Location of Vsrw libraries
#  Vsrw_LIBRARIES    = Required libraries
#  Vsrw_STLIBS       = Location of Vsrw static library

######################################################################
#
# SciFindVsrw: find includes and libraries for Vsrw.
#
# $Id: FindSciVsrw.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# only serial
set(instdirs vsrw)

set(desiredlibs vsrw)
set(desiredheaders
    VsApi.h
    VsAttribute.h
    VsDataset.h
    VsFile.h
    VsFilter.h
    VsGroup.h
    VsLog.h
    VsMDMesh.h
    VsMDVariable.h
    VsMesh.h
    VsObject.h
    VsReader.h
    VsRectilinearMesh.h
    VsRegistry.h
    VsRegistryObject.h
    VsSchema.h
    VsStaggeredField.h
    VsStructuredMesh.h
    VsUniformMesh.h
    VsUnstructuredMesh.h
    VsUtils.h
    VsVariable.h
    VsVariableWithMesh.h)
SciFindPackage(PACKAGE "Vsrw"
  INSTALL_DIR ${instdirs}
  HEADERS ${desiredheaders}
  LIBRARIES ${desiredlibs}
)

if (VSRW_FOUND)
  message(STATUS "Found Vsrw")
  set(HAVE_VSRW 1 CACHE BOOL "Whether have the Vsrw library")
else ()
  message(STATUS "Did not find Vsrw.  Use -DVSRW_DIR to specify the installation directory.")
  if (SciVsrw_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

