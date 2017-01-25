# - FindSciVsreader: Module to find include directories and libraries
#   for Vsreader, VizSchema read-write library. This module was
#   implemented as there is no stock CMake module for Vsreader.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciVsreader REQUIRED)
#
# This module will define the following variables:
#  HAVE_VSREADER         = Whether have the Vsreader library
#  Vsreader_INCLUDE_DIRS = Location of Vsreader includes
#  Vsreader_LIBRARY_DIRS = Location of Vsreader libraries
#  Vsreader_LIBRARIES    = Required libraries
#  Vsreader_STLIBS       = Location of Vsreader static library

######################################################################
#
# SciFindVsreader: find includes and libraries for Vsreader.
#
# $Id: FindSciVsreader.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# only serial
# set(instdirs vsreader)

set(desiredlibs
    vsreader_static)

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

SciFindPackage(PACKAGE "Vsreader"
  # INSTALL_DIR ${instdirs} # Now done by SciFindPackage
  HEADERS ${desiredheaders}
  LIBRARIES ${desiredlibs}
)

if (VSREADER_FOUND)
  message(STATUS "Found Vsreader")
  set(HAVE_VSREADER 1 CACHE BOOL "Whether have the Vsreader library")
else ()
  message(STATUS "Did not find Vsreader.  Use -DVsreader_ROOT_DIR to specify the installation directory.")
  if (SciVsreader_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

