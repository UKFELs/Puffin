# - FindAcis3DInterOp: Module to find include directories and
#   libraries for ACIS Acis3D Interop Library
#
# Module usage:
#   find_package(Acis3DInterOp ...)
#
# This module will define the following variables:
#  HAVE_ACIS3DINTEROP, ACIS3DINTEROP_FOUND = Whether libraries and includes are found
#  Acis3DInterOp_INCLUDE_DIRS    = Location of Acis3DInterOp includes
#  Acis3DInterOp_LIBRARY_DIRS    = Location of Acis3DInterOp libraries
#  Acis3DInterOp_LIBRARIES       = Required libraries

######################################################################
#
# FindAcis3DInterOp: find includes and libraries for ACIS 3D Interop Libray
#
# $Id: FindSciAcis3DInterOp.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# many of the following libraries may not be needed
set(Acis3DInterOp_LIBRARY_LIST
  #SPAIParasolid
  SPAXAcisAssemblyExporter
  SPAXAcisAssemblyImporter
  SPAXAcisBase
  SPAXAcisGeometryKernelUtils
  SPAXAcisKernel
  SPAXAcisManufacturingImporter
  SPAXAcisMeshExporter
  SPAXAcisMeshImporter
  SPAXAcisPMIEntities
  SPAXAcisPMIImporter
  SPAXAssemblyRep
  SPAXBase
  SPAXCATIAV4Base
  SPAXCommon
  SPAXDefaultHeaderExporter
  SPAXDefaultHeaderImporter
  SPAXEBOMAssemblyExporter
  SPAXEBOMAssemblyImporter
  SPAXEBOMBase
  #SPAXGeneric
  #SPAXGenericManufacturingImporter
  #SPAXGenericPMIImporter
  SPAXGeometryRepresentation
  SPAXInterop
  SPAXInteropTkBRep
  SPAXInteropTkBase
  SPAXML
  SPAXManufacturingRep
  SPAXMeshRep
  SPAXPMIRep
  SPAXProeBase
  SPAXProeManufacturingExporter
  SPAXProePMIExporter
  SPAXPropertiesAssemblyImporter
  SPAXPropertiesBRepImporter
  SPAXPropertiesBase
  SPAXPropertiesHeaderImporter
  SPAXXMLTk
  SPAXXercesUtils
  SPAXicuin38
  SPAXicuio38
  SPAXicuuc38
  SpaACIS
  icudt38
  twofish
  xacis2k
  xcatia
  xcore2k
  xiges
  #xmil
  xproe
  xstep
  xvda
)

set(instdirs "Acis3DInterOp")

SciFindPackage(PACKAGE "Acis3DInterOp"
  INSTALL_DIR ${instdirs}
  HEADERS "SPAIInterop.h"
  LIBRARIES ${Acis3DInterOp_LIBRARY_LIST}
  LIBRARY_SUBDIRS "/bin/macos_a64"
)

if (ACIS3DINTEROP_FOUND)
  message(STATUS "[FindSciAcis3DInterOp.cmake] - Found Acis3DInterOp")
  message(STATUS "[FindSciAcis3DInterOp.cmake] - Acis3DInterOp_INCLUDE_DIRS = ${Acis3DInterOp_INCLUDE_DIRS}")
  message(STATUS "[FindSciAcis3DInterOp.cmake] - Acis3DInterOp_LIBRARIES = ${Acis3DInterOp_LIBRARIES}")
  set(HAVE_ACIS3DINTEROP 1 CACHE BOOL "Whether have Acis3DInterOp.")
else ()
  message(STATUS "[FindSciAcis3DInterOp.cmake] - Did not find ACIS3DINTEROP, use -DACIS3DINTEROP_DIR to supply the ACIS3DINTEROP installation directory.")
  if (SciAcis3DInterOp_FIND_REQUIRED)
    message(FATAL_ERROR "[FindSciAcis3DInterOp.cmake] - Failing.")
  endif ()
endif ()

