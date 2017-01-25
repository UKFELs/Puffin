# - FindOce: Module to find include directories and
#   libraries for Opencascade Community Edition
#
# Module usage:
#   find_package(Oce ...)
#
# This module will define the following variables:
#  HAVE_OCE, OCE_FOUND = Whether libraries and includes are found
#  Oce_INCLUDE_DIRS    = Location of Oce includes
#  Oce_LIBRARY_DIRS    = Location of Oce libraries
#  Oce_LIBRARIES       = Required libraries

######################################################################
#
# FindOce: find includes and libraries for OCE
#
# $Id: FindSciOce.cmake 1102 2016-11-02 21:55:51Z alexanda $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# Uses of these libs found from CMakeLists.txt in OCE, and
# in doxygen documentation, which shows
#    Module FoundationClasses
#    Module ModelingData
#    Module ModelingAlgorithms
#    Module Visualization
#    Module ApplicationFramework
#    Module DataExchange
#    Module Draw
#
# Analyze dependencies using otool -L on OS X for more discreteness.
# Below is a layered list, top to bottom, left to right.
# ToDo: define the SEARCHHDRS
#
# Data exchange
# TKVRML

message (STATUS "")
message (STATUS "--------- FindSciOce seeking Oce -----------")

set(OceXdeIges_SEARCHLIBS TKXDEIGES)
set(OceXdeStep_SEARCHLIBS TKXDESTEP)
# The libs below were required by TKXDEIGES
set(OceXde_SEARCHLIBS TKCDF TKV3d TKService TKHLR TKOffset TKPLCAF PTKernel TKPShape TKShapeSchema TKPCAF TKStdLSchema TKStdSchema TKXmlXCAF TKXmlXCAF TKXmlL TKXml TKBinL TKBinXCAF TKBin)
set(OceXde_SEARCHLIBS ${OceXde_SEARCHLIBS} TKXCAF TKXCAFSchema TKXmlXCAF TKBinXCAF TKCAF TKTObj TKLCAF)
# Mesh contains triangulation
set(OceMesh_SEARCHLIBS TKXMesh TKMesh)
set(OceMesh_SEARCHHDRS XBRepMesh.hxx) # contains triangulation
set(OceIges_SEARCHLIBS TKIGES)
set(OceIges_SEARCHHDRS IGESFile_Read.hxx)
# IGES dependends on AdvAlgo
set(OceAdvAlgo_SEARCHLIBS TKFillet TKBool TKPrim TKBO)
set(OceStep_SEARCHLIBS TKSTEP TKSTEP209 TKSTEPAttr TKSTEPBase)
set(OceStep_SEARCHHDRS STEPControl_Reader.hxx)
# STEP and IGES depend on this, but not STL
set(OceIoBase_SEARCHLIBS TKXSBase)
set(OceStl_SEARCHLIBS TKSTL)
set(OceAlgo_SEARCHLIBS TKFeat TKShHealing TKTopAlgo TKGeomAlgo)
set(OceModelData_SEARCHLIBS TKBRep TKG3d TKG2d TKGeomBase)
# AdvTools gone as of OCE-0.17
# set(OceTools_SEARCHLIBS TKMath TKAdvTools)
set(OceTools_SEARCHLIBS TKMath)
set(OceKernel_SEARCHLIBS TKernel)

# All the components
set(SciOce_ALL_COMPONENTS XdeIges XdeStep Xde Mesh Iges AdvAlgo Step IoBase Stl Algo ModelData Tools Kernel)

foreach (comp ${SciOce_FIND_COMPONENTS})
  set(Oce${comp}_FIND TRUE)
endforeach ()

message(STATUS "Looking for components, ${SciOce_FIND_COMPONENTS}.")

# Enforce dependencies
if (OceXdeIges_FIND)
  set(OceIges_FIND TRUE)
  set(OceXde_FIND TRUE)
endif ()
if (OceXdeStep_FIND)
  set(OceStep_FIND TRUE)
  set(OceXde_FIND TRUE)
endif ()
if (OceMesh_FIND)
  set(OceBrep_FIND TRUE)
endif ()
if (OceIges_FIND)
  set(OceAdvAlgo_FIND TRUE)
  set(OceIoBase_FIND TRUE)
endif ()
if (OceStep_FIND)
  set(OceIoBase_FIND TRUE)
endif ()
if (OceIoBase_FIND OR OceStl_FIND)
  set(OceAlgo_FIND TRUE)
endif ()
if (OceAlgo_FIND)
  set(OceModelData_FIND TRUE)
endif ()
if (OceModelData_FIND)
  set(OceTools_FIND TRUE)
endif ()
if (OceTools_FIND)
  set(OceKernel_FIND TRUE)
endif ()

# Set the libraries
set(Oce_SEARCHLIBS)
set(Oce_comps)
foreach (pkg XdeIges XdeStep Xde Mesh Iges AdvAlgo Step IoBase Stl Algo ModelData Tools Kernel)
  if (DEBUG_CMAKE)
    message(STATUS "Oce${pkg}_FIND = ${Oce${pkg}_FIND}.")
  endif ()
  if (Oce${pkg}_FIND)
    set(Oce_comps ${Oce_comps} ${pkg})
    set(Oce_SEARCHLIBS ${Oce_SEARCHLIBS} ${Oce${pkg}_SEARCHLIBS})
  endif ()
endforeach ()
message(STATUS "After dependencies, looking for components, ${Oce_comps}.")
message(STATUS "Oce_SEARCHLIBS = ${Oce_SEARCHLIBS}.")

# Worry about data exchange later

# To Do: Set variables for each group individually

# Set library subdirs
if (WIN32)
  # if (WIN64)
  if (${CMAKE_SIZEOF_VOID_P} MATCHES 8)
    set(libsubdir Win64/)
  else ()
    set(libsubdir Win32/)
  endif ()
else ()
  set(libsubdir)
endif ()

# Only sersh build exists

# All the components
set(SEARCH_RESULTS PROGRAMS FILES INCLUDE_DIRS MODULE_DIRS LIBFLAGS LIBRARY_DIRS LIBRARY_NAMES LIBRARIES STLIBS)
if (WIN32)
  set(SEARCH_RESULTS ${SEARCH_RESULTS} DLLS)
endif ()
foreach (res ${SEARCH_RESULTS})
  set(Oce_${res})
endforeach ()
set(OCE_FOUND TRUE)
# Set the installation search directory for oce with no component suffix
if (USE_OCE_SHARED)
  if (USE_PYC_LIBS)
    set(instdirs oce-pycsh oce-sersh)
  else ()
    set(instdirs oce-sersh oce-pycsh)
  endif ()
else ()
  SciGetInstSubdirs(oce instdirs)
endif ()

foreach (comp ${SciOce_ALL_COMPONENTS})
  if (Oce${comp}_FIND)
    set(Oce${comp}_ROOT_DIR ${Oce_ROOT_DIR})
    SciFindPackage(PACKAGE Oce${comp}
      INSTALL_DIRS "${instdirs}"
      HEADERS "${Oce${comp}_SEARCHHDRS}"
      LIBRARIES "${Oce${comp}_SEARCHLIBS}"
      LIBRARY_SUBDIRS "${libsubdir}lib"
      PROGRAM_SUBDIRS "${libsubdir}bin"
      FIND_QUIETLY
    )
    foreach (res ${SEARCH_RESULTS})
      set(Oce_${res} ${Oce_${res}} ${Oce${comp}_${res}})
      set(Oce${comp}_${res}
        ${Oce${comp}_${res}}
        CACHE STRING "List of all ${res} for ${Oce_${res}}"
      )
      endforeach ()
    string(TOUPPER Oce${comp} pkguc)
    if (NOT ${pkguc}_FOUND)
      message(WARNING "${pkguc}_FOUND = ${${pkguc}_FOUND}.")
      set(OCE_FOUND FALSE)
    endif ()
  endif ()
endforeach ()
foreach (res ${SEARCH_RESULTS})
  if (Oce_${res})
    list(REMOVE_DUPLICATES Oce_${res})
  endif ()
endforeach ()

find_library(Oce_PLUGINS
  NAMES FWOSPlugin
  PATHS ${Oce_LIBRARY_DIRS}
  NO_DEFAULT_PATH)

if (OCE_FOUND)
  # message(STATUS "Found Oce.")
  set(HAVE_OCE 1 CACHE BOOL "Whether have Oce library")
  SciPrintCMakeResults(Oce)
else ()
  message(STATUS "Did not find Oce.  Use -DOCE_ROOT_DIR to specify the installation directory.")
  if (Oce_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

message (STATUS "--------- FindSciOce done with Oce -----------")
message (STATUS "")

