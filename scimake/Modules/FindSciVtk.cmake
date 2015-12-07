# - FindSciVtk: Module to find include directories and
#   libraries for Vtk.
#
# Module usage:
#   find_package(SciVtk ...)
#
# This module will define the following variables:
#  HAVE_VTK, VTK_FOUND = Whether libraries and includes are found
#  Vtk_INCLUDE_DIRS       = Location of Vtk includes
#  Vtk_LIBRARY_DIRS       = Location of Vtk libraries
#  Vtk_LIBRARIES          = Required libraries

##################################################################
#
# Find module for VTK
#
# $Id: FindSciVtk.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
##################################################################

set(Vtk_LIBRARY_LIST
  vtkCommon
  vtkDICOMParser
  vtkFiltering
  vtkGenericFiltering
  vtkGraphics
  vtkHybrid
  vtkIO
  vtkImaging
  # vtkPythonCore
  vtkRendering
  vtkVolumeRendering
  vtkWidgets
  vtkalglib
  vtkexpat
  vtkfreetype
  vtkftgl
  vtklibxml2
  vtkjpeg
  vtkpng
  vtkproj4
  vtksqlite
  vtksys
  vtktiff
  vtkverdict
  vtkzlib
)

if (NOT Vtk_INSTALL_DIRS)
  set(Vtk_INSTALL_DIRS vtk-pycsh vtk-sersh vtk)
endif ()

SciFindPackage(
  PACKAGE Vtk
  INSTALL_DIRS ${Vtk_INSTALL_DIRS}
  INCLUDE_SUBDIRS include include/vtk/include # Second for visit installation
  HEADERS vtkObject.h
  LIBRARY_SUBDIRS lib . # Second for visit installation
  LIBRARIES "${Vtk_LIBRARY_LIST}"
)

if (VTK_FOUND)
  message(STATUS "[FindSciVtk.cmake] - Found VTK")
  message(STATUS "[FindSciVtk.cmake] - Vtk_INCLUDE_DIRS = ${Vtk_INCLUDE_DIRS}")
  message(STATUS "[FindSciVtk.cmake] - Vtk_LIBRARIES = ${Vtk_LIBRARIES}")
  set(HAVE_VTK 1 CACHE BOOL "Whether have Vtk.")
else ()
  message(STATUS "[FindSciVtk.cmake] - Did not find VTK, use -DVtk_ROOT_DIR to supply the VTK installation directory.")
  if (SciVtk_FIND_REQUIRED)
    message(FATAL_ERROR "[FindSciVtk.cmake] - Failing.")
  endif ()
endif ()

