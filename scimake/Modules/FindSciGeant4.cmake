# - FindSciGeant4: Module to find include directories and
#   libraries for Geant4.
#
# Module usage:
#   find_package(SciGeant4 ...)
#
# This module will define the following variables:
#  HAVE_GEANT4, GEANT4_FOUND = Whether libraries and includes are found
#  Geant4_INCLUDE_DIRS       = Location of Geant4 includes
#  Geant4_LIBRARY_DIRS       = Location of Geant4 libraries
#  Geant4_LIBRARIES          = Required libraries
#  Geant4_DLLS               =

######################################################################
#
# FindGeant4: find includes and libraries for hdf5
#
# $Id: FindSciGeant4.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2014-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(Geant4_LIBRARY_LIST
  G4FR
  G4GMocren
  G4RayTracer
  G4Tree
  G4VRML
  G4analysis
  G4clhep
  G4digits_hits
  G4error_propagation
  G4event
  G4geometry
  G4global
  G4graphics_reps
  G4intercoms
  G4interfaces
  G4materials
  G4modeling
  G4parmodels
  G4particles
  G4persistency
  G4physicslists
  G4processes
  G4readout
  G4run
  G4track
  G4tracking
  G4visHepRep
  G4visXXX
  G4vis_management
  G4zlib
)

# The below should be optional
  # G4OpenGL
  # G4gl2ps

SciFindPackage(
  PACKAGE "Geant4"
  HEADERS "globals.hh"
  INCLUDE_SUBDIRS include/Geant4
  LIBRARIES ${Geant4_LIBRARY_LIST}
)

if (GEANT4_FOUND)
  message(STATUS "Found Geant4")
else ()
  message(STATUS "Did not find Geant4.  Use -DGeant4_ROOT_DIR to specify the installation directory.")
endif ()

