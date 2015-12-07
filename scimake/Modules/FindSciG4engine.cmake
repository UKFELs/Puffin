# - FindSciG4engine: Module to find include directories and
#   libraries for G4engine.
#
# Module usage:
#   find_package(SciG4engine ...)
#
# This module will define the following variables:
#  HAVE_G4ENGINE, G4ENGINE_FOUND = Whether libraries and includes are found
#  G4engine_INCLUDE_DIRS       = Location of G4engine includes
#  G4engine_LIBRARY_DIRS       = Location of G4engine libraries
#  G4engine_LIBRARIES          = Required libraries

###########################################################
#
# Find module for G4engine installation
#
# $Id: FindSciG4engine.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
###########################################################

if (DEFINED G4ENGINE_DIR)
  message(STATUS "[FindSciG4engine.cmake] - G4ENGINE_DIR is ${G4ENGINE_DIR}")
endif ()

# We have a problem in SciFindPackage
# where if the executable has the same name as a directory
# scimake's find_program() will return the DIRECTORY instead
# of the executable on windows (as scimake is a windows program
# and the cygwin soft link just looks like a file).
SciFindPackage(
  PACKAGE G4engine
  INSTALL_DIR g4engine-sersh
  PROGRAMS g4engine
)

if (G4ENGINE_FOUND)
  message(STATUS "Found G4engine")
  set(HAVE_G4ENGINE 1 CACHE BOOL "Whether have G4engine")

  set(G4engine_FOUND_PROGRAM ${G4engine_g4engine})
# This can only happen if there is something wrong with SciFindPackage,
# and so a fatal error is appropriate.
  if (NOT G4engine_FOUND_PROGRAM)
    message(FATAL_ERROR "Neither G4engine executable was found?  Failing.")
  endif ()

  if (DEBUG_CMAKE)
    message(STATUS "Using path ${G4engine_FOUND_PROGRAM} to derive base G4engine directory.")
  endif ()

  get_filename_component(G4ENGINE_DIR ${G4engine_FOUND_PROGRAM} PATH)
  # get_filename_component(G4ENGINE_DIR ${G4ENGINE_DIR} PATH)
  get_filename_component(G4ENGINE_DIR ${G4ENGINE_DIR}/.. REALPATH)
  if (DEBUG_CMAKE)
    message(STATUS "G4ENGINE_DIR is ${G4ENGINE_DIR}")
  endif ()

  if (DEBUG_CMAKE)
    message("Trying to run executable ${G4engine_FOUND_PROGRAM} to determine G4engine version.")
  endif ()

# G4engine_VERSION is required by SciComposerBase.cmake to set the package
# installer name.  It is provided by executing "executable --version",
# and contains version number/revision number.
  include(${TXCMAKE_DIR}/TxEngFindVersion.cmake)
  TxEngFindVersion(${G4engine_FOUND_PROGRAM} EXE_VERSION EXE_REVISION)
  set(G4engine_VERSION ${EXE_VERSION})
  set(G4ENGINE_VERSION ${EXE_VERSION})
  set(G4engine_REVISION ${EXE_REVISION})
else ()
  message(STATUS "G4engine not found. Use -DG4ENGINE_DIR to specify the installation directory.")
  if (SciG4engine_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
  set(G4engine_VERSION "G4engineNotFound")
  set(G4engine_REVISION "G4engineNotFound")
endif ()
