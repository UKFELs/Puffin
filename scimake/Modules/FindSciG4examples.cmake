# - FindSciG4examples: Module to find examples for G4engine.
#
# Module usage:
#   find_package(SciG4examples ...)
#

###########################################################
#
# Find module for G4engine examples  installation
#
# $Id: FindSciG4examples.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2014-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
###########################################################

# The command line override is PKG_ROOT_DIR
if (DEFINED G4EXAMPLES_ROOT_DIR)
  message(STATUS "[FindSciG4examples.cmake] - G4EXAMPLES_ROOT_DIR is ${G4EXAMPLES_ROOT_DIR}")
endif ()

SciFindPackage(
  PACKAGE G4examples
  INSTALL_DIR g4examples-lite
  FILES description.ini
)

if (G4EXAMPLES_FOUND)
  get_filename_component(G4examples_DIR "${G4examples_FILES}" PATH)
# EXAMPLES_DIR used in TxComposerBase to install the examples into the composer.
  set(EXAMPLES_DIR "${G4examples_DIR}")
  message(STATUS "[FindSciG4examples.cmake] - EXAMPLES_DIR is ${G4examples_DIR}")
else ()
  message(STATUS "G4examples not found. Use -DEXAMPLES_DIR to specify the installation directory.")
  if (G4examples_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

