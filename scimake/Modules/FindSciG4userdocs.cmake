# - FindSciG4userdcos: Module to find user docs for G4engine.
#
# Module usage:
#   find_package(SciG4userdocs ...)
#

###########################################################
#
# Find module for G4engine user documentation installation
#
# $Id: FindSciG4userdocs.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2014-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
###########################################################

if (DEFINED G4USERDOCS_DIR)
  message(STATUS "[FindSciG4userdocs.cmake] - G4USERDOCS_DIR is ${G4USERDOCS_DIR}")
endif ()

SciFindPackage(
  PACKAGE G4userdocs
  INSTALL_DIR g4userdocs-lite
  FILES html/DagMCDocumentation.html
)

if (G4USERDOCS_FOUND)
  get_filename_component(USERDOCS_DIR "${G4userdocs_FILES}" PATH)
  message(STATUS "[FindSciG4userdocs.cmake] - USERDOCS_DIR is ${USERDOCS_DIR}")
else ()
  message(STATUS "G4userdocs not found. Use -DG4USERDOCS_DIR to specify the installation directory.")
  if (G4userdocs_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

