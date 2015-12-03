# - FindSciPatchelf: Module to find patchelf
#
# Module usage:
#   find_package(SciPatchelf ...)
#
# This module will define the following variables:
#  PATCHELF_FOUND         = Whether Patchelf was found
#  Patchelf_patchelf    = Path to patchelf executable

######################################################################
#
# SciPatchelf: Find Patchelf
#
# $Id: FindSciPatchelf.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2011-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

message("")
message("--------- FindSciPatchelf looking for patchelf ---------")

SciFindPackage(PACKAGE Patchelf
  PROGRAMS patchelf
  # PROGRAM_SUBDIRS bin
)
set(PATCHELF_FOUND ${Patchelf_PROGRAMS_FOUND})

if (PATCHELF_FOUND)
  message(STATUS "Patchelf_patchelf found.")
  message(STATUS "Patchelf_patchelf = ${Patchelf_patchelf}")
else ()
  message(STATUS "Patchelf_patchelf not found. Cannot repair rpath.")
  set(ENABLE_DEVELDOCS FALSE)
endif ()

