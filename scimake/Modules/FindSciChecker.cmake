# - FindSciChecker: Module to find scan_build
#
# Module usage:
#   find_package(SciChecker ...)
#
# This module will define the following variables:
#  CHECKER_FOUND         = Whether Checker was found
#  Checker_scan_build    = Path to scan_build executable

######################################################################
#
# SciChecker: Find Checker
#
# $Id: FindSciChecker.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2011-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

message("")
message("--------- FindSciChecker looking for scan_build ---------")

SciFindPackage(PACKAGE Checker
  PROGRAMS "scan-build"
  PROGRAM_SUBDIRS .
)

if (CHECKER_FOUND)
  message(STATUS "Checker_scan_build found.")
  message(STATUS "Checker_scan_build = ${Checker_scan_build}")
else ()
  message(STATUS "Checker_scan_build not found. API documentation cannot be built.")
  set(ENABLE_DEVELDOCS FALSE)
endif ()

