# - FindSciCppCheck: Module to find cppcheck
#
# Module usage:
#   find_package(SciCppCheck ...)
#
# This module will define the following variables:
#  CPPCHECK_FOUND         = Whether CppCheck was found
#  CppCheck_cppcheck    = Path to cppcheck executable

######################################################################
#
# SciCppCheck: Find CppCheck
#
# $Id: FindSciCppCheck.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

message("")
message("--------- FindSciCppCheck looking for cppcheck ---------")

SciFindPackage(PACKAGE CppCheck
  PROGRAMS "cppcheck"
)

if (CPPCHECK_FOUND)
  message(STATUS "CppCheck_cppcheck found.")
  message(STATUS "CppCheck_cppcheck = ${CppCheck_cppcheck}")
endif ()

