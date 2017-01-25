# - FindSciGraphviz: Module to find dot
#
# Module usage:
#   find_package(SciGraphviz ...)
#
# This module will define the following variables:
#  GRAPHVIZ_FOUND  = Whether Graphviz was found
#  Graphviz_dot    = Path to dot executable

######################################################################
#
# SciGraphviz: Find Graphviz
#
# $Id: FindSciGraphviz.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

message("")
message("--------- FindSciGraphviz looking for dot ---------")

SciFindPackage(PACKAGE Graphviz
  PROGRAMS "dot"
)

if (GRAPHVIZ_FOUND)
  message(STATUS "Graphviz_dot found.")
  message(STATUS "Graphviz_dot = ${Graphviz_dot}")
  get_filename_component(GRAPHVIZ_DOT_DIR ${Graphviz_dot} DIRECTORY)
  message(STATUS "GRAPHVIZ_DOT_DIR = ${GRAPHVIZ_DOT_DIR}")
else ()
  message(STATUS "Graphviz_dot not found.")
  # message(STATUS "Graphviz_dot not found. API documentation cannot be built.")
  # set(ENABLE_DEVELDOCS FALSE)
endif ()

