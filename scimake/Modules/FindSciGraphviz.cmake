# - FindSciGraphviz: Module to find dot
#
# Module usage:
#   find_package(SciGraphviz ...)
#
# This module will define the following variables:
#  GRAPHVIZ_FOUND         = Whether Graphviz was found
#  Graphviz_dot    = Path to dot executable

######################################################################
#
# SciGraphviz: Find Graphviz
#
# $Id: FindSciGraphviz.cmake 798 2015-04-19 14:19:01Z jrobcary $
#
# Copyright 2011-2015, Tech-X Corporation, Boulder, CO.
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
else ()
  message(STATUS "Graphviz_dot not found.")
  # message(STATUS "Graphviz_dot not found. API documentation cannot be built.")
  # set(ENABLE_DEVELDOCS FALSE)
endif ()

