######################################################################
#
# : Compute ntcc specific options
#
# $Id: SciFortranDouble.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${FC_DOUBLE_FLAGS}")
# message(STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")
message(STATUS "CMAKE_Fortran_FLAGS = ${CMAKE_Fortran_FLAGS}")

