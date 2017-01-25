# - FindSciChombo: Module to find include directories and
#   libraries for Chombo.
#
# Module usage:
#   find_package(SciChombo ...)
#
# This module will define the following variables:
#  HAVE_CHOMBO, CHOMBO_FOUND = Whether libraries and includes are found
#  Chombo_INCLUDE_DIRS       = Location of Chombo includes
#  Chombo_LIBRARY_DIRS       = Location of Chombo libraries
#  Chombo_LIBRARIES          = Required libraries

######################################################################
#
# SciFindChombo: find includes and libraries for Chombo.
#
# $Id: FindSciChombo.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################
if (ENABLE_PARALLEL)
  set(CH_MPI "1")
endif ()

if (NOT CH_CXX)
  if (ENABLE_PARALLEL)
    set(REL_CMAKE_CXX_COMPILER "mpicxx")
  else ()
    get_filename_component(REL_CMAKE_CXX_COMPILER ${CMAKE_CXX_COMPILER} NAME)
  endif ()
else ()
  set(REL_CMAKE_CXX_COMPILER ${CH_CXX})
endif ()

if (NOT CH_FC)
  if (ENABLE_PARALLEL)
    set(REL_CMAKE_Fortran_COMPILER "mpif90")
  else ()
    get_filename_component(REL_CMAKE_Fortran_COMPILER ${CMAKE_Fortran_COMPILER} NAME)
  endif ()
else ()
  set(REL_CMAKE_Fortran_COMPILER ${CH_FC})
endif ()

# Sets name for debug builds
if (CH_DEBUG)
  set(DEBUG_NAME "dbg")
endif ()

################################################################################
# Look for par(ser)2d(3d)dbg version of chombo

if (ENABLE_PARALLEL)

   if (CH_SPACEDIM MATCHES "2")
     message(STATUS "Looking for parallel 2D Chombo")
     SciFindPackage(PACKAGE "Chombo"
          INSTALL_DIR "chombo-par2d${DEBUG_NAME}"
        LIBRARIES chombo
        HEADERS "Box.H;CH_assert.H;CH_HDF5.H" )
   endif ()

   if (CH_SPACEDIM MATCHES "3")
     message(STATUS "Looking for parallel 3D Chombo")
     SciFindPackage(PACKAGE "Chombo"
          INSTALL_DIR "chombo-par3d"
        LIBRARIES chombo
        HEADERS "Box.H;CH_assert.H;CH_HDF5.H" )
   endif ()

else ()

   if (CH_SPACEDIM MATCHES "2")
     message(STATUS "Looking for serial 2D Chombo")
     SciFindPackage(PACKAGE "Chombo"
          INSTALL_DIR "chombo-ser2d${DEBUG_NAME}"
        LIBRARIES chombo
        HEADERS "Box.H;CH_assert.H;CH_HDF5.H" )
   endif ()

   if (CH_SPACEDIM MATCHES "3")
     message(STATUS "Looking for serial 3D Chombo")
     SciFindPackage(PACKAGE "Chombo"
          INSTALL_DIR "chombo-ser3d"
        LIBRARIES chombo
        HEADERS "Box.H;CH_assert.H;CH_HDF5.H" )
   endif ()

endif (ENABLE_PARALLEL)
###############################################################################

if (CHOMBO_FOUND)
  message(STATUS "Found Chombo")
  set(HAVE_CHOMBO 1 CACHE BOOL "Whether have the Chombo library")
else ()
  message(STATUS "Did not find Chombo.  Use -DCHOMBO_DIR to specify the installation directory.")
endif ()

