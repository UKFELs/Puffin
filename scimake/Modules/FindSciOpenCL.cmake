######################################################################
#
# @file    FindSciOpenCL.cmake
#
# @brief   Find include directories and libraries for OpenCL.
#          Module usage:
#          find_package(OpenCL ...)
#
#          This module will define the following variables:
#            HAVE_OPENCL, OPENCL_FOUND = Whether found libraries and includes
#            OpenCL_INCLUDE_DIRS       = Location of OpenCL includes
#            OpenCL_LIBRARY_DIRS       = Location of OpenCL libraries
#            OpenCL_LIBRARIES          = Required libraries
#
# @version $Id: FindSciOpenCL.cmake 1081 2016-09-10 15:44:42Z cary $
#
# Copyright &copy; 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

find_package(CUDA)
if (CUDA_FOUND)
  if (OpenCL_ROOT_DIR)
  else ()
    set(OpenCL_ROOT_DIR "${CUDA_TOOLKIT_ROOT_DIR}")
  endif ()
endif ()

if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  set(OpenCL_HEADERS "OpenCL/opencl.h")
else ()
  set(OpenCL_HEADERS "CL/cl.h")
endif ()

SciFindPackage(PACKAGE "OpenCL"
    INSTALL_DIRS "${OpenCL_ROOT_DIR}"
    HEADERS "${OpenCL_HEADERS}"
    LIBRARIES "OpenCL"
    INCLUDE_SUBDIRS "include"
    LIBRARY_SUBDIRS "lib64;lib;lib/x64"
    )

if (OpenCL_INCLUDE_DIRS AND OpenCL_LIBRARIES)
  set(OpenCL_FOUND TRUE)
endif ()

if (OpenCL_FOUND)
  message(STATUS "Found OpenCL")
  set(HAVE_OpenCL 1 CACHE BOOL "Whether have OpenCL")
else ()
  message(STATUS "Did not find OpenCL.  Use -DOpenCL_ROOT_DIR to specify the installation directory.")
  if (SciOpenCL_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

