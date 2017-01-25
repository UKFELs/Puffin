######################################################################
#
# @file    FindSciCuda: Find include directories and libraries for FindSciCuda.cmake
#
# @brief   Find locations of CUDA items
#
# @version $Id: FindSciCuda.cmake 1011 2016-03-10 05:46:12Z benc $
#
# Module usage:
#   find_package(SciCuda ...)
#
# This module will define the following variables:
#  HAVE_CUDA, CUDA_FOUND   = Whether libraries and includes are found
#  CUDA_INCLUDE_DIRS       = Location of Cuda includes
#  CUDA_LIBRARY_DIRS       = Location of Cuda libraries
#  CUDA_LIBRARIES          = Required libraries
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

message("")
message("--------- Looking for CUDA -----------")

if (NOT SciCuda_FIND_VERSIONS)
  set(SciCuda_FIND_VERSIONS 7.5 7.0)
endif ()
message(STATUS "SciCuda_FIND_VERSIONS = ${SciCuda_FIND_VERSIONS}.")
if (NOT WIN32)
  foreach (scver ${SciCuda_FIND_VERSIONS})
    if (EXISTS /usr/local/cuda-${scver})
      set(ENV{CUDA_BIN_PATH} /usr/local/cuda-${scver})
# CUDA_BIN_PATH has to be set as an environment variable.
# Should not set CUDA_TOOLKIT_ROOT_DIR as this will cause regeneration of
# .depend files and a rebuild.
      # set(CUDA_TOOLKIT_ROOT_DIR /usr/local/cuda-${scver})
      break ()
    endif ()
  endforeach ()
endif ()

# CUDA not working with Intel
message(STATUS "${CMAKE_CXX_COMPILER_ID}")
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel")
  set(SCI_ENABLE_CUDA FALSE)
endif()

# Look for explicit enabling of CUDA from configure line or environment
if (NOT DEFINED SCI_ENABLE_CUDA)
  set(SCI_ENABLE_CUDA $ENV{SCI_ENABLE_CUDA})
endif ()

# Message if CUDA explicitly enabled
if (DEFINED SCI_ENABLE_CUDA)
  if (SCI_ENABLE_CUDA)
    message(STATUS "CUDA explicitly enabled.")
  else ()
    message(STATUS "CUDA explicitly disabled.")
  endif ()
else ()
# If CUDA not explicitly enabled, determine whether to use based on
# whether supported
  set(COMPILER_BAD_4_CUDA FALSE)
# This not a complete matrix, as assumes CUDA-7.0
  if ((${C_COMPILER_ID} STREQUAL Clang) AND NOT
      (${C_VERSION} VERSION_LESS 700.0.0))
    set(COMPILER_BAD_4_CUDA TRUE)
  elseif ((${C_COMPILER_ID} STREQUAL GNU) AND NOT
      (${C_VERSION} VERSION_LESS 5.0.0))
    set(COMPILER_BAD_4_CUDA TRUE)
  endif ()
  if(COMPILER_BAD_4_CUDA)
    message(STATUS "CUDA not supported with ${C_COMPILER_ID}-${C_VERSION}.")
    if (LINUX)
      message(STATUS "Comment out the #error line in include/host_config.h in your CUDA installation to use CUDA anyway.")
      message(STATUS "See https://www.pugetsystems.com/labs/articles/Install-NVIDIA-CUDA-on-Fedora-22-with-gcc-5-1-654")
      message(STATUS "Also set SCI_ENABLE_CUDA=TRUE on the configure line or in your environment.")
    endif ()
    set(SCI_ENABLE_CUDA FALSE)
  else ()
    set(SCI_ENABLE_CUDA TRUE)
  endif ()
endif ()

if (SCI_ENABLE_CUDA)
  find_package(CUDA ${SciCuda_FIND_VERSION})
else ()
  set(CUDA_FOUND FALSE)
endif ()
SciPrintVar(CUDA_FOUND)
if (CUDA_FOUND)
  SciPrintVar(CUDA_VERSION)
endif ()

# Macro to do what is needed when CUDA is found
macro(SciDoCudaFound)

# Make calling a __host__ function from a __host__ __device__ function
# an error.
  list(APPEND CUDA_NVCC_FLAGS "--Werror cross-execution-space-call")

  string(FIND ${CMAKE_CXX_FLAGS} "-std=c++11" POS)
  if (NOT ${POS} EQUAL -1)
    if (CUDA_VERSION LESS 7.0)
      message(WARNING "Cuda support of -std=c++11 requires a minimum CUDA version of 7.0")
      set(CUDA_FOUND FALSE)
      SciPrintVar(CUDA_FOUND)
      return()
    else ()
# This fails at cmake-3.4.1, succeeds at cmake-3.2.2, where at the former
# FindCUDA.cmake says
# -Xcompile -std=c++ will choke nvcc (it uses the C preprocessor)
# Seems that this is true for CUDA-7.5 or cmake-3.4.1
      if (${CMAKE_VERSION} VERSION_LESS "3.4.0" OR NOT CUDA_PROPAGATE_HOST_FLAGS)
        list(APPEND CUDA_NVCC_FLAGS "-std=c++11")
      endif ()
    endif ()
  endif ()
# CUDA_VERSION is the found version
  if (CUDA_VERSION LESS 5.0)
    message(FATAL_ERROR "SciCuda requires a minimum CUDA version of 5.0")
  endif ()
# Set the optimization and debug type flags
  if (CMAKE_BUILD_TYPE MATCHES Debug)
    list(APPEND CUDA_NVCC_FLAGS -g -G -lineinfo)
  elseif (CMAKE_BUILD_TYPE MATCHES RelWithDebInfo)
    list(APPEND CUDA_NVCC_FLAGS -g -O2 --use_fast_math)
  else ()
    list(APPEND CUDA_NVCC_FLAGS -O3 --use_fast_math --ptxas-options=-v)
  endif ()

# Query devices and make sure we have enough compute capabilty
# and then set flags according to the compute capabiltiy found.
  set(deviceUtility ${TxSim_ROOT_DIR}/bin/devices)
  if (WIN32)
    set(deviceUtility ${TxSim_ROOT_DIR}/bin/devices.exe)
  endif ()
  message(STATUS "deviceUtility = ${deviceUtility}")

# Allow user to set specific flags with SCI_CUDA_ARCH_FLAGS
  if (NOT DEFINED SCI_CUDA_ARCH_FLAGS)
# Minimum compute capability for printf is 30
    list(APPEND CUDA_NVCC_FLAGS --generate-code arch=compute_30,code=sm_30)
    if (NOT (CUDA_VERSION LESS 6.0))
      list(APPEND CUDA_NVCC_FLAGS --generate-code arch=compute_50,code=sm_50)
    endif ()
# Add other compute capabilities on request
    if (CUDA_ALL_COMPUTE_CAPABILITIES)
      # compute_20 is required for old cards (e.g. 2070s on enrico)
      list(APPEND CUDA_NVCC_FLAGS
          --generate-code arch=compute_20,code=sm_20
          --generate-code arch=compute_20,code=sm_21
          --generate-code arch=compute_35,code=sm_35
      )
      if (NOT (CUDA_VERSION LESS 7.0))
        list(APPEND CUDA_NVCC_FLAGS --generate-code arch=compute_52,code=sm_52)
      endif ()
    endif ()
  else ()
    list(APPEND CUDA_NVCC_FLAGS ${SCI_CUDA_ARCH_FLAGS})
  endif ()

# find_cuda_helper_libs(cusparse)
  if (CUDA_CUDART_LIBRARY AND NOT CUDA_LIBRARY_DIRS)
    get_filename_component(CUDA_LIBRARY_DIRS ${CUDA_CUDART_LIBRARY}
      DIRECTORY CACHE
    )
  endif ()

  if (ENABLE_PARALLEL)
# This is needed to get around nvcc finding what mpicc is linked to
# and using that, which then prevents the openmpi compilers from knowing
# what configuration file to use.
    set(CUDA_HOST_COMPILER ${CMAKE_C_COMPILER})
# list(APPEND CUDA_NVCC_FLAGS -ccbin ${SCI_SERIAL_C_COMPILER})
  endif ()

# If CMake version >= 2.8.11, need to add the CUDA library manually
  if (${CMAKE_VERSION} VERSION_GREATER 2.8.10)
    if (CUDA_CUDA_LIBRARY)
      get_filename_component(CUDA_CUDA_DIR ${CUDA_CUDA_LIBRARY}/.. REALPATH)
      set(CUDA_LIBRARIES ${CUDA_LIBRARIES} ${CUDA_CUDA_LIBRARY})
      if (LINUX)
        set(CUDA_LIBRARIES ${CUDA_LIBRARIES} "-Wl,-rpath -Wl,${CUDA_CUDA_DIR}")
      endif ()
    else ()
      message(WARNING "CUDA_CUDA_LIBRARY not found, so link may fail.")
    endif ()
  endif ()

# The cuda library may not be in the frameworks area
  find_library(CUDA_cuda_SHLIB cuda
    PATHS /usr/local/cuda-${CUDA_VERSION}
    PATH_SUFFIXES lib64 lib
    NO_DEFAULT_PATH
  )
  if (CUDA_cuda_SHLIB)
    get_filename_component(CUDA_cuda_SHLIB_DIR ${CUDA_cuda_SHLIB}
      DIRECTORY CACHE
    )
    set(CUDA_LIBRARY_DIRS ${CUDA_LIBRARY_DIRS} ${CUDA_cuda_SHLIB_DIR})
  else ()
    set(CUDA_cuda_SHLIB ${CUDA_CUDA_LIBRARY})
  endif ()

# Find the cudadevrt library
  find_library(CUDA_CUDADEVRT_LIBRARY
    NAMES cudadevrt
    PATHS ${CUDA_LIBRARY_DIRS}
    )

  if (CUDA_FOUND)
    set(CUDA_BASE_LIBRARIES ${CUDA_cusparse_LIBRARY} ${CUDA_CUDART_LIBRARY})
# cublas is linked to cuda as opposed to dlopening it.  So it cannot
# be linked but must be dlopened.
    if (APPLE)
# Could we instead use "-undefined dynamic_lookup"?
      set(CUDA_BASE_LIBRARIES ${CUDA_BASE_LIBRARIES} ${CUDA_cuda_SHLIB})
    endif ()
  else ()
    return()
  endif ()

# Print results
  SciPrintCMakeResults(CUDA)
  foreach (sfx VERSION CUDA_LIBRARY cuda_SHLIB NVCC_EXECUTABLE
      HOST_FLAGS NVCC_FLAGS TOOLKIT_ROOT_DIR TOOLKIT_INCLUDE INCLUDE_DIRS
      LIBRARY_DIRS LIBRARIES CUDART_LIBRARY
      curand_LIBRARY cublas_LIBRARY
      cusparse_LIBRARY cufft_LIBRARY npp_LIBRARY cupti_LIBRARY
      CUDADEVRT_LIBRARY
      BASE_LIBRARIES
  )
    SciPrintVar(CUDA_${sfx})
  endforeach ()

# Flags to be added when CUDA is found on Windows
  if (${CXX_COMPILER_ID} STREQUAL MSVC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /FS")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /FS")
  endif ()

endmacro()

# This to be a separate option.  Otherwise just use capability 2.0.
option(CUDA_ALL_COMPUTE_CAPABILITIES
  "Whether to compile for all cuda compute capabilities" OFF
)
if (CUDA_FOUND)
  SciDoCudaFound() # Can undo cuda found
endif ()
if (CUDA_FOUND)
  set(HAVE_CUDA_TOOLKIT TRUE)
  message(STATUS "After CUDA additions:")
  SciPrintVar(CMAKE_C_FLAGS)
  SciPrintVar(CMAKE_CXX_FLAGS)
else ()
  set(HAVE_CUDA_TOOLKIT FALSE)
endif ()
SciPrintVar(HAVE_CUDA_TOOLKIT)

# Macros covering presence or absence of cuda
macro(scicuda_add_library)
  if (HAVE_CUDA_TOOLKIT)
    cuda_add_library(${ARGV})
  else ()
    add_library(${ARGV})
  endif ()
endmacro()

macro(scicuda_add_executable)
  if (HAVE_CUDA_TOOLKIT)
    cuda_add_executable(${ARGV})
  else ()
    add_executable(${ARGV})
  endif ()
endmacro()

