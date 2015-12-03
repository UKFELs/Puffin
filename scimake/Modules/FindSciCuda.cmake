######################################################################
# - FindSciCuda: Find include directories and libraries for Cuda.
#
# Module usage:
#   find_package(SciCuda ...)
#
# This module will define the following variables:
#  HAVE_OPENCL, OPENCL_FOUND = Whether libraries and includes are found
#  CUDA_INCLUDE_DIRS       = Location of Cuda includes
#  CUDA_LIBRARY_DIRS       = Location of Cuda libraries
#  CUDA_LIBRARIES          = Required libraries
#
# Copyright 2013-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

message("")
message("--------- Looking for CUDA -----------")
find_package(CUDA 5.0)
# find_cuda_helper_libs(cusparse)
if (CUDA_CUDART_LIBRARY AND NOT CUDA_LIBRARY_DIRS)
  get_filename_component(CUDA_LIBRARY_DIRS ${CUDA_CUDART_LIBRARY}/..  REALPATH)
endif ()
foreach (sfx VERSION CUDA_LIBRARY NVCC_EXECUTABLE
    TOOLKIT_ROOT_DIR TOOLKIT_INCLUDE INCLUDE_DIRS
    LIBRARY_DIRS LIBRARIES CUDART_LIBRARY
    curand_LIBRARY cublas_LIBRARY
    cusparse_LIBRARY cufft_LIBRARY npp_LIBRARY cupti_LIBRARY)
  SciPrintVar(CUDA_${sfx})
endforeach ()
if (CUDA_TOOLKIT_ROOT_DIR)
  set(HAVE_CUDA_TOOLKIT TRUE)
else ()
  set(HAVE_CUDA_TOOLKIT FALSE)
endif ()
SciPrintVar(HAVE_CUDA_TOOLKIT)
message("")

