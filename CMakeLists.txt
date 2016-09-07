###
# (c) Strathclyde University
#
# 25.6.2015 Jonathan Smith - CMake lists for PUFFIN Project
###
cmake_minimum_required (VERSION 2.8.10)
project (puffin)
enable_language (Fortran)
# The version number.
set (Puffin_VERSION_MAJOR 1)
set (Puffin_VERSION_MINOR 6)
set (Puffin_VERSION_PATCH 0)
include(${CMAKE_SOURCE_DIR}/scimake/SciInit.cmake)
#option(ENABLE_PARALLEL "whether to build parallel PUFFIN" ON)


######################################################################
#
# Find packages
#
######################################################################
if(ENABLE_PARALLEL)
  find_package(SciMpi REQUIRED)
  set(USE_MPI 1 CACHE BOOL "setting USE_MPI to 1" FORCE)
endif()
if (USE_MKL)
  find_package(SciMkl)
else()
find_package(SciFftw REQUIRED)
endif()
find_package(SciHdf5)
find_package(OpenMP)
if (OPENMP_FOUND)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
endif()
## FFLAGS depend on the compiler
#get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)#
#
#if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
#  # gfortran
#  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
#  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g")
#elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
#  # ifort (untested)
#  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
#  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
#elseif (Fortran_COMPILER_NAME MATCHES "g77")
#  # g77
#  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
#  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
#else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
#  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
#  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
#  message ("No optimized Fortran compiler flags are known, we just try -O2...")
#  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
#  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
#endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

#message ("-- FFTW_LIB: " ${FFTW_LIB})
#message ("-- FFTW_MPI_LIB: " ${FFTW_MPI_LIB})
#message ("-- FFTW_LIB_DIR: " ${FFTW_LIB_DIR})
#find_package(MPI REQUIRED)
set(ENV{GIT_DIR} ${CMAKE_SOURCE_DIR}/.git)
#set(GIT_GET_BRANCH "git rev-parse --abbrev-ref HEAD")
execute_process(COMMAND git --work-tree=${CMAKE_SOURCE_DIR} rev-parse --abbrev-ref HEAD
  RESULT_VARIABLE GIT_RESULT
  OUTPUT_VARIABLE GIT_BRANCH
  )
if (GIT_BRANCH)
  STRING(REPLACE "\n" "" GIT_BRANCH ${GIT_BRANCH})
else()
  set(GIT_BRANCH "unknown branch, not in git")
endif()

execute_process(COMMAND git --work-tree=${CMAKE_SOURCE_DIR} rev-parse HEAD
  RESULT_VARIABLE GIT_RESULT
  OUTPUT_VARIABLE GIT_REVISION
  )
if (GIT_REVISION)
  STRING(REPLACE "\n" "" GIT_REVISION ${GIT_REVISION})
else()
  set(GIT_REVISION "unknown revision, not in git")
endif()
message(STATUS "git branch : ${GIT_BRANCH} ${GIT_REVISION}")
string(TIMESTAMP CONFIGURE_TIME_STAMP "%Y-%m-%d %H:%M")
######################################################################
#
# Variables for inclusion in config.h
#
######################################################################
set(CXX ${CMAKE_CXX_COMPILER})
set(CFLAGS "${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_${CBT_UC}}")
set(CXXFLAGS "${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_${CBT_UC}}")
set(HAVE_CMAKE 1 CACHE STRING "Whether built with CMake")

######################################################################
#
# Create the configure file
#
######################################################################
configure_file(config-cmake.h.in config.h)
configure_file(source/hdf5_puff.f90.in source/hdf5_puff.f90)
add_subdirectory(source)
install (DIRECTORY utilities/ DESTINATION bin)
install (DIRECTORY inputs/example/ DESTINATION examples)
install (DIRECTORY inputs/gen/ DESTINATION python)
install (DIRECTORY scripts/ DESTINATION examples/job-submission-scripts)



######################################################################
#
# Finalize (installation, packaging)
#
######################################################################
set(COMMON_INSTALL TRUE)
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Puffin")
set(CONFIG_FILES)  # Clear out
include(${CMAKE_SOURCE_DIR}/scimake/SciFinalize.cmake)




# build a CPack driven installer package
include (InstallRequiredSystemLibraries)
set (CPACK_RESOURCE_FILE_LICENSE  
     "${CMAKE_CURRENT_SOURCE_DIR}/License.txt")
set (CPACK_PACKAGE_VERSION_MAJOR "${Puffin_VERSION_MAJOR}")
set (CPACK_PACKAGE_VERSION_MINOR "${Puffin_VERSION_MINOR}")
set (CPACK_PACKAGE_VERSION_PATCH "${Puffin_VERSION_PATCH}")
include (CPack)
