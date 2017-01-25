######################################################################
#
# SciInit: Do the startup stuff for any package
#
# $Id: SciInit.cmake 1102 2016-11-02 21:55:51Z alexanda $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

string(TOUPPER ${PROJECT_NAME} PROJECT_NAMEUC)
set(HAVE_CMAKE 1 CACHE STRING "Whether built with CMake")
set(${PROJECT_NAMEUC}_HAVE_CMAKE TRUE)

#####################################################################
#
# Pull in useful macros
#
#####################################################################

if (NOT DEFINED SCIMAKE_DIR)
  message(STATUS "[SciInit]: CMAKE_CURRENT_LIST_DIR = ${CMAKE_CURRENT_LIST_DIR}.")
  set(SCIMAKE_DIR ${CMAKE_CURRENT_LIST_DIR})
endif ()
include(${SCIMAKE_DIR}/SciFuncsMacros.cmake)
include(${SCIMAKE_DIR}/SciGetDepsFromInstall.cmake)
include(GenerateExportHeader)

#####################################################################
#
# Clean out config.summary
#
#####################################################################

set(CONFIG_SUMMARY ${PROJECT_BINARY_DIR}/config.summary)
file(REMOVE ${CONFIG_SUMMARY})
SciPrintString(
  "CONFIGURING ${CMAKE_PROJECT_NAME} with scimake in ${PROJECT_BINARY_DIR}.")

#####################################################################
#
# Set some vars to upper case for case-free comparisons
#
#####################################################################

if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif ()
message(STATUS "[SciInit]: CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}.")
string(TOUPPER "${CMAKE_BUILD_TYPE}" CMAKE_BUILD_TYPE_UC)

#####################################################################
#
# Set group write installation perm
# May need to be different on different machines
#
#####################################################################

if (NOT DEFINED SCI_GROUP_WRITE)
  set(SCI_GROUP_WRITE ""
      CACHE STRING "Value of group write permissions")
endif ()
if (NOT DEFINED SCI_WORLD_PERMS)
  set(SCI_WORLD_PERMS ""
      CACHE STRING "Value of world permissions")
endif ()

#####################################################################
#
# Set OS-specific flags
#
#####################################################################

message(STATUS "[SciInit]: CMAKE_SYSTEM_NAME is ${CMAKE_SYSTEM_NAME}")
if ("${CMAKE_SYSTEM_NAME}" MATCHES "Darwin")
  set(MACX TRUE CACHE BOOL "True if compiled on Mac OS X")
  message(STATUS "[SciInit]: Compiling on MAC, CMAKE_SYSTEM = ${CMAKE_SYSTEM}.")
elseif ("${CMAKE_SYSTEM_NAME}" MATCHES "Linux")
  set(LINUX TRUE CACHE BOOL "True if compiled on Linux")
  message(STATUS "[SciInit]: Compiling on LINUX")
elseif (WIN32)
  set(WINDOWS TRUE CACHE BOOL "True if compiled on Windows")
  message(STATUS "[SciInit]: Compiling on WINDOWS")
else ()
  message(FATAL_ERROR "[SciInit.cmake] Unrecognized OS!")
endif ()
SciPrintString("CMAKE_SYSTEM_PROCESSOR = ${CMAKE_SYSTEM_PROCESSOR}.")
set(HOST_PROCESSOR ${CMAKE_SYSTEM_PROCESSOR})
if (${CMAKE_SYSTEM_PROCESSOR} STREQUAL x86_64)
  set(HAVE_X86_64 TRUE)
endif ()

######################################################################
#
# Set up standard paths
#
######################################################################

cmake_policy(SET CMP0017 OLD) # Use our modules over theirs
set(CMAKE_MODULE_PATH
  ${SCIMAKE_DIR}/Modules
)

if (DEBUG_CMAKE)
  message(STATUS "[SciInit]: CMAKE_MODULE_PATH = ${CMAKE_MODULE_PATH}")
  set(SCI_DEBUG_VAR DEBUG)
endif ()

# message("SUPRA_SEARCH_PATH = ${SUPRA_SEARCH_PATH}")
if (SUPRA_SEARCH_PATH)
  set(SUPRA_SEARCH_PATH "${SUPRA_SEARCH_PATH}")
else ()
  if (WIN32)
# According to JRC the following must be turned off to compile under
# Windows(AP)
    # set(SUPRA_SEARCH_PATH $ENV{HOME}/software /winsame/internal /winsame/volatile /winsame/contrib /opt /usr/local)
  else ()
# JRC: SUPRA_SEARCH_PATH should include only top directory
# also, system paths should not be needed due to cmake's system search
    set(SUPRA_SEARCH_PATH $ENV{HOME}/software /internal /volatile /contrib /opt /usr/local)
  endif ()
endif ()
SciPrintString("SUPRA_SEARCH_PATH = ${SUPRA_SEARCH_PATH}")

######################################################################
#
# Get system description
#
######################################################################

find_program(HOSTNAME_CMD NAMES hostname)
exec_program(${HOSTNAME_CMD} ARGS OUTPUT_VARIABLE HOSTNAME)
SciPrintString("scimake running on ${HOSTNAME}")
# This not always accurate
string(REGEX REPLACE "\\..*$" "" UQHOSTNAME "${HOSTNAME}")
SciPrintString("UQHOSTNAME = ${UQHOSTNAME}")
string(REGEX REPLACE "${UQHOSTNAME}\\." "" DOMAINNAME "${HOSTNAME}")
SciPrintString("DOMAINNAME = ${DOMAINNAME}")

find_program(UNAME NAMES uname)
macro(getuname name flag)
  exec_program("${UNAME}" ARGS "${flag}" OUTPUT_VARIABLE "${name}")
endmacro(getuname)

if (UNAME)
  getuname(osname -s)
  getuname(osrel  -r)
  getuname(cpu    -m)
  set(HOSTTYPE "${osname}-${cpu}")
  SciPrintString("HOSTTYPE = ${HOSTTYPE}")
  site_name(HOSTNAME)
  SciPrintString("hostname is ${HOSTNAME}")
else ()
  message(WARNING "SciInit: find_program for 'uname' returned nothing")
endif ()

######################################################################
#
# Other useful provenance
#
######################################################################

# Calling it "ENV Var" + "NAME" to avoid conflicts elsewhere
set(HOMENAME $ENV{HOME})
set(USERNAME $ENV{USER})
set(SCRATCHNAME $ENV{SCRATCH})

######################################################################
#
# Get revisions
#
######################################################################

include(${SCIMAKE_DIR}/SciSvnInfo.cmake)

######################################################################
#
# config.h
#
######################################################################

if (NOT NO_CONFIG_H)
  if (WIN32)
    add_definitions(/DHAVE_CONFIG_H)
  else ()
    add_definitions(-DHAVE_CONFIG_H)
  endif ()
endif ()

######################################################################
#
# Fix shared flags on windows
#
######################################################################

include(${SCIMAKE_DIR}/SciWinFlags.cmake)

######################################################################
#
# Load Find Package (also contains code for computing static libraries)
#
######################################################################

include(${SCIMAKE_DIR}/Modules/SciFindPackage.cmake)

######################################################################
#
# C, CXX, Fortran Checks
#
######################################################################

include(${SCIMAKE_DIR}/SciCChecks.cmake)
if (NOT NOCXX)
  include(${SCIMAKE_DIR}/SciCxxChecks.cmake)
endif ()
if (NOT NOFORTRAN)
  message("")
  message("--------- SciFortranChecking ---------")
# Enable Fortran to all those variables
  enable_language(Fortran)
  include(${CMAKE_ROOT}/Modules/CMakeDetermineFortranCompiler.cmake)
  include(${SCIMAKE_DIR}/SciFortranChecks.cmake)
else ()
  message(STATUS "[SciInit]: No Fortran, so no implicit fortran link libraries known.")
endif ()

######################################################################
#
# OpenMP, SSE, AVX, OpenCL
#
######################################################################

include(${SCIMAKE_DIR}/SciOmpSseAvx.cmake)
option(ENABLE_OPENCL    "Whether to enable OpenCL" OFF)
if (ENABLE_OPENCL)
include(${SCIMAKE_DIR}/Modules/FindSciOpenCL.cmake)
endif ()
include(${SCIMAKE_DIR}/SciMultiArchKernels.cmake)

######################################################################
#
# Link checks
#
######################################################################

include(${SCIMAKE_DIR}/SciLinkChecks.cmake)

######################################################################
#
# Search for static or shared libs?
#
######################################################################

if (ENABLE_SHARED)
  message(WARNING "ENABLE_SHARED is deprecated.  Use BUILD_SHARED_LIBS.")
  if (NOT DEFINED BUILD_SHARED_LIBS)
    set(BUILD_SHARED_LIBS TRUE)
  endif ()
endif ()
if (BUILD_SHARED_LIBS AND NOT DEFINED USE_SHARED_LIBS)
  set(USE_SHARED_LIBS TRUE)
endif ()

######################################################################
#
# For MinGW set libraries to Windows style
#
######################################################################

if (USING_MINGW)
  message("")
  message("--------- Setting MinGW library prefix and suffix to windows style  ---------")
  set(CMAKE_STATIC_LIBRARY_PREFIX "")
  set(CMAKE_STATIC_LIBRARY_SUFFIX .lib)
  message(STATUS "[SciInit]: CMAKE_STATIC_LIBRARY_PREFIX = ${CMAKE_STATIC_LIBRARY_PREFIX}.")
  message(STATUS "[SciInit]: CMAKE_STATIC_LIBRARY_SUFFIX = ${CMAKE_STATIC_LIBRARY_SUFFIX}.")
endif ()

######################################################################
#
# Look for MPI
#
######################################################################

option(ENABLE_PARALLEL "Enable parallel build" OFF)
message("")
if (ENABLE_PARALLEL)
  message(STATUS "[SciInit]: ENABLE_PARALLEL requested.  Will search for MPI.")
elseif (INSTALL_PARALLEL)
  message(STATUS "[SciInit]: INSTALL_PARALLEL requested.  Will search for MPI.")
else ()
  message(STATUS "[SciInit]: Not searching for MPI because ENABLE_PARALLEL or INSTALL_PARALLEL not set.")
endif ()
if (ENABLE_PARALLEL OR INSTALL_PARALLEL)
  find_package(SciMpi REQUIRED)
endif ()

######################################################################
#
# Any needed results
#
######################################################################

message("")
if (NOT RESULTS_DIR)
  message(STATUS "[SciInit]: RESULTS_DIR not specified.")
endif ()
if (RESULTS_DIR_BASE AND NOT RESULTS_DIR)
  message(STATUS "[SciInit]: Looking for results starting with ${RESULTS_DIR_BASE}.")
# Get potential files
  file(GLOB resultsdirs RELATIVE ${CMAKE_SOURCE_DIR}
    "${RESULTS_DIR_BASE}-*" "${RESULTS_DIR_BASE}"
  )
  message(STATUS "[SciInit]: resultsdirs = ${resultsdirs}.")
# Looks for first that is a directory
  foreach (resdir ${resultsdirs})
   if (IS_DIRECTORY ${CMAKE_SOURCE_DIR}/${resdir})
     set(RESULTS_DIR ${CMAKE_SOURCE_DIR}/${resdir})
     break()
   endif ()
  endforeach ()
  if (NOT RESULTS_DIR)
    message(STATUS "[SciInit]: RESULTS_DIR not found.")
  endif ()
endif ()
if (RESULTS_DIR)
  message(STATUS "[SciInit]: RESULTS_DIR = ${RESULTS_DIR}.")
endif ()

######################################################################
#
# Testing macros
#
######################################################################

include(${SCIMAKE_DIR}/SciUnitTestMacros.cmake)

