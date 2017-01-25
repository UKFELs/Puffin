# - FindSciIdl: Module to find the IDL executable.
#
# Module usage:
#   find_package(SciIdl ...)
#
# Specify IDL_ROOT to indicate the location of an IDL distribution.
#
# This module will define the following variables:
#   IDL_FOUND           = whether IDL was found
#   Idl_PLATFORM_EXT    = DLM extension, i.e., darwin.x86_64, linux.x86, x86_64...
#   Idl_INCLUDE_DIR     = IDL include directory
#   Idl_LIBRARY         = IDL shared library location
#   Idl_EXECUTABLE      = IDL command
#   Idl_PATH_SEP        = character to separate IDL paths

######################################################################
#
# FindSciIdl.cmake: Find the IDL executable
#
# $Id: FindSciIdl.cmake 1081 2016-09-10 15:44:42Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# convenience variable for ITT's install dir, should be fixed to use
# Program Files env var but it is problematic in cygwin
if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  set(_Idl_PROGRAM_FILES_DIR "C:/Program Files")
  set(_Idl_NAME "IDL")
  set(_Idl_OS "")
  set(_Idl_KNOWN_COMPANIES "Exelis" "ITT")
  set(_Idl_EXECUTABLE_EXT ".exe")
  set(Idl_PATH_SEP ";")
elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "CYGWIN")
  set(_Idl_PROGRAM_FILES_DIR "/cygdrive/c/Program Files")
  set(_Idl_NAME "IDL")
  set(_Idl_OS "")
  set(_Idl_KNOWN_COMPANIES "Exelis" "ITT")
  set(_Idl_EXECUTABLE_EXT ".exe")
  set(Idl_PATH_SEP ";")

  set(CMAKE_FIND_LIBRARY_SUFFIXES ".lib")
  set(CMAKE_FIND_LIBRARY_PREFIXES "")
elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  set(_Idl_PROGRAM_FILES_DIR "/Applications")
  set(_Idl_NAME "idl")
  set(_Idl_OS "darwin.")
  set(_Idl_KNOWN_COMPANIES "exelis" "itt")
  set(_Idl_EXECUTABLE_EXT "")
  set(Idl_PATH_SEP ":")
elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  set(_Idl_PROGRAM_FILES_DIR "/usr/local")
  set(_Idl_NAME "idl")
  set(_Idl_OS "linux.")
  set(_Idl_KNOWN_COMPANIES "exelis" "itt")
  set(_Idl_EXECUTABLE_EXT "")
  set(Idl_PATH_SEP ":")
endif ()

# find idl based on version numbers, if you want a specific one, set
# it prior to running configure
if (NOT DEFINED Idl_FIND_VERSION)
  set(_Idl_KNOWN_VERSIONS "84" "83" "82" "81" "80" "71" "706")
# IDL 8.0 is in a different location than other versions on Windows (extra IDL directory in path)
  foreach (_Idl_COMPANY ${_Idl_KNOWN_COMPANIES})
    list(APPEND
         _Idl_SEARCH_DIRS
         "${_Idl_PROGRAM_FILES_DIR}/${_Idl_COMPANY}/${_Idl_NAME}/${_Idl_NAME}80")
    list(APPEND
         _Idl_SEARCH_DIRS
         "${_Idl_PROGRAM_FILES_DIR}/${_Idl_COMPANY}/${_Idl_NAME}/${_Idl_NAME}81")
    foreach (_Idl_KNOWN_VERSION ${_Idl_KNOWN_VERSIONS})
      list(APPEND _Idl_SEARCH_DIRS
           "${_Idl_PROGRAM_FILES_DIR}/${_Idl_COMPANY}/${_Idl_NAME}${_Idl_KNOWN_VERSION}")
    endforeach (_Idl_KNOWN_VERSION ${_Idl_KNOWN_VERSIONS})
  endforeach (_Idl_COMPANY ${_Idl_KNOWN_COMPANIES})
endif ()

if (NOT "$ENV{IDL_DIR}" STREQUAL "")
  set(_Idl_SEARCH_DIRS "$ENV{IDL_DIR}")
endif ()

find_path(Idl_INCLUDE_DIR
  idl_export.h
  PATHS ${_Idl_SEARCH_DIRS}
  HINTS ${IDL_ROOT}
  PATH_SUFFIXES external/include
)

if ("${CMAKE_SIZEOF_VOID_P}" STREQUAL "4")
  set(Idl_BIN_EXT "x86")
elseif ("${CMAKE_SIZEOF_VOID_P}" STREQUAL "8")
  set(Idl_BIN_EXT "x86_64")
else ()
  set (Idl_BIN_EXT "unknown")
endif ()

set(Idl_PLATFORM_EXT "${_Idl_OS}${Idl_BIN_EXT}")

find_library(Idl_LIBRARY
  NAMES idl
  PATHS ${_Idl_SEARCH_DIRS}
  HINTS ${IDL_ROOT}
  PATH_SUFFIXES /bin/bin.${Idl_PLATFORM_EXT}
)

if (Idl_INCLUDE_DIR AND Idl_LIBRARY)
  set(IDL_FOUND TRUE)
endif ()

if (IDL_FOUND)
  # find the version
  get_filename_component(Idl_ROOT "${Idl_INCLUDE_DIR}/../.." ABSOLUTE)
  set(_Idl_VERSION_FILENAME "${Idl_ROOT}/version.txt")

  if (EXISTS "${_Idl_VERSION_FILENAME}")
    file(READ "${_Idl_VERSION_FILENAME}" _Idl_VERSION)
    string(STRIP "${_Idl_VERSION}" Idl_VERSION)
  endif ()

  if (WIN32 OR CYGWIN)
    set(Idl_EXECUTABLE "${Idl_ROOT}/bin/bin.${Idl_BIN_EXT}/idl${_Idl_EXECUTABLE_EXT}")
  else ()
    set(Idl_EXECUTABLE "${Idl_ROOT}/bin/idl${_Idl_EXECUTABLE_EXT}")
  endif ()

  if (NOT SciIdl_FIND_QUIETLY)
    if (DEFINED Idl_VERSION)
      message(STATUS "Found IDL: ${Idl_LIBRARY} (found version \"${Idl_VERSION}\")")
    else ()
      message(STATUS "Found IDL: ${Idl_LIBRARY} (no version file found)")
    endif ()
  endif ()
  set(HAVE_IDL 1 CACHE BOOL "Whether have IDL")
else ()
   if (SciIdl_FIND_REQUIRED)
      message(FATAL_ERROR "Did not find IDL. Use -DIdl_INCLUDE_DIR and -DIdl_LIBRARY to specify IDL location.")
   endif ()
endif ()

