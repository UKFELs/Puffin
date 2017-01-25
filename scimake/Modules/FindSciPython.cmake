# - FindSciPython: Module to find include directories and
#   libraries for Python.
#
# Module usage:
#   find_package(SciPython ...)
#
# This module will define the following variables:
#  HAVE_PYTHON, PYTHON_FOUND = Whether libraries and includes are found
#  Python_INCLUDE_DIRS      = Location of Python includes
#  Python_LIBRARIES         = Required libraries

##################################################################
#
# Find module for Python
#
# $Id: FindSciPython.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
##################################################################

message("")
message(STATUS "--------- Looking for Python" --------- )
message(STATUS "Seeking Python executable")
# Find in the path
if (WIN32)
  set(pynames python)
elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin" AND "${CMAKE_SYSTEM_VERSION}" STREQUAL "10.8.0")
  set(pynames python2.6)
  message(STATUS "Using Python 2.6 on SnowLeopard.")
else ()
  set(pynames python2.7 python2.6)
endif ()
find_program(Python_EXE NAMES ${pynames} PATHS ${Python_ROOT_DIR})
if (Python_EXE)

# Root directory, naes
  set(PYTHON_FOUND TRUE)
  get_filename_component(Python_EXE ${Python_EXE} REALPATH)
  get_filename_component(Python_NAME ${Python_EXE} NAME)
  if (WIN32)  # Remove .exe on windows
    get_filename_component(Python_NAME_WE ${Python_EXE} NAME_WE)
  else ()
    set(Python_NAME_WE ${Python_NAME})
  endif ()
  #string(REGEX REPLACE ".exe$" "" Python_NAME_WE "${Python_NAME}")
  get_filename_component(Python_BINDIR ${Python_EXE}/.. REALPATH)
  get_filename_component(Python_BINDIR_NAME ${Python_BINDIR} NAME)
  if ("${Python_BINDIR_NAME}" STREQUAL bin)
    get_filename_component(Python_ROOT_DIR ${Python_EXE}/../.. REALPATH)
  else ()
    get_filename_component(Python_ROOT_DIR ${Python_EXE}/.. REALPATH)
  endif ()

# Include directory
  execute_process(COMMAND ${Python_EXE} -c "import distutils.sysconfig; idir = distutils.sysconfig.get_python_inc(1); print idir,"
    OUTPUT_VARIABLE Python_INCLUDE_DIRS
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  file(TO_CMAKE_PATH "${Python_INCLUDE_DIRS}" Python_INCLUDE_DIRS)

# Version
  execute_process(COMMAND ${Python_EXE} -c "import sys;print sys.version[0]"
    OUTPUT_VARIABLE Python_MAJOR
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  execute_process(COMMAND ${Python_EXE} -c "import sys;print sys.version[2]"
    OUTPUT_VARIABLE Python_MINOR
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  execute_process(COMMAND ${Python_EXE} -c "import site;print site.__file__"
    OUTPUT_VARIABLE Python_SITE
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  string(REGEX REPLACE ".pyc$" ".py" Python_SITE "${Python_SITE}")   # Get the non-compiled version.
  file(TO_CMAKE_PATH "${Python_SITE}" Python_SITE)
  if (WIN32)
    set(Python_MAJMIN "${Python_MAJOR}${Python_MINOR}")
  else ()
    set(Python_MAJMIN "${Python_MAJOR}.${Python_MINOR}")
  endif ()
  set(Python_LIBRARY_NAMES python${Python_MAJMIN})

# Shared library
  find_library(Python_LIBRARY
    NAMES ${Python_LIBRARY_NAMES}
    PATHS ${Python_ROOT_DIR}
    PATH_SUFFIXES lib Libs lib/python${Python_MAJMIN}/config
    NO_DEFAULT_PATH
  )
  SciPrintVar(Python_LIBRARY)

  if (Python_LIBRARY)
    get_filename_component(Python_LIBRARY ${Python_LIBRARY} REALPATH)
    get_filename_component(Python_LIBRARY_NAME ${Python_LIBRARY} NAME_WE)
    set(Python_LIBRARIES ${Python_LIBRARY})
    get_filename_component(Python_LIBRARY_DIRS ${Python_LIBRARY}/.. REALPATH)
    if (WIN32)
      find_program(Python_DLLS ${Python_LIBRARY_NAME}.dll)
    endif ()
  else ()
    set(PYTHON_FOUND FALSE)
  endif ()

# Static library
  find_library(Python_STLIBS
    NAMES ${Python_LIBRARY_NAMES}
    PATHS ${Python_ROOT_DIR}
    PATH_SUFFIXES lib/python${Python_MAJMIN}/config
    NO_DEFAULT_PATH
  )
  # SciPrintVar(Python_STLIB)
  if (Python_STLIBS)
    get_filename_component(Python_STLIB_DIRS ${Python_STLIBS}/.. REALPATH)
  endif ()

# Modules
  if (Python_SITE)
    get_filename_component(Python_MODULES_DIR ${Python_SITE}/.. REALPATH)
    file(RELATIVE_PATH Python_MODULES_SUBDIR ${Python_ROOT_DIR} ${Python_MODULES_DIR})
  else ()
    set(PYTHON_FOUND FALSE)
  endif ()

else ()
  set(PYTHON_FOUND FALSE)
endif ()

foreach (var Python_ROOT_DIR Python_BINDIR Python_EXE Python_NAME Python_NAME_WE
    Python_INCLUDE_DIRS Python_MAJMIN Python_LIBRARY Python_LIBRARY_NAME
    Python_LIBRARIES Python_LIBRARY_DIRS Python_STLIBS Python_STLIB_DIRS Python_DLLS Python_MODULES_DIR
    Python_MODULES_SUBDIR Python_SITE)
  SciPrintVar(${var})
endforeach ()

if (PYTHON_FOUND)
  message(STATUS "[FindSciPython.cmake] - Found Python")
  set(HAVE_PYTHON 1 CACHE BOOL "Whether have Python.")
else ()
  message(STATUS "[FindSciPython.cmake] - Did not find Python, use -DPython_ROOT_DIR to supply the Python installation directory.")
  if (SciPython_FIND_REQUIRED)
    message(FATAL_ERROR "[FindSciPython.cmake] - Failing.")
  endif ()
endif ()

