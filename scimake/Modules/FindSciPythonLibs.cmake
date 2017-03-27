# - FindSciPythonLibs: This module finds Python, its include dirs, and
# its libraries.  Unlike the module that comes with cmake, this module
# works off the python that is found in one's path.  If none found,
# then it reverts to using cmake's FindPythonLibs.
#
# INPUTS(not sought if defined)
#
#  PYTHON_INCLUDE_PATH    - path to where Python.h is found.
#                           For compatibility with the CMake module, one can
#                           instead define Python_INCLUDE_DIR
#  PYTHON_LIBRARY         - path to the python library
#                           For compatibility with the CMake module, one can
#                           instead define Python_LIBRARY
#
# OUTPUTS
#
#  PYTHONLIBS_FOUND       - whether the Python libs have been found
#  Python_LIBRARIES       - path to the python library
#  Python_LIB             - the scimake style name of the python library
#  Python_INCLUDE_DIRS    - path to where Python.h is found
#  Python_DEBUG_LIBRARIES - path to the debug library
#
#  PYTHON                 - full path to python
#  Python_STLIBS          - the static library if found, otherwise whatever
#                           python library is found.
#  Python_STLIB           - deprecated.  Synonym for ${Python_STLIB}.
#  Python_LIBS            - deprecated.  Synonym for ${Python_STLIB}.

######################################################################
#
# FindSciPythonLibs.cmake: Wrap cmake's python finder to fix for mingw
#
# $Id: FindSciPythonLibs.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# Messaged added April 23, 2014.  Remove file on Sep. 31, 2014.
message(WARNING "This package file deprecated. Please switch to FindSciPython.cmake.")

option(ENABLE_PYTHON "Whether to enable Python" ON)

if (ENABLE_PYTHON)
  message("")
  message("--------- SciPythonLibs looking for Python libraries ---------")

# Backward compatibility
  if (PYTHON_INCLUDE_PATH)
  message("--------- Setting Python_INCLUDE_DIRS")
    set(Python_INCLUDE_DIRS ${PYTHON_INCLUDE_PATH})
  endif ()
  if (PYTHON_LIBRARY)
  message("--------- Setting Python_LIBRARIES")
    set(Python_LIBRARIES ${Python_LIBRARY})
  endif ()

# If already defined, done
  if (Python_INCLUDE_DIRS AND Python_LIBRARIES)
    set(PYTHONLIBS_FOUND TRUE)
# Search for python on unix using path
  else ()
    find_program(PYTHON python
      HINTS "${Python_ROOT_DIR}"
      PATH_SUFFIXES bin)
    if (DEBUG_CMAKE)
      message(STATUS "PYTHON = ${PYTHON}.")
    endif ()
# Use python to find its include and library dirs
    if (PYTHON)
# Get version
      execute_process(COMMAND ${PYTHON} -V ERROR_VARIABLE Python_VERSION)
      message(STATUS "Python_VERSION = ${Python_VERSION}.")
      string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+"
        Python_VERSION "${Python_VERSION}"
      )
      string(REGEX MATCH "^[0-9]+\\.[0-9]"
                Python_MAJMIN "${Python_VERSION}")

      if (WIN32)
        string(REPLACE "." "" Python_MAJMIN "${Python_MAJMIN}")
      endif ()
      if (DEBUG_CMAKE)
        message(STATUS "Python_VERSION = ${Python_VERSION}.")
        message(STATUS "Python_MAJMIN = ${Python_MAJMIN}.")
      endif ()
      set(Python_LIB python${Python_MAJMIN} CACHE STRING "Python library name")
# Synonyms
      set(Python_LIBRARY_NAME ${Python_LIB})
      set(Python_LIBRARY_NAMES ${Python_LIB})
# Should check here that version >2.5.
# Find includes
      if (NOT Python_INCLUDE_DIRS)
        execute_process(COMMAND ${PYTHON} -c "import os, sys; print os.path.join(sys.prefix, 'include', 'python')"
          OUTPUT_VARIABLE Python_INCDIR)
        string(STRIP "${Python_INCDIR}" Python_INCDIR)
        if (WIN32)
          get_filename_component(Python_INCLUDE_DIRS "${Python_INCDIR}/.." REALPATH)
        else ()
          set(Python_INCLUDE_DIRS "${Python_INCDIR}${Python_MAJMIN}")
        endif ()
      endif ()
      if (DEBUG_CMAKE)
        message(STATUS "Python_INCLUDE_DIRS = ${Python_INCLUDE_DIRS}.")
      endif ()
      execute_process(COMMAND python -c "import distutils.sysconfig; print distutils.sysconfig.get_python_lib(1,1)"
        OUTPUT_VARIABLE Python_TOPLIBDIR)
      string(STRIP "${Python_TOPLIBDIR}" Python_TOPLIBDIR)
      if (DEBUG_CMAKE)
        message(STATUS "Python_TOPLIBDIR = ${Python_TOPLIBDIR}.")
      endif ()
# Find libraries
      if (NOT Python_LIBRARIES)
        if (DEBUG_CMAKE)
          message(STATUS "Looking for library, ${Python_LIB}.")
        endif ()
        if (WIN32)
          get_filename_component(Python_TOPLIBDIR "${Python_TOPLIBDIR}/.." REALPATH)
          file(TO_CMAKE_PATH "${Python_TOPLIBDIR}" Python_TOPLIBDIR)
        endif ()
        if (DEBUG_CMAKE)
          message(STATUS "find_library(Python_LIBRARIES ${Python_LIB}
              PATHS ${Python_TOPLIBDIR}/config ${Python_TOPLIBDIR}/libs
              NO_DEFAULT_PATH)"
          )
        endif ()
        find_library(Python_LIBRARIES ${Python_LIB}
          HINTS "${Python_ROOT_DIR}"
          PATH_SUFFIXES bin lib libs
          PATHS ${Python_TOPLIBDIR}/config ${Python_TOPLIBDIR}/libs
          NO_DEFAULT_PATH)
      endif ()
      if (Python_INCLUDE_DIRS AND Python_LIBRARIES)
        set(PYTHONLIBS_FOUND TRUE)
        get_filename_component(Python_LIBRARY_NAMES ${Python_LIBRARIES} NAME)
        foreach (sfx lib a dylib so)
          string(REGEX REPLACE "\\.${sfx}$" "" Python_LIBRARY_NAMES ${Python_LIBRARY_NAMES})
        endforeach ()
        string(REGEX REPLACE "^lib" "" Python_LIBRARY_NAMES ${Python_LIBRARY_NAMES})
      endif ()
    endif ()
  endif (Python_INCLUDE_DIRS AND Python_LIBRARIES)

# Python not found so find python using cmake's module
  if (NOT PYTHONLIBS_FOUND)
    set(PythonLibs_DIR "${Python_ROOT_DIR}")
    find_package(PythonLibs)

    if (PYTHONLIBS_FOUND)
      option(HAVE_PYTHON "Python library" ON)
      set (Python_INCLUDE_DIRS ${PYTHON_INCLUDE_DIRS})
      set (Python_LIBRARIES ${PYTHON_LIBRARY})
      get_filename_component(Python_LIBRARY_NAMES ${Python_LIBRARIES} NAME_WE)
      message(STATUS "Python_LIBRARY = ${Python_LIBRARIES}")
# MinGW requires one to separate the dir and the lib, and then
# the library needs to have a possible leading lib removed.
      if (USING_MINGW)
        get_filename_component(Python_LIBDIR ${Python_LIBRARIES}/.. REALPATH)
        get_filename_component(Python_LLIB ${Python_LIBRARIES} NAME_WE)
        string(REGEX REPLACE "^lib" "" Python_LLIB ${Python_LLIB})
        set(Python_LIBS "-L${Python_LIBDIR} -l${Python_LLIB}")
      else ()
        set(Python_LIBS ${PYTHON_LIBRARIES})
      endif ()
    endif ()
  endif ()

# Get static libraries
  if (PYTHONLIBS_FOUND)
    get_filename_component(Python_LIBRARY_DIRS ${Python_LIBRARIES}/.. REALPATH)
    get_filename_component(Python_PYLIBDIR ${Python_LIBRARY_DIRS}/.. REALPATH)
# Seek static library
    SciGetStaticLibs("${Python_LIBRARIES}" PYTHON_STLIBS)
# This may not give the static library, but it is the best we have
    if (NOT Python_LIBS)
      if (Python_STLIBS)
        set(Python_LIBS ${Python_STLIBS})
        set(Python_STLIB ${Python_STLIBS}) # Backwards compatibility
      else ()
        set(Python_LIBS ${Python_LIBRARIES})
      endif ()
    endif ()
# Look for executable
    if (NOT PYTHON)
      find_program(PYTHON python
        HINTS "${Python_ROOT_DIR}"
        PATH_SUFFIXES bin
        PATHS ${Python_PYLIBDIR}/bin ${Python_PYLIBDIR}
      )
    endif ()
  endif ()

# Print result
  if (PYTHONLIBS_FOUND)
      execute_process(COMMAND python -c "import sys; print sys.version[:3]"
        OUTPUT_VARIABLE Python_VERSION)
      string(STRIP "${Python_VERSION}" Python_VERSION)
    if (WIN32)
      string(REGEX REPLACE "\\." "" Python_VERSION_WINSTR "${Python_VERSION}")

      file(TO_CMAKE_PATH "${Python_INCLUDE_DIRS}" Python_INCLUDE_DIRS)
    endif ()
    set(HAVE_PYTHON 1 CACHE BOOL "Whether have Python")
  endif ()
  set(PYTHON_VERSION ${Python_VERSION})
  SciPrintCMakeResults(Python)
  SciPrintVar(PYTHON)
  SciPrintVar(Python_VERSION)
  SciPrintVar(Python_PYLIBDIR)
  message(STATUS "Related variables:")
  SciPrintVar(Python_LIB)
  SciPrintVar(Python_LIBS)
  SciPrintVar(Python_LIBRARY_NAME)

endif ()

