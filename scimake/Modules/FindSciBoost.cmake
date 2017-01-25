# - FindSciBoost: Module to find include directories and libraries for
#   Boost. This module was originally developed to set the
#   SUPRA_SEARCH_PATH, so that the system path was not checked before
#   the user specified path, and included the stock FindBoost. This
#   changed after quite a few modifications, as it would still look at
#   system path if the libraries weren't found in the user specified
#   path.
#
#   Should be modified to include the stock FindBoost?
#
# This module can be included in CMake builds using find_package:
#   find_package(SciBoost REQUIRED signals filesystem system ...)
#
# The components list needs to contain actual names of boost libraries
# only: signals for libboost_signals, system for libboost_system, etc.
#
# This module will define the following variables:
#   BOOST_FOUND, HAVE_BOOST = True if Boost is found: the include directory was
#                             found and all the libraries specified were found.
#   Boost_INCLUDE_DIRS      = Location of Boost includes
#   Boost_LIBRARY_DIRS      = Location of Boost libraries
#   Boost_LIBRARIES         = Required libraries

######################################################################
#
# FindSciBoost: find includes and libraries for boost
#
# $Id: FindSciBoost.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

# Default: libraries have boost_ prepended.
set(BOOST_LIB_PREFIX boost_)
# But Windows static have libboost_ prepended.
if (WIN32 AND NOT(USE_SHARED_LIBS OR BUILD_SHARED_LIBS OR ENABLE_SHARED))
  set(BOOST_LIB_PREFIX libboost_)
endif ()
if (DEBUG_CMAKE)
  message(STATUS "BOOST_LIB_PREFIX = ${BOOST_LIB_PREFIX}.")
endif ()

# Set boost libraries to find
if (DEBUG_CMAKE)
  message(STATUS "SciBoost_FIND_COMPONENTS = ${SciBoost_FIND_COMPONENTS}.")
  message(STATUS "SciBoost_FIND_REQUIRED = ${SciBoost_FIND_REQUIRED}.")
endif ()
set(SciBoost_LIBRARY_LIST "")
foreach (COMPONENT ${SciBoost_FIND_COMPONENTS})
  set(SciBoost_LIBRARY_LIST ${SciBoost_LIBRARY_LIST} ${BOOST_LIB_PREFIX}${COMPONENT})
endforeach ()
if (DEBUG_CMAKE)
  message(STATUS "SciBoost_LIBRARY_LIST = ${SciBoost_LIBRARY_LIST}.")
endif ()

SciGetInstSubdirs(Boost instdirs)
if (DEBUG_CMAKE)
  message(STATUS "Boost instdirs = ${instdirs}.")
endif ()
SciFindPackage(PACKAGE "Boost"
  INSTALL_DIRS ${instdirs}
  HEADERS boost/thread.hpp OPTIONAL boost/align/aligned_allocator.hpp
  LIBRARIES "${SciBoost_LIBRARY_LIST}"
)
unset(SciBoost_LIBRARY_LIST CACHE)
if (DEBUG_CMAKE)
  message(STATUS "Boost_DLLS = ${Boost_DLLS}.")
endif ()

# Determine whether Boost libs are shared
set(Boost_LIBS_ARE_SHARED FALSE)
if (Boost_DLLS)
  set(Boost_LIBS_ARE_SHARED TRUE)
else ()
  list(GET Boost_LIBRARIES 0 blib0)
  message(STATUS "blib0 = ${blib0}.")
  if ((blib0 MATCHES "\\.dylib$") OR (blib0 MATCHES "\\.so$") OR (blib0 MATCHES "\\.so\\."))
    set(Boost_LIBS_ARE_SHARED TRUE)
  endif ()
endif ()
message(STATUS "Boost_LIBS_ARE_SHARED = ${Boost_LIBS_ARE_SHARED}.")

# If Boost libs are shared, one must have different defines
# http://boost.2283326.n4.nabble.com/Undefined-reference-to-main-with-Boost-Test -Why-td2576147.html
if (Boost_LIBS_ARE_SHARED)
  message(STATUS "Setting Boost shared library definitions")
  set(Boost_DEFINITIONS -DBOOST_ALL_DYN_LINK)
  if (WIN32 AND NOT Boost_boost_unit_test_framework_LIBRARY)
# This reverses DYN_LINK for tests.  See boost/test/included/unit_test.hpp.
    set(Boost_DEFINITIONS ${Boost_DEFINITIONS} -DBOOST_TEST_INCLUDED)
  endif ()
endif ()
message(STATUS "Boost_DEFINITIONS = ${Boost_DEFINITIONS}.")

# Final check
if (BOOST_FOUND AND NOT Boost_INCLUDE_DIRS)
  set(BOOST_FOUND FALSE)
  message(STATUS "Reversing Boost found as Boost_INCLUDE_DIRS is empty.")
endif ()

if (BOOST_FOUND)
  # message(STATUS "Found Boost")
  set(HAVE_BOOST 1 CACHE BOOL "Whether have the Boost library")
else ()
  message(STATUS "Did not find Boost.  Use -DBoost_ROOT_DIR to specify installation directory.")
  if (SciBoost_FIND_REQUIRED)
    message(FATAL_ERROR "Failed finding boost.")
  endif ()
endif ()

# Set names for all the libs
if ("${BOOST_LIB_PREFIX}" STREQUAL "libboost_")
  foreach (COMPONENT ${SciBoost_FIND_COMPONENTS})
    set(Boost_boost_${COMPONENT}_LIBRARY ${Boost_${BOOST_LIB_PREFIX}${COMPONENT}_LIBRARY})
  endforeach ()
endif ()

