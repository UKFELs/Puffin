# - FindSciHdf5: Module to find include directories and
#   libraries for Hdf5.
#
# Module usage:
#   find_package(SciHdf5 ...)
#
# This module will define the following variables:
#  HAVE_HDF5, HDF5_FOUND   = Whether libraries and includes are found
#  Hdf5_INCLUDE_DIRS       = Location of Hdf5 includes
#  Hdf5_LIBRARY_DIRS       = Location of Hdf5 libraries
#  Hdf5_LIBRARIES          = Required libraries
#  Hdf5_DLLS               =

######################################################################
#
# FindSciHdf5: find includes and libraries for hdf5
#
# $Id: FindSciHdf5.cmake 1031 2016-04-10 14:59:03Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# The names of the hdf5 libraries can vary, so instead we find
# the includes and look for the cmake file.  But for that, we
# need the version.
message("")
message(STATUS "Initial search for Hdf5 components")

SciGetInstSubdirs(Hdf5 instdirs)

SciFindPackage(PACKAGE "Hdf5"
  INSTALL_DIRS ${instdirs}
  HEADERS hdf5.h H5pubconf.h
# Last in list is for finding within VisIt installation
  INCLUDE_SUBDIRS include include/hdf5/include
  MODULE_SUBDIRS include
      include/static  # 1.8.16
  FIND_QUIETLY
)

# The version is given by H5_PACKAGE_VERSION in H5pubconf.h
if (NOT Hdf5_H5pubconf_h)
  set(Hdf5_FOUND FALSE)
  message(FATAL_ERROR "Hfpubconf.h not found.")
endif ()
if (DEBUG_CMAKE)
  message(STATUS "Hdf5_H5pubconf_h = ${Hdf5_H5pubconf_h}.")
  message(STATUS "Hdf5_H5pubconf_h_INCLUDE_DIR = ${Hdf5_H5pubconf_h_INCLUDE_DIR}.")
endif ()
file(STRINGS ${Hdf5_H5pubconf_h} line
  REGEX "^#define *H5_PACKAGE_VERSION"
)
if (DEBUG_CMAKE)
  message(STATUS "line = ${line}.")
endif ()
string(REGEX REPLACE "#define *H5_PACKAGE_VERSION*" "" val "${line}")
string(REGEX REPLACE "\"" "" Hdf5_VERSION "${val}")
string(STRIP "${Hdf5_VERSION}" Hdf5_VERSION)
message(STATUS "Hdf5_VERSION = ${Hdf5_VERSION}.")

# Fill in what we know
if (${Hdf5_hdf5_h_INCLUDE_DIR} MATCHES "include/hdf5/include$")
  get_filename_component(Hdf5_ROOT_DIR ${Hdf5_hdf5_h_INCLUDE_DIR}/../../.. REALPATH)
else ()
  get_filename_component(Hdf5_ROOT_DIR ${Hdf5_hdf5_h_INCLUDE_DIR}/.. REALPATH)
endif ()
message(STATUS "Hdf5_ROOT_DIR = ${Hdf5_ROOT_DIR}.")

# Version known, can look for config file
SciFindPackage(PACKAGE "Hdf5"
  CONFIG_SUBDIRS
# They keep changing the location
    lib/cmake/hdf5-${Hdf5_VERSION}   # 1.8.6
    share/cmake/hdf5-${Hdf5_VERSION} # 1.8.7
    share/cmake/hdf5   # 1.8.12
    share/cmake        # 1.8.18
    cmake        # 1.8.16-windows
  # USE_CONFIG_FILE # Cannot always source, so decide later
  CONFIG_FILE_ONLY
  FIND_QUIETLY
)
message(STATUS "Hdf5_CONFIG_CMAKE = ${Hdf5_CONFIG_CMAKE}.")

# Not all version have good files to source
if (FALSE)
if (${Hdf5_VERSION} STREQUAL 1.8.12 OR ${Hdf5_VERSION} STREQUAL 1.8.9)
  message(STATUS "Not using ${Hdf5_CONFIG_CMAKE}.")
else ()
  include(${Hdf5_CONFIG_CMAKE})
  if (DEBUG_CMAKE)
    message(STATUS "After including ${Hdf5_CONFIG_CMAKE}, HDF5_LIBRARIES = ${HDF5_LIBRARIES}.")
  endif ()
endif ()
endif ()

# Get the libraries in proper order
if (HDF5_LIBRARIES)
  set(hlibs ${HDF5_LIBRARIES})
else ()
  file(GLOB hlibs ${Hdf5_ROOT_DIR}/lib/*hdf5*)
endif ()
if (DEBUG_CMAKE)
  message(STATUS "hlibs = ${hlibs}.")
endif ()
set(hlnms)
foreach (lb ${hlibs})
  get_filename_component(ln ${lb} NAME_WE)
# Remove leading lib
  string(REGEX REPLACE "^lib" "" ln "${ln}")
  set(hlnms ${hlnms} ${ln})
endforeach ()
if (DEBUG_CMAKE)
  message(STATUS "hlnms = ${hlnms}.")
endif ()
if (Hdf5_NEEDED_LIBS)
  set(desiredlibs ${Hdf5_NEEDED_LIBS})
else ()
  set(desiredlibs)
  foreach (nm hdf5_tools hdf5_toolsdll hdf5_hl_fortran hdf5_hl_f90cstub
    hdf5_fortran hdf5_f90cstub hdf5_hl hdf5_hldll hdf5 hdf5dll
    hdf5_debug
  )
    list(FIND hlnms ${nm} indx)
    if (NOT(${indx} EQUAL -1))
      set(desiredlibs ${desiredlibs} ${nm})
    endif ()
  endforeach ()
  set(desiredlibs ${desiredlibs} OPTIONAL)
endif ()
if (DEBUG_CMAKE)
  message(STATUS "desiredlibs = ${desiredlibs}.")
endif ()

# Get execs
file(GLOB hexecs ${Hdf5_ROOT_DIR}/bin/h5diff* ${Hdf5_ROOT_DIR}/bin/h5ls*)
set(desiredexecs)
foreach (ex ${hexecs})
  get_filename_component(en ${ex} NAME_WE)
  set(desiredexecs ${desiredexecs} ${en})
endforeach ()

if (DEBUG_CMAKE)
  message(STATUS "Looking for the HDF5 libraries, ${desiredlibs}.")
endif ()

set(desiredmods)
if (CMAKE_Fortran_COMPILER_WORKS)
  set(desiredmods hdf5)
endif ()
SciFindPackage(PACKAGE "Hdf5"
  PROGRAMS ${desiredexecs}
  HEADERS hdf5.h
  LIBRARIES ${desiredlibs}
  MODULES ${desiredmods}
  INCLUDE_SUBDIRS include include/hdf5/include # Last for VisIt installation
  MODULE_SUBDIRS include/fortran lib
      include         # 1.8.13
      include/static  # 1.8.16
)

if (HDF5_FOUND)
# Backward compatibility
  set(HAVE_HDF5 1 CACHE BOOL "Whether have the HDF5 library")
  set(OLD_H5S_SELECT_HYPERSLAB_IFC 0 CACHE BOOL
    "Whether using the old 1.6.3 H5Sselect_hyperslab interface")
  if (WIN32 AND Hdf5_DLLS)
    set(Hdf5_DEFINITIONS ${Hdf5_DEFINITIONS} -DH5_BUILT_AS_DYNAMIC_LIB)
    message(STATUS "Adding to Hdf5_DEFINITIONS that H5 build dynamic.")
    SciPrintVar(Hdf5_DEFINITIONS)
  endif ()
# if linux add dl to the libraries variable
# JRC: Cannot do this here as interferes with static linking of vorpal.
# Must be done in the CMakeLists.txt where the executable is being linked.
  if (LINUX)
    # set(Hdf5_LIBRARIES ${Hdf5_LIBRARIES} dl)
  endif ()
else ()
  message(STATUS "Did not find Hdf5.  Use -DHdf5_ROOT_DIR to specify the installation directory.")
  if (SciHdf5_FIND_REQUIRED)
    message(FATAL_ERROR "Failing.")
  endif ()
endif ()

