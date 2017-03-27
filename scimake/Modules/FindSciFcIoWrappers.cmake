# - FindSciFcIoWrappers: Module to find include directories and
#   libraries for Fc IO Wrappers
#
# Module usage:
#   find_package(SciFcIoWrappers ...)
#
# This module will define the following variables:
#  HAVE_FCIOWRAPPERS, FCIOWRAPPERS_FOUND = Whether libraries and includes are found
#  FcIoWrappers_INCLUDE_DIRS       = Location of FcIoWrappers includes
#  FcIoWrappers_LIBRARY_DIRS       = Location of FcIoWrappers libraries
#  FcIoWrappers_LIBRARIES          = Required libraries
#
# If

######################################################################
#
# FindSciFcIoWrappers: find includes and libraries for Fcio Wrappers
#
# $Id: FindSciFcIoWrappers.cmake 1081 2016-09-10 15:44:42Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (ENABLE_PARALLEL)
  set(instdir "fciowrappers-par;fciowrappers-ben")
else ()
  set(instdir "fciowrappers")
endif ()
SciFindPackage(PACKAGE "FcIoWrappers"
  INSTALL_DIR "${instdir}"
  HEADERS "vshdf5_dummy.h"
  MODULES "ezcdf;ezcdf_attrib;ezcdf_genget;ezcdf_genput;ezcdf_inqvar;ezcdf_opncls;hdf5_api"
  LIBRARIES "ezcdf;vshdf5"
  LIBRARY_SUBDIRS "lib"
)

if (FCIOWRAPPERS_FOUND)
  set(HAVE_FCIOWRAPPERS 1 CACHE BOOL "Whether have the FCIOWRAPPERS library")
endif ()

# Get dependencies - allow for builds without netcdf
string(FIND "${FcIoWrappers_LIBRARY_NAMES}" "vshdf5" VSHDF5_LOC)
string(COMPARE NOTEQUAL "-1" ${VSHDF5_LOC} FC_VSHDF5_FOUND)
if (FCIOWRAPPERS_FOUND OR ${FC_VSHDF5_FOUND})
# Find share directory (FcIoWrappers_ROOT_DIR may not be set)
  if (NOT FcIoWrappers_ROOT_DIR)
    foreach (libdir ${FcIoWrappers_LIBRARY_DIRS})
      if (EXISTS ${libdir}/../share)
        SciGetRealDir(${libdir}/.. FcIoWrappers_ROOT_DIR)
      endif ()
    endforeach ()
    message(STATUS "Setting FcIoWrappers_ROOT_DIR = ${FcIoWrappers_ROOT_DIR}")
  endif ()
  if (FcIoWrappers_ROOT_DIR)
    include(${SCIMAKE_DIR}/SciGetDepsFromInstall.cmake)
# Get hdf5 libraries
    SciGetDepsFromInstall(Hdf5 "${FcIoWrappers_ROOT_DIR}" HDF5)
    message(STATUS "HDF5_FOUND = ${HDF5_FOUND}.")
    SciGetDepsFromInstall(Z "${FcIoWrappers_ROOT_DIR}" Z)
    message(STATUS "Z_FOUND = ${Z_FOUND}.")
# Get netcdf libraries
    if (FCIOWRAPPERS_FOUND)
      SciGetDepsFromInstall(Netcdf "${FcIoWrappers_ROOT_DIR}" NETCDF)
      message(STATUS "NETCDF_FOUND = ${NETCDF_FOUND}.")
    endif ()
  endif ()
endif ()

