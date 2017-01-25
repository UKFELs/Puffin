# - FindSciFmcfm: Module to find include directories and
#   libraries for Fmcfm.
#
# Module usage:
#   find_package(SciFmcfm ...)
#
# This module will define the following variables:
#  HAVE_FMCFM, FMCFM_FOUND = Whether libraries and includes are found
#  Fmcfm_INCLUDE_DIRS       = Location of Fmcfm includes
#  Fmcfm_LIBRARY_DIRS       = Location of Fmcfm libraries
#  Fmcfm_LIBRARIES          = Required libraries

######################################################################
#
# FindSciFmcfm: find includes and libraries for Fmcfm
#
# $Id: FindSciFmcfm.cmake 1081 2016-09-10 15:44:42Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (ENABLE_PARALLEL)
  set(instdir fmcfm-par)
else ()
  set(instdir fmcfm)
endif ()
SciFindPackage(PACKAGE "Fmcfm"
  INSTALL_DIR "${instdir}"
  HEADERS "FmGlf23TransportModel.h;FmNclassTransportModel.h;FmGyroTransportModel.h;FmTglfTransportModel.h"
  LIBRARIES "fmcfmcppwrap;fmcfmcxx;fmcfmwrap;fmcfm"
  LIBRARY_SUBDIRS "lib/${Fortran_COMP_LIB_SUBDIR};lib"
)

if (FMCFM_FOUND)
  # message(STATUS "Found FMCFM")
  set(HAVE_FMCFM 1 CACHE BOOL "Whether have the FMCFM library")
# Find gacodes
  include(${SCIMAKE_DIR}/SciGetDepsFromInstall.cmake)
# Get gacodes libraries
  SciGetDepsFromInstall(GaCode ${Fmcfm_DIR} GACODE)
# Get ntcctransport libraries
  SciGetDepsFromInstall(NtccTransport ${Fmcfm_DIR} NTCCTRANSPORT)
endif ()

