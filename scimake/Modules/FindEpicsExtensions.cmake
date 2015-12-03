# - FindEpicsExtensions: Module to find include directories and libraries
#   for Epics Extensions. This module was implemented as there is no stock
#   CMake module for Epics.
#
# This module can be included in CMake builds in find_package:
#   find_package(EpicsExtensions REQUIRED)
#
# This module will define the following variables:
#  HAVE_EPICSEXTENSIONS = Whether have the Epics Extensions library
#  EpicsExtensions_INCLUDE_DIRS = Location of Epics Extensions includes
#  EpicsExtensions_LIBRARY_DIRS = Location of Epics Extensions libraries
#  EpicsExtensions_LIBRARIES    = Required libraries
#  EpicsExtensions_STLIBS       = Location of Epics Extensions static library

######################################################################
#
# FindEpicsExtensions: find includes and libraries for Epics Extensions
#
# $Id: FindEpicsExtensions.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (ENABLE_PARALLEL)
  set(epicsdir epics-par)
  set(sddslib SDDSmpi)
  set(pgapacklib ";pgapack")
else ()
  set(epicsdir epics)
  set(sddslib SDDS1)
  set(pgapacklib "")
endif ()

SciFindPackage(
        PACKAGE EpicsExtensions
        INSTALL_DIR "${epicsdir}/extensions"
        HEADERS "awe.h;gsl/gsl_poly.h"
        LIBRARIES
  "mdbcommon;matlib;fftpack;${sddslib};rpnlib;mdbmth;namelist;mdblib;meschach;gsl${pgapacklib}"
        INCLUDE_SUBDIRS "src/SDDS/include;src/SDDS/gsl"
        LIBRARY_SUBDIRS "lib/linux-x86_64;lib/linux-x86"
)
