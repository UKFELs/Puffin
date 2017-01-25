# - FindSciNetlibLite: Module to find include directories and
#   libraries for NetlibLite
#
# Module usage:
#   find_package(SciNetlibLite ...)
#
# This module will define the following variables:
#  HAVE_NETLIBLITE, NETLIBLITE_FOUND = Whether libraries and includes are found
#  NetlibLite_INCLUDE_DIRS       = Location of Polyswift includes
#  NetlibLite_LIBRARY_DIRS       = Location of Polyswift libraries
#  NetlibLite_LIBRARIES          = Required libraries

######################################################################
#
# FindNetlibLite: find includes and libraries for txbase
#
# $Id: FindSciNetlibLite.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (ENABLE_PARALLEL)
  set(instdirs netlib_lite-ben netlib_lite)
else ()
  set(instdirs netlib_lite)
endif ()

SciFindPackage(PACKAGE "NetlibLite"
  INSTALL_DIRS ${instdirs}
  MODULES "lsode_mod"
  LIBRARIES "lsode;nlother;r8slatec"
)

