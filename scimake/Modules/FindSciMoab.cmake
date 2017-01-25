# - FindSciMoab: Module to find include directories and
#   libraries for Moab.
#
# Module usage:
#   find_package(SciMoab ...)
#
# This module will define the following variables:
#  HAVE_MOAB, MOAB_FOUND = Whether libraries and includes are found
#  Moab_INCLUDE_DIRS       = Location of Moab includes
#  Moab_LIBRARY_DIRS       = Location of Moab libraries
#  Moab_LIBRARIES          = Required libraries
#  Moab_DLLS               =

######################################################################
#
# FindMoab: find includes and libraries for hdf5
#
# $Id: FindSciMoab.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2014-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (NOT DEFINED MOAB_COMPONENTS)
  set(moabfindlibs dagmc iMesh MOAB)
else ()
  set(moabfindlibs ${MOAB_COMPONENTS})
endif ()

set(USE_PYC_LIBS TRUE)
if (NOT DEFINED USE_SHARED_LIBS)
  set(USE_SHARED_LIBS TRUE)
endif ()
SciGetInstSubdirs(moab instdirs)

SciFindPackage(PACKAGE "Moab"
  INSTALL_DIRS ${instdirs}
  HEADERS "moab/Core.hpp"
  LIBRARIES "${moabfindlibs}"
  LIBRARY_SUBDIRS lib/${CXX_COMP_LIB_SUBDIR} lib
)

if (MOAB_FOUND)
  message(STATUS "Found Moab")
else ()
  message(STATUS "Did not find Moab.  Use -DMoab_ROOT_DIR to specify the installation directory.")
endif ()

