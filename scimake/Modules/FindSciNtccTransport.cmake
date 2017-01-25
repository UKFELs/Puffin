# - FindSciNtccTransport: Module to find include directories and
#   libraries for Ntcc Transport
#
# Module usage:
#   find_package(SciNtccTransport ...)
#
# This module will define the following variables:
#  HAVE_NTCCTRANSPORT, NTCCTRANSPORT_FOUND = Whether libraries and includes are found
#  NtccTransport_INCLUDE_DIRS       = Location of Polyswift includes
#  NtccTransport_LIBRARY_DIRS       = Location of Polyswift libraries
#  NtccTransport_LIBRARIES          = Required libraries

######################################################################
#
# FindSciNtccTransport: find includes and libraries for Ntcc Transport
#
# $Id: FindSciNtccTransport.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (ENABLE_PARALLEL)
  set(instdir "ntcc_transport-par")
  set(GLF23_INST_LIB glf23mpi)
else ()
  set(instdir "ntcc_transport")
  set(GLF23_INST_LIB glf23)
endif ()

SciFindPackage(PACKAGE NtccTransport
  INSTALL_DIRS ${instdir}
  MODULES glf23_data_mod nclass_interface
  LIBRARIES ${GLF23_INST_LIB} mmm95 ifspppl nclass kapisn mmm71
)

if (NTCCTRANSPORT_FOUND)
  set(HAVE_NTCCTRANSPORT 1 CACHE BOOL "Whether have the ntcc_transport library")
  set(HAVE_NtccTransport 1 CACHE BOOL "Whether have ntcc_transport the library")
endif ()

