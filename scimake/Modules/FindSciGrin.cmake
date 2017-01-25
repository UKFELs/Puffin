# - FindSciGrin: Module to find include directories and libraries
#   for Grin. This module was implemented as there is no stock
#   CMake module for Grin.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciGrin REQUIRED)
#
# This module will define the following variables:
#  HAVE_GRIN         = Whether have the Grin library
#  Grin_INCLUDE_DIRS = Location of Grin includes
#  Grin_LIBRARY_DIRS = Location of Grin libraries
#  Grin_LIBRARIES    = Required libraries
#  Grin_STLIBS       = Location of Grin static library

######################################################################
#
# SciFindGrin: find includes and libraries for Grin.
#
# $Id: FindSciGrin.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH})

if (WIN32)
  set(GRIN_LIB_PREFIX "")
else (WIN32)
  set(GRIN_LIB_PREFIX "lib")
endif (WIN32)

if (WIN32)
  set(GRIN_LIB_SUFFIX "lib")
else (WIN32)
  set(GRIN_LIB_SUFFIX "a")
endif (WIN32)

if (DEFINED GRIN_FIND_VERSION)
  set(Grin_SEARCH "grin${GRIN_FIND_VERSION}")
else ()
  set(Grin_SEARCH "grin")
endif ()

#
#  Define what to search for
#
set(Grin_MESSAGE_SEARCH "grin")
if (NOT DEFINED Grin_SEARCH_HEADERS)
  set(Grin_SEARCH_HEADERS "datatype.h;GrExprParser.h;hyper2f1.h;gamma.h;grfblk;lapack_names.h;GrBasisFunctions.h;GrGaussQuadrature.h;MvConst.h;GrCmdLineArgParser.h;GrGreenFunctions.h;MvCubspline.h;GrContourGeometry.h;GrHDF5.h;MvFunctors.h;GrContourGeometryReader.h;GrInfinityVacuumMatrix.h;MvMatrix.h;GrContourMatrices.h;GrResponseMatrixLaplaceN.h;MvSolve.h;GrContourSolver.h;GrSegment.h;MvVector.h;GrContourTopology.h;GrTypes.h;param1;GrDiracLaplaceNField.h;GrVacuumMatrices.h;psi.h;greenp.h;GrWall.h;GrExprAdaptor.h;GrXYReader.h;")
endif ()
if (NOT DEFINED Grin_SEARCH_LIBS)
  set(Grin_SEARCH_LIBS "${GRIN_LIB_PREFIX}4m3d.${GRIN_LIB_SUFFIX};${GRIN_LIB_PREFIX}grin.${GRIN_LIB_SUFFIX};${GRIN_LIB_PREFIX}mv.${GRIN_LIB_SUFFIX};${GRIN_LIB_PREFIX}pppack.${GRIN_LIB_SUFFIX};${GRIN_LIB_PREFIX}specfun.${GRIN_LIB_SUFFIX};${GRIN_LIB_PREFIX}vacuum.${GRIN_LIB_SUFFIX}")
endif ()

SciFindPackage(PACKAGE "Grin"
              INSTALL_DIR ${Grin_SEARCH}
              HEADERS  ${Grin_SEARCH_HEADERS}
              LIBRARIES ${Grin_SEARCH_LIBS}
              )

if (GRIN_FOUND)
  message(STATUS "Found ${Grin_MESSAGE_SEARCH}")
  set(HAVE_GRIN 1 CACHE BOOL "Whether have the ${Grin_MESSAGE_SEARCH} library")
else ()
  message(STATUS "Did not find ${Grin_MESSAGE_SEARCH}. Use -DGRIN_DIR to specify the installation directory.")
  if (SciGrin_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

