######################################################################
#
# SciFortranChecks: check various Fortran capabilities
#
# $Id: SciFortranChecks.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
# THIS FILE NEEDS ALOT OF WORK.  JUST STARTING WITH GNU.
#
#
######################################################################

include(${SCIMAKE_DIR}/SciFortranFindVersion.cmake)

# Set the lib subdir from the Compiler ID and version
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL Cray)
  string(REGEX REPLACE "\\.[0-9]+-.*$" "" Fortran_MAJOR_VERSION ${Fortran_VERSION})
  set(Fortran_COMP_LIB_SUBDIR cray${Fortran_MAJOR_VERSION})
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(FC_MOD_FLAGS "-emf")
  set(FC_DOUBLE_FLAGS "-s real64")
elseif ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL GNU)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}")
  if (NOT USING_MINGW)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -pipe")
    set(CMAKE_Fortran_FLAGS_RELEASE "-fPIC -O3")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-fPIC -O0 -g -Wp,-DDEBUG")
  endif ()
  string(SUBSTRING ${Fortran_VERSION} 0 3 Fortran_MAJOR_VERSION)
  set(Fortran_COMP_LIB_SUBDIR gfortran${Fortran_MAJOR_VERSION})
  set(FC_DOUBLE_FLAGS "-fdefault-real-8 -fdefault-double-8")
elseif ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL Intel)
  string(REGEX REPLACE "\\.[0-9]+.*$" "" Fortran_MAJOR_VERSION ${Fortran_VERSION})
  set(Fortran_COMP_LIB_SUBDIR icpc${Fortran_MAJOR_VERSION})
  # CMake sets it to i_dynamic by default but that's debateable
  set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
  set(FC_DOUBLE_FLAGS "-autodouble")
elseif ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL PathScale)
  string(SUBSTRING ${Fortran_VERSION} 0 1 Fortran_MAJOR_VERSION)
  set(Fortran_COMP_LIB_SUBDIR path${Fortran_MAJOR_VERSION})
  set(FC_DOUBLE_FLAGS "-r8")
elseif ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL PGI)
  string(REGEX REPLACE "\\.[0-9]+-.*$" "" Fortran_MAJOR_VERSION ${Fortran_VERSION})
  set(Fortran_COMP_LIB_SUBDIR pgi${Fortran_MAJOR_VERSION})
# Compiler optimization flags set based on "ultra" optimization in
# flags.m4.  Overrides scimake default, since that had -Mipa=fast (no inline).
  set(CMAKE_Fortran_FLAGS_RELEASE
    "-fast -O3 -DNDEBUG -Munroll -Minline=levels:5 -Mipa=fast,inline -Mmovnt")
# For a fully-optimized build, set IPA options for linker too
  set(CMAKE_EXE_LINKER_FLAGS_RELEASE
    "${CMAKE_EXE_LINKER_FLAGS_RELEASE} -Mipa=fast,inline")
  set(FC_DOUBLE_FLAGS "-r8")
elseif ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL XL)
# This should be the basename of the compiler
  string(REGEX REPLACE "\\.[0-9]+.*$" "" Fortran_MAJOR_VERSION ${Fortran_VERSION})
  string(REGEX REPLACE "^0+" "" Fortran_MAJOR_VERSION ${Fortran_MAJOR_VERSION})
#SriV. want file name without extension.
  get_filename_component(REL_CMAKE_Fortran_COMPILER ${CMAKE_Fortran_COMPILER} NAME_WE)
# Since we install ben builds in a completely different directory, can
# use same name for Fortran_COMP_LIB_SUBDIR
  if (${REL_CMAKE_Fortran_COMPILER} MATCHES ".*_r$")
    set(Fortran_COMP_LIB_SUBDIR xlC_r${Fortran_MAJOR_VERSION})
  else ()
    set(Fortran_COMP_LIB_SUBDIR xlC${Fortran_MAJOR_VERSION})
  endif ()
  set(SEPARATE_INSTANTIATIONS 1 CACHE BOOL "Whether to separate instantiations -- for correct compilation on xl")
# Customize flags because cmake defaults are very poor
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -qnooptdebug -qreport -qmaxmem=-1")
  set(CMAKE_Fortran_FLAGS_FULL "${CMAKE_Fortran_FLAGS_RELEASE}")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -qnoopt -O0 -qcheck")
  set(FC_DOUBLE_FLAGS "-qautodbl=dbl4")
endif ()
if (SCI_FC_PROMOTE_REAL_TO_DOUBLE)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${FC_DOUBLE_FLAGS}")
endif ()
set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${FC_MOD_FLAGS}")
SciPrintString("  Fortran_COMP_LIB_SUBDIR = ${Fortran_COMP_LIB_SUBDIR}")

SciPrintString("")
SciPrintString("  CMake detected fortran implicit libraries:")
SciPrintVar(CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES)
SciPrintVar(CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES)

# The variables below will contain the libraries needed to add to
# a C++ linked executable when it links fortran compiled objects
set(Fortran_IMPLICIT_LIBRARY_DIRS "")
set(Fortran_IMPLICIT_LIBRARY_NAMES "")
set(Fortran_IMPLICIT_LIBRARIES "")
set(Fortran_IGNORED_LIBRARIES "")
foreach (scilib ${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES})

# Whether finished with this library
  set(libprocessed FALSE)

# Ignore mpi and io libraries
  if (${scilib} MATCHES "^mpich")
    if (DEBUG_CMAKE)
      message("${scilib} is an MPICH library.  Ignoring.")
    endif ()
    set(Fortran_IGNORED_LIBRARIES ${Fortran_IGNORED_LIBRARIES} ${scilib})
    set(libprocessed TRUE)
  elseif (${scilib} MATCHES "^open-" OR ${scilib} MATCHES "^mpi$" OR
        ${scilib} MATCHES "^mpi_usempi")
# mpi_mpifh needed for fortran messaging
    if (DEBUG_CMAKE)
      message("${scilib} is an OpenMPI library.  Ignoring.")
    endif ()
    set(Fortran_IGNORED_LIBRARIES ${Fortran_IGNORED_LIBRARIES} ${scilib})
    set(libprocessed TRUE)
  elseif (${scilib} MATCHES "^darshan-" OR ${scilib} MATCHES "^libmpi")
    if (DEBUG_CMAKE)
      message("${scilib} is an MPI library.  Ignoring.")
    endif ()
    set(Fortran_IGNORED_LIBRARIES ${Fortran_IGNORED_LIBRARIES} ${scilib})
    set(libprocessed TRUE)
  endif ()

# Ignore system libraries
  if (NOT libprocessed)
    foreach (lib pthread dl nsl util rt m c z)
      if (${scilib} STREQUAL ${lib})
        if (DEBUG_CMAKE)
          message("${scilib} is a system library.  Ignoring.")
        endif ()
        set(Fortran_IGNORED_LIBRARIES ${Fortran_IGNORED_LIBRARIES} ${scilib})
        set(libprocessed TRUE)
        break ()
      endif ()
    endforeach ()
  endif ()

# Ignore communication and queue libraries
  if (NOT libprocessed)
    foreach (lib rdmacm ibverbs torque)
      if (${scilib} STREQUAL ${lib})
        if (DEBUG_CMAKE)
          message("${scilib} is an infiniband or torque library.  Ignoring.")
        endif ()
        set(Fortran_IGNORED_LIBRARIES ${Fortran_IGNORED_LIBRARIES} ${scilib})
        set(libprocessed TRUE)
        break ()
      endif ()
    endforeach ()
  endif ()

# Pull out Hopper Cray wrapper libraries added by wrapper for any compiler
  if (NOT libprocessed)
    foreach (lib fftw3 fftw3f rca AtpSigHandler AtpSigHCommData mpl sma xpmem dmapp ugni pmi alpslli alpsutil udreg)
      if (${scilib} STREQUAL ${lib})
        if (DEBUG_CMAKE)
          message("${scilib} is a Hopper Cray pgi wrapper library.  Ignoring.")
        endif ()
        set(Fortran_IGNORED_LIBRARIES ${Fortran_IGNORED_LIBRARIES} ${scilib})
        set(libprocessed TRUE)
        break ()
      endif ()
    endforeach ()
  endif ()

# Pull out Hopper Cray gnu wrapper libraries that wrapper adds
  if ((${C_COMPILER_ID} STREQUAL "GNU") AND NOT libprocessed)
    foreach (lib scicpp_gnu sci_gnu_mp sci_gnu)
      if (${scilib} STREQUAL ${lib})
        if (DEBUG_CMAKE)
          message("${scilib} is a Hopper Cray gnu wrapper library.  Ignoring.")
        endif ()
        set(Fortran_IGNORED_LIBRARIES ${Fortran_IGNORED_LIBRARIES} ${scilib})
        set(libprocessed TRUE)
        break ()
      endif ()
    endforeach ()
  endif ()

# Pull out Hopper Cray pgi wrapper libraries that wrapper adds
  if ((${C_COMPILER_ID} STREQUAL "Intel") AND NOT libprocessed)
    foreach (lib scicpp_intel sci_intel_mp zceh svml ipgo intlc irc_s iomp5)
      if (${scilib} STREQUAL ${lib})
        if (DEBUG_CMAKE)
          message("${scilib} is a Hopper Cray pgi wrapper library.  Ignoring.")
        endif ()
        set(Fortran_IGNORED_LIBRARIES ${Fortran_IGNORED_LIBRARIES} ${scilib})
        set(libprocessed TRUE)
        break ()
      endif ()
    endforeach ()
  endif ()

# Pull out Hopper Cray pgi wrapper libraries that wrapper adds
  if ((${C_COMPILER_ID} STREQUAL "PGI") AND NOT libprocessed)
    foreach (lib scicpp_pgi sci_pgi_mp zceh stdmpz Cmpz pgmp nspgc pgc)
      if (${scilib} STREQUAL ${lib})
        if (DEBUG_CMAKE)
          message("${scilib} is a Hopper Cray pgi wrapper library.  Ignoring.")
        endif ()
        set(Fortran_IGNORED_LIBRARIES ${Fortran_IGNORED_LIBRARIES} ${scilib})
        set(libprocessed TRUE)
        break ()
      endif ()
    endforeach ()
  endif ()

# Ignore bgp libraries
  if (NOT libprocessed)
    foreach (lib opa dcmf.cnk dcmfcoll.cnk SPI.cna)
      if (${scilib} STREQUAL ${lib})
        if (DEBUG_CMAKE)
          message("${scilib} is a BGP compute node library.  Ignoring.")
        endif ()
        set(Fortran_IGNORED_LIBRARIES ${Fortran_IGNORED_LIBRARIES} ${scilib})
        set(libprocessed TRUE)
        break ()
      endif ()
    endforeach ()
  endif ()

# The remaining libs are added
  if (NOT libprocessed)
    set(scilibpathvar ${scilib}_LIBRARY)        # Cache variable
    # message("Looking for ${scilib}.")
    find_library(${scilibpathvar} ${scilib}
        PATHS ${CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES}
        NO_DEFAULT_PATH)
    set(scilibpath ${${scilibpathvar}})
    if (scilibpath)
      set(Fortran_IMPLICIT_LIBRARIES ${Fortran_IMPLICIT_LIBRARIES} ${scilibpath})
      set(Fortran_IMPLICIT_LIBRARY_NAMES ${Fortran_IMPLICIT_LIBRARY_NAMES} ${scilib})
      get_filename_component(scilibdir ${scilibpath}/.. REALPATH)
      set(Fortran_IMPLICIT_LIBRARY_DIRS ${Fortran_IMPLICIT_LIBRARY_DIRS} ${scilibdir})
    endif ()
  endif ()

endforeach ()
list(REMOVE_DUPLICATES Fortran_IMPLICIT_LIBRARIES)
list(REMOVE_DUPLICATES Fortran_IMPLICIT_LIBRARY_NAMES)
list(REMOVE_DUPLICATES Fortran_IMPLICIT_LIBRARY_DIRS)
SciGetStaticLibs("${Fortran_IMPLICIT_LIBRARIES}" Fortran_IMPLICIT_STLIBS)

if (Fortran_IMPLICIT_LIBFLAGS)
  string(STRIP ${Fortran_IMPLICIT_LIBFLAGS} Fortran_IMPLICIT_LIBFLAGS)
endif ()
SciPrintString("")
SciPrintString("  RESULTS FOR fortran implicit libraries:")
SciPrintVar(Fortran_IMPLICIT_LIBRARIES)
SciPrintVar(Fortran_IMPLICIT_LIBRARY_NAMES)
SciPrintVar(Fortran_IMPLICIT_LIBRARY_DIRS)
SciPrintVar(Fortran_IMPLICIT_STLIBS)
SciPrintVar(Fortran_IMPLICIT_LIBFLAGS)
SciPrintVar(Fortran_IGNORED_LIBRARIES)

# Set release flags.  Assume same for now.  If different, we will
# put in the if, elseif coding.
if ("${CMAKE_BUILD_TYPE}" STREQUAL RELEASE AND OPTIMIZATION)
  set(CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_C_FLAGS_RELEASE})
  SciPrintVar(CMAKE_Fortran_FLAGS_RELEASE)
endif ()

# Default macros for the fortran-C interface
set(FC_FUNC  "FC_FUNC(name,NAME) name ## _")
set(FC_FUNC_ "FC_FUNC_(name,NAME) name ## _")

#
# Detect name mangling convention used between Fortran and C
#
include(FortranCInterface)
FortranCInterface_HEADER(
  ${CMAKE_BINARY_DIR}/FCMangle.h
  MACRO_NAMESPACE "FC_"
  SYMBOL_NAMESPACE "FC_"
  SYMBOLS mysub mymod:my_sub
)

file(STRINGS ${CMAKE_BINARY_DIR}/FCMangle.h CONTENTS REGEX "FC_GLOBAL\\(.*,.*\\) +(.*)")
string(REGEX MATCH "FC_GLOBAL\\(.*,.*\\) +(.*)" RESULT "${CONTENTS}")
set(FC_FUNC "FC_FUNC(name,NAME) ${CMAKE_MATCH_1}")

file(STRINGS ${CMAKE_BINARY_DIR}/FCMangle.h CONTENTS REGEX "FC_GLOBAL_\\(.*,.*\\) +(.*)")
string(REGEX MATCH "FC_GLOBAL_\\(.*,.*\\) +(.*)" RESULT "${CONTENTS}")
set(FC_FUNC_ "FC_FUNC_(name,NAME) ${CMAKE_MATCH_1}")

#
# Detect module file name
#
message(STATUS "Compiling trycompile/modulesrcfile.f90.")
execute_process(
  COMMAND ${CMAKE_Fortran_COMPILER};${FC_MOD_FLAGS};-c;${SCIMAKE_DIR}/trycompile/modulesrcfile.f90;-o;modulesrcfile.o
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
set(SCI_FC_MODULENAME_CAPITALIZED FALSE)
set(SCI_FC_MODULE_SUFFIX)
if (EXISTS ${CMAKE_BINARY_DIR}/modulename.mod)
  set(SCI_FC_MODULE_SUFFIX mod)
  file(REMOVE ${CMAKE_BINARY_DIR}/modulename.mod)
elseif (EXISTS ${CMAKE_BINARY_DIR}/modulename.MOD)
  set(SCI_FC_MODULE_SUFFIX MOD)
  file(REMOVE ${CMAKE_BINARY_DIR}/modulename.MOD)
elseif (EXISTS ${CMAKE_BINARY_DIR}/MODULENAME.MOD)
  set(SCI_FC_MODULE_SUFFIX MOD)
  set(SCI_FC_MODULENAME_CAPITALIZED TRUE)
  file(REMOVE ${CMAKE_BINARY_DIR}/MODULENAME.MOD)
elseif (EXISTS ${CMAKE_BINARY_DIR}/MODULENAME.mod)
  set(SCI_FC_MODULE_SUFFIX mod)
  set(SCI_FC_MODULENAME_CAPITALIZED TRUE)
  file(REMOVE ${CMAKE_BINARY_DIR}/MODULENAME.mod)
endif ()
file(REMOVE ${CMAKE_BINARY_DIR}/modulesrcfile.o)

get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
SciPrintVar(Fortran_COMPILER_NAME)
SciPrintVar(SCI_FC_MODULENAME_CAPITALIZED)
SciPrintVar(SCI_FC_MODULE_SUFFIX)
set(CMAKE_Fortran_FLAGS_FULL ${CMAKE_Fortran_FLAGS_RELEASE})
SciPrintVar(CMAKE_Fortran_FLAGS_FULL)
SciPrintVar(CMAKE_Fortran_FLAGS_RELEASE)
SciPrintVar(CMAKE_Fortran_FLAGS_RELWITHDEBINFO)
SciPrintVar(CMAKE_Fortran_FLAGS_DEBUG)
SciPrintVar(CMAKE_Fortran_FLAGS)

#  This checks for the FortranC interface including the mangling
#  http://www.cmake.org/cmake/help/git-master/module/FortranCInterface.html
option(CHECK_FortranC_INTERFACE "Determine whether to determine interoperability" OFF)

if (CHECK_FortranC_INTERFACE)
  set(HAVE_F90_INTERFACE FALSE)
  if (CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    include(FortranCInterface)
    FortranCInterface_VERIFY(CXX)
    if (FortranCInterface_VERIFIED_CXX)
      set(HAVE_F90_INTERFACE TRUE)
      FortranCInterface_HEADER(${CMAKE_CURRENT_BINARY_DIR}/FCMangle.h
        MACRO_NAMESPACE "FC_")
    endif ()
  else ()
    message(STATUS "${CMAKE_Fortran_COMPILER} does not appear to support F90")
  endif ()
  SciPrintVar(FortranCInterface_GLOBAL_SYMBOLS)
  SciPrintVar(FortranCInterface_MODULE_SYMBOLS)
endif ()

SciPrintString("")

