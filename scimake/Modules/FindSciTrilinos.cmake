# - FindSciTrilinos: Module to find include directories and
#   libraries for Trilinos.
#
# Module usage:
#   find_package(SciTrilinos ...)
#
# If USE_TRILINOS_CONFIG_CMAKE is TRUE, use the supra-search-path
# to find TrilinosConfig.cmake, include it, then translate to our
# variable conventions:
#
# Otherwise use SciFindPackage.
#
# In either case, the following variables get defined.
#
# Trilinos_DIR          : root installation directory for Trilinos
# Trilinos_PROGRAMS     : any Trilinos executables (empty for trilinos)
# Trilinos_FILES        : any other files (empty for trilinos)
# Trilinos_INCLUDE_DIRS : include directories needed for compiling C/C++
# Trilinos_MODULE_DIRS  : include directories needed for compiling fortran
# Trilinos_LIBFLAGS     : any flags (except rpath flags) needed for linking the
#                         trilinos libraries
# Trilinos_LIBRARY_DIRS : the directories containing the libraries
# Trilinos_LIBRARY_NAMES: the base library names, no extensions, no prefix
# Trilinos_LIBRARIES    : the full paths to the libraries
# Trilinos_STLIBS       : the full paths to the static libs if found, otherwise
#                         the same as in the above variable
# Trilinos_TPL_LIBRARIES: third-party libraries needed for linking to Trilinos
# Trilinos_SLU_LIBRARIES: libraries specific to SuperLU and SuperLU_Dist
#
# and separate those into
#
#  Trilinos_LINALG_LIBRARIES: the blas and lapack libraries used by Trilinos
#  Trilinos_SYSTEM_LIBRARIES: any other system libraries needed to link Trilinos

######################################################################
#
# FindSciTrilinos: find includes and libraries for Trilinos
#
# $Id: FindSciTrilinos.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

if (TRILINOS_BUILD)
 set(trilinosdir ${TRILINOS_BUILD})
else ()
if (ENABLE_PARALLEL)
 set(trilinosdir trilinos-parcomm)
else ()
 set(trilinosdir trilinos-sercomm)
endif ()
endif ()

# First get the config file
SciFindPackage(PACKAGE "Trilinos"
  INSTALL_DIR ${trilinosdir}
  FIND_CONFIG_FILE
  USE_CONFIG_FILE
  NOPRINT
  FIND_QUIETLY
)

# Cleanup and conform to scimake conventions
if (Trilinos_INCLUDE_DIRS)
  get_filename_component(Trilinos_INCLUDE_DIRS ${Trilinos_INCLUDE_DIRS} REALPATH)
endif ()
if (Trilinos_LIBRARY_DIRS)
  get_filename_component(Trilinos_LIBRARY_DIRS ${Trilinos_LIBRARY_DIRS} REALPATH)
endif ()
set(Trilinos_LIBRARY_NAMES ${Trilinos_LIBRARIES})
set(Trilinos_LIBRARIES)
foreach (trilib ${Trilinos_LIBRARY_NAMES})
  find_library(Trilinos_${trilib}_LIBRARY ${trilib}
    PATHS ${Trilinos_LIBRARY_DIRS}
    NO_DEFAULT_PATH
  )
  set(Trilinos_LIBRARIES ${Trilinos_LIBRARIES} ${Trilinos_${trilib}_LIBRARY})
endforeach ()
SciGetStaticLibs("${Trilinos_LIBRARIES}" Trilinos_STLIBS)

# Print results
SciPrintCMakeResults(Trilinos)

if (TRILINOS_FOUND)
# Should now have all standard variables plus Trilinos_TPL_LIBRARIES

# Some options
  option(HAVE_AMESOS "Trilinos Amesos Library" ON)
  option(HAVE_EPETRAEXT "Trilinos Epetraext library" ON)
  option(HAVE_TRILINOS "Trilinos libraries" ON)

# Remove duplicates
  if (Trilinos_TPL_LIBRARIES)
    list(REVERSE Trilinos_TPL_LIBRARIES)
    list(REMOVE_DUPLICATES Trilinos_TPL_LIBRARIES)
    list(REVERSE Trilinos_TPL_LIBRARIES)
  endif ()

# Separate the third-party libraries into various groups, as only
# some needed for linking
  set(Trilinos_LINALG_LIBRARIES)
  set(Trilinos_MPI_LIBRARIES)
  set(Trilinos_SLU_LIBRARIES)
  set(Trilinos_MUMPS_LIBRARIES)
  set(Trilinos_SYSTEM_LIBRARIES)
  set(Trilinos_WRAPPER_LIBRARIES)
  set(Trilinos_USE_VENDOR_LINALG)
  foreach (lib ${Trilinos_TPL_LIBRARIES})
    get_filename_component(libname ${lib} NAME_WE)
    if (${libname} MATCHES "blas$" OR ${libname} MATCHES "lapack$" OR
        ${libname} MATCHES "acml$" OR ${libname} MATCHES "mkl" OR
        ${libname} MATCHES "f2c$" OR ${libname} MATCHES "atlas$")
      set(Trilinos_LINALG_LIBRARIES ${Trilinos_LINALG_LIBRARIES} ${lib})
      list(REMOVE_ITEM Trilinos_TPL_LIBRARIES ${lib})
      if (${libname} MATCHES "mkl")
        set(Trilinos_USE_VENDOR_LINALG "mkl")
      endif()
      #set(Trilinos_USE_VENDOR_LINALG ${Trilinos_USE_VENDOR_LINALG} PARENT_SCOPE)
# Cray wrappers include these, but needed for serial build.
    elseif (${libname} MATCHES "sci_pgi" OR ${libname} MATCHES "sci_gnu" OR
        ${libname} MATCHES "sci_intel")
      set(Trilinos_LINALG_LIBRARIES ${Trilinos_LINALG_LIBRARIES} ${lib})
    elseif (${libname} MATCHES "superlu$" OR ${libname} MATCHES "superlu_dist$")
      set(Trilinos_SLU_LIBRARIES ${Trilinos_SLU_LIBRARIES} ${lib})
    elseif (${libname} MATCHES "HYPRE$")
      set(Trilinos_HYPRE_LIBRARIES ${Trilinos_HYPRE_LIBRARIES} ${lib})
    elseif (${libname} MATCHES ".*mumps.*" OR ${libname} MATCHES "libpord")
      set(Trilinos_MUMPS_LIBRARIES ${Trilinos_MUMPS_LIBRARIES} ${lib})
    elseif (${libname} MATCHES "libseq")
      set(Trilinos_MUMPS_LIBRARIES ${Trilinos_MUMPS_LIBRARIES} ${lib})
    elseif (${libname} MATCHES "msmpi$")
      set(Trilinos_MPI_LIBRARIES ${Trilinos_MPI_LIBRARIES} ${lib})
    else ()
      set(Trilinos_SYSTEM_LIBRARIES ${Trilinos_SYSTEM_LIBRARIES} ${lib})
    endif ()
  endforeach ()
# If trilinos does not get the mumps scalapack, add it.
  if (Trilinos_MUMPS_LIBRARIES)
    list(GET Trilinos_MUMPS_LIBRARIES 0 mumpslib1)
    get_filename_component(mumpslibdir ${mumpslib1} PATH)
    list(FIND Trilinos_MUMPS_LIBRARIES ${mumpslibdir}/libscalapack.so idx)
    if ((${idx} EQUAL -1) AND (EXISTS ${mumpslibdir}/libscalapack.so))
      set(Trilinos_MUMPS_LIBRARIES ${Trilinos_MUMPS_LIBRARIES}
        ${mumpslibdir}/libscalapack.so
      )
    endif ()
  endif ()

# Make sure mp library present on Cray
  string(TOLOWER ${C_COMPILER_ID} cid)
  list(FIND Trilinos_LINALG_LIBRARIES sci_${cid} idx)
  if (${idx} EQUAL -1)
    message(STATUS "sci_${cid} not found in Trilinos_LINALG_LIBRARIES.")
  else ()
    message(STATUS "sci_${cid} found at index ${idx} in Trilinos_LINALG_LIBRARIES.")
    list(FIND Trilinos_LINALG_LIBRARIES sci_${cid}_mp jdx)
    if (${jdx} EQUAL -1)
      message(WARNING "Trilinos_LINALG_LIBRARIES contains sci_${cid} but not sci_${cid}_mp.  Appending.")
      list(APPEND Trilinos_LINALG_LIBRARIES sci_${cid}_mp)
    endif ()
  endif ()

# Find the libdirs of all groups
  foreach (grp TPL LINALG SLU HYPRE MUMPS MPI SYSTEM)
    set(libs ${Trilinos_${grp}_LIBRARIES})
    unset(Trilinos_${grp}_LIBRARY_DIRS)
    unset(Trilinos_${grp}_LIBRARY_NAMES)
    foreach (lib ${libs})
      get_filename_component(libdir ${lib}/.. REALPATH)
      list(APPEND Trilinos_${grp}_LIBRARY_DIRS ${libdir})
      get_filename_component(libname ${lib} NAME_WE)
      string(REGEX REPLACE "^lib" "" libname ${libname})
      list(APPEND Trilinos_${grp}_LIBRARY_NAMES ${libname})
    endforeach ()
    if (Trilinos_${grp}_LIBRARY_DIRS)
      list(REMOVE_DUPLICATES Trilinos_${grp}_LIBRARY_DIRS)
    endif ()
  endforeach ()

# For windows only, find f2c
  find_library(F2C_LIBRARY f2c PATHS ${Trilinos_LINALG_LIBRARY_DIRS} NO_DEFAULT_PATH)
  message(STATUS "F2C_LIBRARY = ${F2C_LIBRARY}.")
  if (${F2C_LIBRARY} MATCHES "/usr/lib")
    message (STATUS "System f2c library, ${F2C_LIBRARY}, found.  Unsetting.")
    set(F2C_LIBRARY "")
  endif ()
  message(STATUS "F2C_LIBRARY = ${F2C_LIBRARY}.")
  if (F2C_LIBRARY)
    message(STATUS "  F2C_LIBRARY = ${F2C_LIBRARY}.")
# In this case, we need the clapack_cmake blas as well
    get_filename_component(libdir ${F2C_LIBRARY}/.. REALPATH)
    find_library(BLAS_LIBRARY blas PATHS ${libdir} NO_DEFAULT_PATH)
    if (BLAS_LIBRARY)
      message(STATUS "  BLAS_LIBRARY found.")
      set(Trilinos_TPL_LIBRARIES ${Trilinos_TPL_LIBRARIES} ${BLAS_LIBRARY})
      set(Trilinos_LINALG_LIBRARIES ${Trilinos_LINALG_LIBRARIES}
          ${BLAS_LIBRARY})
    else ()
      message(STATUS "  BLAS_LIBRARY not found.")
    endif ()
    set(Trilinos_TPL_LIBRARIES ${Trilinos_TPL_LIBRARIES} ${F2C_LIBRARY})
    set(Trilinos_LINALG_LIBRARIES ${Trilinos_LINALG_LIBRARIES} ${F2C_LIBRARY})
  else ()
    message(STATUS "  F2C_LIBRARY not found.")
  endif ()

# IF Trilinos_LINALG_LIBRARY_NAMES contains blas but not lapack, warn.
  list(FIND Trilinos_LINALG_LIBRARY_NAMES blas indx)
  if (NOT ${indx} EQUAL -1)
    list(FIND Trilinos_LINALG_LIBRARY_NAMES lapack indx)
    if (${indx} EQUAL -1)
      message(WARNING "Trilinos_LINALG_LIBRARY_NAMES contains blas but not lapack.  Fixing.")
    endif ()
  else ()
    message(WARNING "Trilinos_LINALG_LIBRARY_NAMES does not contain blas.")
  endif ()

# Get static libs and print
  foreach (grp TPL LINALG MPI SLU HYPRE MUMPS SYSTEM WRAPPER)
    SciGetStaticLibs("${Trilinos_${grp}_LIBRARIES}" Trilinos_${grp}_STLIBS)
    SciPrintVar(Trilinos_${grp}_LIBRARY_DIRS)
    SciPrintVar(Trilinos_${grp}_LIBRARY_NAMES)
    SciPrintVar(Trilinos_${grp}_LIBRARIES)
    SciPrintVar(Trilinos_${grp}_STLIBS)
  endforeach ()

else ()

  message(STATUS "Did not find Trilinos. Use -DTrilinos_ROOT_DIR to specify the installation directory.")

endif ()

