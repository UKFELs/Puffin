# - FindSciMpi: This module looks for MPI first as being in the compilers, i.e.,
# MPI compiler wrappers, by trying to compile sample code.  It then
# looks for MPI using the standard CMake module, unless the compilers
# have the Cray names, in which case we know that this will fail.
#
# The following variables are set:
#
# SCI_HAVE_MPICXX_COMPILER_WRAPPER: TRUE if the C++ compiler automatically
#   includes and links to MPI.  If this is named 'CC', then no further MPI
#   searching is done.
# SCI_HAVE_MPIFC_COMPILER_WRAPPER: TRUE if the Fortran compiler automatically
#   includes and links to MPI.  If this is named 'ftn', then no further
#   MPI searching is done.
# SCIMPI_FOUND is set to TRUE if either of the above is true.
#
# In the case where we further search using the standard MPI module,
# and that is successfule, the following additional variables are set:
#
# MPI_INCLUDE_DIRS: the directories containing the C/C++ MPI header files
# MPI_MODULE_DIRS:  the directories containing the Fortran module files
#                   and the Fortran include files
# MPI_LIBRARY_DIRS: the directories containing the MPI libraries
# MPI_PROGRAMS:  mpiexec

######################################################################
#
# FindSciMpi: check whether the compiler wraps MPI, if not, find MPI
#
# $Id: FindSciMpi.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

set(SEARCH_FOR_MPI TRUE)
set(HAVE_MPI 0 CACHE BOOL "Whether have MPI")

# Determine whether mpi is already in the C++ compiler
try_compile(SCI_HAVE_MPICXX_COMPILER_WRAPPER ${PROJECT_BINARY_DIR}/scimake
  ${SCIMAKE_DIR}/trycompile/mpi_h.cxx)
if (SCI_HAVE_MPICXX_COMPILER_WRAPPER)
  message(STATUS "Using C/C++ compiler wrappers.")
# If so, set that to the compiler
  set(MPI_C_COMPILER ${CMAKE_C_COMPILER})
  set(MPI_CXX_COMPILER ${CMAKE_CXX_COMPILER})
  set(SCIMPI_FOUND TRUE)
# Search for MPI libraries not possible with Cray compiler wrappers
  if (${CMAKE_CXX_COMPILER} MATCHES "CC$")
    set(SEARCH_FOR_MPI FALSE)
  endif ()
endif ()

# If have fortran determine whether mpi is already in the Fortran compiler
if (CMAKE_Fortran_COMPILER_WORKS)
  try_compile(SCI_HAVE_MPIFC_COMPILER_WRAPPER ${PROJECT_BINARY_DIR}/scimake
    ${SCIMAKE_DIR}/trycompile/mpi_mod.f90)
  if (SCI_HAVE_MPIFC_COMPILER_WRAPPER)
    message(STATUS "Using Fortran compiler wrapper.")
    set(MPI_Fortran_COMPILER ${CMAKE_Fortran_COMPILER})
    set(SCIMPI_FOUND TRUE)
  endif ()
# Search for MPI libraries not possible with Cray compiler wrappers
  if (${CMAKE_Fortran_COMPILER} MATCHES "ftn$")
    set(SEARCH_FOR_MPI FALSE)
  endif ()
endif ()

# If building parallel Fortran on 64 bit Windows
# and using the MinGW compiler(s), as instructed on
# https://ice.txcorp.com/trac/bilder/wiki/InstallMinGW, the
# following sets the mpi variables directly because
# find_package(MPI REQUIRED) gets this wrong.
if (WIN32 AND "$ENV{PROCESSOR_ARCHITECTURE}" STREQUAL "AMD64" AND
  ("${CMAKE_Fortran_COMPILER}" STREQUAL "C:/MinGW/bin/mingw32-gfortran.exe"))
  message(STATUS "Using mingw32-gfortran.exe and linking to the MPI libraries distributed with HPC (To get the MPI libraries to link correctly, follow the instructions on https://ice.txcorp.com/trac/bilder/wiki/InstallMinGW exactly).")
  set(MPI_Fortran C:/winsame/contrib-mingw/microsoft-hpc-mingw/lib/amd64/msmpi.lib)
  if (${CMAKE_C_COMPILER} MATCHES "C:/MinGW/bin/mingw32-gcc.exe")
    message(STATUS "Using mingw32-gcc and linking to the MPI libraries distributed with HPC.")
    set(MPI_C C:/winsame/contrib-mingw/microsoft-hpc-mingw/lib/amd64/msmpi.lib)
  else ()
    if (${CMAKE_C_COMPILER} STREQUAL "")
      message(STATUS "No C compiler set.")
    else ()
      message(STATUS "WARNING: CMAKE_C_COMPILER is set to ${CMAKE_C_COMPILER}.  When setting CMAKE_Fortran_COMPILER to C:/MinGW/bin/mingw32-gfortran.exe for parallel Fortran builds with mixed C on 64 bit Windows, set CMAKE_C_COMPILER to C:/MinGW/bin/mingw32-gcc and follow the instructions on https://ice.txcorp.com/trac/bilder/wiki/InstallMinGW exactly to ensure the MPI libraries distributed with HPC get linked correctly.")
    endif ()
  endif ()
  if (${CMAKE_CXX_COMPILER} MATCHES "C:/MinGW/bin/mingw32-g\\+\\+.exe")
    message(STATUS "Using mingw32-g++ and linking to the MPI libraries distributed with HPC.")
    set(MPI_CXX C:/winsame/contrib-mingw/microsoft-hpc-mingw/lib/amd64/msmpi.lib)
  else ()
    if (${CMAKE_CXX_COMPILER} STREQUAL "")
      message(STATUS "No C++ compiler set.")
    else ()
      message(STATUS "WARNING: CMAKE_CXX_COMPILER is set to ${CMAKE_CXX_COMPILER}.  When setting CMAKE_Fortran_COMPILER to C:/MinGW/bin/mingw32-gfortran.exe for parallel Fortran builds with mixed C++ on 64 bit Windows, set CMAKE_CXX_COMPILER to C:/MinGW/bin/mingw32-g++ and follow the instructions on https://ice.txcorp.com/trac/bilder/wiki/InstallMinGW exactly to ensure the MPI libraries distributed with HPC get linked correctly.")
    endif ()
  endif ()
  set(MPI_PROGRAMS C:/winsame/contrib-mingw/microsoft-hpc-mingw/Bin/mpiexec.exe)
  set(MPI_INCLUDE_DIRS C:/winsame/contrib-mingw/microsoft-hpc-mingw/include)
  set(MPI_MODULE_DIRS C:/winsame/contrib-mingw/microsoft-hpc-mingw/include)
  set(MPI_LIBRARIES C:/winsame/contrib-mingw/microsoft-hpc-mingw/Lib/amd64/msmpifec.lib;
      C:/winsame/contrib-mingw/microsoft-hpc-mingw/lib/amd64/msmpi.lib)
  set(MPI_STLIBS C:/winsame/contrib-mingw/microsoft-hpc-mingw/Lib/amd64/msmpifec.lib;
      C:/winsame/contrib-mingw/microsoft-hpc-mingw/lib/amd64/msmpi.lib)
  set(MPI_DLLS C:/winsame/contrib-mingw/microsoft-hpc-mingw/Lib/amd64/msmpi.dll)
  set(MPI_LINK_FLAGS -L/winsame/contrib-mingw/microsoft-hpc-mingw/Lib/amd64 -lmsmpifec -lmsmpi)
  set(SCIMPI_FOUND TRUE)
  set(SEARCH_FOR_MPI FALSE)
  message(STATUS "Enabling MPI")
  SciPrintCMakeResults("MPI")
  SciPrintVar(MPI_LINK_FLAGS)
endif ()

# Pass down the required variable.  This has file name capitalization.
if (SEARCH_FOR_MPI)
  if (SciMpi_FIND_REQUIRED)
    set(mpireq REQUIRED)
  else ()
    set(mpireq)
  endif ()
  find_package(MPI ${mpireq})
endif ()

# If know more than compiler wrappers, pull out standard values
set(MPI_IS_OPEN_MPI FALSE)
if (MPI_FOUND)
  # Fix up problems with the stock find_package(MPI)
  if (NOT MPI_INCLUDE_DIRS OR NOT MPI_LIBRARIES OR NOT MPIEXEC)
    if (${CMAKE_C_COMPILER_ID} MATCHES "Intel")
      execute_process(
        COMMAND ${CMAKE_C_COMPILER} -show
        OUTPUT_VARIABLE mpiiccOutput
      )
      string(REPLACE " " ";" mpiiccOutputList "${mpiiccOutput}")
      foreach (arg ${mpiiccOutputList})
        if (${arg} MATCHES "^-I")
          string(REGEX REPLACE "^-I" "" idir ${arg})
          list(APPEND MPI_INCLUDE_DIRS ${idir})
        elseif (${arg} MATCHES "^-L")
          string(REGEX REPLACE "^-L" "" ldir ${arg})
          list(APPEND MPI_LIBRARY_DIRS ${ldir})
        elseif (${arg} MATCHES "^-l")
          string(REGEX REPLACE "^-l" "" lib ${arg})
          list(APPEND MPI_LIBRARIES ${lib})
        endif ()
      endforeach ()
      # Try to find both static and dynamic
      set(MPI_DYLIBS)
      if (WIN32)
        set(libsuffix "lib")
      else ()
        set(libsuffix "a")
      endif ()
      foreach (flib ${MPI_LIBRARIES})
        set(libfile "lib${flib}.${libsuffix}")
        find_file(libst ${libfile}
          PATHS ${MPI_LIBRARY_DIRS} NO_DEFAULT_PATH)
        find_library(libdyn ${flib} lib${flib}
          PATHS ${MPI_LIBRARY_DIRS} NO_DEFAULT_PATH)
        list(APPEND MPI_DYLIBS ${libdyn})
        list(APPEND MPI_STLIBS ${libst})
      endforeach ()
    endif ()
    set(MPI_LIBRARY ${MPI_DYLIBS})
  endif ()

# Fix the variables
  set(MPI_INCLUDE_DIRS ${MPI_INCLUDE_PATH})

# Get the library dirs.
# Assuming that finding fortran libs later will not affect this.
  set(MPI_LIBRARY_DIRS)
  foreach (lib ${MPI_LIBRARIES})
    get_filename_component(dir ${lib}/.. REALPATH)
    set(MPI_LIBRARY_DIRS ${MPI_LIBRARY_DIRS} ${dir})
  endforeach ()
  if (MPI_LIBRARY_DIRS)
    list(REMOVE_DUPLICATES MPI_LIBRARY_DIRS)
  endif ()

# If fortran libraries not found, look for them in the MPI_DIRS by names
  if (CMAKE_Fortran_COMPILER_WORKS)
# Find MPI gets this wrong on windows
    if (NOT MPI_Fortran_LIBRARIES)
      if ("${MPI_Fortran_LIBRARIES}" STREQUAL "${MPI_C_LIBRARIES}")
        foreach (libname mpi_f90 mpi_f77 msmpifec)
          find_library(MPI_${libname} ${libname} ${MPI_LIBRARY_DIRS})
          if (MPI_${libname})
            set(MPI_Fortran_LIBRARIES ${MPI_Fortran_LIBRARIES} ${MPI_${libname}})
          endif ()
        endforeach ()
# Fortran depends on C
        set(MPI_Fortran_LIBRARIES ${MPI_Fortran_LIBRARIES} ${MPI_C_LIBRARIES})
      endif ()
    endif ()
  endif ()

# Get master library list, removing duplicates
  set(mpilibs ${MPI_LIBRARIES})
  if (MPI_Fortran_LIBRARIES)
    message(STATUS "Adding fortran mpi libraries, ${MPI_Fortran_LIBRARIES}, to mpi libraries.")
    set(mpilibs ${MPI_Fortran_LIBRARIES} ${mpilibs})
  endif ()
  if (mpilibs)
    list(REVERSE mpilibs)
    list(REMOVE_DUPLICATES mpilibs)
    list(REVERSE mpilibs)
# Strip any system libs off and put final result into NAMES and LIBRARIES
    set(MPI_LIBRARIES)
    set(MPI_LIBRARY_NAMES)
    foreach (lib ${mpilibs})
      if (${lib} MATCHES "/libdl\\." OR ${lib} MATCHES "/libnsl\\." OR ${lib} MATCHES "/libutil\\." OR ${lib} MATCHES "/libm\\.")
      else ()
        set(MPI_LIBRARIES ${MPI_LIBRARIES} ${lib})
        get_filename_component(libname ${lib} NAME_WE)
        string(REGEX REPLACE "^lib" "" libname ${libname})
        set(MPI_LIBRARY_NAMES ${MPI_LIBRARY_NAMES} ${libname})
      endif ()
    endforeach ()
# Get static libs
    # message(STATUS "Getting static libraries for ${MPI_LIBRARIES}")
    SciGetStaticLibs("${MPI_LIBRARIES}" MPI_STLIBS)
  endif ()

# Assume only one include dir
  if (MPI_DIR AND MPI_INCLUDE_DIRS)
    get_filename_component(MPI_DIR ${MPI_INCLUDE_DIRS}/.. REALPATH)
  endif ()

# Get module includes
  set(MPI_MODULE_DIRS ${MPI_Fortran_INCLUDE_PATH})

# Get the executables
  get_filename_component(MPI_PROGRAMS ${MPIEXEC} REALPATH)

# set the root directory variable
  get_filename_component(MPI_ROOT_DIR ${MPIEXEC}/../.. REALPATH)

# determine if openmpi
  string(FIND "${MPI_ROOT_DIR}" "openmpi"  OPENMPI_SUBSTR_LOC)
  if (OPENMPI_SUBSTR_LOC GREATER "-1")
    set(MPI_IS_OPEN_MPI TRUE)
    message(STATUS "Found open source message passing interface (OpenMpi).")
  endif ()

# MPI link for flags
# The string strip line is needed because cmake
# doesn't allow for leading/trailing whitespace.
#
# Kludge for strip call
  if (MPI_LINK_FLAGS)
    string(STRIP "${MPI_LINK_FLAGS}" MPI_LINK_FLAGS)
  endif ()

  message(STATUS "Enabling MPI")
  SciPrintCMakeResults("MPI")
  SciPrintVar(MPI_LINK_FLAGS)

# Pass up the found variable.  This is all caps.
  set(SCIMPI_FOUND TRUE)

endif ()

# Either gives MPI
if (MPI_FOUND OR SCIMPI_FOUND)
  set(HAVE_MPI 1 CACHE BOOL "Whether MPI was found" FORCE)
endif ()

if (NOT SCIMPI_FOUND)
  if (SciMpi_FIND_REQUIRED)
    message(FATAL_ERROR "MPI required but not found.")
  else ()
    message(STATUS "MPI not enabled.")
  endif ()
endif ()

