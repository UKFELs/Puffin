# FindFFTW3
# ---------
#
# Find the FFTW3 library
#
# Result Variables
# ^^^^^^^^^^^^^^^^
#
# ``FFTW3_FOUND``
#   True if FFTW3 was found.
# ``FFTW3_INCLUDE_DIRS``
#   Where to find fftw3.h, etc.
# ``FFTW3_LIBRARIES``
#   List of libraries when using FFTW3.
#
# Hints
# ^^^^^
#
# Set ``FFTW3_ROOT`` to the root directory of the FFTW3 installation.

find_path(FFTW3_INCLUDE_DIR
  NAMES fftw3.h
  HINTS ${FFTW3_ROOT}
  PATH_SUFFIXES include
)

find_library(FFTW3_LIBRARY
  NAMES fftw3
  HINTS ${FFTW3_ROOT}
  PATH_SUFFIXES lib
)

find_library(FFTW3_MPI_LIBRARY
  NAMES fftw3_mpi
  HINTS ${FFTW3_ROOT}
  PATH_SUFFIXES lib
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FFTW3
  REQUIRED_VARS FFTW3_LIBRARY FFTW3_INCLUDE_DIR
  HANDLE_COMPONENTS
)

if(FFTW3_FOUND)
  set(FFTW3_INCLUDE_DIRS ${FFTW3_INCLUDE_DIR})
  set(FFTW3_LIBRARIES ${FFTW3_LIBRARY})
  if(FFTW3_MPI_LIBRARY)
    list(APPEND FFTW3_LIBRARIES ${FFTW3_MPI_LIBRARY})
  endif()
endif()

mark_as_advanced(FFTW3_INCLUDE_DIR FFTW3_LIBRARY FFTW3_MPI_LIBRARY)
