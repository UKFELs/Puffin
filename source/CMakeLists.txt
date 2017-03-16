enable_language(Fortran)
set (C_SOURCES "AIO.c")
FILE(GLOB F_SOURCES *.f90 ${PROJECT_BINARY_DIR}/source/puffProvenance.f90)

#
# USE_MKL to be set by user, but actually a flag not just for MKL but for other openMP options
# Should probably break into USE_MKL and USE_OPENMP
# Also need to figure out if we gain from ilp64 lib instead of lp64, (also would be ilp64 blacs...)
#

if (USE_MKL)
  if (USE_OPENMP)
    set (MKL_LIBS mkl_intel_lp64 mkl_core mkl_intel_thread pthread)
  else()
    set (MKL_LIBS mkl_intel_lp64 mkl_core mkl_sequential )
  endif()
#  set (MKL_LIBRARY_DIRS ${MKLROOT}/lib/intel64)
  set (MKL_LIBRARY_DIRS ${Mkl_LIBRARY_DIRS})
  set (MKL_INCLUDE_DIRS ${MKLROOT}/include)
endif()

# todo, make finding of FFTW depend on scimake, or before that be user-settable.

# Don't need all the libraries, so just use this.
set (FFTW_LIBS fftw3_mpi fftw3)
set (BASE_LINK_LIBS dl m)
#add_dependencies() seems not to be needed at this point

if (USE_MKL)
  include_directories(${MKL_INCLUDE_DIRS} ${MPI_Fortran_INCLUDE_PATH})
  link_directories(${MKL_LIBRARY_DIRS} ${MPI_Fortran_LIBRARIES})
else()
  include_directories(${Fftw3_INCLUDE_DIRS} ${Hdf5_INCLUDE_DIRS} ${MPI_Fortran_INCLUDE_PATH})
  link_directories(${Fftw3_LIBRARY_DIRS} ${Hdf5_INCLUDE_DIRS} ${MPI_Fortran_LIBRARIES})
endif ()


if (USE_MKL)
add_definitions(${MPI_C_COMPILE_FLAGS} ${OpenMP_C_FLAGS})
else()
add_definitions(${MPI_C_COMPILE_FLAGS} ${OpenMP_C_FLAGS})
endif()

#  add_definitions(${MPI_C_COMPILE_FLAGS} "-DMKL_ILP64")

add_library(AIO ${C_SOURCES})

#C definitions are not needed for Fortran apps.
#remove_definitions(${MPI_C_COMPILE_FLAGS} ${OpenMP_C_FLAGS})


if (USE_MKL)
  add_definitions(${OpenMP_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS})
else()
  add_definitions(${OpenMP_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS})
endif()

add_executable(puffin ${F_SOURCES})
add_dependencies(puffin AIO)

if (USE_MKL)
  target_link_libraries(puffin AIO ${MKL_LIBS} ${MPI_Fortran_LIBRARIES} ${BASE_LINK_LIBS})
else()
  target_link_libraries(puffin AIO ${FFTW_LIBS} ${Hdf5_LIBRARIES} ${MPI_Fortran_LIBRARIES} ${BASE_LINK_LIBS})
endif()

install (TARGETS puffin DESTINATION bin)
