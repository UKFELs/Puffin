enable_language(Fortran)
set (C_SOURCES "AIO.c")

FILE(GLOB F_SOURCES *.f90)

# todo, make finding of FFTW depend on scimake, or before that be user-settable.
# eg see if user has set it, and if not use this...
#set (FFTW_Root_PATH "/gpfs/home/HCP084/bwm06/shared/contrib-sys-gcc491/fftw-2.1.5-par")
set (FFTW_INCLUDE_PATH ${FFTW_Root_PATH}/include)
#set (FFTW_LIB_DIR ${FFTW_Root_PATH}/lib)
set (FFTW_LIBS fftw_mpi fftw)
set (BASE_LINK_LIBS m)
#add_dependencies() seems not to be needed at this point
include_directories(${FFTW_INC_DIR} ${MPI_Fortran_INCLUDE_PATH})

link_directories(${FFTW_LIB_DIR} ${MPI_Fortran_LIBRARIES})
add_definitions(${MPI_C_COMPILE_FLAGS})
add_library(AIO ${C_SOURCES})
add_definitions(${MPI_Fortran_COMPILE_FLAGS})
add_executable(puffin ${F_SOURCES})
add_dependencies(puffin AIO)
target_link_libraries(puffin AIO ${FFTW_LIBS} ${MPI_Fortran_LIBRARIES} ${BASE_LINK_LIBS})
install (TARGETS puffin DESTINATION bin)
