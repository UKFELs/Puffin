enable_language(Fortran)
set (C_SOURCES "AIO.c")
#set (F_SOURCES 
#AFFTW_Constants.f90 EArrayFunctions.f90 IelectronGrid.f90 EDerivGlobals.f90
#Istiffness.f90 Aparatype.f90 Erandom.f90  Jresume.f90 BCIOWrapper.f90
#EreadDists.f90 Jrhs.f90 BFileType.f90 Fdatawrite.f90 Jsetupcalcs.f90
#Bnrutil.f90 Ferror_fn.f90 JsetupTrans.f90 BParallelInfoType.f90
#Ffunctions.f90 KDerivative.f90 BsddsWriter.f90 FgToP2.f90 Kmasks.f90
#BTransformInfoType.f90 FparBeam.f90 Lattice.f90 BtypesAndConstants.f90
#FreadData.f90 Ltransforms.f90 CinitConds.f90 GenMacros.f90 MRK4.f90
#CIO.f90 Gextra.f90 Msetup.f90 GMPfDists.f90 newGM.f90 GparticleFunctions.f90
#CremLow.f90 Hbasis_fn.f90 noise.f90 Dchecks.f90 Hgrids.f90
#DParallelSetUp.f90 HinitVars.f90 NMain.f90
#)
FILE(GLOB F_SOURCES *.f90)
#set (F_SOURCES *.f90)
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
