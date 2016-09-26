!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE transforms

! This module contains subroutines called in Main.f90 dealing with
! fourier transforms, calculations in fourier space etc.

USE ParallelInfoType
USE TransformInfoType
!USE FFTW_Constants

USE Globals
USE IO
USE masks

use, intrinsic :: iso_c_binding
IMPLICIT NONE

!INCLUDE 'fftw3-mpi.f03'

real(kind=wp) :: tr_time_s, tr_time_e
type(C_PTR) :: cdata
complex(C_DOUBLE_COMPLEX), pointer :: Afftw(:,:,:)


!INCLUDE 'mpif.h'

CONTAINS

SUBROUTINE getTransformPlans4FEL(nnodes,qOK)

  IMPLICIT NONE

! This Subroutine calls the appropriate transform creation routines in
! FFTW depending on whether the field is in 1D approximation or not
!
!                 ARGUMENTS
!
! nnodes      number of field nodes in x,y,z2 respectively
! qOK         Logical value for error checking
!---------------------------------------------------

  INTEGER(KIND=IP), INTENT(IN) :: nnodes(3)
  LOGICAL,INTENT(OUT) :: qOK

!                 LOCAL ARGS
!
! qOKL - Local Logical error checker.

  LOGICAL  :: qOKL
  integer error

!                   Begin

  qOK = .FALSE.

!     Determine if 1D or 3D transform is needed
  IF(nnodes(iX_CG) == 1 .AND. nnodes(iY_CG) == 1) THEN
     tTransInfo_G%qOneD=.TRUE.
     !CALL getTransformPlans_OneD(nnodes(iZ2_CG),qOKL)
     qOKL = .true.
  ELSE
     tTransInfo_G%qOneD=.FALSE.
     CALL getTransformPlans_MultiD(nnodes,3,qOKL)
  END IF

  IF (.NOT. qOKL) GOTO 1000

  qOK = .TRUE.
  GOTO 2000

1000  CALL Error_log('Error in transforms:getTransformPlans4FEL',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE getTransformPlans4FEL

!*************************************************

SUBROUTINE getTransformPlans_MultiD(sizes,nDims,qOK)

  IMPLICIT NONE

! Subroutine to create multi-dimensional forward
! and backward FFTW-MPI plans for Fourier transforms
! of data of a specified size, and get the sizes of
! the local data distribution before and after the
! transform.
!
!                  ARGUMENTS
!
! sizes       IN   Array of size (3) describing the
!                  number of field nodes in x, y,
!                  and z2 respectively
! qOK         OUT  Error flag

  INTEGER, INTENT(IN) :: sizes(3)
  INTEGER, INTENT(IN) :: nDims
  LOGICAL, INTENT(OUT) :: qOK


  integer(C_INTPTR_T) :: L, M, N, local_N, local_j_offset, &
                         alloc_local


!                         Begin

  qOK = .FALSE.

  L = int(sizes(iX_CG), C_INTPTR_T)
  M = int(sizes(iY_CG), C_INTPTR_T)
  N = int(sizes(iZ2_CG), C_INTPTR_T)

!      Get the local sizes for the distribution of the transform data.


  alloc_local = fftw_mpi_local_size_3d(N, M, L, &
                                    tProcInfo_G%comm, &
                                    local_N, local_j_offset)


  tTransInfo_G%loc_nz2 = int(local_N, kind=ip)

  tTransInfo_G%loc_z2_start = &
                   int(local_j_offset, kind=ip)

  tTransInfo_G%total_local_size = &
                   int(alloc_local, kind=ip)


!     Allocate data

  cdata = fftw_alloc_complex(alloc_local)

!     Assign to fortran pointer

  call c_f_pointer(cdata, Afftw, [L,M,local_N])



!     Create plans

  tTransInfo_G%fplan = fftw_mpi_plan_dft_3d(N, M, L, &
                        Afftw, Afftw, tProcInfo_G%comm, &
                        FFTW_FORWARD, FFTW_MEASURE)


  tTransInfo_G%bplan = fftw_mpi_plan_dft_3d(N, M, L, &
                        Afftw, Afftw, tProcInfo_G%comm, &
                        FFTW_BACKWARD, FFTW_MEASURE)





!!!!!!!!!!!!!!!!     OOOLLLDDDD

!      Create the forward transform plan

!  CALL fftwnd_f77_mpi_create_plan(tTransInfo_G%fplan, &
!       tProcInfo_G%comm,   &
!       nDims,            &
!       sizes,            &
!       FFTW_FORWARD,     &
!       FFTW_MEASURE)

!  CALL fftwnd_f77_mpi_local_sizes(tTransInfo_G%fplan,&
!       tTransInfo_G%loc_nz2, &
!       tTransInfo_G%loc_z2_start, &
!       tTransInfo_G%loc_nz2_aft_trans, &
!       tTransInfo_G%loc_z2_start_aft_trans, &
!       tTransInfo_G%total_local_size)

!      Create the backward transform plan

!  CALL fftwnd_f77_mpi_create_plan(tTransInfo_G%bplan, &
!       tProcInfo_G%comm,   &
!       nDims,            &
!       sizes,            &
!       FFTW_BACKWARD,    &
!       FFTW_MEASURE)

!     Set error flag to true

  qOK = .TRUE.
  GOTO 2000

1000  CALL Error_log('Error in transforms:getTransformPlans_MultiD',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE getTransformPlans_MultiD
!******************************************************

! SUBROUTINE getTransformPlans_OneD(nn1D,qOK)
!
!   IMPLICIT NONE
! !
! ! Subroutine to create 1 dimensional forward and
! ! backward FFTW-MPI plans for Fourier transforms
! ! of data of a specified size, and get the sizes
! ! of the local data before and after the transform.
! !
! !            ARGUMENTS
! !
! ! nn1D        IN   Number of field nodes in z2
! ! qOK         OUT  Error flag; .false. if error occurs
!
!   INTEGER, INTENT(IN)   :: nn1D
!   LOGICAL, INTENT(OUT)  :: qOK
!
! !                         Begin
!
!   qOk = .FALSE.
!
! !           Create the forward transform plan
!
!   CALL fftw_f77_mpi_create_plan(tTransInfo_G%fplan, &
!        tProcInfo_G%comm,   &
!        nn1D,             &
!        FFTW_FORWARD,     &
!        FFTW_MEASURE)
!
! !      Get the local sizes for the distribution of the
! !                 transform data.
!
!   CALL fftw_f77_mpi_local_sizes(tTransInfo_G%fplan,&
!        tTransInfo_G%loc_nz2,                &
!        tTransInfo_G%loc_z2_start,           &
!        tTransInfo_G%loc_nz2_aft_trans,      &
!        tTransInfo_G%loc_z2_start_aft_trans, &
!        tTransInfo_G%total_local_size)
!
! !            Create the backward transform plan
!
!   CALL fftw_f77_mpi_create_plan(tTransInfo_G%bplan, &
!        tProcInfo_G%comm,   &
!        nn1D,             &
!        FFTW_BACKWARD,    &
!        FFTW_MEASURE)
!
! !            Set error flag to true and exit
!
!   qOK = .TRUE.
!   GOTO 2000
!
! 1000  CALL Error_log('Error in transforms:getTransformPlans_OneD',tErrorLog_G)
!
! 2000 CONTINUE
!
! END SUBROUTINE getTransformPlans_OneD

!*****************************************************

SUBROUTINE clearTransformPlans(qOK)

  IMPLICIT NONE
!
! This subroutine calls the appropriate routine to clear
! the FFTW-MPI plans, depending on whether the field is
! 1D or not
!
!                      ARGUMENTS
!
! qOK         OUT  Error flag; .false. if error occurs

  LOGICAL, INTENT(OUT) :: qOK

!                     LOCAL ARGS
!
! qOKL       Local error flag

  LOGICAL :: qOKL

!                      Begin

  qOK = .FALSE.

!     Identify 1D or 3D case and call appropriate subroutine

  IF (tTransInfo_G%qOneD) THEN
!     CALL clearTransformPlans_OneD(qOKL)
  ELSE
     CALL clearTransformPlans_ThreeD(qOKL)
  ENDIF

  IF (.NOT. qOKL) GOTO 1000

! Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:clearTransformPlans',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE clearTransformPlans

!******************************************************

! SUBROUTINE clearTransformPlans_OneD(qOK)
!
!   IMPLICIT NONE
! !
! ! Subroutine to clear 1D FFTW transform plans. Calls
! ! FFTW-MPI subroutine.
! !
! !                   ARGUMENTS
! !
! ! qOK            error flag; .false. if error occurs
!
!   LOGICAL, INTENT(OUT) :: qOK
!
! !                    BEGIN:-
!
!   qOK = .FALSE.
!
! !        perform 1D fftw plan deallocation
!
!   CALL fftw_f77_mpi_destroy_plan(tTransInfo_G%fplan)
!   CALL fftw_f77_mpi_destroy_plan(tTransInfo_G%bplan)
!
!   qOK = .TRUE.
!
!   GOTO 2000
!
! 1000  CALL Error_log('Error in transforms:clearTransformPlans_OneD',tErrorLog_G)
!
! 2000 CONTINUE
!
! END SUBROUTINE clearTransformPlans_OneD

!*****************************************************

SUBROUTINE clearTransformPlans_ThreeD(qOK)

  IMPLICIT NONE
!
! Subroutine to destroy multi-dimensional FFTW plans.
! Calls FFTW supplied subroutine.
!
!           ARGUMENTS
!
! qOK            error flag; .false. if error occurs
!
  LOGICAL, INTENT(OUT)  :: qOK

!                  Begin
  qOK = .FALSE.


!       multi-D fftw plan deallocation

  CALL fftw_destroy_plan(tTransInfo_G%fplan)
  CALL fftw_destroy_plan(tTransInfo_G%bplan)

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:clearTransformPlans_ThreeD',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE clearTransformPlans_ThreeD

!*****************************************************

! SUBROUTINE SetupParallelFourierField(sA_local,&
!                                       work,qOK)
!
!   IMPLICIT NONE
! !
! ! This subroutine returns the fourier transformed field
! ! distributed across processes according to fftw.
! ! The local distributed array is returned on sA_local.
! !
! !                  ARGUMENTS
! !
! ! sizes       IN      Array containing the number of nodes
! !                     in each dimension
! ! sA          IN      The global field array
! ! sA_local    INOUT   The local transformed field array
! !                     NOTE: indices must go from 0:N-1 (C
! !                     standard, rather than the usual 1:N
! !                     Fortran standard). This is due to
! !                     the FFTW fortran interface.
! ! work        INOUT   work array to speed up the transform,
! !                     of identical size to sA_local
! ! qOK         OUT     Error flag; .false. if error has occured
!
!   COMPLEX(KIND=WP), INTENT(INOUT),&
!        DIMENSION(0:tTransInfo_G%total_local_size-1)&
!        :: sA_local, work
!   LOGICAL, INTENT(OUT) :: qOK
!
! !                  LOCAL VARS
! !
! ! strans              number of nodes in transverse direction
!
!   INTEGER(KIND=IP) :: strans, first, last
!   LOGICAL  ::  qOKL
!
! !                      Begin
!
!   qOK = .FALSE.
!
! ! Transverse node size = num nodes in x * number of nodes in y
!
!   strans = NX_G * NY_G
!
!   IF (strans <= 1_IP) THEN
!     CALL Error_log('strans less than 1',tErrorLog_G)
!     GOTO 1000
!   END IF
!
! !    Calculate beginning and end of the local processes
! !    portion of sA and check they are valid
!
! !  first = ((tTransInfo_G%loc_z2_start+1)*strans) &
! !              -(strans)+1
! !
! !  last = (((tTransInfo_G%loc_z2_start+1)*strans)-(strans))+ &
! !              (tTransInfo_G%loc_nz2*strans)
! !
! !  IF (last > SIZE(sA) ) THEN
! !    CALL Error_log('last index out of bounds of field array',tErrorLog_G)
! !    GOTO 1000
! !  END IF
!
! ! Arrange real space field data across processors according to
! ! FFTW_MPI. Split the data along the z2-axis.
!
! !  sA_local(0:((strans*tTransInfo_G%loc_nz2)-1))   =   sA(first:last)
!
! !      Transform the local field data into fourier space.
!
!   CALL Transform(tTransInfo_G%fplan, &
!        work, sA_local, qOKL)
!
!   IF (.NOT. qOKL) GOTO 1000
!
! !           Set error flag and exit
!
!   qOK = .TRUE.
!
!   GOTO 2000
!
! 1000  CALL Error_log('Error in transforms:SetupParallelFourierField',tErrorLog_G)
!
! 2000 CONTINUE
!
! END SUBROUTINE SetupParallelFourierField

!***********************************************************

subroutine Transform(plan, &
     local_in, &
     qOK)

  implicit none

! This subroutine performs the FFTW-MPI fourier transfrom
! described by the given plan.
!
!                  ARGUMENTS
!
! plan      IN     FFTW-MPI plan. This plan MUST be of
!                    type integer*8.
! work      INOUT  The work array of identical size to
!                    local_in.
! local_in  INOUT  The data to be transformed, arranged
!                    in parallel according to the FFTW-MPI
!                    2.1.5 documentation.
! qOK       OUT    Error flag; if .false. error has occured

  type(C_PTR), intent(inout) :: plan

  complex(C_DOUBLE_COMPLEX), pointer, intent(inout) :: local_in(:,:,:)

  logical, intent(out) :: qOK

!                 LOCAL ARGS
!
!       qOKL                  Local error flag

  logical :: qOKL

!                     Begin

  qOK = .FALSE.

!       Select 1D or 3D case and execute transform

  if (tTransInfo_G%qOneD) then
     !call Transform_OneD(plan,work,local_in,qOKL)
  else
     call Transform_MultiD(plan,local_in,qOKL)
  end if

  if (.NOT. qOKL) GOTO 1000

!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:Transform',tErrorLog_G)

2000 CONTINUE

end subroutine Transform

!******************************************************

subroutine Transform_MultiD(plan, local_in, qOK)

  implicit none

! Subroutine to perform multi-dimensional transpose
! fourier transform using FFTW-MPI. The local data
! to be transformed should be input in 1D form. It
! will be returned in normal order(NOT transpose).
! See fftw v 2.1.5 manual for details.
!
!                   ARGUMENTS
!
! plan       IN      The FFTW transform plan. MUST be of type
!                    INTEGER*8.
! work      INOUT    The work array to speed up the transform
! local_in  INOUT    The array to be transformed.
! qOK       OUT      Error flag; if .false. error has occured



!  INTEGER*8, INTENT(IN)  :: plan
!
!  COMPLEX(KIND=WP),INTENT(INOUT),&
!       DIMENSION(0:tTransInfo_G%total_local_size-1)&
!       :: work
!
!  COMPLEX(KIND=WP),INTENT(INOUT),&
!       DIMENSION(0:tTransInfo_G%total_local_size-1)&
!       :: local_in
!
!  LOGICAL, INTENT(OUT) :: qOK



  type(C_PTR), intent(inout) :: plan

  complex(C_DOUBLE_COMPLEX), pointer, intent(inout) :: local_in(:,:,:)

  logical, intent(out) :: qOK



!                       Begin

  qOK = .FALSE.

!            Perform fourier transform

!  CALL fftwnd_f77_mpi(plan,1,local_in,work,USE_WORK,&
!       FFTW_NORMAL_ORDER)


  call fftw_mpi_execute_dft(plan, local_in, local_in)


!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:Transform_MultiD',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE Transform_MultiD

!******************************************************

! SUBROUTINE Transform_OneD(plan,work,local_in,qOK)
!
!   IMPLICIT NONE
!
! ! Subroutine to perform 1D transpose fourier transform using
! ! FFTW-MPI. The returned array will be in transpose order.
! !
! !                   ARGUMENTS
! !
! ! plan       IN      The FFTW transform plan. MUST be of type
! !                    INTEGER*8.
! ! work      INOUT    The work array to speed up the transform
! ! local_in  INOUT    The array to be transformed.
! ! qOK       OUT      Error flag; if .false. error has occured
!
!   INTEGER*8, INTENT(IN)  :: plan
!
!   COMPLEX(KIND=WP),INTENT(INOUT),&
!        DIMENSION(0:tTransInfo_G%total_local_size-1)&
!        :: work
!
!   COMPLEX(KIND=WP),INTENT(INOUT),&
!        DIMENSION(0:tTransInfo_G%total_local_size-1)&
!        :: local_in
!
!   LOGICAL, INTENT(OUT) :: qOK
!
! !                       Begin
!
!   qOK = .FALSE.
!
! !             Perform fourier transform
!
!   CALL fftw_f77_mpi(plan,1,local_in,work,USE_WORK)
!
! !              Set error flag and exit
!
!   qOK = .TRUE.
!
!   GOTO 2000
!
! 1000  CALL Error_log('Error in transforms:Transform_OneD',tErrorLog_G)
!
! 2000 CONTINUE
!
! END SUBROUTINE Transform_OneD

!******************************************************

SUBROUTINE GetKValues(recvs,displs,qOK)

  IMPLICIT NONE
!
! Calculates the Fourier space k-values
!
!                  ARGUMENTS
!
! iX,iY,iZ2          Number of nodes in x,y, and z2
! recvs,displs       Arrays describing local sizes
!                    of the Fourier transformed
!                    field
! qOK       OUT      Error flag; if .false. error has occured

  INTEGER(KIND=IP),INTENT(INOUT)  :: recvs(:),displs(:)
  LOGICAL, INTENT(OUT) :: qOK

! Local vars:-
  INTEGER(KIND=IP)  :: maxx, maxy, maxz2, i
  INTEGER(KIND=IP),DIMENSION(:),ALLOCATABLE  :: nx
  INTEGER(KIND=IP),DIMENSION(:),ALLOCATABLE  :: ny
  INTEGER(KIND=IP),DIMENSION(:),ALLOCATABLE  :: nz2
  REAL(KIND=WP),DIMENSION(:),ALLOCATABLE     :: kz2_loc
  REAL(KIND=WP)  :: slengthX, sLengthY,sLengthZ2, pi
  INTEGER(KIND=IP) :: loc_z2_start,loc_nz2
  INTEGER(KIND=IP) :: error, trans

!------------------------------------------------------
!                      Begin

  qOK = .FALSE.

  pi=4.0_WP*ATAN(1.0_WP)
! Calculate the Full length along x,y,z2
  slengthX  =  REAL((NX_G),KIND=WP) * sLengthOfElmX_G
  slengthY  =  REAL((NY_G),KIND=WP) * sLengthOfElmY_G
  slengthZ2 =  REAL((NZ2_G),KIND=WP) * sLengthOfElmZ2_G

! Calculate maximum x, y and z2 values for the n arrays
! Arrays go from (0:N/2-1) then (-N/2:-1)
  maxx = CEILING(REAL(NX_G,KIND=WP)/2.0_WP)
  maxy = CEILING(REAL(NY_G,KIND=WP)/2.0_WP)
  maxz2 = CEILING(REAL(NZ2_G,KIND=WP)/2.0_WP)

! Calculate nx, ny and nz2 values...
! nx and ny are calculated in full by all
! processes.
! nz2 is calculated locally in segments

! If 1D  use transpose order.
  IF ( tTransInfo_G%qOneD) THEN
    loc_z2_start = tTransInfo_G%loc_z2_start_aft_trans
    loc_nz2 = tTransInfo_G%loc_nz2_aft_trans
  ELSE
! If not use normal order tranforms and get kx and ky
    loc_z2_start = tTransInfo_G%loc_z2_start
    loc_nz2 = tTransInfo_G%loc_nz2

    ALLOCATE(nx(0:NX_G-1))
    ALLOCATE(ny(0:NY_G-1))

    DO i = 0,NX_G-1
      IF ( i >= maxx ) THEN
        nx(i) = i - NX_G
      ELSE
        nx(i) = i
      END IF
    END DO

    DO i = 0,NY_G-1
      IF (i >= maxy ) THEN
        ny(i) = i - NY_G
      ELSE
        ny(i) = i
      END IF
    END DO

    kx_G=2.0_WP*pi*REAL(nx,KIND=WP)/sLengthX
    ky_G=2.0_WP*pi*REAL(ny,KIND=WP)/sLengthY
    DEALLOCATE(nx,ny)
  END IF
!----------------------------------------------------------------------
! Kz2 vals calculated in parallel according to FFTW distribution
  IF (loc_nz2/=0) THEN
    ALLOCATE(nz2(0:loc_nz2-1))
    DO i = 0,loc_nz2-1
      IF ( loc_z2_start + i >= maxz2) THEN
        nz2(i) = loc_z2_start + i - NZ2_G
      ELSE
        nz2(i) = loc_z2_start + i
      END IF
    END DO

    kz2_loc_G=2.0_WP*pi*REAL(nz2,KIND=WP)/sLengthZ2

    DEALLOCATE(nz2)
  ENDIF

!Get gathering arrays for global all_to_all operations
  trans = NX_G*NY_G
  CALL getGathArrs(trans*tTransInfo_G%loc_nz2,recvs,displs)

!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:GetKValues',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE GetKValues

!********************************************************
END MODULE Transforms
