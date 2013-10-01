MODULE transforms

! This module contains subroutines called in Main.f90 dealing with
! fourier transforms, calculations in fourier space etc.
!
USE ParallelInfoType
USE TransformInfoType
USE FFTW_Constants
USE DerivsGlobals
USE Derivative
USE IO

IMPLICIT NONE

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

!                   Begin
!---------------------------------------------------

  qOK = .FALSE.

!     Determine if 1D or 3D transform is needed
  IF(nnodes(iX_CG) == 1 .AND. nnodes(iY_CG) == 1) THEN
     tTransInfo_G%qOneD=.TRUE.
     CALL getTransformPlans_OneD(nnodes(iZ2_CG),qOKL)
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

!------------------------------------------------
!                         Begin

  qOK = .FALSE.

!      Create the forward transform plan

  CALL fftwnd_f77_mpi_create_plan(tTransInfo_G%fplan, &
       tProcInfo_G%comm,   &
       nDims,            &
       sizes,            &
       FFTW_FORWARD,     &
       FFTW_MEASURE)

!      Get the local sizes for the distribution of the transform data.

  CALL fftwnd_f77_mpi_local_sizes(tTransInfo_G%fplan,&
       tTransInfo_G%loc_nz2, &
       tTransInfo_G%loc_z2_start, &
       tTransInfo_G%loc_nz2_aft_trans, &
       tTransInfo_G%loc_z2_start_aft_trans, &
       tTransInfo_G%total_local_size)	
			       
!      Create the backward transform plan

  CALL fftwnd_f77_mpi_create_plan(tTransInfo_G%bplan, &
       tProcInfo_G%comm,   &
       nDims,            &
       sizes,            &
       FFTW_BACKWARD,    &
       FFTW_MEASURE)

!     Set error flag to true

  qOK = .TRUE.
  GOTO 2000

1000  CALL Error_log('Error in transforms:getTransformPlans_MultiD',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE getTransformPlans_MultiD
!******************************************************
SUBROUTINE getTransformPlans_OneD(nn1D,qOK)

  IMPLICIT NONE
!
! Subroutine to create 1 dimensional forward and 
! backward FFTW-MPI plans for Fourier transforms
! of data of a specified size, and get the sizes
! of the local data before and after the transform.
!
!            ARGUMENTS
!
! nn1D        IN   Number of field nodes in z2
! qOK         OUT  Error flag; .false. if error occurs

  INTEGER, INTENT(IN)   :: nn1D
  LOGICAL, INTENT(OUT)  :: qOK

!-------------------------------------------------------
!                         Begin

  qOk = .FALSE.

!           Create the forward transform plan

  CALL fftw_f77_mpi_create_plan(tTransInfo_G%fplan, &
       tProcInfo_G%comm,   &
       nn1D,             &
       FFTW_FORWARD,     &
       FFTW_MEASURE)

!      Get the local sizes for the distribution of the 
!                 transform data.

  CALL fftw_f77_mpi_local_sizes(tTransInfo_G%fplan,&
       tTransInfo_G%loc_nz2,                &
       tTransInfo_G%loc_z2_start,           &
       tTransInfo_G%loc_nz2_aft_trans,      &
       tTransInfo_G%loc_z2_start_aft_trans, &
       tTransInfo_G%total_local_size)

!            Create the backward transform plan

  CALL fftw_f77_mpi_create_plan(tTransInfo_G%bplan, &
       tProcInfo_G%comm,   &
       nn1D,             &
       FFTW_BACKWARD,    &
       FFTW_MEASURE)

!            Set error flag to true and exit

  qOK = .TRUE.
  GOTO 2000

1000  CALL Error_log('Error in transforms:getTransformPlans_OneD',tErrorLog_G)

2000 CONTINUE
    
END SUBROUTINE getTransformPlans_OneD
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

!-----------------------------------------------------
!                      Begin

  qOK = .FALSE.

!     Identify 1D or 3D case and call appropriate subroutine

  IF (tTransInfo_G%qOneD) THEN
     CALL clearTransformPlans_OneD(qOKL)
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
SUBROUTINE clearTransformPlans_OneD(qOK)

  IMPLICIT NONE
!
! Subroutine to clear 1D FFTW transform plans. Calls 
! FFTW-MPI subroutine.
!
!                   ARGUMENTS
!
! qOK            error flag; .false. if error occurs

  LOGICAL, INTENT(OUT) :: qOK

!-----------------------------------------------------
!                    BEGIN:-

  qOK = .FALSE.

!        perform 1D fftw plan deallocation

  CALL fftw_f77_mpi_destroy_plan(tTransInfo_G%fplan)
  CALL fftw_f77_mpi_destroy_plan(tTransInfo_G%bplan) 

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:clearTransformPlans_OneD',tErrorLog_G)

2000 CONTINUE
  
END SUBROUTINE clearTransformPlans_OneD
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
!	  
!-----------------------------------------------------
!                  Begin
  qOK = .FALSE.


!       multi-D fftw plan deallocation

  CALL fftwnd_f77_mpi_destroy_plan(tTransInfo_G%fplan)
  CALL fftwnd_f77_mpi_destroy_plan(tTransInfo_G%bplan) 

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:clearTransformPlans_ThreeD',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE clearTransformPlans_ThreeD
!*****************************************************
SUBROUTINE SetupParallelFourierField(sA,sA_local,&
                            work,qOK)      

  IMPLICIT NONE
!
! This subroutine returns the fourier transformed field
! distributed across processes according to fftw.
! The local distributed array is returned on sA_local.
!
!                  ARGUMENTS
!
! sizes       IN      Array containing the number of nodes 
!                     in each dimension
! sA          IN      The global field array
! sA_local    INOUT   The local transformed field array
!                     NOTE: indices must go from 0:N-1 (C 
!                     standard, rather than the usual 1:N 
!                     Fortran standard). This is due to
!                     the FFTW fortran interface.
! work        INOUT   work array to speed up the transform,
!                     of identical size to sA_local	   
! qOK         OUT     Error flag; .false. if error has occured

  COMPLEX(KIND=WP), INTENT(IN) :: sA(:)
  COMPLEX(KIND=WP), INTENT(INOUT),&
       DIMENSION(0:tTransInfo_G%total_local_size-1)&
       :: sA_local, work
  LOGICAL, INTENT(OUT) :: qOK

!                  LOCAL VARS
!
! strans              number of nodes in transverse direction

  INTEGER(KIND=IP) :: strans, first, last
  LOGICAL  ::  qOKL 

!--------------------------------------------------------
!          Begin

  qOK = .FALSE.

! Transverse node size = num nodes in x * number of nodes in y

  strans = NX_G * NY_G

  IF (strans <= 1_IP) THEN 
    CALL Error_log('strans less than 1',tErrorLog_G)
    GOTO 1000
  END IF

!    Calculate beginning and end of the local processes
!    portion of sA and check they are valid

  first = ((tTransInfo_G%loc_z2_start+1)*strans) &
              -(strans)+1

  last = (((tTransInfo_G%loc_z2_start+1)*strans)-(strans))+ &
              (tTransInfo_G%loc_nz2*strans)

  IF (last > SIZE(sA) ) THEN
    CALL Error_log('last index out of bounds of field array',tErrorLog_G)
    GOTO 1000
  END IF

! Arrange real space field data across processors according to 
! FFTW_MPI. Split the data along the z2-axis.

  sA_local(0:((strans*tTransInfo_G%loc_nz2)-1))   =   sA(first:last)
  
!      Transform the local field data into fourier space.

  CALL Transform(tTransInfo_G%fplan, &
       work, sA_local, qOKL)

  IF (.NOT. qOKL) GOTO 1000

!           Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:SetupParallelFourierField',tErrorLog_G)

2000 CONTINUE
      
END SUBROUTINE SetupParallelFourierField
      
!***********************************************************
SUBROUTINE Transform(plan, &
     work, &
     local_in, &
     qOK)

  IMPLICIT NONE

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

  INTEGER*8,       INTENT(IN)  ::	plan
	
  COMPLEX(KIND=WP),INTENT(INOUT),&
       DIMENSION(0:tTransInfo_G%total_local_size-1)&
       :: work
  
  COMPLEX(KIND=WP),INTENT(INOUT),&
       DIMENSION(0:tTransInfo_G%total_local_size-1)&
       :: local_in

  LOGICAL, INTENT(OUT) :: qOK

!                 LOCAL ARGS
!
!       qOKL                  Local error flag

  LOGICAL :: qOKL

!-------------------------------------------------------	
!                     Begin

  qOK = .FALSE.

!       Select 1D or 3D case and execute transform

  IF (tTransInfo_G%qOneD) THEN
     CALL Transform_OneD(plan,work,local_in,qOKL)
  ELSE
     CALL Transform_MultiD(plan,work,local_in,qOKL)
  END IF

  IF (.NOT. qOKL) GOTO 1000

!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:Transform',tErrorLog_G)

2000 CONTINUE
	
END SUBROUTINE Transform
!******************************************************
SUBROUTINE Transform_MultiD(plan,work,local_in,qOK)
  
  IMPLICIT NONE				   

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

  INTEGER*8, INTENT(IN)  :: plan

  COMPLEX(KIND=WP),INTENT(INOUT),&
       DIMENSION(0:tTransInfo_G%total_local_size-1)&
       :: work
	
  COMPLEX(KIND=WP),INTENT(INOUT),&
       DIMENSION(0:tTransInfo_G%total_local_size-1)&
       :: local_in

  LOGICAL, INTENT(OUT) :: qOK
!-------------------------------------------------------
!                       Begin

  qOK = .FALSE.

!            Perform fourier transform

  CALL fftwnd_f77_mpi(plan,1,local_in,work,USE_WORK,&
       FFTW_NORMAL_ORDER) 

!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:Transform_MultiD',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE Transform_MultiD
!******************************************************
SUBROUTINE Transform_OneD(plan,work,local_in,qOK)

  IMPLICIT NONE

! Subroutine to perform 1D transpose fourier transform using
! FFTW-MPI. The returned array will be in transpose order. 
!
!                   ARGUMENTS
!
! plan       IN      The FFTW transform plan. MUST be of type
!                    INTEGER*8.
! work      INOUT    The work array to speed up the transform
! local_in  INOUT    The array to be transformed. 
! qOK       OUT      Error flag; if .false. error has occured

  INTEGER*8, INTENT(IN)  :: plan

  COMPLEX(KIND=WP),INTENT(INOUT),&
       DIMENSION(0:tTransInfo_G%total_local_size-1)&
       :: work

  COMPLEX(KIND=WP),INTENT(INOUT),&
       DIMENSION(0:tTransInfo_G%total_local_size-1)&
       :: local_in

  LOGICAL, INTENT(OUT) :: qOK

!------------------------------------------------------
!                       Begin

  qOK = .FALSE.

!             Perform fourier transform

  CALL fftw_f77_mpi(plan,1,local_in,work,USE_WORK)

!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:Transform_OneD',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE Transform_OneD
!******************************************************
SUBROUTINE multiplyexp(h,Field,qOK)

  IMPLICIT NONE
!
! Subroutine to calculate the RHS of the diffraction
! solution in Fourier space.
!
!                    ARGUMENTS
!
! h          IN                Integration step size
! Field     INOUT    local portion of Fourier transformed field
! qOK       OUT       Error flag; if .false. error has occured

  REAL(KIND=WP), INTENT(IN) :: h

  COMPLEX(KIND=WP), INTENT(INOUT) :: &
       Field(0:tTransInfo_G%total_local_size-1)

  LOGICAL, INTENT(OUT) :: qOK

!                    LOCAL ARGS
!
! posI                        Imaginary unit
!                             (square root of -1)
! ind,x_inc,y_inc,z2_inc      Indices for loop
! loc_nz2                     Number of FT field nodes
!                             on the local process
   
  COMPLEX(KIND=WP) :: posI
  INTEGER(KIND=IP) :: ind,x_inc,y_inc,z2_inc
  INTEGER(KIND=IP) :: loc_nz2
  REAL(KIND=WP) :: cutoff,delz2

!------------------------------------------------------
!                      Begin

  qOK = .FALSE.

  posI=CMPLX(0.0,1.0,KIND=WP)
  delz2=sLengthOfElmZ2_G
  cutoff=2.0_WP*pi*sfilt/(REAL(NZ2_G,KIND=WP)*delz2)
  loc_nz2 = tTransInfo_G%loc_nz2

!      Main loop, multiply FT field by exp factor
  
  DO x_inc=0,NX_G-1_IP
     DO y_inc=0,NY_G-1_IP
        DO z2_inc=0,loc_nz2-1_IP

           ind=x_inc+y_inc*NX_G+z2_inc*NX_G*NY_G

           IF ((kz2_loc_G(z2_inc)>cutoff) .OR. &
                (kz2_loc_G(z2_inc)<-cutoff)) THEN
              
              IF (kz2_loc_G(z2_inc)/=0.0_WP) THEN

                Field(ind)=exp(posI*h*(kx_G(x_inc)**2 + &
                           ky_G(y_inc)**2) / &
                           (2.0_WP*kz2_loc_G(z2_inc)))*Field(ind)

              END IF

           ELSE
              
              IF (qFilter) Field(ind)=CMPLX(0.0_WP,0.0_WP)
              
           END IF

        END DO
     END DO
  END DO

!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:RearrangeExp',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE multiplyexp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE DiffractionStep(h,recvs,displs,sV,sA,qOK)

  IMPLICIT NONE
!
! Subroutine to perform free space radiation field diffraction 
! in the dimensionless scaled notation.
! Diffraction step algorithm described in
! LT Campbell and BWJ McNeil, Physics of Plasmas 19, 093119 (2012)
!
!                       ARGUMENTS
!
! h            IN         step size forward in units of
!                         zbar
! recvs and
! displs       IN         Arrays for MPI communication.
!                         They describe the layout of data
!                         across processes. See MPI docs.
! sV           IN         Electron macroparticle array, 
!                         containing coordinates in each 
!                         of the 6 scaled dimensions of this
!                         process's local macroparticles.
! sA           INOUT      Input as global field array at
!                         zbar. Output as global field array
!                         at zbar + h.
! qOK          OUT        Error flag; if .false. error has occured

  REAL(KIND=WP), INTENT(IN)      ::   h
  INTEGER(KIND=IP), INTENT(IN)   :: recvs(:),displs(:)
  REAL(KIND=WP), DIMENSION(:), INTENT(IN)  :: sV
  REAL(KIND=WP), DIMENSION(:), INTENT(INOUT)  :: sA
  LOGICAL, INTENT(OUT)  ::  qOK

!                       LOCAL ARGS
!
! work          'work' array used to speed up parallel
!               transforms in FFTW
! sA_local      The local Fourier transformed field array
! qOKL          Local error flag

  COMPLEX(KIND=WP),DIMENSION(:),ALLOCATABLE :: &
       work,sA_local
  LOGICAL :: qOKL

!------------------------------------------------------
!                      Begin

  qOK = .FALSE.

!     Allocate arrays and get distributed FT of field. 
!     Transforming from A(x,y,z2,zbar) to A(kx,ky,kz2,zbar)

  ALLOCATE(sA_local(0:tTransInfo_G%TOTAL_LOCAL_SIZE-1))
  ALLOCATE(work(0:tTransInfo_G%TOTAL_LOCAL_SIZE-1))
	 
  CALL setupParallelFourierField(CMPLX(Vector(iRe_A_CG,sA),&
       Vector(iIm_A_CG,sA),KIND=WP),&
       sA_local,work,qOKL) 

!    Multiply field by the exp factor to obtain A(kx,ky,kz2,zbar+h)

  CALL MultiplyExp(h,sA_local,qOKL)	
 
!   Perform the backward fourier transform to obtain A(x,y,z2,zbar+h)

  CALL Transform(tTransInfo_G%bplan, &
       work, &
       sA_local, &
       qOKL)

  IF (.NOT. qOKL) GOTO 1000

  DEALLOCATE(work)


!      Scale the field data to normalize transforms

  sA_local = sA_local/ffact

!   Collect data back onto global field var sA on every process

  CALL gather2Acomtoreal(sA_local,sA,&
       (NX_G*NY_G*tTransInfo_G%loc_nz2),&
       NX_G*NY_G*NZ2_G,&
       tTransInfo_G%TOTAL_LOCAL_SIZE,&
       recvs,displs)

  DEALLOCATE(sA_local)

!        Clear up field emerging outside e-beam

  CALL clearA(sA,sV,qOKL)

!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:DiffractionStep',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE DiffractionStep
!**************************************************
SUBROUTINE clearA(sA,sV,qOK)

! qOK       OUT      Error flag; if .false. error has occured

  IMPLICIT NONE
    
  REAL(KIND=WP),INTENT(IN) :: sV(:)
  REAL(KIND=WP),INTENT(INOUT) :: sA(:)
  LOGICAL, INTENT(OUT) :: qOK
  INTEGER(KIND=IP) :: error,iz2A,nA,trans
  REAL(KIND=WP) :: loc_max,glo_max

!------------------------------------------------------
!                      Begin

  qOK = .FALSE.

! Set field to zero behind electrons...

! Find furthest back electron
  loc_max = MAXVAL(Vector(iRe_Z2_CG,sV))
  CALL MPI_ALLREDUCE(loc_max,glo_max,1,MPI_DOUBLE_PRECISION,&
       MPI_MAX,MPI_COMM_WORLD,error)

! Get corresponding node and increment by 1
  iz2A  = ceiling(glo_max/sLengthOfElmZ2_G)  + 2_IP

  trans=NX_G*NY_G
  nA=size(sA)/2

! Force trailing nodes = 0
  if (seedend<nA) then
    if (iz2A > seedend) then !if electron has passed end of seed
      if (iz2A < nA) then
        sA(((iz2A-1)*trans+1):nA)=0.0_WP !real
        sA(((iz2A-1)*trans+1+nA):2*nA)=0.0_WP !imag
      end if
    else
      sA(((seedend-1)*trans+1):nA)=0.0_WP !real
      sA(((seedend-1)*trans+1+nA):2*nA)=0.0_WP !imag
    end if
  end if
!Other initial condition is z2(0)=0
  sA(1:trans)=0.0_WP
  sA(nA+1:nA+trans)=0.0_WP

!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:clearA',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE clearA

!**************************************************
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
