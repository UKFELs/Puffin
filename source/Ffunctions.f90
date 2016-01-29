!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE Functions

USE paratype
USE ParallelInfoType 
USE error_fn
USE typesAndConstants

IMPLICIT NONE

CONTAINS
!********************************************************

  FUNCTION gaussian(x,xc,sigma)

    IMPLICIT NONE
!
! Return array of the Gaussian function for aray of 
! points x
!
    REAL(KIND=WP), INTENT(IN) :: x(:)
    REAL (KIND=WP),INTENT(IN) :: xc,sigma
! Local vars
    REAL(KIND=WP) :: gaussian(SIZE(x)),&
         ngaussian(SIZE(x))
    REAL(KIND=WP) ::pi,s_twopi_sigma


! BEGIN:-

    pi=4.0_WP*ATAN(1.0_WP)
    s_twopi_sigma=sqrt(2*pi)*sigma
	
    ngaussian=exp(-((x-xc)/sigma)**2/2.0_WP)
	
    gaussian=ngaussian

  END FUNCTION gaussian

!********************************************************




  FUNCTION linspace(xstart,xend,n)

    IMPLICIT NONE
!
!Generate a uniform array of n values between xstart and xend
!
    REAL(KIND=WP),INTENT(IN) ::xstart,xend
    INTEGER(KIND=IP),INTENT(IN) :: n
    REAL(KIND=WP),DIMENSION(n) :: linspace
    
! Local vars:-

    REAL(KIND=WP) :: dx
    INTEGER(KIND=IP) :: i

! BEGIN:-

    IF(n>1) THEN
       dx=(xend-xstart)/REAL((n-1),KIND=WP)
       linspace=(/ (xstart+i*dx,i=0,(n-1))/)
    ELSE IF (n==1) THEN
       linspace=(/ xstart /)
    ELSE
       STOP "*** Number of points must be >0 in LINSPACE ***"
    END IF

  END FUNCTION linspace

!********************************************************




  FUNCTION getx(i,i_nmp,s_xc,sigma,s_xstart,s_h_in,&
       s_tol_in)

    IMPLICIT NONE
!
! CALCULATE THE X VALUE WHERE IT HAS THE SAME AREA
! UNDER THE GAUSSIAN
!
    INTEGER(KIND=IP),INTENT(IN) :: i,i_nmp
    REAL(KIND=WP),INTENT(IN) :: s_xc,sigma,s_xstart
    REAL(KIND=WP),INTENT(IN),OPTIONAL :: s_h_in 
    REAL(KIND=WP),INTENT(IN),OPTIONAL :: s_tol_in 

! Local vars:-

    REAL(KIND=WP) :: s_h,s_tol,getx,s_A1,s_x,s_area, &
         s_integral,s_integralstart

! BEGIN:-

! Read in the input file name
    If (PRESENT(s_h_in)) then
       s_h = s_h_in
    else
       s_h = 1.0_WP
    endif
    If (PRESENT(s_tol_in)) then
       s_tol = s_tol_in
    else
       s_tol = 1e-4_WP
    endif

    s_A1=i*1.0_WP/REAL(i_nmp)
    s_x=s_xstart
    s_area=0.0_WP
    s_integralstart=0.5_WP*(1.0_WP+erf((s_x-s_xc)/(SQRT(2.0_WP)*sigma)))
    s_area=s_integralstart
    	
    IF (s_A1<s_area)  THEN
       PRINT * ,"Error In 'getx (Dfunctions.f90)' Subroutine,",&
            " try to use less than ",i_nmp
       STOP 
    ENDIF
	
10  s_integral=0.5_WP*(1.0_WP+erf((s_x+s_h-s_xc)/(SQRT(2.0_WP)*sigma)))-0.5_WP*(1.0_WP+erf((s_x-s_xc)/(SQRT(2.0_WP)*sigma)))
	
    IF (s_h>s_tol) THEN
       IF(s_area+s_integral>s_A1) THEN
          s_h=s_h/2.0_WP
          GOTO 10
       ELSE
          s_area=s_area+s_integral
          s_x=s_x+s_h
          GO TO 10
       ENDIF
    ELSE
       getx=s_x
    ENDIF
    
  END FUNCTION getx

!********************************************************




  SUBROUTINE GaussianGrid(i_Macro,s_MeanGuass,s_SigmaGuass,s_start,s_end,sgrid)

    IMPLICIT NONE

    INTEGER(KIND=IP),INTENT(IN) :: i_Macro
    REAL(KIND=WP),INTENT(IN) :: s_MeanGuass,s_SigmaGuass,s_start,s_end

    REAL(KIND=WP),INTENT(OUT) :: sgrid(:)

! Local vars:-

    INTEGER(KIND=IP) :: i
    INTEGER(KIND=IP) :: nHalfGridPoints,nGridPoints
    REAL(KIND=WP) :: sHalfLength
    REAL(KIND=WP) :: sX, sY
    LOGICAL :: qEven

! BEGIN:-
! Number of grid points         
    nGridPoints = i_Macro + 1_IP   

! Calculate half the number of gridpoints  as grid is
! mirror image around centre        
    nHalfGridPoints = ceiling(REAL(nGridPoints) / 2_WP)  

! Find out if have even number of grid points        
    If ((nHalfGridPoints * 2_IP).NE. nGridPoints) Then
       qEven = .FALSE.
    else
       qEven = .TRUE.
    End if

! Calculate half the electron pulse length         
    sHalfLength = (abs(s_end - s_start)) / 2.0_WP

! Calculate constant        
    sX =  erf((sHalfLength) /&
         (sqrt(2.0_WP) * s_SigmaGuass)) / i_Macro

! Grid start and end positions      
    sGrid(1) = s_start
    sGrid(i_Macro + 1_IP) = s_end

! If have an even number of grid points
    If (qEven) Then

! Need only half area for first grid point as its
! position is mirror image around centre
       sY = sX / 2.0_WP
       Do i = 1_IP, nHalfGridPoints - 1_IP     
          sGrid(nHalfGridPoints + i) = sqrt(2.0_WP)&
               * s_SigmaGuass * erfi((i+(i-1)) * sX)
       End do

       Do i = 2_IP, nHalfGridPoints
          sGrid(i) = -sGrid(nGridPoints - i + 1_IP) 
       End do

! If have an odd number of grid points then have
! a grid point positioned at centre
    else
       sGrid(nHalfGridPoints) = 0.0_WP
       Do i = 1_IP, nHalfGridPoints - 2_IP     
          sgrid(nHalfGridPoints + i) = sqrt(2.0_WP) *&
               s_SigmaGuass * erfi(2.0_WP*i * sX) 
       End do
       Do i = 2_IP, nHalfGridPoints - 1_IP
          sgrid(i) = -sGrid(nGridPoints - i + 1_IP) 
       End do
    End if
    
    sgrid(2_IP:(nGridPoints - 1_IP)) = &
         sgrid(2_IP:(nGridPoints - 1_IP)) + s_MeanGuass

  END SUBROUTINE GaussianGrid

!********************************************************










  SUBROUTINE GaussianDistribution(i_Macro,s_grid,&
       s_MeanGuass,s_SigmaGuass,s_func)

    IMPLICIT NONE
!
! Input parameters
!
    INTEGER(KIND=IP),INTENT(IN) :: i_Macro
    REAL(KIND=WP),INTENT(IN)	:: s_MeanGuass,s_SigmaGuass
    REAL(KIND=WP),INTENT(IN) :: s_grid(:)
! Output parameters
    REAL(KIND=WP),INTENT(OUT) :: s_func(:)
! Local parameters
    INTEGER(KIND=IP) :: i
    REAL(KIND=WP) :: s_new
    REAL(KIND=WP),ALLOCATABLE :: s_d(:)

! BEGIN:-
    ALLOCATE(s_d(i_Macro))
    DO i=1,i_Macro
       s_new=0.5*(s_grid(i+1)+s_grid(i))
       s_func(i)=EXP(-((s_new-s_MeanGuass)/s_SigmaGuass)**2&
            /2.0_WP)
       s_d(i)=s_grid(i+1)-s_grid(i)
    ENDDO

    s_func=s_func/sum(s_d*s_func)

    DEALLOCATE(s_d)

  END SUBROUTINE GaussianDistribution

!--------------------------------------------------------












  SUBROUTINE GaussianDistributionz2(i_Macro,s_grid,&
       s_MeanGuass,s_SigmaGuass,s_func,&
       MPI_DOUBLE_PRECISION,MPI_SUM)

    IMPLICIT NONE
!
! Input parameters
!
    INTEGER(KIND=IP),INTENT(IN) :: i_Macro
    INTEGER(KIND=IP),INTENT(IN)	:: MPI_DOUBLE_PRECISION,&
         MPI_SUM
    REAL(KIND=WP),INTENT(IN) :: s_MeanGuass,s_SigmaGuass
    REAL(KIND=WP),INTENT(IN) :: s_grid(:)
! Output parameters
    REAL(KIND=WP),INTENT(OUT) :: s_func(:)
! Local parameters
    INTEGER(KIND=IP) :: i,error
    REAL(KIND=WP) :: s_new,tdenom,denom
    REAL(KIND=WP),ALLOCATABLE :: s_d(:)


! BEGIN:-

    ALLOCATE(s_d(i_Macro))
    DO i=1,i_Macro
       s_new=0.5*(s_grid(i+1)+s_grid(i))
       s_func(i)=EXP(-((s_new-s_MeanGuass)/s_SigmaGuass)**2&
            /2.0_WP)
       s_d(i)=s_grid(i+1)-s_grid(i)
    ENDDO
    tdenom = sum(s_d*s_func) 

    CALL MPI_ALLREDUCE(tdenom,denom,1,MPI_DOUBLE_PRECISION,&
         MPI_SUM, &
         tProcInfo_G%comm,error)
    
    s_func=s_func/denom

    DEALLOCATE(s_d)

  END SUBROUTINE GaussianDistributionz2

!********************************************************











  SUBROUTINE hpsort(N,RA)
    
    IMPLICIT NONE
!
! INPUTS & OUTPUTS PARAMETERS
!
    INTEGER(KIND=IP), INTENT(IN) :: N
    REAL(KIND=WP),INTENT(INOUT) :: RA(:)

! LOCAL PARAMETERS   

    INTEGER(KIND=IP) :: i,IR,J,L
    REAL(KIND=WP) :: RRA



! BEGIN:-   

    IF (N.LT.2_IP) return
    L=N/2_IP+1_IP
    IR=N
10  CONTINUE
    IF (L.GT.1_IP) THEN
       L=L-1
       RRA=RA(L)
    ELSE
       RRA=RA(IR)
       RA(IR)=RA(1)
       IR=IR-1_IP
       IF (IR.EQ.1_IP) THEN
          RA(1)=RRA
          RETURN
       ENDIF
    ENDIF
    i=L
    J=L+L
20  IF (J.LE.IR) THEN
       IF(J.LT.IR) THEN
          IF(RA(J).LT.RA(J+1_IP)) J=J+1_IP
       ENDIF
       IF(RRA.LT.RA(J)) THEN
          RA(I)=RA(J)
          I=J
          J=J+J
       ELSE
          J=IR+1_IP
       ENDIF
       GOTO 20
    ENDIF
    RA(I)=RRA
    GOTO 10
    
  END SUBROUTINE hpsort

!********************************************************








  FUNCTION epsilonParameter(saw,sgamma_r,fx,fy)

    IMPLICIT NONE

    REAL(KIND=WP),INTENT(IN)  :: saw,sgamma_r,fx,fy

! Local vars:-

    REAL(KIND=WP) :: epsilonParameter
    REAL(KIND=WP) :: beta_av


! BEGIN:-   

    beta_av = SQRT(sgamma_r**2 - 1.0_WP - saw**2)&
         /sgamma_r

    epsilonParameter=(1-beta_av)/beta_av

  END FUNCTION epsilonParameter

!********************************************************









  FUNCTION GainLength(sWigglerWaveLength,rho)

    IMPLICIT NONE

    REAL(KIND=WP),INTENT(IN) :: sWigglerWaveLength,rho

! Local vars:-

    REAL(KIND=WP) :: GainLength

! BEGIN:-

    GainLength = sWigglerWaveLength/(4.0_WP * pi * rho)

  END FUNCTION GainLength
!********************************************************

  FUNCTION MatchedBeamRadius(srho, sEmit, k_beta)

    IMPLICIT NONE

    REAL(KIND=WP),INTENT(IN) :: srho, sEmit, k_beta

! Local vars:-

    REAL(KIND=WP) :: MatchedBeamRadius

! BEGiN:-

    MatchedBeamRadius = SQRT( sRho * 18.0_WP * sEmit /&
        k_beta)

  END FUNCTION MatchedBeamRadius

!********************************************************






  FUNCTION DiffractionLength(z,sRaleighLength,sigma)

    IMPLICIT NONE

    REAL(KIND=WP),INTENT(IN) :: z,sRaleighLength,sigma

! Local vars:-

    REAL(KIND=WP) :: DiffractionLength

! BEGIN:-

    !IF(tProcInfo_G%QROOT)print *, 'RaleighLength= ',&
    !     sRaleighLength

    DiffractionLength = SQRT(2.0_WP*&
         ((z/sRaleighLength)**2 + 1.0_WP)) * 6.0_WP*sigma

  END FUNCTION DiffractionLength

!********************************************************

  FUNCTION RaleighLength(srho,sigma)

! sigma of the seed field

    IMPLICIT NONE

    REAL(KIND=WP),INTENT(IN) :: srho,sigma

    REAL(KIND=WP) :: RaleighLength

! BEGIN:-

    RaleighLength = sigma**2/(2.0_WP*srho)

  END FUNCTION RaleighLength

! ####################################################

  function arr_mean_para(s_ar)

! Return the mean of an array of real values

    use ParallelSetUp

    implicit none

    real(kind=wp), intent(in) :: s_ar(:)
    real(kind=wp) :: arr_mean_para
    real(kind=wp) :: loc_sum, glob_sum    

    loc_sum = sum(s_ar)  ! local sum

    call sum_mpi_real(loc_sum,glob_sum)  ! sum globally

    arr_mean_para = glob_sum / size(s_ar)  ! get mean

  end function arr_mean_para




  function arr_mean_para_weighted(s_ar, weights)

! Return the mean of an array of real values

    use ParallelSetUp

    implicit none

    real(kind=wp), intent(in) :: s_ar(:), weights(:)
    real(kind=wp) :: arr_mean_para_weighted
    real(kind=wp) :: loc_sum, glob_sum, glob_weight_sum, &
                     loc_weight_sum

    loc_sum = sum(weights * s_ar)  ! local sum
    loc_weight_sum = sum(weights)

    call sum_mpi_real(loc_sum,glob_sum)  ! sum globally
    call sum_mpi_real(loc_weight_sum,glob_weight_sum)  ! sum globally

    arr_mean_para_weighted = glob_sum / glob_weight_sum  ! get mean

  end function arr_mean_para_weighted



END MODULE Functions
