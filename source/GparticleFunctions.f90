!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE particleFunctions

USE paratype
USE ParallelInfoType
USE error_fn
USE Functions
USE typesAndConstants
USE FileType
USE IO
use parallelsetup
use globals

IMPLICIT NONE

INTEGER(KIND=IP), PARAMETER :: iLinear_CG = 1_IP
INTEGER(KIND=IP), PARAMETER :: iGaussian_CG = 2_IP

INTEGER(KIND=IP), PARAMETER :: iTopHatDistribution_CG = 1_IP
INTEGER(KIND=IP), PARAMETER :: iGaussianDistribution_CG = 2_IP

INTEGER(KIND=IP), PARAMETER :: i_oldgenmacro=1_IP
INTEGER(KIND=IP), PARAMETER :: i_newgenmacro=2_IP

CONTAINS
!********************************************************
  SUBROUTINE PulseGrid(iGridType,iNumMP,sStart,&
       sEnd,sMean,sSigma,sGrid,qOK)

    IMPLICIT NONE
! Calculate the electron grid positions
!
! iGridType - INPUT  - If linear or gaussian grid
! iNumMP    - INPUT  - Number of macro electrons
! sStart    - INPUT  - Start position for gaussian
! sEnd      - INPUT  - End position for gaussian
! sMean     - INPUT  - Mean position for gaussian
!                      optional for linear grid only
! sSigma    - INPUT  - Sigma for gaussian optional
!                      for linear grid only
! sGrid(:)  - OUTPUT - Grid positions
! qOK       - OUTPUT - Error flag
!
    INTEGER(KIND=IP),INTENT(IN)	:: iGridType,iNumMP
    REAL(KIND=WP),INTENT(IN)    :: sStart,sEnd
    REAL(KIND=WP),INTENT(IN),OPTIONAL	:: sMean,sSigma
    REAL(KIND=WP),INTENT(OUT) :: sGrid(:)
    LOGICAL, INTENT(OUT) :: qOK
!--------------------------------------------------------
! BEGIN:-
! Set error flag to false         
    qOK = .FALSE.    

! Create grid
    SELECT CASE (iGridType)
    CASE(iLinear_CG)
       sGrid=linspace(sStart,sEnd,iNumMP+1)
    CASE(iGaussian_CG)
       IF (.NOT. PRESENT(sMean)) THEN
          CALL Error_log('Error creating gaussian grid no mean given.',tErrorLog_G)
          GOTO 1000    
       END IF
       IF (.NOT. PRESENT(sSigma)) THEN
          CALL Error_log('Error creating gaussian grid no sigma given..',tErrorLog_G)
          GOTO 1000    
       END IF
       CALL GaussianGrid(iNumMP,sMean,sSigma,sStart,sEnd,sGrid)
    END SELECT

!  Set error flag and exit         
    qOK = .TRUE.				    
    GOTO 2000

! Error Handler
1000 CALL Error_log('Error in Chow:PulseGrid',tErrorLog_G)
    PRINT*,'Error in Chow:PulseGrid'
2000 CONTINUE

  END SUBROUTINE PulseGrid
!********************************************************
  SUBROUTINE EvalIntegral(s_gridPoints,s_mean,s_sigma,&
       s_integral)

    IMPLICIT NONE

    REAL(KIND=WP),INTENT(IN)  :: s_gridPoints(:)
    REAL(KIND=WP),INTENT(IN)  :: s_mean,s_sigma
    REAL(KIND=WP),INTENT(OUT) :: s_integral(:) 
!
!LOCAL VARIABLES
    INTEGER(KIND=IP) :: i
    REAL(KIND=WP)    :: s_value1,s_value2

    DO i=1,(SIZE(s_gridPoints)-1)
       s_value2=(s_gridPoints(i+1)-s_mean)/&
            (s_sigma*SQRT(2.0_WP))
       s_value1=(s_gridPoints(i)-s_mean)/&
            (s_sigma*SQRT(2.0_WP))
       s_integral(i) = 0.5_WP *&
            (erf(s_value2)-erf(s_value1))
    END DO

  END SUBROUTINE EvalIntegral
!********************************************************
  SUBROUTINE DistributionIntegral(iDistributionType,&
       iNumMP,sGrid,sMean,sSigma,sIntegral,qOK)

    IMPLICIT NONE
!
! Calculate the integral of the chosen distribution for
! the electron pulse
!
! iDistributionType - INPUT  - If Top-hat or gaussian 
!                              distribution
! iNumMP            - INPUT  - Number of macro electrons
! sGrid(:)          - INTPUT - Grid positions
! sEnd              - INPUT  - End position for gaussian
! sMean             - INPUT  - Mean position for gaussian 
! sSigma            - INPUT  - Sigma for gaussian 
! sIntegral(:)      - OUTPUT - Integral under the
!                              specific distribution
! qOK               - OUTPUT - Error flag
!
    INTEGER(KIND=IP),INTENT(IN)	:: iDistributionType,iNumMP
    REAL(KIND=WP),INTENT(IN)		:: sGrid(:),sMean,sSigma
    REAL(KIND=WP),INTENT(OUT)		:: sIntegral(:)
    LOGICAL,      INTENT(OUT)         :: qOK
!
!LOCAL VARIABLES
!
    REAL(KIND=WP),ALLOCATABLE :: sFunc(:),sDel(:)
    INTEGER(KIND=IP) :: i
!--------------------------------------------------------
! BEGIN:-
! Set error flag to false         
    qOK = .FALSE.    

! Create grid         
    SELECT CASE (iDistributionType)
    CASE(iTopHatDistribution_CG)
! ALLOCATE LOCAL ARRAYS         
       ALLOCATE(sFunc(iNumMP),sDel(iNumMP))  
       CALL GaussianDistribution(iNumMP,sGrid,sMean,&
            sSigma,sFunc)
       DO i=1,iNumMP
          sDel(i)=sGrid(i+1)-sGrid(i)		
       ENDDO
       sIntegral= sFunc * sDel  
!********************************************************
! DEALLOCATE LOCAL ARRAYS         
       DEALLOCATE(sFunc,sDel)

    CASE(iGaussianDistribution_CG)
       CALL EvalIntegral(sGrid,sMean,sSigma,sIntegral)
    END SELECT

!  Set error flag and exit
    qOK = .TRUE.				    
    GOTO 2000
!
! Error Handler
1000 CALL Error_log('Error in ElectronGrid:DistributionIntegral',&
          tErrorLog_G)
    PRINT*,'Error in ElectronGrid:DistributionIntegral'
2000 CONTINUE
  END SUBROUTINE DistributionIntegral
!********************************************************
  SUBROUTINE DistributionIntegralz2(iDistributionType,&
     iLocNumMP,iNumMP,sGrid,sMean,sSigma,&
     MPI_DOUBLE_PRECISION,MPI_SUM,sIntegral,qOK)

    IMPLICIT NONE
!
! Calculate the integral of the chosen distribution
! for the electron pulse
!
! iDistributionType     - INPUT  - If Top-hat or gaussian
!                                  distribution
! iNumMP                - INPUT  - Number of macro electrons
! sGrid(:)              - INTPUT - Grid positions
! sEnd                  - INPUT  - End position for gaussian
! sMean                 - INPUT  - Mean position for gaussian 
! sSigma                - INPUT  - Sigma for gaussian 
! sIntegral(:)          - OUTPUT - Integral under the
!                                  specific distribution
! qOK                   - OUTPUT - Error flag
!
    INTEGER(KIND=IP),INTENT(IN)	:: iDistributionType,&
         iLocNumMP,iNumMP
    INTEGER(KIND=IP),INTENT(IN)	:: MPI_DOUBLE_PRECISION,&
         MPI_SUM
    REAL(KIND=WP),INTENT(IN)    :: sGrid(:),sMean,sSigma
    REAL(KIND=WP),INTENT(OUT)	:: sIntegral(:)
    LOGICAL,      INTENT(OUT)   :: qOK
!
! LOCAL VARIABLES
!
    REAL(KIND=WP),ALLOCATABLE :: sFunc(:),sDel(:)
    INTEGER(KIND=IP) :: i
!--------------------------------------------------------
! BEGIN:-
! Set error flag to false         
    qOK = .FALSE.    

! Create grid         
    SELECT CASE (iDistributionType)
    CASE(iTopHatDistribution_CG)
       ALLOCATE(sFunc(iLocNumMP),sDel(iLocNumMP))
       CALL GaussianDistributionZ2(iLocNumMP,sGrid,&
            sMean,sSigma,sFunc,&
            MPI_DOUBLE_PRECISION,MPI_SUM)
       DO i=1,ilocNumMP
          sDel(i)=sGrid(i+1)-sGrid(i)		
       ENDDO
       sIntegral= sFunc * sDel
       DEALLOCATE(sFunc,sDel)

    CASE(iGaussianDistribution_CG)
       CALL EvalIntegral(sGrid,sMean,sSigma,sIntegral)
    END SELECT

!  Set error flag and exit         
    qOK = .TRUE.				    
    GOTO 2000     

! Error Handler
1000 CALL Error_log('Error in ElectronGrid:DistributionIntegral',&
          tErrorLog_G)
    PRINT*,'Error in ElectronGrid:DistributionIntegral'
2000 CONTINUE

  END SUBROUTINE DistributionIntegralz2
!********************************************************



subroutine flattop2(sig_edj, len_f, sGrid, iNumMP, iLocNumMP, qPara, s_Integral)


! Create a normalised flat top distribution with rounded edges.
! Round esges are modelled with half-gaussians. Uniprocessor version.
  
  real(kind=wp), intent(in) :: sig_edj,  & ! sigma of each edge
                               len_f,    & ! length of flat section in centre
                               sGrid(:)    ! grid points
                               
  integer(kind=ip), intent(in) :: iNumMP,   & ! Total number of MPs 
                                  iLocNumMP   ! Local number of MPs

  logical, intent(in) :: qPara

  real(kind=wp), intent(out) :: s_Integral(:)


  real(kind=wp) :: len_gauss, sSt, sEd, sg1st, sftst, sg2st, sg1cen, &
                   sg2cen, norm_pk, tres, a1, a2, tot_ar, tmp_gdpt

  integer(kind=ip) :: ii
  integer :: iT, iStrange, error  ! MPI Vars


!!! Get grid pts assigned to each section - gaussian -> flat -> gaussian


  len_gauss = gExtEj_G * sig_edj   ! model out how many sigma?? 

! Get start and end of gaussian (need to send data around to determine global sts and ends in parallel version)

  sSt = sGrid(1_ip)
  sEd = sGrid(iLocNumMP)

  if (qPara) then

    CALL MPI_ALLREDUCE(MPI_IN_PLACE, sSt, 1, MPI_DOUBLE_PRECISION, &
         MPI_MIN, MPI_COMM_WORLD, error)

    CALL MPI_ALLREDUCE(MPI_IN_PLACE, sEd, 1, MPI_DOUBLE_PRECISION, &
         MPI_MAX, MPI_COMM_WORLD, error)

  end if

  sg1st = sSt              ! Start of 1st half gauss
  sftst = sSt + len_gauss  ! Start of flat-top section
  sg2st = sftst + len_f    ! Start of second half-gaussian

  sg1cen = sftst      ! mean of 1st gauss
  sg2cen = sg2st

  norm_pk = 1.0_wp / (sqrt(2.0_wp*pi) * sig_edj) ! Peak of normalized gaussian

  iStrange = 0_ip

  do ii = 1,iLocNumMP

    if (sGrid(ii) < sftst .and. sGrid(ii+1) < sftst ) then

      ! area between 2 gaussian pts

       call AreaNDist(sGrid(ii:ii+1), sg1cen, sig_edj, tres)
       s_integral(ii) = tres

    else if (sGrid(ii) < sftst .and. (sGrid(ii+1) > sftst .and.  sGrid(ii+1) < sg2st ) ) then

      ! area to top of gaussian (sftst) + area from top of gaussian to next grid point

      tmp_gdpt = sg1cen ! temp grid point

      call AreaNDist( (/sGrid(ii),  tmp_gdpt/), sg1cen, sig_edj, tres)


      a1 = tres ! Gauss section
      a2 = norm_pk * (sGrid(ii+1) - tmp_gdpt )    ! flat top section
      s_integral(ii) = a1 + a2

    else if (    (sGrid(ii) > sftst  .and.  sGrid(ii) < sg2st )   .and. (sGrid(ii+1) > sftst  .and.  sGrid(ii+1) < sg2st )  ) then

    ! Flat top section

      s_integral(ii) = norm_pk * (sGrid(ii+1) - sGrid(ii) )

    else if (    (sGrid(ii) > sftst  .and.  sGrid(ii) < sg2st )   .and. (sGrid(ii+1) > sg2st  .and.  sGrid(ii+1) < sEd )  ) then

    ! A mix of flat-top and gaussian sections

      tmp_gdpt = sg2cen ! temp grid point

      call AreaNDist( (/tmp_gdpt,  sGrid(ii+1)/), sg2cen, sig_edj, tres)


      a1 = norm_pk * (tmp_gdpt - sGrid(ii)) ! Flat top section
      a2 = tres    ! Gaussian section
      s_integral(ii) = a1 + a2


    else if (    (sGrid(ii) > sg2st)   .and. (sGrid(ii+1) > sg2st)  ) then

    ! Last gaussian section

       call AreaNDist(sGrid(ii:ii+1), sg2cen, sig_edj, tres)
       s_integral(ii) = tres

    else 

      iStrange = iStrange + 1_ip

    end if


  end do

  if (qPara) then

    CALL MPI_ALLREDUCE(iStrange, iT, 1, MPI_INTEGER, &
         MPI_SUM, MPI_COMM_WORLD, error)

    iStrange = iT

  end if 

  if (iStrange > 0) then
    
    if (tProcInfo_G%qRoot) print*, 'ERROR IN subroutine flattop2: perhaps the beam is not sampled finely enough'
    if (tProcInfo_G%qRoot) print*, 'sum of iStrange was : ', iStrange

    call mpi_finalize(error)
    stop

  end if


! Sum over integrals and normalize result

  tot_ar = sum(s_integral)

  if (qPara) then

    CALL MPI_ALLREDUCE(MPI_IN_PLACE, tot_ar, 1, MPI_DOUBLE_PRECISION, &
         MPI_SUM, MPI_COMM_WORLD, error)
    
  end if

  s_integral = s_integral / tot_ar

  print*, tot_ar

end subroutine flattop2




subroutine AreaNDist(s_gridPoints, s_mean, s_sigma, elem)

  real(kind=wp), intent(in) :: s_gridPoints(2)
  real(kind=wp), intent(in) :: s_mean, s_sigma
  real(kind=wp), intent(out) :: elem

  real(kind=wp) :: s_value1, s_value2


  s_value2=(s_gridPoints(2)-s_mean)/&
       (s_sigma*SQRT(2.0_WP))

  s_value1=(s_gridPoints(1)-s_mean)/&
       (s_sigma*SQRT(2.0_WP))

  elem = 0.5_WP *&
       (erf(s_value2)-erf(s_value1))


end subroutine AreaNDist




END MODULE particleFunctions
