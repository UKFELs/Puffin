MODULE particleFunctions

USE paratype
USE ParallelInfoType
USE error_fn
USE Functions
USE typesAndConstants
USE FileType
USE IO

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
END MODULE particleFunctions
