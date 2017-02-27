!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Module to check various parameters specified in the input file
!> by the user

module checks

use Paratype
use ParallelInfoType
use TransformInfoType
use IO
use typesAndConstants
use Globals
use particleFunctions
use grids

implicit none

contains

subroutine CheckParameters(sLenEPulse,iNumElectrons,nbeams,&
       sLengthofElm,iNodes,sWigglerLength,sStepSize,&
       nSteps,srho,saw,sgammar,focusfactor,mag,sSigE,f_x, f_y, &
       qSwitches,qSimple,sSigF, &
       freqf, SmeanZ2, qFlatTopS, nseeds, qOK)

  implicit none

! Subroutine to check that the electron and field
! parameters are sensible.
!
! Code for the checks originally written by Cynthia Nam
! and Dr Pamela Johnston. This subroutine was initially
! created by collecting the code from parts of an older
! code. It has been modified further to include other checks,
! and modified to be compatible with the fuller
! parallelization of this code.
!
!                ARGUMENTS

  REAL(KIND=WP),INTENT(INOUT) :: sLenEPulse(:,:)
  INTEGER(KIND=IP),INTENT(INOUT) :: iNumElectrons(:,:)
  INTEGER(KIND=IP), INTENT(IN) :: nbeams, nseeds
  REAL(KIND=WP),INTENT(INOUT) :: sLengthofElm(:)
  INTEGER(KIND=IP),INTENT(INOUT) :: iNodes(:)
  REAL(KIND=WP), INTENT(INOUT) :: sWigglerLength(:)
  REAL(KIND=WP), INTENT(INOUT) :: sStepSize
  INTEGER(KIND=IP), INTENT(INOUT) :: nSteps
  REAL(KIND=WP), INTENT(IN) :: srho,saw,sgammar,focusfactor
  real(kind=wp), intent(in) :: mag(:)
  REAL(KIND=WP), INTENT(INOUT) :: sSigE(:,:), sSigF(:,:), &
                                  freqf(:), SmeanZ2(:)
  REAL(KIND=WP), INTENT(INOUT) :: f_x, f_y
  LOGICAL, INTENT(INOUT) :: qSwitches(:)
  logical, intent(in) :: qSimple, qFlatTopS(:)
  LOGICAL, INTENT(OUT)  :: qOK

!           Local vars

  INTEGER(KIND=IP) :: i,nninner, error
  REAL(KIND=WP) :: sEE,dx,lwx,lexm,maxr
  LOGICAL :: qOKL  

  qOK = .FALSE.

  if (qSimple) call check1D(qSwitches(iOneD_CG), qSwitches(iDiffraction_CG), qSwitches(iFocussing_CG), &
  	                        iNodes)

  do i = 1,nbeams

    if (qSimple) then

      call chkESampleLens(sLenEPulse(i,:),iNumElectrons(i,:), srho,qSwitches(iOneD_CG),qOKL)
      if (.NOT. qOKL) goto 1000

    end if  

  end do

  call stpFSampleLens(iNodes,sWigglerLength,sLengthOfElm,qSwitches(iOneD_CG),qOKL)

  if (qSimple) then

    call chkFSampleLens(iNodes,sWigglerLength,sLengthOfElm,srho,qOKL)
    if (.NOT. qOKL) goto 1000

    call checkIntSampling(sStepSize,nSteps,sLengthOfElm,qSwitches,qOKL)
    if (.NOT. qOKL) goto 1000

    call checkFreeParams(srho,saw,sgammar,f_x,f_y,focusfactor,qOKL)
    if (.NOT. qOKL) goto 1000

    call checkOscMag(sgammar, mag, nbeams)

    call checkRndEjLens(iNumElectrons, sLenEPulse, sSigE, nbeams)

!   Work out charge for each beam...

!    call calcCharge()

 
    do i = 1, nSeeds
  
      call chkFldBnds(qFlatTopS(i), qRndFj_G(i), sSigF(i,iZ2_CG), sSigFj_G(i), & 
                      SmeanZ2(i))
  
    end do


  end if






  if (tProcInfo_G%qRoot) print '(I5,1X,A17,1X,F8.4)',nsteps,&
       'Step(s) and z-bar=',nsteps*sStepSize
  
!     Set error flag and exit

  qOK = .true.

  goto 2000

1000 call Error_log('Error in setupcalcs:CheckParameters',tErrorLog_G)

2000 continue

end subroutine CheckParameters
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!subroutine calcCharge()
!
!! ! This has to be called AFTER the beam is matched and so forth
!
!if (qHardEdgeX) then
!    r_av = sig_av                 # Hard edged circle in x and y
!else
!    r_av = sqrt(2) * sig_av  # Gaussian dist in x and y
!end if
!
!tArea = pi * r_av**2.0_wp          # Tranverse beam area
!
!if (qFlatTopZ2) then
!    if (qRoundZ2) then
!        lArea = sqrt(2.0_wp*pi) * sigRoundz  &
!               + (sigz-(gExtEj_G*sigRoundz))     # flat top + gaussian
!    else
!        lArea = sigz                  # longitudinal integral over charge dist (flat top)
!    end if
!else
!    lArea = sqrt(2*pi) * sigz     # longitudinal integral over charge dist(gaussian)
!end if
!
!n_p = N / (tArea * lArea)              # Electron number density
!
!end subroutine calcCharge

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine chkFldBnds(qF, qR, sig, sigEj, cen)

! Check Field Bounds
!
! If the front (or head) of the seed field in z2
! is < 0, shift to +ive z2, so that the front lies
! at z2=0.
!

  real(kind=wp), intent(in) :: sig, sigEj
  logical, intent(in) :: qF, qR  
  real(kind=wp), intent(inout) :: cen

  real(kind=wp) :: start, tl, shft



!       Get Total Length (tl) of seed field


  if (qF) then

    if (qR) then

!     flat top w/ rounded edge:

      tl = sigEj * gExtEj_G  +  2.0_wp * sig

    else 

!     Flat top only

      tl = 2.0_wp * sig

    end if

  else 

!     Gaussian seed field case

    tl = sig * gExtEj_G

  end if

  start =  cen - tl / 2.0_wp


  if (start < 0.0_wp) then

    shft = 0.0_wp - start
    cen = cen + shft


  end if 



end subroutine chkFldBnds















!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine check1D(qOneD, qDiffraction, qfocusing, iNodes)

! If 1D, make sure no diffraction or focusing is employed
!
! -Lawrence

  logical, intent(in) :: qOneD
  logical, intent(inout) :: qDiffraction, qfocusing
  integer(kind=ip), intent(inout) :: iNodes(:)

  if (qOneD) then

    qDiffraction = .false.
    qfocusing = .false.

    iNodes(iX_CG) = 1_IP
    iNodes(iY_CG) = 1_IP

  end if


end subroutine check1D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE stpFSampleLens(iNodes,sWigglerLength,sLengthOfElm,qOneD,qOK)

  INTEGER(KIND=IP), INTENT(INOUT) :: iNodes(:)
  REAL(KIND=WP), INTENT(IN) :: sWigglerLength(:)
  LOGICAL, INTENT(IN) :: qOneD
  REAL(KIND=WP), INTENT(OUT) :: sLengthOfElm(:)
  LOGICAL, INTENT(OUT) :: qOK


  qOK = .FALSE.

  IF (qOneD) THEN
  
    iNodes(iX_CG) = 1_IP
    iNodes(iY_CG) = 1_IP
    
    sLengthOfElm(iX_CG) = 1.0_WP
    sLengthOfElm(iY_CG) = 1.0_WP
    
  ELSE
  
    IF (iNodes(iX_CG)==1_IP) THEN
      CALL Error_log('The field in x is sampled with only one node...',tErrorLog_G)
      CALL Error_log('...and you have not specified a 1D option in the input file.',tErrorLog_G)
      GOTO 1000
    END IF

    IF (iNodes(iY_CG)==1_IP) THEN
      CALL Error_log('The field in x is sampled with only one node...',tErrorLog_G)
      CALL Error_log('...and you have not specified a 1D option in the input file.',tErrorLog_G)
      GOTO 1000
    END IF  

    sLengthOfElm(iX_CG) = sWigglerLength(iX_CG) / (iNodes(iX_CG) - 1_IP)
    sLengthOfElm(iY_CG) = sWigglerLength(iY_CG) / (iNodes(iY_CG) - 1_IP)

  END IF

  sLengthOfElm(iZ2_CG) = sWigglerLength(iZ2_CG) / (iNodes(iZ2_CG) - 1_IP)


!     Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:stpFSampleLens',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE stpFSampleLens


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine checkOscMag(sgammar, mag, nbeams)

  real(kind=wp), intent(in) :: sgammar, mag(:)
  integer(kind=ip), intent(in) :: nbeams

  integer(kind=ip) :: ii, error

  do ii = 1, nbeams

    if (sgammar <= mag(ii)) then

      PRINT*, 'ERROR: Magnitude of beam oscillation is too large in beam number ', ii
      print*, 'ERROR: Ensure magnitude of beam energy modulation is < gamma_r'
  
      call MPI_FINALIZE(error)
      STOP

    end if

  end do

end subroutine checkOscMag


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE chkESampleLens(sLenEPulse,iNumElectrons,rho,qOneD,qOK)

!                 ARGUMENTS

  REAL(KIND=WP), INTENT(INOUT) :: sLenEPulse(:)
  REAL(KIND=WP), INTENT(IN) ::  rho
  INTEGER(KIND=IP), INTENT(OUT) :: iNumElectrons(:)
  LOGICAL, INTENT(IN) :: qOneD
  LOGICAL, INTENT(INOUT) :: qOK

!                 LOCAL ARGS

  INTEGER(KIND=IP) :: i
  REAL(KIND=WP) :: wlen, maxspcing, spcing
  
  qOK = .FALSE.

  if (qOneD) then
    iNumElectrons(iX_CG)  = 1_IP
    iNumElectrons(iY_CG)  = 1_IP
    iNumElectrons(iPX_CG) = 1_IP
    iNumElectrons(iPY_CG) = 1_IP
  end if

  wlen = 4.0_WP * pi * rho
  maxspcing = wlen / 8.0_WP

  spcing = sLenEPulse(iZ2_CG) / iNumElectrons(iZ2_CG)
 
!     Check sampling

  IF (spcing > maxspcing) THEN
    CALL Error_log('Macroparticle spacing in z2 > 1/8th of resonant wavelength',tErrorLog_G)
    GOTO 1000
  END IF 
    
  DO i= 1, SIZE(sLenEPulse)

    IF (sLenEPulse(i) <=0.0_WP .AND. iNumElectrons(i) > 1_IP) THEN
      CALL Error_log('Negative or zero electron pulse length',tErrorLog_G)
      GOTO 1000
    END IF

    IF (iNumElectrons(i) < 1_IP) THEN
      CALL Error_log('Number of electrons is less than 1 in one or more dimensions.',tErrorLog_G)
      GOTO 1000
    END IF

!     Check length of electron pulse is valid

    IF (iNumElectrons(i) == 1_IP) THEN
      sLenEPulse(i)=1.0_WP
    END IF

  END DO


!     Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:chkESampleLens',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE chkESampleLens

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine checkRndEjLens(iNMP, eSamLen, sigs, nbeams)

    integer(kind=ip), intent(in) :: iNMP(:,:), nbeams
    real(kind=wp), intent(in) :: eSamLen(:,:), sigs(:,:)
    
    integer(kind=ip) :: inttypes(6_ip), ib
    real(kind=wp) :: gausslen
    logical :: qOKL



    do ib = 1, nBeams

      call getIntTypes(iNMP(ib,:), eSamLen(ib,:), sigs(ib,:), inttypes)
      
      if (inttypes(iZ2_CG) == iTopHatDistribution_CG) then

        if (qRndEj_G(ib)) then

          gausslen = sSigEj_G(ib) * gExtEj_G !    Check if there is enough room for the rounded edges
          
          if ((eSamLen(ib,iZ2_CG) - gausslen) <= 0) then

            print*, 'ERROR:- electron beam model not long enough in z2 to include the gaussian tails'
            call UnDefineParallelLibrary(qOKL)
            stop

          end if

        end if

      end if

    end do


end subroutine checkRndEjLens




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




SUBROUTINE chkFSampleLens(iNodes,sWigglerLength,sLengthOfElm,rho,qOK)

!                  ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: iNodes(:)
  REAL(KIND=WP), INTENT(IN) :: sWigglerLength(:), sLengthOfElm(:),rho
  LOGICAL, INTENT(OUT) :: qOK

!                  LOCAL ARGS

  INTEGER(KIND=IP) :: i
  REAL(KIND=WP) :: wlen,maxspcing

  qOK = .FALSE.

  wlen = 4.0_WP * pi * rho
  maxspcing = wlen / 8.0_WP

!     Check got nodes set in every direction
  
  DO i = 1, SIZE(iNodes)

    IF (iNodes(i) <= 0_IP) THEN
      CALL Error_log('Number of nodes <=0 in one or more dimensions',tErrorLog_G)
      GOTO 1000
    END IF

  END DO
 
!     Check length of elements in field are valid

  DO i = 1, SIZE(sLengthOfElm)

    IF(sLengthOfElm(i) <= 0_IP) THEN
      CALL Error_log('Length of elements <=0 in one or more dimensions.',tErrorLog_G)
      GOTO 1000
    END IF

    IF (sLengthOfElm(iZ2_CG) > maxspcing) THEN
      CALL Error_log('Length of field elements > 1/8th of resonant wavelength in z2.',tErrorLog_G)
      GOTO 1000
    END IF
    
  END DO

!     Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:chkFSampleLens',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE chkFSampleLens

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  
SUBROUTINE checkIntSampling(sStepSize,nSteps,sLengthOfElm,qSwitches,qOK)

!                 ARGUMENTS

  REAL(KIND=WP), INTENT(IN) :: sStepSize,sLengthOfElm(:)
  INTEGER(KIND=IP), INTENT(IN) :: nSteps
  LOGICAL, INTENT(IN) :: qSwitches(:)
  LOGICAL, INTENT(OUT) :: qOK

  qOK = .FALSE.

!     Check if the stepsize is OK

  IF(sStepSize <= 0.0_WP) THEN
    CALL Error_log('Step size <=0.',tErrorLog_G)
    GOTO 1000
  END IF
  
  !IF (qSwitches(iFieldEvolve_CG)) THEN

    !IF( sStepSize >= (0.5_WP * sLengthOfElm(iZ2_CG))) THEN
      !CALL Error_log('The integration step size is too large.',tErrorLog_G)
      !GOTO 1000
    !END IF

  !END IF

  IF(nSteps <= 0_IP) THEN
    CALL Error_log('Number of steps <=0.',tErrorLog_G)
    GOTO 1000
  END IF

!     If nsteps not set then set to one Raleigh length

!  IF (nSteps <= 0_IP) THEN

!    nSteps = CEILING((sSigE(iX_CG)**2 &
!      / (2.0_WP *srho)) / sStepSize)

!  END IF

!     Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:checkIntSampling',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE checkIntSampling

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE getElmLengths(sLenEPulse,sWigglerLength,iNodes,qOneD,sLengthOfElm)

  REAL(KIND=WP), INTENT(IN) :: sLenEPulse(3), sWigglerLength(3)
  INTEGER(KIND=IP), INTENT(IN) :: iNodes(3)
  LOGICAL, INTENT(IN) :: qOneD
  REAL(KIND=WP), INTENT(OUT) :: sLengthOfElm(3)

!     Calculate field element lengths, dx dy dz2

  IF (qOneD) THEN

    sLengthOfElm(iX_CG:iY_CG) = 1.0_WP

  ELSE

    sLengthOfElm(iX_CG:iY_CG) = sWigglerLength(iX_CG:iY_CG)&
                      / REAL(iNodes(iX_CG:iY_CG)-1,KIND=WP)

  END IF

  sLengthOfElm(iZ2_CG) = (sLenEPulse(iZ2_CG) + &
                          sWigglerLength(iZ2_CG)) / &
                          REAL(iNodes(iZ2_CG)-1,KIND=WP) 

END SUBROUTINE getElmLengths

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE checkFreeParams(srho,saw,sgammar,f_x,f_y,focusfactor,qOK)

  REAL(KIND=WP), INTENT(IN) :: srho,saw,sgammar,f_x,f_y,focusfactor
  LOGICAL, INTENT(OUT) :: qOK

  qOK = .FALSE.

!     Check got valid value for rho

  IF (srho <= 0.0_WP) THEN
    CALL Error_log('Parameter rho <=0.',tErrorLog_G)
    GOTO 1000    
  END IF

!     Check got valid value for aw and gamma
  
  IF (saw <= 0.0_WP) THEN
    CALL Error_log('Parameter saw <=0.',tErrorLog_G)
    GOTO 1000    
  END IF
  
  IF (sgammar <= 0.0_WP) THEN
    CALL Error_log('Parameter gamma_r <= 0.',tErrorLog_G)
    GOTO 1000    
  END IF
    
!     Check got valid value for fx or fy

  IF (.NOT. (f_x==1.0_WP .OR. f_y==1.0_WP)) THEN
    CALL Error_log('Either fx OR fy must be 1.',tErrorLog_G)
    GOTO 1000
  END IF

!     Check got valid value for focusing factor

  IF (focusfactor<=0_WP) THEN
    CALL Error_log('focusfactor must be > 0.',tErrorLog_G)
    GOTO 1000
  END IF

!     Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:checkFreeParams',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE checkFreeParams

end module checks
