MODULE lattice

USE paratype
USE Globals
USE ArrayFunctions
USE ElectronInit

IMPLICIT NONE

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE readLatt(lattFile,zMod,delta,D,Dfact,ModNum,taper,rho,&
                      sStepSize)

  IMPLICIT NONE

! Subroutine to read the information from the lattice file
! and define some of the dispersion parameters.
!
! Modified from Brian McNeil's 1D lattice FEL code 
! to work with this 3D unaveraged model.
!
!                ARGUMENTS
! 
! lattFile      The name of the lattice file (INPUT)
! zMod          Array containing the cumulative 
!               interaction lengths of each module in 
!               z-bar (OUTPUT)
! delta         Array containing lengths of dispersive 
!               sections in slippage lengths (OUTPUT)
! D             Dispersive factor of the chicane, 
!               D=10*Dfact/6*delta (OUTPUT)
! Dfact         Dispersive strength of the chicane (INPUT)
! ModNum        Number of Modules (INPUT)
! rho           FEL parameter

  CHARACTER(32_IP), INTENT(IN) :: lattFile
  REAL(KIND=WP), DIMENSION(:), INTENT(INOUT) :: zMod,delta,D
  REAL(KIND=WP), INTENT(IN) :: Dfact
  REAL(KIND=WP), INTENT(INOUT)  ::  taper
  REAL(KIND=WP), INTENT(IN) :: rho
  INTEGER(KIND=IP), INTENT(IN) :: ModNum
  REAL(KIND=WP), INTENT(OUT)   :: sStepSize

!                LOCAL VARS

  INTEGER(KIND=IP)   :: i,ios,nw,error,ri,NL
  REAL(KIND=WP)      :: pi,c1

  OPEN(1,FILE=lattFile, IOSTAT=ios, ACTION='READ', POSITION ='REWIND')
  IF (ios /= 0_IP) STOP "OPEN(input file) not performed correctly, IOSTAT /= 0"

  pi = 4.0_WP*ATAN(1.0_WP)
  c1 = 2.0_WP*rho


!     Read whitespace

NL = 31_IP      !    Number of lines in header

do ri = 1,NL

  read (1,*)

end do

!     Read module data from lattice file

  DO i=1,ModNum

    READ (1,*) nw, delta(i), mf(i), delmz(i), tapers(i)  !, resFactor(i) ! Wiggler periods, Chicane slippage periods, aw shift, stepsize

!     Calculate cumulative interaction length of modules

    IF (i==1) THEN  
      zMod(i) = REAL(nw,KIND=WP)
      zMod(i) = 2.0_WP*pi*c1*zMod(i)
    ELSE
      zMod(i) = zMod(i-1)+2.0_WP*pi*c1*REAL(nw,KIND=WP) 
    END IF

  END DO
	
  CLOSE(1, STATUS='KEEP')

!     Convert from # undulator periods to z-bar

  delta = 2.0_WP*pi*c1*delta
  D = Dfact*10.0/6.0*delta ! The Dispersion parameter

  sStepSize =  delmz(1)
  taper = tapers(1)

  END SUBROUTINE readLatt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE disperse(y_e,D,delta,i,sStepSize,sZ)

  IMPLICIT NONE

! Subroutine to apply phase changes to electrons beam
! due to passing through a chicane...
! Modified from Brian McNeil's Phase shift routine to 
! work with p2 (electron momentum in z2 frame) as opposed
! to bar{p}_z
!
!              ARGUMENTS
!
! y_e              electron array data
! D                Dispersion parameter for the chicane
!                  (D=10*Dfact/6*delta, Dfact is the
!                  dispersive strength factor of the chicane
! delta            Slippage in resonant wavelengths
	
  REAL(KIND=WP), DIMENSION(:), INTENT(INOUT) :: y_e
  REAL(KIND=WP), INTENT(IN) :: D,delta
  INTEGER(KIND=IP), INTENT(IN) :: i
  REAL(KIND=WP), INTENT(OUT) :: sStepSize
  REAL(KIND=WP), INTENT(INOUT) :: sZ

  INTEGER(KIND=IP)  ::  e_tot, z2_start, z2_end, Q_start, Q_end
 
  REAL(KIND=WP), ALLOCATABLE :: sgamma_j(:),spx0_offset(:),spy0_offset(:), &
                                sx_offset(:),sy_offset(:)

  REAL(KIND=WP)     :: shift_corr, awo, sx_offseto, sx_offsetn, &
                       sy_offseto, sy_offsetn, sZ_new, spx0_offseto, &
                       spy0_offseto, spx0_offsetn, spy0_offsetn, &
                       beta_av, sEta_eff, sl1

  LOGICAL :: qOKL





  e_tot = iGloNumElectrons_G

  z2_start = iBStartPosition_G(iRe_z2_CG)
  z2_end = iBEndPosition_G(iRe_z2_CG)

  Q_start = iBStartPosition_G(iRe_Q_CG)
  Q_end = iBEndPosition_G(iRe_Q_CG)









!     NEW FOR SWISSFEL - TWO COLOUR UNDULATOR SCHEME

!     Factors for redefining undulator parameter in
!     2 colour undulator scheme.




!     Match beam to new undulator module - only need to 
!     do this if diffrent from last module


  !IF (mf(i+1) /= mf(i)) THEN


!     Find gamma for each electron

    sl1 = 2.0_WP*sAw_G**2.0_WP/ (fx_G**2.0_WP + fy_G**2.0_WP)

    ALLOCATE(sgamma_j(iNumberElectrons_G))

    sgamma_j = SQRT((1.0_WP + ( sl1 * (Vector(iRe_PPerp_CG,y_e)**2.0_WP  &
                                   + Vector(iIm_PPerp_CG,y_e)**2.0_WP) )) * &
                  (1.0_WP + sEta_G * Vector(iRe_Q_CG,y_e) )**2.0_WP / &
                  ( sEta_G * Vector(iRe_Q_CG,y_e) * &
                              (sEta_G * Vector(iRe_Q_CG,y_e) + 2.0_WP) ) )





!          For chicanes 


  y_e(z2_start:z2_end) = y_e(z2_start:z2_end) - &
                         2.0_WP * D *  &
                         (sgamma_j - sGammaR_G) / sGammaR_G &
                         + delta








!     Get p_perp and x, y offsets for this undulator module

    ALLOCATE(spx0_offset(iNumberElectrons_G), spy0_offset(iNumberElectrons_G))
    ALLOCATE(sx_offset(iNumberElectrons_G),sy_offset(iNumberElectrons_G))

    spx0_offset    = -fy_G * n2col * COS(sZ / (2.0_WP * sRho_G))
    spy0_offset    = fx_G * n2col * SIN(sZ / (2.0_WP * sRho_G))


    sx_offset = -4.0_WP * SQRT(2.0_WP) * sFocusfactor_G * sKBeta_G * &
                 n2col * (sRho_G**2.0_WP) /  &
                 ( SQRT(fx_G**2.0_WP + fy_G**2.0_WP) * sEta_G ) * &
                 sGammaR_G / sgamma_j * (1.0_WP + sEta_G * Vector(iRe_Q_CG,y_e)) *  &
                 fy_G * sin(sZ / (2.0_WP * sRho_G) )

    sy_offset = 4.0_WP * SQRT(2.0_WP) * sFocusfactor_G * sKBeta_G * &
                 n2col * (sRho_G**2.0_WP) /  &
                 ( SQRT(fx_G**2.0_WP + fy_G**2.0_WP) * sEta_G ) * &
                 sGammaR_G / sgamma_j * (1.0_WP + sEta_G * Vector(iRe_Q_CG,y_e)) *  &
                 fx_G * cos(sZ / (2.0_WP * sRho_G) )


!     Take off tranverse phase space offsets to center the beam

    CALL PutValueInVector(iRe_X_CG, &
            Vector(iRe_X_CG,y_e) - sx_offset, &
            y_e,    &
            qOKL)

    CALL PutValueInVector(iRe_Y_CG, &
            Vector(iRe_Y_CG,y_e) - sy_offset, &
            y_e,    &
            qOKL)   


    CALL PutValueInVector(iRe_PPerp_CG, &
            Vector(iRe_PPerp_CG,y_e) - spx0_offset, &
            y_e,    &
            qOKL)

    CALL PutValueInVector(iIm_PPerp_CG, &
            Vector(iIm_PPerp_CG,y_e) - spy0_offset, &
            y_e,    &
            qOKL) 



!     Change undulator tuning factor to next undulator module

    n2col0 = mf(i+1)
    n2col = mf(i+1)
    undgrad = tapers(i+1)
    sz0 = sz
    

!     Get new pperp offsets with new undulator tuning factors

    spx0_offset    = -fy_G * n2col * COS(sZ / (2.0_WP * sRho_G))
    spy0_offset    = fx_G * n2col * SIN(sZ / (2.0_WP * sRho_G))


!     Add on new offset to perp momentum


    CALL PutValueInVector(iRe_PPerp_CG, &
            Vector(iRe_PPerp_CG,y_e) + spx0_offset, &
            y_e,    &
            qOKL)

    CALL PutValueInVector(iIm_PPerp_CG, &
            Vector(iIm_PPerp_CG,y_e) + spy0_offset, &
            y_e,    &
            qOKL) 



!     Get new p2 required to keep energy constant


!          beta = SQRT( 1.0_WP - ((1.0_WP/sgamma_j**2) * (1.0_WP + ( sl1 * ( Vector(iRe_PPerp_CG,y_e)**2.0_WP + Vector(iIm_PPerp_CG,y_e)**2.0_WP  ) ) ) ) )
!     p2 = (1/eta) * ( (1/beta_z)  - 1)


    CALL PutValueInVector(iRe_Q_CG, &
            1.0_WP/sEta_G * ( ( 1.0_WP / ( SQRT( 1.0_WP - ((1.0_WP/sgamma_j**2) * &
            (1.0_WP + ( sl1 * &
            ( Vector(iRe_PPerp_CG,y_e)**2.0_WP + Vector(iIm_PPerp_CG,y_e)**2.0_WP  ) ) ) &
             ) ) ) ) - 1.0_WP   )   , &
            y_e,    &
            qOKL) 


!     Get new x offsets and add on to centered beam



    sx_offset = -4.0_WP * SQRT(2.0_WP) * sFocusfactor_G * sKBeta_G * &
                 n2col * (sRho_G**2.0_WP) /  &
                 ( SQRT(fx_G**2.0_WP + fy_G**2.0_WP) * sEta_G ) * &
                 sGammaR_G / sgamma_j * (1.0_WP + sEta_G * Vector(iRe_Q_CG,y_e)) *  &
                 fy_G * sin(sZ / (2.0_WP * sRho_G) )

    sy_offset = 4.0_WP * SQRT(2.0_WP) * sFocusfactor_G * sKBeta_G * &
                 n2col * (sRho_G**2.0_WP) /  &
                 ( SQRT(fx_G**2.0_WP + fy_G**2.0_WP) * sEta_G ) * &
                 sGammaR_G / sgamma_j * (1.0_WP + sEta_G * Vector(iRe_Q_CG,y_e)) *  &
                 fx_G * cos(sZ / (2.0_WP * sRho_G) )  


!     Add on new offsets to initialize beam for new undulator module

    CALL PutValueInVector(iRe_X_CG, &
            Vector(iRe_X_CG,y_e) + sx_offset, &
            y_e,    &
            qOKL)

    CALL PutValueInVector(iRe_Y_CG, &
            Vector(iRe_Y_CG,y_e) + sy_offset, &
            y_e,    &
            qOKL)   



    DEALLOCATE(sgamma_j,spx0_offset,spy0_offset,sx_offset,sy_offset)

!     Work out new effective eta


    beta_av = SQRT(sGammaR_G**2.0_WP - 1.0_WP - (saw_G)**2.0_WP) / &
                   sGammaR_G

    sEta_eff = (1.0_WP - beta_av) / beta_av

!     Ratio between effective and initial etas:-

    m2col = sEta_eff / sEta_G


  !END IF


  sStepSize = delmz(i+1) ! Change step size - make sure is still integer
                         ! of 4pirho in input file!!


!!!!!!!!!!!!!!!! SCRAP !!!!!!!!!!!!!!!!!!!!!!!!!



!     New offsets for beginning of new undulator


!    sx_offsetn = xOffSet(sRho_G, &   
!                   sAw_G,  &
!                   sGammaR_G, &
!                   sEta_G, &
!                   1.0_WP,&
!                   -fy_G*COS(sZ / (2.0_WP * sRho_G)),&
!                   -fx_G * SIN(sZ / (2.0_WP * sRho_G)),&
!                   fx_G,&
!                   fy_G,&
!                   sZ)

!    sy_offsetn =  yOffSet(sRho_G, &
!                   sAw_G,  &
!                   sGammaR_G, &
!                   sEta_G, &
!                   1.0_WP,&
!                   -fy_G*COS(sZ / (2.0_WP * sRho_G)),&
!                   -fx_G * SIN(sZ / (2.0_WP * sRho_G)),&
!                   fx_G,&
!                   fy_G,&
!                   sZ)
  

!    spx0_offsetn    = -fy_G*COS(sZ / (2.0_WP * sRho_G))
!    spy0_offsetn    = fx_G * SIN(sZ / (2.0_WP * sRho_G))


!     First, take transverse displacement off i.e. center the beam.

 


!     Then, add on new offset for this undulator

!    CALL PutValueInVector(iRe_X_CG, &
!            Vector(iRe_X_CG,y_e) + sx_offsetn, &
!            y_e,    &
!            qOKL)

!    CALL PutValueInVector(iRe_Y_CG, &
!            Vector(iRe_Y_CG,y_e) + sy_offsetn, &
!            y_e,    &
!            qOKL)

!    CALL PutValueInVector(iRe_PPerp_CG, &
!            Vector(iRe_PPerp_CG,y_e) + spx0_offsetn, &
!            y_e,    &
!            qOKL)

!    CALL PutValueInVector(iIm_PPerp_CG, &
!            Vector(iIm_PPerp_CG,y_e) + spy0_offsetn, &
!            y_e,    &
!            qOKL) 
























!     Get new beta_z for each electron to keep same
!     energy.....






!     Work out new effective eta
!
!
!  beta_av = SQRT(sGammaR_G**2.0_WP - 1.0_WP - saw_G**2.0_WP) / &
!                 sGammaR_G
!
!  sEta_eff = (1.0_WP - beta_av) / beta_av
!
!!     Ratio between effective and initial etas:-
!
!  m2col = sEta_eff / sEta_G
!
!
!
!
!
!
!
!
!
!
!!     ...and then get corresponding p_2 for each electron
!
!
!
!
!  DEALLOCATE(sgamma_j)
!
!
!
!
!
!
!
!
!
!
!
!  spx0_offseto    = -fy_G*COS(sZ / (2.0_WP * sRho_G))
!  spy0_offseto    = fx_G * SIN(sZ / (2.0_WP * sRho_G))
!
!  sx_offseto = xOffSet(sRho_G, &
!                 sAw_G,  &
!                 sGammaR_G, &
!                 sEta_G, &
!                 1.0_WP,&
!                 -fy_G*COS(sZ / (2.0_WP * sRho_G)),&
!                 -fx_G * SIN(sZ / (2.0_WP * sRho_G)),&
!                 fx_G,&
!                 fy_G,&
!                 sZ)
!
!
!
!  sy_offseto =  yOffSet(sRho_G, &
!                 sAw_G,  &
!                 sGammaR_G, &
!                 sEta_G, &
!                 1.0_WP,&
!                 -fy_G*COS(sZ / (2.0_WP * sRho_G)),&
!                 -fx_G * SIN(sZ / (2.0_WP * sRho_G)),&
!                 fx_G,&
!                 fy_G,&
!                 sZ)
!
!
!
!!     Change to new undulator parameters for the new wiggler module
!
!  awo = sAw_G ! Save current aw val
!
!  saw_G = saw_save_G * mf(i+1)
!  srho_G = srho_save_G * (mf(i+1))**(2.0_WP/3.0_WP)
!  sFocusfactor_G = sFocusfactor_save_G * mf(i+1)
!
!  sStepSize = delmz(i+1) ! Change step size so get whole number of steps
!                         ! per undulator period.
!
!
!  beta_av = SQRT(sGammaR_G**2.0_WP - 1.0_WP - saw_G**2.0_WP) / &
!                 sGammaR_G
!
!  sEta_G = (1.0_WP - beta_av) / beta_av
!
!!     Work out correction to match radiation phases
!
!  IF (awo /= sAw_G) THEN ! Only need to do this if different from last module
!
!
!
!
!    sZ_new = CEILING(sZ / (4.0_WP * pi * sRho_G)) * (4.0_WP * pi * sRho_G)
!
!    shift_corr = sZ_new - sZ
!
!    sZ = sZ + shift_corr    ! Shift zbar to correct oscillation...
!
!    y_e(z2_start:z2_end) = y_e(z2_start:z2_end) + &     ! Shift z2 to correct oscillation
!                              shift_corr
!
!!     Adjust zmod (cumulative interaction lengths) according to phase adjustment
!
!    zMod = zMod + shift_corr
!
!!     Need to shift transverse displacement of beam for new undulator
!
!    sx_offsetn = xOffSet(sRho_G, &   ! New offsets for beginning of new undulator
!                   sAw_G,  &
!                   sGammaR_G, &
!                   sEta_G, &
!                   1.0_WP,&
!                   -fy_G*COS(sZ / (2.0_WP * sRho_G)),&
!                   -fx_G * SIN(sZ / (2.0_WP * sRho_G)),&
!                   fx_G,&
!                   fy_G,&
!                   sZ)
!
!    sy_offsetn =  yOffSet(sRho_G, &
!                   sAw_G,  &
!                   sGammaR_G, &
!                   sEta_G, &
!                   1.0_WP,&
!                   -fy_G*COS(sZ / (2.0_WP * sRho_G)),&
!                   -fx_G * SIN(sZ / (2.0_WP * sRho_G)),&
!                   fx_G,&
!                   fy_G,&
!                   sZ)
!  
!
!    spx0_offsetn    = -fy_G*COS(sZ / (2.0_WP * sRho_G))
!    spy0_offsetn    = fx_G * SIN(sZ / (2.0_WP * sRho_G))
!
!!     First, take transverse displacement off i.e. center the beam.
!
! 
!    CALL PutValueInVector(iRe_X_CG, &
!            Vector(iRe_X_CG,y_e) - sx_offseto, &
!            y_e,    &
!            qOKL)
!
!    CALL PutValueInVector(iRe_Y_CG, &
!            Vector(iRe_Y_CG,y_e) - sy_offseto, &
!            y_e,    &
!            qOKL)   
!
!
!    CALL PutValueInVector(iRe_PPerp_CG, &
!            Vector(iRe_PPerp_CG,y_e) - spx0_offseto, &
!            y_e,    &
!            qOKL)
!
!    CALL PutValueInVector(iIm_PPerp_CG, &
!            Vector(iIm_PPerp_CG,y_e) - spy0_offseto, &
!            y_e,    &
!            qOKL) 
!
!!     Then, add on new offset for this undulator
!
!    CALL PutValueInVector(iRe_X_CG, &
!            Vector(iRe_X_CG,y_e) + sx_offsetn, &
!            y_e,    &
!            qOKL)
!
!    CALL PutValueInVector(iRe_Y_CG, &
!            Vector(iRe_Y_CG,y_e) + sy_offsetn, &
!            y_e,    &
!            qOKL)
!
!    CALL PutValueInVector(iRe_PPerp_CG, &
!            Vector(iRe_PPerp_CG,y_e) + spx0_offsetn, &
!            y_e,    &
!            qOKL)
!
!    CALL PutValueInVector(iIm_PPerp_CG, &
!            Vector(iIm_PPerp_CG,y_e) + spy0_offsetn, &
!            y_e,    &
!            qOKL) 
!
!  END IF
!
!!!!!!!!!!!!!!!!!!!!!!! END SCRAP !!!!!!!!!!!!!!!!!!!!!!!!


  END SUBROUTINE disperse	

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
  FUNCTION lineCount(fname)

!     Function to count the number of lines on a file
!            
!                ARGUMENTS

  INTEGER(KIND=IP)          :: lineCount
  CHARACTER(*), INTENT(IN)  :: fname

!                LOCAL ARGS

  INTEGER :: ios

  OPEN(1,FILE=fname, IOSTAT=ios, ACTION='READ', POSITION ='REWIND')
  IF (ios /= 0) STOP "OPEN(input file) not performed correctly, IOSTAT /= 0"

  lineCount = 0_IP

  DO 
    lineCount=lineCount+1_IP
    READ(1,*,END=10)
  END DO

  10 CLOSE(1, STATUS='KEEP')

  END FUNCTION lineCount

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  FUNCTION numOfMods(fname)

!     Function to count the number of lines on a file
!            
!                ARGUMENTS

  INTEGER(KIND=IP)          :: numOfMods
  CHARACTER(*), INTENT(IN)  :: fname

!                LOCAL ARGS

  INTEGER :: ios

  OPEN(1,FILE=fname, IOSTAT=ios, ACTION='READ', POSITION ='REWIND')
  IF (ios /= 0) STOP "OPEN(input file) not performed correctly, IOSTAT /= 0"

  READ (1,*)
  READ (1,*)
  READ (1,*) numOfMods

  10 CLOSE(1, STATUS='KEEP')

  END FUNCTION numOfMods

END MODULE lattice

