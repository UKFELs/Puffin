MODULE lattice

USE paratype
USE DerivsGlobals
USE ArrayFunctions

IMPLICIT NONE

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE readLatt(lattFile,zMod,delta,D,Dfact,ModNum,rho)

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
  REAL(KIND=WP), INTENT(IN) :: rho
  INTEGER(KIND=IP), INTENT(IN) :: ModNum

!                LOCAL VARS
	
  INTEGER(KIND=IP)   :: i,ios,nw,error
  REAL(KIND=WP)      :: pi,c1

  OPEN(1,FILE=lattFile, IOSTAT=ios, ACTION='READ', POSITION ='REWIND')
  IF (ios /= 0_IP) STOP "OPEN(input file) not performed correctly, IOSTAT /= 0"

  pi = 4.0_WP*ATAN(1.0_WP)
  c1 = 2.0_WP*rho

  print*, modNum

!     Read module data from lattice file

  DO i=1,ModNum
    READ (1,*) nw, delta(i) !, resFactor(i) ! Wiggler periods, Chicane slippage periods, resonance change in p

!     Calculate cumulative interaction length of modules

    IF (i==1) THEN  
      zMod(i) = REAL(nw,KIND=WP)
    ELSE
      zMod(i) = zMod(i-1)+REAL(nw,KIND=WP)
    END IF

  END DO
	
  CLOSE(1, STATUS='KEEP')

!     Convert from # undulator periods to z-bar

  zMod = 2.0_WP*pi*c1*zMod
  delta = 2.0_WP*pi*c1*delta
  D = Dfact*10.0/6.0*delta ! The Dispersion parameter

  END SUBROUTINE readLatt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE disperse(y_e,D,delta)

  IMPLICIT NONE

! Subroutine to apply phase changes to electrons beam
! due to passing through a chicane...
! Modified from Brian McNeil's Phase shift routine to 
! work with Q(electron momentum in z2 frame) as opposed
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
	
  INTEGER(KIND=IP)  ::  e_tot, z2_start, z2_end, Q_start, Q_end

  e_tot = iGloNumElectrons_G
	
  z2_start = iBStartPosition_G(iRe_z2_CG)
  z2_end = iBEndPosition_G(iRe_z2_CG)
	
  Q_start = iBStartPosition_G(iRe_Q_CG)
  Q_end = iBEndPosition_G(iRe_Q_CG)
	
  y_e(z2_start:z2_end) = y_e(z2_start:z2_end)- &
                       D*(1.0_WP-(((1-y_e(Q_start:Q_end))) &
                       /(2.0_WP*sRho_G)))+delta

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

END MODULE lattice
