! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

MODULE InitVars

USE paratype
USE typesAndConstants
USE ArrayFunctions

IMPLICIT NONE

!====================================================================
! Define variables
! These are the input variables, to which the values in the input file
! are assigned.
!-------------------------------------------------------------------- 
!
! sStepSize          - Step size for integration
! nSteps             - Number of steps 
! sZ                 - z position
! iWriteNthSteps     - Steps to write data at (optional) 
! tArrayE            - Write out electron array data
! tArrayA            - Write out field data.
! tArrayZ	     - Write out Z data
!
! sLenEPulse(3)      - Length of electron Pulse in x,y,z2 direction
! iNumElm(3)         - Total number of Elements 
! sWigglerLength(3)  - Length of wiggler in x,y,z2 direction
! iNumElectrons(3)   - Number of electrons in x,y,z2 direction
! i_RealE            - Number of real Electrons 
!                      (DEFINED AS REAL FOR PRECISION)
! q_noise            - If including noise in elctron distribution
!
! sSigmaGaussian     - Sigma spread of electron gaussian distribution
!
! sElectronThreshold - Beyond this threshold level, electrons 
!                      are ignored/removed
! sA0_Re   	     - Initial field value (real)
! sA0_Im   	     - Initial field value (imaginary)
!
! sEmit_n            - Normalised beam emittance
! srho               - Pierce parameter, describe the strength
!                      of the field
! saw                - Wiggler parameter
! sgamma_r           - Mean electron velocity at resonance
! sFocusfactor       - Focussing factor - sqrt(2) for natural
!                      helical wiggler
! sWigglerWaveLength - Wavelength of the wiggler
! qOKL               - Local error flag
! sSeedSigma         - Information of the seed field 
! qSwitches          - if allowing different scenarios when running code
! qSeparateStepFiles - if to write data to separate step 
!                      files or all steps in one file
! qFormattedFiles    - if output data files to be formatted or binary
! zFileName          - Input data file name
! zFile              - Input file name without extension
!=====================================================================
!	









!!!!!!!!!!!!!!!!!!!!
!!! NEW
!!!

REAL(KIND=WP), ALLOCATABLE  :: sLenEPulse(:,:)
INTEGER(KIND=IP), ALLOCATABLE  :: iNumElectrons(:,:)
REAL(KIND=WP), ALLOCATABLE     :: sEleSig(:,:)
REAL(KIND=WP), ALLOCATABLE    :: sQe(:), beamCenZ2(:), gamma_d(:), &
                                 chirp(:), sEmit_n(:), mag(:), fr(:)

REAL(KIND=WP), ALLOCATABLE :: sA0_Re(:),sA0_Im(:)

INTEGER(KIND=IP) :: nbeams
logical, allocatable :: qMatched_A(:)

real(kind=wp), allocatable :: alphax(:), alphay(:), emitx(:), emity(:)

!!!
!!! END NEW
!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!
!!! NEW FOR SEED FILE
!!!

INTEGER(KIND=IP) :: nseeds
REAL(KIND=WP), ALLOCATABLE :: freqf(:), ph_sh(:), SmeanZ2(:)
LOGICAL, ALLOCATABLE :: qFlatTopS(:)
REAL(KIND=WP), ALLOCATABLE    :: sSeedSigma(:,:)

!!!
!!!
!!!!!!!!!!!!!!!!!!!!

CHARACTER(1024_IP) :: infile, emptstring

CHARACTER(32_IP) :: zUndType


REAL(KIND=WP)    :: sFieldModelLength(nSpaceDimensions_CG)   



LOGICAL          :: q_noise

logical :: qMeasure


REAL(KIND=WP)    :: sElectronThreshold, sDiffFrac, sBeta

REAL(KIND=WP)    :: srho, saw, sgammar, lambda_w
REAL(KIND=WP)    :: fx, fy
REAL(KIND=WP)    :: sFocusfactor

LOGICAL          :: qOKL   

LOGICAL          :: qSwitches(nSwitches_CG)
LOGICAL          :: qSeparateStepFiles
LOGICAL          :: qFormattedFiles

REAL(KIND=WP)    :: sFiltFrac, taper

logical          :: qSimple
character(1024_IP), allocatable :: dist_f(:),field_file(:)

CHARACTER(1024_IP) :: zFileName
CHARACTER(1024_IP) :: zFile
CHARACTER(1024_IP) :: LattFile 

!===============================================================
! The following variables are used to store the electron
! and field values.
!
! sV		- 1D array holding the electron information.
! sA		- 1D complex array describing the scaled 3D field.
! iNodes	- Number of field nodes in x, y and z2.
! iTransNodes   - Number of field nodes in each direction.
! ndims		- Number of space dimensions.
!------------------------------------------------------------- 

REAL(KIND=WP)    :: sLengthOfElm(nSpaceDimensions_CG)
INTEGER,DIMENSION(3)  :: iNodes

INTEGER  :: ndims

REAL(KIND=WP)  :: redwigglengthX,redwigglengthY

integer(kind=ip) :: nodesperlambda, stepsPerPeriod, &
                    nperiods


!=============================================================
! Variables used for propagation of electron and field values.
!
! iCount - Counter to identify which steps to write.
! iStep  - Used to loop over integration steps.
! sStep  - Step size
! sDyDz  - Value of the derivative of electron variables at position z
! sDADz  - Value of derivative of field variables at z
! qWrite - If to write result to a file
! qAllSteps - If to write out all steps
!=============================================================

INTEGER(KIND=IP)  :: iCount
INTEGER(KIND=IP)  :: iStep, p
REAL(KIND=WP)     :: sStep
LOGICAL           :: qWrite,qResume

!=============================================================
! MPI variables
!============================================================= 

INTEGER(KIND=IP)  :: error, i
! Gathering arrays: specify size of distributed data on each
! process.   

INTEGER(KIND=IP) :: sendbuff,recvbuff,statr,req,lrank,rrank

!=============================================================
! FFTW-MPI variables
!=============================================================

LOGICAL             :: qMod

END MODULE InitVars
