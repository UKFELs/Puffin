!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE Read_data

USE ArrayFunctions
USE TypesandConstants
USE Globals
USE ParallelSetUp
use MASPin
use H5in

!****************************************************************
!
! Module containing the routines which read in the data files
! in Puffin.
!
! -Lawrence Campbell
! lawrence.campbell@strath.ac.uk
! University of Strathclyde
! June 2016
!
!****************************************************************



CONTAINS


!> read_in Read in the namelist data files for Puffin.
!! @params zfilename, Name of the main input file passed to Puffin
!! at runtime.
!! @params zDataFileName, Base name of the sdds output files.
!! @params qSeparateFiles, Turn on individual (rather than collective)
!! MPI rank writing in the hdf5 output files.
!! @todo Individual and collective writing to combined file to come
!! For collective write, we want to work out how many particles on
!! each rank, what the cumulative num electrons is, and then determine
!! the array slice based on that.
subroutine read_in(zfilename, &
       zDataFileName, &
       qSeparateFiles, &
       qFormattedFiles, &
       qResume, &
       sZ0, &
       LattFile,&
       iWriteNthSteps, &
       iWriteIntNthSteps, &
       tArrayZ, &
       tArrayA, &
       tArrayVariables, &
       sLenEPulse, &
       iNumNodes, &
       sWigglerLength, &
       iRedNodesX,iRedNodesY, &
       nodesperlambda, &
       stepsPerPeriod, &
       nperiods, &
       sQe, &
       q_noise, &
       iNumElectrons, &
       sSigmaGaussian, &
       sElectronThreshold, &
       bcenter, &
       gamma_d, &
       chirp, &
       mag, fr, &
       nbeams, &
       dist_f, &
       qSimple, &
       sA0_Re, &
       sA0_Im, &
       sFiltFrac, &
       sDiffFrac, &
       sBeta, &
       srho, &
       saw, &
       sgamma_r, &
       lambda_w, &
       sEmit_n, &
       sux, &
       suy, &
       Dfact, &
       sFocusfactor, &
       taper,    &
       zUndType, &
       sSigmaF, &
       freqf, SmeanZ2, &
       ph_sh, &
       qFlatTopS, nseeds, &
       sPEOut, &
       iDumpNthSteps, &
       qSwitches, &
       qMatched_A, &
       qOK)

       IMPLICIT NONE

!******************************************************
! Read input data from the main Puffin input file
! 
! zFileName          - FileName containing input data
! nRowProcessors     - Number of row processors
! nColProcessors     - Number of column processors
!
! zDataFileName      - Data file name
! qSeparateStepFiles - if to write data to separate step
!                      files or all steps in one file
! qFormattedFiles    - if output data files to be
!                      formatted or binary
! sStepSize          - Step size for integration
! nSteps             - Number of steps
! sZ                 - IN: Starting z position
!                    - OUT: Final z position
! iWriteNthSteps     - Steps to write data at (optional)
!                       (eg every 4th step)
! tArrayZ            - Write out Z data
! tArrayVariables    - Write out A,p,Q,Z2 data
!
! sLenEPulse(3)      - Length of electron Pulse in
!                      x,y,z2 direction
! iNumNodes(3) 	     - Total number of nodes
! sWigglerLength(3)  - Length of wiggler in x,y,z2
!                      direction
! i_RealE            - Number of real electrons
! q_noise            - Noise in initial electron
!                      distribution
!
! iNumElectrons(3)   - Number of electrons in
!                      x,y,z2 direction
! sSigmaGaussian     - Sigma spread of electron
!                      gaussian distribution
! sElectronThreshold - Beyond this threshold level,
!                      electrons are ignored/removed
!
! sA0_Re,            - Initial field value (real)
! sA0_Im,            - Initial field value (imaginary)
!
! sEmit_n            - Normalised beam emittance
! srho               - Pierce parameter
! saw                - Wiggler parameter
! sgamma_r           - Mean electron velocity at
!                      resonance
! sWiggleWaveLength  - Wiggler wave length
! sSeedSigma         - Seed field sigma spread for
!                      gaussian seed field
! qSwitches          - if allowing different scenarios
! qOK                - Error flag
!********************************************************
  CHARACTER(*),INTENT(IN) :: zfilename

  CHARACTER(1024_IP),  INTENT(OUT)  :: zDataFileName
  LOGICAL,           INTENT(OUT)  :: qSeparateFiles
  LOGICAL,           INTENT(OUT)  :: qFormattedFiles
  LOGICAL,           INTENT(OUT)  :: qResume
  REAL(KIND=WP) ,    INTENT(OUT)  :: sZ0
  CHARACTER(32_IP),  INTENT(INOUT):: LattFile

  INTEGER(KIND=IP),  INTENT(OUT)  :: iWriteNthSteps, iWriteIntNthSteps
  TYPE(cArraySegment)             :: tArrayZ
  TYPE(cArraySegment)             :: tArrayA(:)
  TYPE(cArraySegment)             :: tArrayVariables(:)

  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sLenEPulse(:,:)
  INTEGER(KIND=IP),  INTENT(OUT)  :: iNumNodes(:)

  REAL(KIND=WP),     INTENT(OUT)  :: sWigglerLength(:)

  INTEGER(KIND=IP),  INTENT(OUT)  :: iRedNodesX,&
                                       iRedNodesY

  REAL(KIND=WP),  ALLOCATABLE, INTENT(OUT)  :: sQe(:)
  LOGICAL,           INTENT(OUT)  :: q_noise

  INTEGER(KIND=IP),  ALLOCATABLE, INTENT(OUT)  :: iNumElectrons(:,:)

  REAL(KIND=WP),  ALLOCATABLE, INTENT(OUT)  :: sSigmaGaussian(:,:)

  REAL(KIND=WP),     INTENT(OUT)  :: sElectronThreshold
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: bcenter(:), gamma_d(:), &
                                              chirp(:), sEmit_n(:), &
                                              mag(:), fr(:)

  INTEGER(KIND=IP), INTENT(INOUT) :: nbeams, nseeds

  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sA0_Re(:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sA0_Im(:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: freqf(:), SmeanZ2(:), &
                                              ph_sh(:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sSigmaF(:,:)
  LOGICAL, ALLOCATABLE, INTENT(OUT) :: qFlatTopS(:)
  LOGICAL, INTENT(out) :: qSimple

  CHARACTER(32_ip), ALLOCATABLE, INTENT(INOUT) :: dist_f(:)

  REAL(KIND=WP),     INTENT(OUT)  :: sFiltFrac,sDiffFrac,sBeta
  REAL(KIND=WP),     INTENT(OUT)  :: srho
  REAL(KIND=WP),     INTENT(OUT)  :: saw
  REAL(KIND=WP),     INTENT(OUT)  :: sgamma_r, lambda_w
  REAL(KIND=WP),     INTENT(OUT)  :: sux
  REAL(KIND=WP),     INTENT(OUT)  :: suy
  REAL(KIND=WP),     INTENT(OUT)  :: Dfact
  REAL(KIND=WP),     INTENT(OUT)  :: sFocusfactor, taper
  character(32_IP),  intent(out)  :: zUndType
  REAL(KIND=WP),     INTENT(OUT)  :: sPEOut
  INTEGER(KIND=IP),  INTENT(OUT)  :: iDumpNthSteps
  LOGICAL,           INTENT(OUT)  :: qSwitches(:)
  LOGICAL, ALLOCATABLE, INTENT(OUT)  :: qMatched_A(:)
  LOGICAL,           INTENT(OUT)  :: qOK

! Define local variables

  integer(kind=ip), intent(out) :: stepsPerPeriod, nodesperlambda, nperiods ! Steps per lambda_w, nodes per lambda_r
  real(kind=wp) :: dz2, zbar
  integer(kind=ip) :: nwaves

  INTEGER::ios
  CHARACTER(1024_IP) :: beam_file, seed_file
  LOGICAL :: qOKL, qMatched !   TEMP VAR FOR NOW, SHOULD MAKE FOR EACH BEAM

  logical :: qWriteZ, qWriteA, &
             qWritePperp, qWriteP2, qWriteZ2, &
             qWriteX, qWriteY

  logical :: qOneD, qFieldEvolve, qElectronsEvolve, &
             qElectronFieldCoupling, qFocussing, &
             qDiffraction, qDump, qUndEnds, qhdf5, qsdds, &
             qscaled, qInitWrLat

  integer(kind=ip) :: iNumNodesX, iNumNodesY, nodesPerLambdar
  real(kind=wp) :: sFModelLengthX, sFModelLengthY, sFModelLengthZ2

  real(kind=wp) :: sKBetaXSF, sKBetaYSF

  real(kind=wp) :: sRedistLen
  integer(kind=ip) :: iRedistStp

namelist /mdata/ qOneD, qFieldEvolve, qElectronsEvolve, &
                 qElectronFieldCoupling, qFocussing, &
                 qDiffraction, qFilter, qUndEnds, &
                 q_noise, qDump, qResume, qSeparateFiles, &
                 qFormattedFiles, qWriteZ, qWriteA, &
                 qWritePperp, qWriteP2, qWriteZ2, &
                 qWriteX, qWriteY, qsdds, qhdf5, &
                 beam_file, sElectronThreshold, &
                 iNumNodesY, iNumNodesX, &
                 nodesPerLambdar, sFModelLengthX, &
                 sFModelLengthY, sFModelLengthZ2, &
                 iRedNodesX, iRedNodesY, sFiltFrac, &
                 sDiffFrac, sBeta, seed_file, srho, &
                 sux, suy, saw, sgamma_r, sFocusfactor, &
                 lambda_w, Dfact, zundType, taper, &
                 LattFile, stepsPerPeriod, nPeriods, &
                 sZ0, zDataFileName, iWriteNthSteps, &
                 iWriteIntNthSteps, iDumpNthSteps, sPEOut, &
                 qFMesh_G, sKBetaXSF, sKBetaYSF, sRedistLen, &
                 iRedistStp, qscaled, nspinDX, nspinDY, qInitWrLat, &
                 t_mag_G, t_fr_G, qFMod_G


! Begin subroutine:
! Set error flag to false

  qOK = .FALSE.

! Initialise array!
  qSwitches = .FALSE.

! Default vals...

  qOneD = .true.
  qFieldEvolve = .true.
  qElectronsEvolve = .true.
  qElectronFieldCoupling = .true.
  qFocussing = .false.
  qDiffraction = .true.
  qFilter = .true.
  q_noise = .true.
  qUndEnds = .false.
  qDump = .false.
  qResume = .false.
  qSeparateFiles = .false.
  qFormattedFiles = .false.
  qWriteZ = .true.
  qWriteA = .true.
  qWritePperp = .true.
  qWriteP2 = .true.
  qWriteZ2 = .true.
  qWriteX = .true.
  qWriteY = .true.
  qsdds = .true.
  qhdf5 = .false.
  qFMesh_G = .true.
  qscaled = .true.
  qInitWrLat = .false.
!  qplain = .false.
  qFMod_G = .false.
  t_mag_G = 0.0
  t_fr_G = 1_wp

  beam_file = 'beam_file.in'
  sElectronThreshold     = 0.05
  iNumNodesX             = 129
  iNumNodesY             = 129
  nodesPerLambdar        = 17
  sFModelLengthX         = 1.0
  sFModelLengthY         = 1.0
  sFModelLengthZ2        = 4.0
  iRedNodesX             = -1
  iRedNodesY             = -1
  nspinDX                = -1
  nspinDY                = -1
  sFiltFrac              = 0.3
  sDiffFrac              = 1.0
  sBeta                  = 1.0
  seed_file              = 'seed_file.in'
  srho                   = 0.01
  sux                    = 1.0
  suy                    = 1.0
  saw                    = 1.0
  sgamma_r               = 100.0
  sFocusfactor           = 1.41213562373095
  lambda_w               = 0.04
  Dfact                  = 0.0
  zundType               = ''
  taper                  = 0.0
  lattFile               = ''
  stepsPerPeriod         = 30
  nPeriods               = 8
  sZ0                    = 0.0
  zDataFileName          = 'DataFile.dat'
  iWriteNthSteps         = 30
  iWriteIntNthSteps      = 30
  iDumpNthSteps          = 3000
  sPEOut                 = 100.0
  sKBetaXSF = -0.1_wp
  sKBetaYSF = -0.1_wp

  sRedistLen = -0.1_wp
  iRedistStp = -2_ip

! Open and read namelist

  open(168,file=zfilename, status='OLD', recl=80, delim='APOSTROPHE')
  read(168,nml=mdata)
  close(UNIT=168,STATUS='KEEP')

  qSwitches(iOneD_CG) = qOneD
  qSwitches(iFieldEvolve_CG) = qFieldEvolve
  qSwitches(iElectronsEvolve_CG) = qElectronsEvolve
  qSwitches(iElectronFieldCoupling_CG) = qElectronFieldCoupling
  qSwitches(iFocussing_CG) = qFocussing
  qSwitches(iDiffraction_CG) = qDiffraction
  qSwitches(iDump_CG) = qDump

  qUndEnds_G = qUndEnds
  qsdds_G  = qsdds
  qhdf5_G = qhdf5
  qscaled_G = qscaled
  qInitWrLat_G = qInitWrLat



  tArrayZ%qWrite = qWriteZ
  tArrayZ%zVariable = 'Z' ! Assign SDDS column names

  tArrayA(iRe_A_CG)%qWrite = qWriteA
  tArrayA(iRe_A_CG)%zVariable = 'RE_A'

  tArrayA(iIm_A_CG)%qWrite = qWriteA
  tArrayA(iIm_A_CG)%zVariable = 'IM_A'

  tArrayVariables(iRe_PPerp_CG)%qWrite = qWritePperp
  tArrayVariables(iRe_PPerp_CG)%zVariable = 'RE_PPerp'
  tArrayVariables(iIm_PPerp_CG)%qWrite = qWritePperp
  tArrayVariables(iIm_PPerp_CG)%zVariable = 'IM_PPerp'

  tArrayVariables(iRe_Gam_CG)%qWrite = qWriteP2
  tArrayVariables(iRe_Gam_CG)%zVariable = 'Gamma'

  tArrayVariables(iRe_Z2_CG)%qWrite = qWriteZ2
  tArrayVariables(iRe_Z2_CG)%zVariable = 'Z2'

  tArrayVariables(iRe_X_CG)%qWrite = qWriteX
  tArrayVariables(iRe_X_CG)%zVariable = 'X'

  tArrayVariables(iRe_Y_CG)%qWrite = qWriteY
  tArrayVariables(iRe_Y_CG)%zVariable = 'Y'


  iNumNodes(iX_CG) = iNumNodesX
  iNumNodes(iY_CG) = iNumNodesY
  nodesperlambda   = nodesPerLambdar
  sWigglerLength(iX_CG) = sFModelLengthX
  sWigglerLength(iY_CG) = sFModelLengthY
  sWigglerLength(iZ2_CG) = sFModelLengthZ2



!!!!!!!!!!!!
!!!! NEW SPACE
!!!!!!!!!!!!

  iRedNodesX_G = iRedNodesX
  iRedNodesY_G = iRedNodesY


  sKBetaXSF_G = sKBetaXSF
  sKBetaYSF_G = sKBetaYSF


  sRedistLen_G = sRedistLen
  iRedistStp_G = iRedistStp

  zBFile_G = beam_file
  zSFile_G = seed_file





  CALL read_beamfile(qSimple, dist_f, beam_file,sEmit_n,sSigmaGaussian,sLenEPulse, &
                     iNumElectrons,sQe,chirp,bcenter, mag, fr, gamma_d,nbeams, &
                     qMatched_A,qOKL)

  CALL read_seedfile(seed_file,nseeds,sSigmaF,sA0_Re,sA0_Im,freqf,&
                     ph_sh, qFlatTopS,SmeanZ2,qOKL)


  call FileNameNoExtension(beam_file, zBFile_G, qOKL)
  call FileNameNoExtension(seed_file, zSFile_G, qOKL)


  if (qOneD) qEquiXY_G = .true.


  IF  (.NOT. qOKL) GOTO 1000

  call get_exec()

!  CALL read_seedfile(32_IP)  ! SOON !


! Set the error flag and exit
  qOK = .TRUE.
  GOTO 2000

1000 CALL Error_log('Error in read_data:read_in',tErrorLog_G)
  PRINT*,'Error in readData'
2000 CONTINUE

END SUBROUTINE read_in

!********************************************************


SUBROUTINE read_beamfile(qSimple, dist_f, be_f, sEmit_n,sSigmaE,sLenE, &
                         iNumElectrons,sQe,chirp, bcenter, mag, fr,gammaf,nbeams,&
                         qMatched_A,qOK)

  IMPLICIT NONE


! Read the beamfile into Puffin


!                     ARGUMENTS

  LOGICAL, INTENT(OUT) :: qSimple
  CHARACTER(*), INTENT(INOUT) :: be_f     ! beam file name
  CHARACTER(1024_ip), INTENT(INOUT), ALLOCATABLE :: dist_f(:)     ! dist file names
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sEmit_n(:),chirp(:), mag(:), fr(:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sSigmaE(:,:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sLenE(:,:)
  INTEGER(KIND=IP), ALLOCATABLE, INTENT(OUT) :: iNumElectrons(:,:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sQe(:),bcenter(:),gammaf(:)
  INTEGER(KIND=IP), INTENT(INOUT) :: nbeams
  LOGICAL, INTENT(OUT) :: qOK
  logical, intent(inout), allocatable :: qMatched_A(:)
  logical :: qEquiXY
  integer(kind=ip) :: nseqparts

!                     LOCAL ARGS

  INTEGER(KIND=IP) :: b_ind
  logical :: qFixCharge
  INTEGER::ios
  CHARACTER(96) :: dum1, dum2, dtype



! Declare namelists

  namelist /nblist/ nbeams, dtype
  namelist /blist/ sSigmaE, sLenE, iNumElectrons, &
                   sEmit_n, sQe, bcenter,  gammaf, &
                   chirp, mag, fr, qRndEj_G, sSigEj_G, &
                   qMatched_A, qEquiXY, nseqparts, qFixCharge


  namelist /bdlist/ dist_f, nMPs4MASP_G
  namelist /bh5list/ dist_f

  qOK = .FALSE.

! Open the file
!  OPEN(UNIT=168,FILE=be_f,IOSTAT=ios,&
!    ACTION='READ',POSITION='REWIND')
!  IF  (ios/=0_IP) THEN
!    CALL Error_log('Error in read_in:OPEN(input file) not performed correctly, IOSTAT/=0',tErrorLog_G)
!    GOTO 1000
!  END IF

!     Read in file header to get number of beams

!  READ(UNIT=168,FMT=*) dum1, dum2, dtype
!  dtype = TRIM(ADJUSTL(dtype))


!!!!! Need to make qMatched an array
!!!!! Maybe make qMathcField or something to choose which beam is matched to transverse field area (numerically - sampling wise)..???


!  Default vals

  nbeams = 1
  dtype = 'simple'



! Read first namelist - number of beams only

  open(161,file=be_f, status='OLD', recl=80, delim='APOSTROPHE')
  read(161,nml=nblist)


! Allocate arrays


  allocate(sSigmaE(nbeams,6))
  allocate(sLenE(nbeams,6))
  allocate(iNumElectrons(nbeams,6))
  allocate(sEmit_n(nbeams),sQe(nbeams),bcenter(nbeams),gammaf(nbeams))
  allocate(chirp(nbeams), qMatched_A(nbeams))
  allocate(mag(nbeams), fr(nbeams))
  allocate(qRndEj_G(nbeams), sSigEj_G(nbeams))


! &&&&&&&&&& Default vals

  sSigmaE(1,1:2) = 1.0_wp
  sSigmaE(1,3) = 1E8
  sSigmaE(1,4:5) = 1.0_wp
  sSigmaE(1,6) = 0.001_wp

  sLenE(1,1:2) = 6.0_wp
  sLenE(1,3) = 1.0_wp
  sLenE(1,4:5) = 6.0_wp
  sLenE(1,6) = 0.006_wp

  iNumElectrons(1,1:2) = 1
  iNumElectrons(1,3) = -1
  iNumElectrons(1,4:5) = 1
  iNumElectrons(1,6) = 19

  sEmit_n = 1.0_wp
  sQe = 1E-9
  bcenter = 0.0_wp
  gammaf = 1.0_wp
  chirp = 0.0_wp
  qMatched_A = .false.
  mag = 0.0_wp
  fr = 1.0_wp
  qRndEj_G = .true.
  sSigEj_G = 0.02_wp
  qEquiXY = .false.
  nseqparts = 1000_ip
  qSimple = .false.
  qFixCharge = .false.

! &&&&&&&&&&&&&&&&&&&&&

  if (dtype == 'simple') then

! Read in arrays

    iInputType_G = iGenHom_G
    qSimple = .true.

    read(161,nml=blist)

    close(UNIT=161,STATUS='KEEP')

  else if (dtype == 'dist') then


!    Need to change this to namelist - could have array of strings...??
!    And can the namelist define each element individually....????


    iInputType_G = iReadDist_G

!    READ(UNIT=161,FMT=*)
!    READ(UNIT=161,FMT=*)
!    READ(UNIT=161,FMT=*)
!    READ(UNIT=161,FMT=*)
!    READ(UNIT=161,FMT=*)
!    READ(UNIT=161,FMT=*) nbeams
!
    allocate(dist_f(nbeams))


    read(161,nml=bdlist)

    close(UNIT=161,STATUS='KEEP')

    iNumElectrons = 1
    sLenE = 1
    sSigmaE = 1

!    !!!!!!!!!!!!!!!!
!    !!!!!!!!!!!!!!!!
!    ! READ IN FNAMES
!    !!!!!!!!!!!!!!!!
!    !!!!!!!!!!!!!!!!
!    READ(UNIT=161,FMT=*)
!    READ(UNIT=161,FMT=*)
!    READ(UNIT=161,FMT=*)
!
!    DO b_ind = 1, nbeams
!      READ(UNIT=161,FMT=*) dist_f(b_ind)
!    END DO
!
!    CLOSE(UNIT=161,STATUS='KEEP')

  else if (dtype == 'particle') then

    if (nbeams /= 1) then

      if (tProcInfo_G%qRoot) print*, 'WARNING - currently only 1 file', &
                                      'is supported for the particle beam type'
      if (tProcInfo_G%qRoot) print*, 'Only the 1st file will be read in....'

    end if

    allocate(dist_f(nbeams))

    iInputType_G = iReadMASP_G
    nMPs4MASP_G = 3455789_ip  ! default?

    read(161,nml=bdlist)

    close(UNIT=161,STATUS='KEEP')

  else if (dtype == 'h5') then
    iInputType_G = iReadH5_G
    read(161,nml=bh5list)

    close(UNIT=161,STATUS='KEEP')

  end if


  qEquiXY_G = qEquiXY
  nseqparts_G = nseqparts
  qFixCharge_G = qFixCharge


! Set the error flag and exit
  qOK = .TRUE.
  GOTO 2000

1000 CALL Error_log('Error in Read_Data:read_beamfile',tErrorLog_G)
    PRINT*,'Error in read_beamfile'
2000 CONTINUE

END SUBROUTINE read_beamfile

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE read_seedfile(se_f, nseeds,sSigmaF,sA0_X,sA0_Y,freqf,ph_sh,&
                         qFlatTop, meanZ2,qOK)

  IMPLICIT NONE

!                     ARGUMENTS

  CHARACTER(*), INTENT(IN) :: se_f     ! seed file name
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sSigmaF(:,:), &
                                             sA0_X(:),sA0_Y(:), &
                                             freqf(:), meanZ2(:), &
                                             ph_sh(:)
  INTEGER(KIND=IP), INTENT(INOUT) :: nseeds

  LOGICAL, ALLOCATABLE, INTENT(OUT) :: qFlatTop(:)
  LOGICAL, INTENT(OUT) :: qOK

!                     LOCAL ARGS

  INTEGER(KIND=IP) :: s_ind
  INTEGER::ios


  namelist /nslist/ nseeds
  namelist /slist/ freqf, ph_sh, sA0_X, sA0_Y, sSigmaF, &
                   qFlatTop, meanZ2, qRndFj_G,  sSigFj_G, &
                   qMatchS_G


  qOK = .FALSE.


! Default vals

  nseeds = 1


! Open the file
  open(161,file=se_f, status='OLD', recl=80, delim='APOSTROPHE')


  read(161,nml=nslist)

  ALLOCATE(sSigmaF(nseeds,3))
  ALLOCATE(sA0_X(nseeds), sA0_Y(nseeds), ph_sh(nseeds))
  ALLOCATE(freqf(nseeds),qFlatTop(nseeds),meanZ2(nseeds))
  allocate(qRndFj_G(nseeds), sSigFj_G(nseeds))
  allocate(qMatchS_G(nseeds))

!  Default value

  sSigmaF = 1.0_wp
  freqf = 1.0_wp
  ph_sh = 0.0_wp
  sA0_X = 1.0_wp
  sA0_Y = 1.0_wp
  qFlatTop = .false.
  meanZ2 = 0.0_wp
  qRndFj_G = .false.
  sSigFj_G = 0.01_wp
  qMatchS_G = .true.

  read(161,nml=slist)
  close(UNIT=161,STATUS='KEEP')


! Set the error flag and exit
  qOK = .TRUE.
  GOTO 2000

1000 CALL Error_log('Error in Read_Data:read_seedfile',tErrorLog_G)
    PRINT*,'Error in read_seedfile'
2000 CONTINUE



END SUBROUTINE read_seedfile


subroutine get_exec()


! Retrieves command line used to initiate run

  call get_command(cmd_call_G)

end subroutine get_exec


END MODULE Read_data
