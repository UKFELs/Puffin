! ###############################################
! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause
! ###############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Module containing the routines which read in the input files in Puffin.


module Read_data

use ArrayFunctions
use TypesandConstants
use Globals
use ParallelSetUp
use MASPin
use H5in
use cwrites

contains

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Read in the namelist data files for Puffin.
!> @param[in] zfilename Name of the main input file passed to Puffin
!> at runtime.
!> @param[out] zDataFileName Base name of the sdds output files.
!> @param[out] qSeparateFiles Turn on individual (rather than collective)
!> MPI rank writing in the hdf5 output files.
!> @param[out] qFormattedFiles Turn on ascii format files (in SDDS only)
!> @param[out] qResume Whether reuming from a previous run
!> @param[out] sZ0 Initial zbar
!> @param[out] LattFile Name of Puffin lattice file
!> @param[out] iWriteNthSteps Steps to write full 'raw' data at
!> @param[out] iWriteIntNthSteps Steps to write integrated data at
!> @param[out] tArrayZ SDDS filetype for Z data
!> @param[out] tArrayA SDDS filetype for field data
!> @param[out] tArrayVariables SDDS filetype for electron macroparticle dump data
!> @param[out] sLenEPulse 6*nbeams element array - Length of electron pulse in x, y, z2, px, 
!> py, and gamma, in the simple beam case
!> @param[out] iNumNodes 3 element array - Number of field nodes in x, y, and z2
!> @param[out] sWigglerLength 3 element array - Length of radiation field mesh in
!> x, y, and z2
!> @param[out] nodesperlambda Number of radiation nodes to be used in the mesh 
!> per reference resonant wavelength in z2.
!> @param[out] stepsPerPeriod Number of integration steps per undulator period 
!> (ignored if lattuce is specified).
!> @param[out] nPeriods Number of periods in undulator. Ignored if lattice is 
!> specified
!> @param[out] sQe Charge in the electron beam if simple beam input is used.
!> @param[out] q_noise If adding shot-noise to the beam (can switch off to see 
!> only Coherent Spontaneous Emission (CSE))
!> iNumElectrons[out] Number of electron macroparticles in each dimension,
!> for simple beam case.
!> @param[out] sSigmaGaussian Gaussian sigma of electron beam density profile
!> in each dimension (use 1E8 for flat top) for simple beam input
!> @param[out] sElectronThreshold Macroparticle charge weight limit, below which
!> the electron macroparticle will be thrown away, expressed as a % of the mean 
!> charge weight.
!> @param[out] bcenter Center of beam in z2 / t.
!> @param[out] gamma_d Array of size nbeams. Energy of beam in the simple beam case, 
!> scaled to the reference energy (gamma_r). So gamma_d = 1 for beam energy 
!> gamma_r.
!> @param[out] chirp Array of size nbeams. Energy chirp of electron beam in simple
!> beam case. Expressed in units of \f$ \frac{d \gamma}{d \bar{z}_2} \f$ in the 
!> scaled case, and \f$ \frac{d \gamma}{d t} \f$ in the unscaled (SI) case. Only
!> specified in simple beam case.
!> @param[out] mag Magnitude of beam energy oscillation. The oscillation is 
!> sinusoidal. Set to zero to have no energy oscillation. Default = 0. In units 
!> of \f$ \gamma \f$ 
!> @param[out] fr Wavenumber \f$ (\frac{2 \pi}{\lambda}) \f$ of beam modulation.
!> @param[out] nbeams Number of electron beams input into FEL
!> @param[out] dist_f Name of external files to read from in the dist and 
!> particle cases.
!> @param[out] qSimple If simple beam input is beaing used.
!> @param[out] sA0_Re Magnitude of x-polarized injected seed field, if used. 
!> @param[out] sA0_Im Magnitude of y-polarized injected seed field, if used.
!> @param[out] sFiltFrac Cutoff frequency for high-pass filter in diffraction 
!> step. Expressed as a fraction of the resonant reference frequency.
!> @param[out] sDiffFrac Diffraction step size, expressed as fraction of 
!> reference resonant frequency
!> @param[out] sBeta Absorption coefficient for absorbing boundaries in the 
!> field mesh in the transverse plane.
!> @param[out] sRho FEL, or pierce, parameter
!> @param[out] saw Undulator parameter (peak, not rms) for reference frame
!> @param[out] sgamma_r Reference beam energy
!> @param[out] lambda_w Undulator period
!> @param[out] sEmit_n Array of length nbeams. Scaled RMS Beam emittance in 
!> simple beam case.
!> @param[out] sux Relative magnitude of undulator magnetic field in 
!> x-direction. Either sux OR suy should = 1. For Puffin undulator type
!> only.
!> @param[out] suy Relative magnitude of undulator magnetic field in 
!> y-direction.
!> @param[out] taper Taper of magnetic undulator field as function of z, only
!> used in simple wiggler case (no lattice). In units of 
!> \f$ \frac{d \alpha}{d \bar{z}} \f$.
!> @param[out] zUndType Undulator type to be use in simple wiggler case.
!> @param[out] sSigmaF Standard deviation of radiation seed field magnitude in 
!> each dimension.
!> @param[out] freqf Frequency of seed field, scaled to the reference resonant 
!> frequency.
!> @param[out] SmeanZ2 Mean position of seed in z2.
!> @param[out] ph_sh Phase shift of seed
!> @param[out] qFlatTopS If using flat top seed profile
!> @param[out] nseeds Number of radiation seeds 
!> @param[out] qSwitches Various simulation control flags
!> @param[out] qMatched_A If matching transverse area of beam to wiggler. Simple
!> beam case only.
!> @param[out] qOK Error flag

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
       field_file, &
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
       alphax, alphay, emitx, emity, &
       sux, &
       suy, &
       taper,    &
       zUndType, &
       sSigmaF, &
       freqf, SmeanZ2, &
       ph_sh, &
       qFlatTopS, nseeds, &
       qSwitches, &
       qMatched_A, &
       qMeasure, &
       qOK)

       IMPLICIT NONE

  CHARACTER(*),INTENT(IN) :: zfilename

  CHARACTER(1024_IP),  INTENT(OUT)  :: zDataFileName
  LOGICAL,           INTENT(OUT)  :: qSeparateFiles
  LOGICAL,           INTENT(OUT)  :: qFormattedFiles
  LOGICAL,           INTENT(OUT)  :: qResume
  REAL(KIND=WP) ,    INTENT(OUT)  :: sZ0
  CHARACTER(1024_IP),  INTENT(INOUT):: LattFile
  INTEGER(KIND=IP),  INTENT(OUT)  :: iWriteNthSteps, iWriteIntNthSteps
  TYPE(cArraySegment)             :: tArrayZ
  TYPE(cArraySegment)             :: tArrayA(:)
  TYPE(cArraySegment)             :: tArrayVariables(:)

  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sLenEPulse(:,:)
  INTEGER(KIND=IP),  INTENT(OUT)  :: iNumNodes(:)

  REAL(KIND=WP),     INTENT(OUT)  :: sWigglerLength(:)

  REAL(KIND=WP),  ALLOCATABLE, INTENT(OUT)  :: sQe(:)
  LOGICAL,           INTENT(OUT)  :: q_noise

  INTEGER(KIND=IP),  ALLOCATABLE, INTENT(OUT)  :: iNumElectrons(:,:)

  REAL(KIND=WP),  ALLOCATABLE, INTENT(OUT)  :: sSigmaGaussian(:,:)

  REAL(KIND=WP),     INTENT(OUT)  :: sElectronThreshold
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: bcenter(:), gamma_d(:), &
                                              chirp(:), sEmit_n(:), &
                                              mag(:), fr(:), &
                                              alphax(:), alphay(:), emitx(:), &
                                              emity(:)

  INTEGER(KIND=IP), INTENT(INOUT) :: nbeams, nseeds

  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sA0_Re(:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sA0_Im(:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: freqf(:), SmeanZ2(:), &
                                              ph_sh(:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sSigmaF(:,:)
  LOGICAL, ALLOCATABLE, INTENT(OUT) :: qFlatTopS(:)
  LOGICAL, INTENT(out) :: qSimple

  CHARACTER(1024_ip), ALLOCATABLE, INTENT(INOUT) :: dist_f(:), & !< particle distribution file
                                                field_file(:)    !< field input file (if any)

  REAL(KIND=WP),     INTENT(OUT)  :: sFiltFrac,sDiffFrac,sBeta
  REAL(KIND=WP),     INTENT(OUT)  :: srho
  REAL(KIND=WP),     INTENT(OUT)  :: saw
  REAL(KIND=WP),     INTENT(OUT)  :: sgamma_r, lambda_w
  REAL(KIND=WP),     INTENT(OUT)  :: sux
  REAL(KIND=WP),     INTENT(OUT)  :: suy
  REAL(KIND=WP),     INTENT(OUT)  :: taper
  character(32_IP),  intent(out)  :: zUndType
  LOGICAL,           INTENT(OUT)  :: qSwitches(:)
  LOGICAL, ALLOCATABLE, INTENT(OUT)  :: qMatched_A(:)
  logical, intent(out) :: qMeasure
  LOGICAL,           INTENT(OUT)  :: qOK

! Define local variables

  integer(kind=ip), intent(out) :: stepsPerPeriod, nodesperlambda, nperiods ! Steps per lambda_w, nodes per lambda_r
  real(kind=wp) :: dz2, zbar, sPerWaves
  integer(kind=ip) :: nwaves, iRedNodesX, iRedNodesY

  INTEGER::ios
  CHARACTER(1024_IP) :: beam_file, seed_file, wr_file
  LOGICAL :: qOKL, qMatched !   TEMP VAR FOR NOW, SHOULD MAKE FOR EACH BEAM

  logical :: qWriteZ, qWriteA, &
             qWritePperp, qWriteP2, qWriteZ2, &
             qWriteX, qWriteY

  logical :: qOneD, qFieldEvolve, qElectronsEvolve, &
             qElectronFieldCoupling, qFocussing, &
             qDiffraction, qDump, qUndEnds, qhdf5, qsdds, &
             qscaled, qInitWrLat, qDumpEnd

  integer(kind=ip) :: iNumNodesX, iNumNodesY, nodesPerLambdar
  real(kind=wp) :: sFModelLengthX, sFModelLengthY, sFModelLengthZ2

  real(kind=wp) :: sKBetaXSF, sKBetaYSF

  real(kind=wp) :: sRedistLen
  integer(kind=ip) :: iRedistStp
  integer(kind=ip) :: meshType



!   redundant data!!! For compatibility only....

  real(kind=wp) :: Dfact, speout
  integer(kind=ip) :: iDumpNthSteps




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
                 sux, suy, saw, sgamma_r, &
                 lambda_w, zundType, taper, &
                 LattFile, stepsPerPeriod, nPeriods, &
                 sZ0, zDataFileName, iWriteNthSteps, &
                 iWriteIntNthSteps, &
                 qFMesh_G, sKBetaXSF, sKBetaYSF, sRedistLen, &
                 iRedistStp, qscaled, nspinDX, nspinDY, qInitWrLat, qDumpEnd, &
                 wr_file, qMeasure, DFact, iDumpNthSteps, speout, meshType, &
                 sPerWaves, t_mag_G, t_fr_G, qFMod_G


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
  qsdds = .false.
  qhdf5 = .true.
  qFMesh_G = .true.
  qscaled = .true.
  qInitWrLat = .false.
  qDumpEnd = .true.
  qMeasure = .true.
  Dfact = -1000.0_wp
  iDumpNthSteps = -1000_ip
  speout = -1000.0_wp
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
  sPerWaves              = -1.0_wp
  iRedNodesX             = -1
  iRedNodesY             = -1
  nspinDX                = -1
  nspinDY                = -1
  sFiltFrac              = 0.3
  sDiffFrac              = 1.0
  sBeta                  = 1.0
  seed_file              = ''
  wr_file = ''
  srho                   = 0.01
  sux                    = 1.0
  suy                    = 1.0
  saw                    = 1.0
  sgamma_r               = 100.0
  lambda_w               = 0.04
  zundType               = ''
  taper                  = 0.0
  lattFile               = ''
  stepsPerPeriod         = 30
  nPeriods               = 8
  sZ0                    = 0.0
  zDataFileName          = 'DataFile.dat'
  iWriteNthSteps         = 30
  iWriteIntNthSteps      = 30
  meshType = 0_ip
  sKBetaXSF = -0.1_wp
  sKBetaYSF = -0.1_wp

  sRedistLen = -0.1_wp
  iRedistStp = -2_ip

! Open and read namelist

  open(168,file=zfilename, status='OLD', recl=80, delim='APOSTROPHE')
  read(168,nml=mdata)
  close(UNIT=168,STATUS='KEEP')

  if (qOneD) qDiffraction = .false.

  qSwitches(iOneD_CG) = qOneD
  qSwitches(iFieldEvolve_CG) = qFieldEvolve
  qSwitches(iElectronsEvolve_CG) = qElectronsEvolve
  qSwitches(iElectronFieldCoupling_CG) = qElectronFieldCoupling
  qSwitches(iFocussing_CG) = qFocussing
  qSwitches(iDiffraction_CG) = qDiffraction
  qSwitches(iResume_CG) = qResume
  qDiffraction_G = qDiffraction
  qSwitches(iDump_CG) = qDump

  qUndEnds_G = qUndEnds
  qsdds_G  = qsdds
  qhdf5_G = qhdf5
  qscaled_G = qscaled
  qInitWrLat_G = qInitWrLat

  qDumpEnd_G = qDumpEnd


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



  if (wr_file /= '') then
    qWrArray_G = .true.
    call getWrArray(wr_file)
  else
    qWrArray_G = .false.
  end if

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
  
  sPerWaves_G = sPerWaves

  fieldMesh = meshType


  if (DFact /= -1000.0_wp) then
    if (tProcInfo_G%qRoot) then

      print*, ''
      print*, 'WARNING: Use of Dfact deprecated. It is kept only so your'
      print*, 'old input files will not break!! It will do nothing. To specify'
      print*, 'chicane strengths, please use the lattice file.'
      print*, ''

    end if
  end if


  if (iDumpNthSteps /= -1000_ip) then
    if (tProcInfo_G%qRoot) then

      print*, ''
      print*, 'WARNING: Use of iDumpNthSteps deprecated. It is kept only so your'
      print*, 'old input files will not break!! It will do nothing.'
      print*, ''

    end if
  end if

  if (speout /= -1000.0_wp) then
    if (tProcInfo_G%qRoot) then

      print*, ''
      print*, 'WARNING: Use of speout deprecated. It is kept only so your'
      print*, 'old input files will not break!! It will do nothing.'
      print*, ''

    end if
  end if



  CALL read_beamfile(qSimple, dist_f, beam_file,sEmit_n,sSigmaGaussian,sLenEPulse, &
                     alphax, alphay, emitx, emity, &
                     iNumElectrons,sQe,chirp,bcenter, mag, fr, gamma_d,nbeams, &
                     qMatched_A,qOKL)

  CALL read_seedfile(seed_file,nseeds,sSigmaF,sA0_Re,sA0_Im,freqf,&
                     ph_sh, qFlatTopS,SmeanZ2,field_file,qOKL)

  call FileNameNoExtension(beam_file, zBFile_G, qOKL)
  
  if (seed_file == '') then
    zSFile_G = 'unused'
  else
    call FileNameNoExtension(seed_file, zSFile_G, qOKL)
  end if

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
                         alphax, alphay, emitx, emity, &
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
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: alphax(:), alphay(:), emitx(:), &
                                             emity(:)
  INTEGER(KIND=IP), ALLOCATABLE, INTENT(OUT) :: iNumElectrons(:,:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sQe(:),bcenter(:),gammaf(:)
  INTEGER(KIND=IP), INTENT(INOUT) :: nbeams
  LOGICAL, INTENT(OUT) :: qOK
  logical, intent(inout), allocatable :: qMatched_A(:)
  logical :: qEquiXY
  integer(kind=ip) :: nseqparts
  real(kind=wp) :: fillFact

!                     LOCAL ARGS

  INTEGER(KIND=IP) :: b_ind, TrLdMeth
  logical :: qFixCharge, qAMatch
  INTEGER::ios
  CHARACTER(96) :: dtype



! Declare namelists

  namelist /nblist/ nbeams, dtype
  namelist /blist/ sSigmaE, sLenE, iNumElectrons, &
                   sEmit_n, sQe, bcenter,  gammaf, &
                   chirp, mag, fr, qRndEj_G, sSigEj_G, &
                   qMatched_A, qEquiXY, nseqparts, qFixCharge, &
                   alphax, alphay, emitx, emity, TrLdMeth, fillFact


  namelist /bdlist/ dist_f, nMPs4MASP_G
  namelist /bh5list/ dist_f

  qOK = .FALSE.

  qAMatch = .false.

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
  allocate(alphax(nbeams), alphay(nbeams))
  allocate(emitx(nbeams), emity(nbeams))


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

  sEmit_n = -1.0_wp
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
  alphax = 0.0_wp
  alphay = 0.0_wp
  emitx = -1.0_wp
  emity = -1.0_wp
  TrLdMeth = 1_ip
  fillFact = 1_wp

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
    if (nbeams /= 1) then 
      
      if (tProcInfo_G%qRoot) print*, 'WARNING - currently only 1 file', &
                                      'is supported for the h5 beam type'
      if (tProcInfo_G%qRoot) print*, 'Only the 1st file will be read in....'

    end if
    allocate(dist_f(nbeams))
    iInputType_G = iReadH5_G
    read(161,nml=bh5list)

    close(UNIT=161,STATUS='KEEP')

  end if


  qEquiXY_G = qEquiXY
  nseqparts_G = nseqparts
  qFixCharge_G = qFixCharge
  TrLdMeth_G = TrLdMeth
  fillFact_G = fillFact

  do b_ind = 1, nbeams

    if (sEmit_n(b_ind) > 0.0_wp) then
      if (tProcInfo_G%qRoot) print*, ''
      if (tProcInfo_G%qRoot) print*, '************************************************'
      if (tProcInfo_G%qRoot) print*, 'WARNING - use of sEmit_n deprecated - use emitx and emity instead'
      if (tProcInfo_G%qRoot) print*, 'For now, emitx and emity will = sEmit_n where not specified'
    
      if (emitx(b_ind) <= 0.0_wp) emitx(b_ind) = sEmit_n(b_ind)
      if (emity(b_ind) <= 0.0_wp) emity(b_ind) = sEmit_n(b_ind)
    
    end if  

  end do

  

  do b_ind = 1, nbeams

    if (emitx(b_ind) <= 0.0_wp) then
      alphax(b_ind) = 0.0_wp
    end if

    if (emity(b_ind) <= 0.0_wp) then
      alphay(b_ind) = 0.0_wp
    end if

    if (qMatched_A(b_ind)) then
      qAMatch = .true.
      alphax(b_ind) = 0.0_wp
      alphay(b_ind) = 0.0_wp
    end if

  end do


  if (qAMatch) then
    if (tProcInfo_G%qRoot) print*, ''
    if (tProcInfo_G%qRoot) print*, '************************************************'
    if (tProcInfo_G%qRoot) print*, 'You have chosen to match at least one beam'
    if (tProcInfo_G%qRoot) print*, 'Please recall that the matching is only done', &
                                  'for the in-undulator weak or strong focusing ', &
                                  'of the first module, and not for any FODO lattice!!! '
    if (tProcInfo_G%qRoot) print*, 'alphax and alphay will then be ignored....'
    if (tProcInfo_G%qRoot) print*, '(if this is 1D then you wont care about this!)'
      
  end if

!  if (emitx(1) <= 0.0_wp) emitx(1) = 1.0_wp
!  if (emity(1) <= 0.0_wp) emity(1) = 1.0_wp

!  if (alphax(1) <= 0.0_wp) alphax(1) = 1.0_wp
!  if (alphay(1) <= 0.0_wp) alphay(1) = 1.0_wp


! Set the error flag and exit
  qOK = .TRUE.
  GOTO 2000

1000 CALL Error_log('Error in Read_Data:read_beamfile',tErrorLog_G)
    PRINT*,'Error in read_beamfile'
2000 CONTINUE

END SUBROUTINE read_beamfile

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE read_seedfile(se_f, nseeds,sSigmaF,sA0_X,sA0_Y,freqf,ph_sh,&
                         qFlatTop, meanZ2,field_file,qOK)

  IMPLICIT NONE

!                     ARGUMENTS

  CHARACTER(*), INTENT(IN) :: se_f     ! seed file name
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sSigmaF(:,:), &
                                             sA0_X(:),sA0_Y(:), &
                                             freqf(:), meanZ2(:), &
                                             ph_sh(:)
  CHARACTER(len=1024), INTENT(INOUT), ALLOCATABLE :: field_file(:) ! field file names
  INTEGER(KIND=IP), INTENT(INOUT) :: nseeds

  LOGICAL, ALLOCATABLE, INTENT(OUT) :: qFlatTop(:)
  LOGICAL, INTENT(OUT) :: qOK

!                     LOCAL ARGS

  INTEGER(KIND=IP) :: s_ind
  INTEGER::ios
  CHARACTER(len=1024) :: dtype


  namelist /nslist/ nseeds,dtype
  namelist /slist/ freqf, ph_sh, sA0_X, sA0_Y, sSigmaF, &
                   qFlatTop, meanZ2, qRndFj_G,  sSigFj_G, &
                   qMatchS_G
  namelist /sh5list/ field_file

  qOK = .FALSE.


! Default vals

  nseeds = 1
  dtype = 'simple'
  allocate(field_file(1))
  field_file = ''

!   Open the file
  if (se_f .ne. '') then
    open(161,file=se_f, status='OLD', recl=80, delim='APOSTROPHE')
    read(161,nml=nslist)
  end if
  ALLOCATE(sSigmaF(nseeds,3))
  ALLOCATE(sA0_X(nseeds), sA0_Y(nseeds), ph_sh(nseeds))
  ALLOCATE(freqf(nseeds),qFlatTop(nseeds),meanZ2(nseeds))
  allocate(qRndFj_G(nseeds), sSigFj_G(nseeds))
  allocate(qMatchS_G(nseeds))

!  Default value

  sSigmaF = 1.0_wp
  freqf = 1.0_wp
  ph_sh = 0.0_wp
  sA0_X = 0.0_wp
  sA0_Y = 0.0_wp
  qFlatTop = .false.
  meanZ2 = 0.0_wp
  qRndFj_G = .false.
  sSigFj_G = 0.01_wp
  qMatchS_G = .true.
  if (dtype == 'simple') then 
    if (se_f .ne. '') then
      read(161,nml=slist)
      close(UNIT=161,STATUS='KEEP')
    end if

    iFieldSeedType_G=iSimpleSeed_G

! Set the error flag and exit
    qOK = .TRUE.
    GOTO 2000
  end if



  if (dtype == 'h5') then
    iFieldSeedType_G=iReadH5Field_G

    if (se_f .ne. '') then
      read(161,nml=sh5list)
      close(UNIT=161,STATUS='KEEP')
    end if

    qOK = .TRUE.
    GOTO 2000
    
  end if

1000 CALL Error_log('Error in Read_Data:read_seedfile',tErrorLog_G)
    PRINT*,'Error in read_seedfile'
2000 CONTINUE



END SUBROUTINE read_seedfile


subroutine get_exec()


! Retrieves command line used to initiate run

  call get_command(cmd_call_G)

end subroutine get_exec


END MODULE Read_data
