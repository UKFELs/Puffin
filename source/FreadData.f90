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

CONTAINS

SUBROUTINE read_in(zfilename, &
       zDataFileName, &
       qSeparateFiles, &
       qFormattedFiles, &
       qResume, &
       sStepSize, &
       nSteps, &
       sZ0, &
       LattFile,&
       iWriteNthSteps, &
       iIntWriteNthSteps_l, &
       tArrayZ, &
       tArrayA, &
       tArrayVariables, &
       sLenEPulse, &
       iNumNodes, &
       sWigglerLength, &
       sLengthofElm, &
       iRedNodesX,iRedNodesY, &
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
       sFiltFact, &
       sDiffFrac, &
       sBeta, &
       srho, &
       saw, &
       sgamma, &
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
       qFlatTopS, nseeds, &
       sPEOut, &
       iDumpNthSteps, &
       qSwitches, &
       qOK)
       
       IMPLICIT NONE
       
!******************************************************
! Read input data from a file
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

  CHARACTER(32_IP),  INTENT(OUT)  :: zDataFileName
  LOGICAL,           INTENT(OUT)  :: qSeparateFiles
  LOGICAL,           INTENT(OUT)  :: qFormattedFiles
  LOGICAL,           INTENT(OUT)  :: qResume
  REAL(KIND=WP),     INTENT(OUT)  :: sStepSize
  INTEGER(KIND=IP),  INTENT(OUT)  :: nSteps
  REAL(KIND=WP) ,    INTENT(OUT)  :: sZ0
  CHARACTER(32_IP),  INTENT(INOUT):: LattFile
    
  INTEGER(KIND=IP),  INTENT(OUT)  :: iWriteNthSteps, iIntWriteNthSteps_l
  TYPE(cArraySegment)             :: tArrayZ
  TYPE(cArraySegment)             :: tArrayA(:)
  TYPE(cArraySegment)             :: tArrayVariables(:)

  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sLenEPulse(:,:)
  INTEGER(KIND=IP),  INTENT(OUT)  :: iNumNodes(:)
    
  REAL(KIND=WP),     INTENT(OUT)  :: sWigglerLength(:) , sLengthofElm(:)  
    
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
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: freqf(:), SmeanZ2(:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sSigmaF(:,:)
  LOGICAL, ALLOCATABLE, INTENT(OUT) :: qFlatTopS(:)
  LOGICAL, INTENT(out) :: qSimple
  CHARACTER(*), ALLOCATABLE, INTENT(INOUT) :: dist_f(:)
  
  REAL(KIND=WP),     INTENT(OUT)  :: sFiltFact,sDiffFrac,sBeta
  REAL(KIND=WP),     INTENT(OUT)  :: srho
  REAL(KIND=WP),     INTENT(OUT)  :: saw
  REAL(KIND=WP),     INTENT(OUT)  :: sgamma, lambda_w
  REAL(KIND=WP),     INTENT(OUT)  :: sux
  REAL(KIND=WP),     INTENT(OUT)  :: suy
  REAL(KIND=WP),     INTENT(OUT)  :: Dfact
  REAL(KIND=WP),     INTENT(OUT)  :: sFocusfactor, taper
  character(32_IP),  intent(out)  :: zUndType
  REAL(KIND=WP),     INTENT(OUT)  :: sPEOut
  INTEGER(KIND=IP),  INTENT(OUT)  :: iDumpNthSteps
  LOGICAL,           INTENT(OUT)  :: qSwitches(:)
  LOGICAL,           INTENT(OUT)  :: qOK

! Define local variables
    
  integer(kind=ip) :: stpsprlam, nodesperlambda, nperiods ! Steps per lambda_w, nodes per lambda_r
  real(kind=wp) :: dz2, zbar
  integer(kind=ip) :: nwaves

  INTEGER::ios
  CHARACTER(32_IP) :: beam_file, seed_file
  LOGICAL :: qOKL, qMatched !   TEMP VAR FOR NOW, SHOULD MAKE FOR EACH BEAM


! Begin subroutine:
! Set error flag to false         
!
    qOK = .FALSE.

! Initialise array!
  qSwitches = .FALSE.

! Open the file         
  OPEN(UNIT=168,FILE=zfilename,IOSTAT=ios,&
       ACTION='READ',POSITION='REWIND')
  IF  (ios/=0_IP) THEN
     GOTO 1000
  END IF


!     Read in blank space at top of file

  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)

!     Read in flags and switches....

  READ(UNIT=168,FMT=*) qSwitches(iOneD_CG)
  READ(UNIT=168,FMT=*) qSwitches(iFieldEvolve_CG)
  READ(UNIT=168,FMT=*) qSwitches(iElectronsEvolve_CG)
  READ(UNIT=168,FMT=*) qSwitches(iElectronFieldCoupling_CG)
  READ(UNIT=168,FMT=*) qSwitches(iFocussing_CG)
  READ(UNIT=168,FMT=*) qSwitches(iMatchedBeam_CG)
  READ(UNIT=168,FMT=*) qSwitches(iDiffraction_CG)
  READ(UNIT=168,FMT=*) qfilter
  READ(UNIT=168,FMT=*) q_noise    ! qSwitches(iNoise_CG)
  READ(UNIT=168,FMT=*) qUndEnds_G
  READ(UNIT=168,FMT=*) qSwitches(iDump_CG)
  READ(UNIT=168,FMT=*) qResume
  READ(UNIT=168,FMT=*) qSeparateFiles
  READ(UNIT=168,FMT=*) qFormattedFiles  
      
  READ(UNIT=168,FMT=*) tArrayZ%qWrite
  tArrayZ%zVariable = 'Z' ! Assign SDDS column names
    
  READ(UNIT=168,FMT=*) tArrayA(iRe_A_CG)%qWrite        
  tArrayA(iRe_A_CG)%zVariable = 'RE_A'
    
  tArrayA(iIm_A_CG)%qWrite = tArrayA(iRe_A_CG)%qWrite	   
  tArrayA(iIm_A_CG)%zVariable = 'IM_A'
    
  READ(UNIT=168,FMT=*) tArrayVariables(iRe_PPerp_CG)%qWrite
  tArrayVariables(iRe_PPerp_CG)%zVariable = 'RE_PPerp'
      tArrayVariables(iIm_PPerp_CG)%qWrite = &
       tArrayVariables(iRe_PPerp_CG)%qWrite
  tArrayVariables(iIm_PPerp_CG)%zVariable = 'IM_PPerp'
    
  READ(UNIT=168,FMT=*) tArrayVariables(iRe_Gam_CG)%qWrite
  tArrayVariables(iRe_Gam_CG)%zVariable = 'Gamma'
    
  READ(UNIT=168,FMT=*) tArrayVariables(iRe_Z2_CG)%qWrite
  tArrayVariables(iRe_Z2_CG)%zVariable = 'Z2'
    
  READ(UNIT=168,FMT=*) tArrayVariables(iRe_X_CG)%qWrite
  tArrayVariables(iRe_X_CG)%zVariable = 'X'	
    	
  READ(UNIT=168,FMT=*) tArrayVariables(iRe_Y_CG)%qWrite
  tArrayVariables(iRe_Y_CG)%zVariable = 'Y'	


!     Read whitespace...

  READ(UNIT=168,FMT=*) 
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)

!     Read electron beam params 

    READ(UNIT=168,FMT=*) beam_file
    READ(UNIT=168,FMT=*) sElectronThreshold

!     Read whitespace...

  READ(UNIT=168,FMT=*) 
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*) 
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)   
    
!     Read field params
    
  READ(UNIT=168,FMT=*) iNumNodes(iX_CG)
  READ(UNIT=168,FMT=*) iNumNodes(iY_CG)
  READ(UNIT=168,FMT=*) nodesperlambda
  READ(UNIT=168,FMT=*) sWigglerLength(iX_CG)
  READ(UNIT=168,FMT=*) sWigglerLength(iY_CG)
  READ(UNIT=168,FMT=*) sWigglerLength(iZ2_CG)
  READ(UNIT=168,FMT=*) iRedNodesX
  READ(UNIT=168,FMT=*) iRedNodesY
  READ(UNIT=168,FMT=*) sFiltFact
  READ(UNIT=168,FMT=*) sDiffFrac
  READ(UNIT=168,FMT=*) sBeta
  READ(UNIT=168,FMT=*) seed_file

!     Read whitespace...

  READ(UNIT=168,FMT=*) 
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*) 
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)  
  READ(UNIT=168,FMT=*)      
  READ(UNIT=168,FMT=*)  
    
            
!     Read Independant vars 

  READ(UNIT=168,FMT=*) srho
  READ(UNIT=168,FMT=*) sux
  READ(UNIT=168,FMT=*) suy  
  READ(UNIT=168,FMT=*) saw
  READ(UNIT=168,FMT=*) sgamma
  READ(UNIT=168,FMT=*) sFocusfactor
  READ(UNIT=168,FMT=*) lambda_w
  READ(UNIT=168,FMT=*) Dfact
  READ(UNIT=168,FMT=*) zUndType
!  READ(UNIT=168,FMT=*) kx_und_G
!  READ(UNIT=168,FMT=*) ky_und_G
  READ(UNIT=168,FMT=*) taper
  
!     Read whitespace...

  READ(UNIT=168,FMT=*) 
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*) 
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)  
  READ(UNIT=168,FMT=*)      
  READ(UNIT=168,FMT=*) 
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  
  
!     Read vars for integration lengths and ouput


  READ(UNIT=168,FMT=*) LattFile  
  READ(UNIT=168,FMT=*) stpsprlam
  READ(UNIT=168,FMT=*) nperiods
  READ(UNIT=168,FMT=*) sZ0
  READ(UNIT=168,FMT=*) zDataFileName 
  READ(UNIT=168,FMT=*) iWriteNthSteps 
  READ(UNIT=168,FMT=*) iIntWriteNthSteps_l  
  READ(UNIT=168,FMT=*) iDumpNthSteps  
  READ(UNIT=168,FMT=*) sPEOut  ! Put to 100% if all are to be written
  
  CLOSE(UNIT=168,STATUS='KEEP')  



  sStepSize = 4.0_WP * pi * srho / real(stpsprlam,kind=wp)
  nSteps = nperiods * stpsprlam

  if (tProcInfo_G%qRoot) print*, 'step size is --- ', sStepSize






  dz2 = 4.0_WP * pi * srho / real(nodesperlambda-1_IP,kind=wp)

  iNumNodes(iZ2_CG) = ceiling(sWigglerLength(iZ2_CG) / dz2) + 1_IP

  if (tProcInfo_G%qRoot) print*, 'number of nodes in z2 --- ', iNumNodes(iZ2_CG)

  zBFile_G = beam_file
  zSFile_G = seed_file

  CALL read_beamfile(qSimple, dist_f, beam_file,sEmit_n,sSigmaGaussian,sLenEPulse, &
                     iNumElectrons,sQe,chirp,bcenter, mag, fr, gamma_d,nbeams, &
                     qMatched,qOKL)

  CALL read_seedfile(seed_file,nseeds,sSigmaF,sA0_Re,sA0_Im,freqf,&
                     qFlatTopS,SmeanZ2,qOKL)


  call FileNameNoExtension(beam_file, zBFile_G, qOKL)
  call FileNameNoExtension(seed_file, zSFile_G, qOKL)

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
                         qMatched,qOK)

  IMPLICIT NONE

!                     ARGUMENTS

  LOGICAL, INTENT(OUT) :: qSimple
  CHARACTER(*), INTENT(INOUT) :: be_f     ! beam file name
  CHARACTER(*), INTENT(INOUT), ALLOCATABLE :: dist_f(:)     ! dist file names
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sEmit_n(:),chirp(:), mag(:), fr(:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sSigmaE(:,:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sLenE(:,:)
  INTEGER(KIND=IP), ALLOCATABLE, INTENT(OUT) :: iNumElectrons(:,:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sQe(:),bcenter(:),gammaf(:)
  INTEGER(KIND=IP), INTENT(INOUT) :: nbeams
  LOGICAL, INTENT(OUT) :: qOK, qMatched

!                     LOCAL ARGS

  INTEGER(KIND=IP) :: b_ind
  INTEGER::ios
  CHARACTER(96) :: dum1, dum2, dtype

  qOK = .FALSE.
  
! Open the file         
  OPEN(UNIT=168,FILE=be_f,IOSTAT=ios,&
    ACTION='READ',POSITION='REWIND')
  IF  (ios/=0_IP) THEN
    CALL Error_log('Error in read_in:OPEN(input file) not performed correctly, IOSTAT/=0',tErrorLog_G)
    GOTO 1000
  END IF

!     Read in file header to get number of beams

  READ(UNIT=168,FMT=*) dum1, dum2, dtype
  dtype = TRIM(ADJUSTL(dtype))

  if (dtype == 'dist') then
    qSimple = .false.
  else
    qsimple = .true.
  end if

  if (qSimple) then


    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*) nbeams


    ALLOCATE(sSigmaE(nbeams,6))
    ALLOCATE(sLenE(nbeams,6))
    ALLOCATE(iNumElectrons(nbeams,6))
    ALLOCATE(sEmit_n(nbeams),sQe(nbeams),bcenter(nbeams),gammaf(nbeams))
    ALLOCATE(chirp(nbeams))
    allocate(mag(nbeams), fr(nbeams))
    allocate(qRndEj_G(nbeams), sSigEj_G(nbeams))
    
!     Loop round beams, reading in data

    DO b_ind = 1,nbeams

      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*) sSigmaE(b_ind,iX_CG)
      READ(UNIT=168,FMT=*) sSigmaE(b_ind,iY_CG)
      READ(UNIT=168,FMT=*) sSigmaE(b_ind,iZ2_CG)
      READ(UNIT=168,FMT=*) sSigmaE(b_ind,iPX_CG)
      READ(UNIT=168,FMT=*) sSigmaE(b_ind,iPY_CG)
      READ(UNIT=168,FMT=*) sSigmaE(b_ind,iGam_CG)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*) sLenE(b_ind,iX_CG)
      READ(UNIT=168,FMT=*) sLenE(b_ind,iY_CG)
      READ(UNIT=168,FMT=*) sLenE(b_ind,iZ2_CG)
      READ(UNIT=168,FMT=*) sLenE(b_ind,iPX_CG)
      READ(UNIT=168,FMT=*) sLenE(b_ind,iPY_CG)
      READ(UNIT=168,FMT=*) sLenE(b_ind,iGam_CG)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*) iNumElectrons(b_ind,iX_CG)
      READ(UNIT=168,FMT=*) iNumElectrons(b_ind,iY_CG)
      READ(UNIT=168,FMT=*) iNumElectrons(b_ind,iZ2_CG)
      READ(UNIT=168,FMT=*) iNumElectrons(b_ind,iPX_CG)
      READ(UNIT=168,FMT=*) iNumElectrons(b_ind,iPY_CG)
      READ(UNIT=168,FMT=*) iNumElectrons(b_ind,iGam_CG)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*)
      READ(UNIT=168,FMT=*) qMatched
      READ(UNIT=168,FMT=*) gammaf(b_ind)
      READ(UNIT=168,FMT=*) sEmit_n(b_ind)
      READ(UNIT=168,FMT=*) chirp(b_ind)
      READ(UNIT=168,FMT=*) bcenter(b_ind)
      READ(UNIT=168,FMT=*) mag(b_ind)
      READ(UNIT=168,FMT=*) fr(b_ind)      
      READ(UNIT=168,FMT=*) qRndEj_G(b_ind)
      READ(UNIT=168,FMT=*) sSigEj_G(b_ind)
      READ(UNIT=168,FMT=*) sQe(b_ind)
    
    END DO

  else

    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*) nbeams

    allocate(dist_f(nbeams))
    allocate(iNumElectrons(nbeams,6))
    allocate(sLenE(nbeams,6))
    allocate(sSigmaE(nbeams,6))
    allocate(qRndEj_G(nbeams))
    iNumElectrons = 1
    sLenE = 1
    sSigmaE = 1
    !!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!! 
    ! READ IN FNAMES
    !!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!
    READ(UNIT=168,FMT=*) 
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)

    DO b_ind = 1, nbeams
      READ(UNIT=168,FMT=*) dist_f(b_ind)
    END DO

  end if

  CLOSE(UNIT=168,STATUS='KEEP')

! Set the error flag and exit
  qOK = .TRUE.
  GOTO 2000 

1000 CALL Error_log('Error in Read_Data:read_beamfile',tErrorLog_G)
    PRINT*,'Error in read_beamfile'
2000 CONTINUE

END SUBROUTINE read_beamfile

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE read_seedfile(se_f, nseeds,sSigmaF,sA0_X,sA0_Y,freqf,qFlatTop, &
                         meanZ2,qOK)

  IMPLICIT NONE

!                     ARGUMENTS

  CHARACTER(*), INTENT(IN) :: se_f     ! seed file name
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sSigmaF(:,:), &
                                             sA0_X(:),sA0_Y(:), &
                                             freqf(:), meanZ2(:)
  INTEGER(KIND=IP), INTENT(INOUT) :: nseeds

  LOGICAL, ALLOCATABLE, INTENT(OUT) :: qFlatTop(:)
  LOGICAL, INTENT(OUT) :: qOK

!                     LOCAL ARGS

  INTEGER(KIND=IP) :: s_ind
  INTEGER::ios

  qOK = .FALSE.
  
! Open the file         
  OPEN(UNIT=168,FILE=se_f,IOSTAT=ios,&
    ACTION='READ',POSITION='REWIND')
  IF  (ios/=0_IP) THEN
    CALL Error_log('Error in read_in:OPEN(input file) not performed correctly, IOSTAT/=0',tErrorLog_G)
    GOTO 1000
  END IF

!     Read in file header to get number of beams

  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*)
  READ(UNIT=168,FMT=*) nseeds

  ALLOCATE(sSigmaF(nseeds,3))
  ALLOCATE(sA0_X(nseeds), sA0_Y(nseeds))
  ALLOCATE(freqf(nseeds),qFlatTop(nseeds),meanZ2(nseeds))
  allocate(qRndFj_G(nseeds), sSigFj_G(nseeds))
    
!     Loop round seeds, reading in data

  DO s_ind = 1,nseeds

    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*)
    READ(UNIT=168,FMT=*) freqf(s_ind)
    READ(UNIT=168,FMT=*) sA0_X(s_ind)
    READ(UNIT=168,FMT=*) sA0_Y(s_ind)
    READ(UNIT=168,FMT=*) sSigmaF(s_ind,iX_CG)
    READ(UNIT=168,FMT=*) sSigmaF(s_ind,iY_CG)
    READ(UNIT=168,FMT=*) sSigmaF(s_ind,iZ2_CG)
    READ(UNIT=168,FMT=*) qFlatTop(s_ind)
    READ(UNIT=168,FMT=*) meanZ2(s_ind)
    READ(UNIT=168,FMT=*) qRndFj_G(s_ind)
    READ(UNIT=168,FMT=*) sSigFj_G(s_ind)
    
  END DO

  CLOSE(UNIT=168,STATUS='KEEP')

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
