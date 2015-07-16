!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE SETUPTRANS

USE paratype
USE ParallelInfoType
USE typesAndConstants
USE functions
USE IO

IMPLICIT NONE

! This module contains the subroutines used to precondition
! the beam in Puffin. This involves matching the electron
! beam to the undulator, working out the necessary active nodes
! and checking diffraction length of the resonant wavelength.
! -Lawrence Campbell
!  20th Jan 2013
!
! Contact : lawrence.campbell@strath.ac.uk
!           University of Strathclyde
!           Glasgow

CONTAINS


SUBROUTINE MatchBeams(srho,sEmit_n,saw, &
                      sFF,sgamr,&
                      iNNE,sLenE,sSigE, &
                      sSigF,iNNF,sLenF,&
                      sDelF,zUndType,iRNX,iRNY, &
                      ux,uy,qOK)

! Subroutine which matches the beam in x and y
! and defines the inner field nodes to use in
! the linear solver calculation
!
!         ARGUMENTS
!
! srho                FEL parameter
! sEmit_n             Scaled emmittance
! saw                 RMS undulator parameter
! sgamma_r            Relativistic factor for beam energy 
! sFF                 Focussing factor
! sUndPer             Undulator period
! ux, uy              Undulator polarization
! iNNF                Number of field nodes in each dimension (x,y,z2)
! sLenE               Length of electron pulse in each dimension 
!                     (x,y,z2,px,py,p2)
! sSigE               Electron pulse standard deviation in each
!                     dimension (x,y,z2,px,py,p2)
! sSigF               Seed field standard deviation in each dimension
!                     (x,y,z2)
! sLenF               Total modelled length of radiation field in each 
!                     dimension (x,y,z2)
! sDelF               Distance between radiation field nodes in (x,y,z2)
! iRNX                Total modelled length of radiation field inner
!                     'active' nodes to be driven by the electron beam
!                     in x
! iRNY                Total modelled length of radiation field inner
!                     'active' nodes to be driven by the electron beam
!                     in y
! qOK                 Error flag

  REAL(KIND=WP), INTENT(IN) :: srho,sEmit_n(:),saw, &
                               sFF,sgamr, &
                               ux, uy

  INTEGER(KIND=IP), INTENT(IN) :: iNNF(:),iNNE(:,:)

  REAL(KIND=WP), INTENT(INOUT) :: sLenE(:,:), sSigE(:,:), &
                                  sSigF(:), sLenF(:),&
                                  sDelF(:) 

  character(32_IP),  intent(in)  :: zUndType
  INTEGER(KIND=IP), INTENT(INOUT) :: iRNX,iRNY

  LOGICAL, INTENT(OUT) :: qOK

!     LOCAL ARGS:-
!
! sKbeta             scaled betatron wavenumber
! seta               Scaled longitudinal velocity
! sBetaz             longitudinal velocity normalized to c
! qOKL               Local error flag

  real(kind=wp) :: sbetaz, seta, sKbeta
  LOGICAL :: qOKL
  
!     Set error flag

  qOK = .FALSE.

!     Get k_beta and eta...



  sKbeta = saw / 2.0_WP / sFF / sRho / sgamr

  sbetaz = SQRT(sgamr**2.0_WP - 1.0_WP - (saw)**2.0_WP) / &
                 sgamr

  seta = (1.0_WP - sbetaz) / sbetaz


!     Matching 1st beam only for now.....

  CALL MatchBeam(srho,sEmit_n(1),sKbeta, &
                 sFF,sEta,&
                 iNNE(1,:),sLenE(1,:),sSigE(1,:), &
                 sSigF,iNNF,sLenF,&
                 sDelF,iRNX,iRNY, &
                 zUndType,ux,uy,qOKL)

  IF (.NOT. qOKL) GOTO 1000
  
  qOK = .TRUE.
  
  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:matchbeam',tErrorLog_G)

2000 CONTINUE
                     
END SUBROUTINE MatchBeams

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE MatchBeam(srho,sEmit_n,sKbeta, &
                      sFF,sEta,&
                      iNNE,sLenE,sSigE, &
                      sSigF,iNNF,sLenF,&
                      sDelF,iRNX,iRNY, &
                      zUndType,ux,uy,qOK)

! Subroutine which matches the beam in x and y
! and defines the inner field nodes to use in
! the linear solver calculation
!
!         ARGUMENTS
!
! srho                FEL parameter
! sEmit_n             Scaled emmittance
! saw                 RMS undulator parameter
! sgamma_r            Relativistic factor for beam energy 
! sFF                 Focussing factor
! sUndPer             Undulator period
! ux, uy              Undulator polarization
! iNNF                Number of field nodes in each dimension (x,y,z2)
! sLenE               Length of electron pulse in each dimension 
!                     (x,y,z2,px,py,p2)
! sSigE               Electron pulse standard deviation in each
!                     dimension (x,y,z2,px,py,p2)
! sSigF               Seed field standard deviation in each dimension
!                     (x,y,z2)
! sLenF               Total modelled length of radiation field in each 
!                     dimension (x,y,z2)
! sDelF               Distance between radiation field nodes in (x,y,z2)
! iRNX                Total modelled length of radiation field inner
!                     'active' nodes to be driven by the electron beam
!                     in x
! iRNY                Total modelled length of radiation field inner
!                     'active' nodes to be driven by the electron beam
!                     in y
! qOK                 Error flag

  REAL(KIND=WP), INTENT(IN) :: srho,sEmit_n,sKbeta, &
                                 sFF,sEta, &
                                 ux, uy

  INTEGER(KIND=IP), INTENT(IN) :: iNNF(:),iNNE(:)

  REAL(KIND=WP), INTENT(INOUT) :: sLenE(:), sSigE(:), &
                                  sSigF(:), sLenF(:),&
                                  sDelF(:) 

  character(32_IP),  intent(in)  :: zUndType

  INTEGER(KIND=IP), INTENT(INOUT) :: iRNX,iRNY

  LOGICAL, INTENT(OUT) :: qOK

!     LOCAL ARGS:-
! qOKL               Local error flag

  LOGICAL :: qOKL
  
!     Set error flag

  qOK = .FALSE.

!     Get matched beam sigma in x and y
                        
  CALL GetMBParams(srho,sEmit_n,sKbeta,&
                   sFF,sEta,sLenE,sSigE, &
                   sSigF,ux,uy,zUndType,qOKL)

  IF (.NOT. qOKL) GOTO 1000

!     Define inner 'active' node set based on max
!     transverse radius of beam

!     in x...

  CALL GetInnerNodes(iNNF(iX_CG),iNNE(iX_CG),&
                     sLenE(iX_CG),sLenF(iX_CG),&
                     sDelF(iX_CG),iRNX,&
                     qOKL)

  IF (.NOT. qOKL) GOTO 1000
  
!     ...and y.

  CALL GetInnerNodes(iNNF(iY_CG),iNNE(iY_CG),&
                     sLenE(iY_CG),sLenF(iY_CG),&
                     sDelF(iY_CG),iRNY,&
                     qOKL)

  IF (.NOT. qOKL) GOTO 1000
  
  qOK = .TRUE.
  
  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:matchbeam',tErrorLog_G)

2000 CONTINUE
                     
END SUBROUTINE MatchBeam

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE GetInnerNodes(iNodes,iNumElectrons,&
                         sLenEPulse,sWigglerLength,&
                         sLengthOfElm,iRedNodes,&
                         qOK)

IMPLICIT NONE

! This subroutine calculates the number of inner 'active'
! radiation field nodes, and the element (sampling) length,
! so that the element length is matched to the electron
! macroparticle sampling length. It assumes the electron
! pulse transverse radius is matched to the undulator.
!
!        ARGUMENTS
!
! iNodes                 Number of radiation field nodes in
!                        each dimension (x,y,z2)
! iNumElectrons          Number of macroparticles in each 
!                        dimension (x,y,z2,px,py,pz2)
! sLenEPulse             Electron pulse length in x,y,z2
! sLengthOfElm           Radiation field element length in
!                        x,y,z2.

  INTEGER(KIND=IP), INTENT(IN) :: iNodes,iNumElectrons
  REAL(KIND=WP), INTENT(IN) :: sLenEPulse
  REAL(KIND=WP), INTENT(OUT) :: sWigglerLength,sLengthOfElm
  INTEGER(KIND=IP), INTENT(INOUT) :: iRedNodes
  LOGICAL, INTENT(OUT) :: qOK

!        Local args:-

  REAL(KIND=WP) :: dx,maxr,redwigglength
  INTEGER(KIND=IP) :: nninner
  LOGICAL :: qOKL

  qOK = .FALSE.

!     Match electron macroparticle element lengths to field
!     element lengths.

  dx = sLenEPulse / REAL(iNumElectrons,KIND=WP) !macroparticle dx

!     Make length of radiation elm = len of macroparticle elm

  sLengthOfElm=dx 

!     Max matched radius of beam
                                   
  maxr = sLenEPulse * SQRT(2.0_WP) 

!     Num inner nodes

  nninner = ceiling(maxr / sLengthOfElm) + 1_IP

  redwigglength = dx*REAL((nninner-1_IP),KIND=WP)

  sWigglerLength = REAL(iNodes-1,KIND=WP)*dx

  IF(tProcInfo_G%qRoot) THEN

    PRINT*, 'le/ne= ', sLenEPulse/iNumElectrons
    PRINT*, 'lw/nn= ', sWigglerLength/REAL(iNodes-1,KIND=WP)

  END IF

!     Ensure the reduced node set in the transverse plane is at least
!     large enough to track all the macroparticles in the case
!     of a mono-energetic matched beam...

  IF (iRedNodes < NINT(redwigglength/sLengthOfElm)+1_IP) THEN

    iRedNodes=NINT(redwigglength/sLengthOfElm)+1_IP

  END IF

!     Ensure if number of electron macroparticles in x and y are even, then 
!     so is the number of 'active' elements (so that each macroparticle is 
!     initialized in the center of each element in the transverse plane.)... 
 
  IF (MOD(iNodes,2)==0) THEN

    IF (MOD(iRedNodes,2)==1) iRedNodes=iRedNodes+1  

  END IF
 
  IF (MOD(iNodes,2)==1) THEN

    IF (MOD(iRedNodes,2)==0) iRedNodes=iRedNodes+1  

  END IF
  
  qOK = .TRUE.
  
  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:GetInnerNodes',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE GetInnerNodes

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE GetMBParams(srho,sEmit_n,k_beta,sFF,sEta,sLenE, &
                        sSigE,sSigF,ux,uy,zUndType,qOK)

    IMPLICIT NONE

! Calculate matched beam sigma and adjust sampling lengths
!
!               ARGUMENTS
!
! srho               FEL parameter
! sEmit_n            Scaled beam emittance
! saw                RMS undulator parameter
! sgamma_r           Relativistic factor of beam energy
! sFF                Focussing factor - sqrt(2) for natural helical
!                    wiggler
! sUndPer            Wiggler wavelength
! sLenE              Length of electron Pulse in x,y,z2 direction
! sSigE              Sigma spread of electron beam gaussian distribution
!                    in each dimension (x,y,z2,px,py,p2)
! sSigF              Seed field sigma spread for gaussian distribution
!                    in each dimension (x,y,z2,px,py,p2)
! ux,uy              Polarization variables of undulator
! qOK                Error flag

  REAL(KIND=WP), INTENT(IN)    :: srho	      
  REAL(KIND=WP), INTENT(IN)    :: sEmit_n	      
  REAL(KIND=WP), INTENT(IN)    :: k_beta	      
  REAL(KIND=WP), INTENT(IN)    :: sFF,sEta
  REAL(KIND=WP), INTENT(INOUT) :: sLenE(:)	   
  REAL(KIND=WP), INTENT(INOUT) :: sSigE(:)    
  REAL(KIND=WP), INTENT(INOUT) :: sSigF(:)
  REAL(KIND=WP), INTENT(IN)    :: ux, uy	
  character(32_IP),  intent(in)  :: zUndType
  LOGICAL,       INTENT(OUT)   :: qOK

!          LOCAL VARS

  INTEGER(KIND=IP) :: error

!     Set error flag to false

  qOK = .FALSE.

!     Matched beam radius used for electron sigma spread        

  sSigE(iX_CG:iY_CG) = MatchedBeamRadius(srho,&
                 sEmit_n,k_beta)

!     p spread

  if (zUndType == 'curved' .or. zUndType == 'planepole') then

    sSigE(iPX_CG:iPY_CG) = 18.0_WP*sEmit_n* &
              SQRT(ux**2 + uy**2) / (2.0_WP * SQRT(2.0_WP)) * &
              SQRT(sEta) / &
              (sFF*k_beta*sSigE(iX_CG:iY_CG))

  else

    sSigE(iPX_CG:iPY_CG) = 18.0_WP*sEmit_n* &
              SQRT(ux**2 + uy**2) / (2.0_WP * SQRT(2.0_WP)) * &
              SQRT(sEta) / &
              (sFF*k_beta*sSigE(iX_CG:iY_CG))

  end if
!     The sigma values above are the rms radii
!     Need to change to sigma of current distribution
!     Here it is assumed lex=6sigma

  sSigE(iX_CG:iY_CG)=sSigE(iX_CG:iY_CG)/3.0_WP/sqrt(2.0_WP)
  sSigE(iPX_CG:iPY_CG)=sSigE(iPX_CG:iPY_CG)/3.0_WP/sqrt(2.0_WP)

!     Length of electron pulse from new sigma, modelling to 6*sigma
         
  sLenE(iX_CG:iY_CG)   = 6.0_WP * sSigE(iX_CG:iY_CG)
         
  sLenE(iPX_CG:iPY_CG) = 6.0_WP * sSigE(iPX_CG:iPY_CG)

!     Seed field sigma spread made equal to electron 
!     sigma spread         


  sSigF(iX_CG) = sSigE(iX_CG)
         
  sSigF(iY_CG) = sSigE(iY_CG)
  

  if (tProcInfo_G%qRoot) print*,''
  if (tProcInfo_G%qRoot) print*, '---------------------'
  IF (tProcInfo_G%qRoot) PRINT*, 'Matching field grid sampling to beam sampling...'
	  
  IF (tProcInfo_G%qRoot) PRINT*, 'New Gaussian sigma of field in x is', sSigF(iX_CG)
  
  IF (tProcInfo_G%qRoot) PRINT*, 'New Gaussian sigma of electron beam in x is ',sSigE(iX_CG)

  IF (tProcInfo_G%qRoot) PRINT*, '...so total sampled length of beam is ', sLenE(iX_CG)
  if (tProcInfo_G%qRoot) print*,''
  IF (tProcInfo_G%qRoot) PRINT*, 'New Gaussian sigma of e-beam in px is ', sSigE(iPX_CG:iPY_CG)
  if (tProcInfo_G%qRoot) print*,''
  IF (tProcInfo_G%qRoot) PRINT*, 'Scaled Rayleigh length (in gain lengths) = ', sEmit_n/k_beta/2.0_wp

  IF (tProcInfo_G%qRoot) PRINT*, 'Scaled betatron wavelength (in gain lengths) = ', 2.0_WP*pi/k_beta

! Set error flag and exit         

  qOK = .TRUE.				    

  GOTO 2000     

! Error Handler

1000 CALL Error_log('Error in DFunctions:CalculateMatchedBeamParameters',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE GetMBParams

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE CheckSourceDiff(sDelZ,iSteps,srho,sSigE,sLenF,sDelF,iNNF,qOK)

! Subroutine which checks the radiation field in x and y is sampled 
! to a large enough length to model diffraction of the resonant
! FEL wavelength, based on the initial electron beam sigma.
!
!          ARGUMENTS
!
! 
!
  REAL(KIND=WP), INTENT(IN) :: sDelZ,sSigE(:,:),srho
  
  INTEGER(KIND=IP), INTENT(IN) :: iSteps,iNNF(:)
  
  REAL(KIND=WP), INTENT(INOUT) :: sDelF(:),sLenF(:)
  
  LOGICAL, INTENT(OUT) :: qOK

!          LOCAL ARGS
!
! qUpdate        Update length for diffraction?
! qOKL           Local error flag

  LOGICAL  :: qUpdate,qOKL

!     Set error flag

  qOK = .FALSE.

! Checking the wiggler has enough space in x and y for 
! diffraction based on the initial parameters
! X:-

  CALL Check4Diff(sDelZ*REAL(iSteps,KIND=WP),&
            RaleighLength(srho,sSigE(1,iX_CG)),&
            sSigE(1,iX_CG),&
            sLenF(iX_CG),& 
            qUpdate,&
            qOKL)

  IF (.NOT. qOKL) GOTO 1000

  IF (qUpdate) THEN
    
!    sDelF(iX_CG) = sLenF(iX_CG) / REAL(iNNF(iX_CG)-1_IP,KIND=WP)
          
    IF(tProcInfo_G%QROOT)PRINT *, &
      'WARNING: INITIAL E BEAM SIGMA IS TOO SMALL', &
      'THERE MAY BE TOO MUCH DIFFRACTION IN Y'
  
  ENDIF

! Y:-

  CALL Check4Diff(sDelZ*REAL(iSteps,KIND=WP),&
            RaleighLength(srho,sSigE(1,iY_CG)),&
            sSigE(1,iY_CG),&
            sLenF(iY_CG),& 
            qUpdate, &
            qOKL)

  IF (.NOT. qOKL)  GOTO 1000

  IF (qUpdate) THEN 
    
!    sDelF(iY_CG) = sLenF(iY_CG) / REAL(iNNF(iY_CG)-1_IP,KIND=WP)
      
    IF(tProcInfo_G%QROOT)PRINT *, &
      'WARNING: INITIAL E BEAM SIGMA IS TOO SMALL', &
      'THERE MAY BE TOO MUCH DIFFRACTION IN Y'
      
  ENDIF

!     Set error flag and exit

  qOK = .TRUE.
  
  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:CheckXYDiff',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE CheckSourceDiff

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Check4Diff(z,sRaleighLength,&
       sigma,sWigglerLength,qUpdatedWigglerLength,qOK)

  IMPLICIT NONE

! Check wiggler long enough to allow for diffraction
!
! ARGS:-
!
! z                     - INPUT  - Total z diffracting over
! sRaleighLength        - INPUT  - Raleigh length
! sigma                 - INPUT  - Field sigma
! sWigglerLength	- UPDATED - Wiggler length
! qUpdatedWigglerLength - OUTPUT - If updated wiggler length
! qOK			- OUTPUT - Error flag

  REAL(KIND=WP),INTENT(IN)    :: z,sRaleighLength,sigma
  REAL(KIND=WP),INTENT(INOUT) :: sWigglerLength
  LOGICAL,      INTENT(OUT)   :: qUpdatedWigglerLength
  LOGICAL,      INTENT(OUT)   :: qOK

! Define local variables
!
! sDiffractionLength    - Length required for diffraction

  REAL(KIND=WP) :: sDiffractionLength

!     Set error flag to false         

  qOK = .FALSE.         

!     Set updated wiggler length to false

  qUpdatedWigglerLength = .FALSE.

!     Calculate the length required for diffraction         
    
  sDiffractionLength = DiffractionLength(z,&
         sRaleighLength,sigma)

!     If wiggler length is smaller than required
!     for diffraction set to required wiggler length        
  IF (sWigglerLength<sDiffractionLength) THEN

!    sWigglerLength=sDiffractionLength
    qUpdatedWigglerLength = .TRUE.  

  ENDIF

!     Set error flag and exit
    
  qOK = .TRUE.				    
  
  GOTO 2000     

1000 CALL Error_log('Error in setupcalcs:Check4Diff',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE Check4Diff  
  
END MODULE SETUPTRANS
