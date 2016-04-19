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
use globals


implicit none

! This module contains the subroutines used to precondition
! the beam in Puffin. This involves matching the electron
! beam transverse radius to the undulator, working out the 
! necessary radiation transverse grid size, and checking 
! diffraction length of the resonant wavelength.
!
! -Lawrence Campbell
!  20th Jan 2013
!
! Contact : lawrence.campbell@strath.ac.uk
!           University of Strathclyde
!           Glasgow

contains





  subroutine stptrns(sSigE, sLenE, iNMPs, sEmit, sGamFrac, &
                     qMatchA, qMatchS, qFMesh, sSigF)

    real(kind=wp), intent(in) :: sEmit(:), sGamFrac(:)

    real(kind=wp), intent(inout) :: sSigE(:,:), sLenE(:,:), sSigF(:,:)

    integer(kind=ip), intent(in) :: iNMPs(:,:)
    logical, intent(in) :: qMatchA(:), qMatchS(:), qFMesh

    real(kind=wp) :: sLenF


    call MatchBeams(sSigE, sLenE, sEmit, sGamFrac, &
                        qMatchA)

    call matchSeeds(qMatchS, sSigE, sSigF)

!    call CheckSourceDiff(sDelZ,iSteps,srho,sSigE,sLenF,sDelF,iNNF,qOK)

    if (qFMesh) call fixXYMesh(sLenE, iNMPs)


    delta_G = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G

  end subroutine stptrns


  subroutine fixXYMesh(sLenE, iNMPs)

    real(kind=wp), intent(in) :: sLenE(:,:) 
    integer(kind=ip), intent(in) :: iNMPs(:,:)
  

    call fixMesh(sLengthOfElmX_G, sLenE(1, iX_CG), iNMPs(1, iX_CG))
    call fixMesh(sLengthOfElmY_G, sLenE(1, iX_CG), iNMPs(1, iY_CG))

  end subroutine fixXYMesh



  subroutine fixMesh(dx, sLenE, iNMPs)


    real(kind=wp), intent(in) :: sLenE 
    integer(kind=ip), intent(in) :: iNMPs
    real(kind=wp), intent(out) :: dx

    dx = sLenE / REAL(iNMPs,kind=wp) !macroparticle dx


!     Make length of radiation elm = len of macroparticle elm / nmps per elm

    dx = dx / sMNum_G

  end subroutine fixMesh











  subroutine matchSeeds(qMatchS, sSigE, sSigF)

!  'Matches' the transverse profile of the seed field
!  to the transverse profile of the electron beam.
!
!  It will try to match each seed field to each beam,
!  - if there are more seeds than beams, then it will
!  match the excess seeds to the first beam.
!
!  qMatchS   - Should this seed be matched?
!  sSigE - rms width of e-beam in each dimension, 
!          for each beam
!  sSigF - rms width of seed FIELD (NOT intensity),
!          in each dimension

    logical, intent(in) :: qMatchS(:)
    real(kind=wp), intent(in) :: sSigE(:,:)
    real(kind=wp), intent(out) :: sSigF(:,:)
    
    integer(kind=ip) :: nseeds, nbeams, ic

    nseeds = size(sSigF(:,1))
    nbeams = size(sSigE(:,1))

    do ic = 1, nSeeds

      if (qMatchS(ic)) then

        if (ic > nbeams) then

          call matchTrEField(sSigE(1,:), sSigF(ic,:))

        else

          call matchTrEField(sSigE(ic,:), sSigF(ic,:))

        end if

      end if

    end do

  end subroutine matchSeeds



  subroutine matchTrEField(sSigE, sSigF)

!  sSigE - rms width of e-beam in each dimension
!  sSigF - rms width of seed FIELD (NOT intensity) in each dimension

    real(kind=wp), intent(in) :: sSigE(:)
    real(kind=wp), intent(out) :: sSigF(:)

    sSigF(iX_CG) = sSigE(iX_CG)
    sSigF(iY_CG) = sSigE(iY_CG)


  end subroutine matchTrEField





























































subroutine MatchBeams(sSigE, sLenE, sEmit, sGamFrac, &
                      qMatchA)

! Subroutine which matches the beam in x and y
!
!         ARGUMENTS

  real(kind=wp), intent(in) :: sEmit(:), sGamFrac(:)
  logical, intent(in) :: qMatchA(:)

  real(kind=wp), intent(inout) :: sLenE(:,:), sSigE(:,:)

  integer(kind=ip) :: nbeams, ic
  
  nbeams = size(sEmit)

  do ic = 1, nbeams

    if (qMatchA(ic)) then

      call matchTransBeam(sSigE(ic,:), sLenE(ic,:), &
                      sEmit(ic), sGamFrac(ic))

      if (tProcInfo_G%qRoot) print*, &
             'New Gaussian sigma of electron beam in x is ',sSigE(ic, iX_CG)
      if (tProcInfo_G%qRoot) print*, &
            '...so total sampled length of beam in x is ', sLenE(ic, iX_CG)
      if (tProcInfo_G%qRoot) print*,''
      if (tProcInfo_G%qRoot) print*, &
            'New Gaussian sigma of e-beam in px is ', sSigE(ic, iPX_CG)
      if (tProcInfo_G%qRoot) print*, &
            'New Gaussian sigma of e-beam in py is ', sSigE(ic, iPY_CG)

    end if

  end do

  end subroutine MatchBeams


  subroutine matchTransBeam(sSigE, sLenE, sEmit, sEnfrac)

    real(kind=wp), intent(in) :: sEmit, sEnfrac
    real(kind=wp), intent(out) :: sSigE(:), sLenE(:)

    real(kind=wp) :: kbx, kby



    call getKBetas(kbx, kby, sEnfrac)

    if (tProcInfo_G%qRoot) print*, &
    'Scaled betatron wavenumber in x (in gain lengths) = ', kbx

    call getKBetas(kbx, kby, sEnfrac)
    if (tProcInfo_G%qRoot) print*, &
    'Scaled betatron wavenumber in y (in gain lengths) = ', kby

    call matchxPx(sSigE(iX_CG), sSigE(iPX_CG), sEmit, &
                  kbx, sEnFrac)

    sLenE(iX_CG) = sSigE(iX_CG) * 6_wp
    sLenE(iPX_CG) = sSigE(iPX_CG) * 6_wp


    call matchxPx(sSigE(iY_CG), sSigE(iPY_CG), sEmit, &
                  kby, sEnFrac)

    sLenE(iY_CG) = sSigE(iY_CG) * 6_wp    
    sLenE(iPY_CG) = sSigE(iPY_CG) * 6_wp    

    if (kbx == 0_wp) then

      sSigE(iX_CG) = sSigE(iY_CG)
      sLenE(iX_CG) = sLenE(iY_CG)
      sSigE(iPX_CG) = sSigE(iPY_CG)
      sLenE(iPX_CG) = sLenE(iPY_CG)

    end if

  end subroutine matchTransBeam


  subroutine getKBetas(kbx, kby, gamma_fr)

    real(kind=wp), intent(out) :: kbx, kby 
    real(kind=wp), intent(in) :: gamma_fr

    kbx = sKBetaX_G / gamma_fr
    kby = sKBetaY_G / gamma_fr

  end subroutine getKBetas



  subroutine matchxPx(sigx, sigpx, emit, kx, sEnFrac)


    real(kind=wp), intent(in) :: emit, kx, sEnfrac
    real(kind=wp), intent(out) :: sigx, sigpx


    if (kx /= 0_wp) then

      sigx = sqrt(sRho_G * emit / kx)
      sigpx = sqrt(sEta_G) / 2.0_wp / sKappa_G * &
               sEnfrac * emit / sigx

    end if



  end subroutine matchxPx




































! subroutine MatchBeams(srho,sEmit,saw, &
!                       sFF,sgamr,&
!                       iNNE,sLenE,sSigE, &
!                       sSigF,iNNF,sLenF,&
!                       sDelF,zUndType,iRNX,iRNY, &
!                       ux,uy,qOK)


!   real(KIND=WP), intent(IN) :: srho,sEmit_n(:),saw, &
!                                sFF,sgamr, &
!                                ux, uy

!   INTEGER(KIND=IP), intent(IN) :: iNNF(:),iNNE(:,:)

!   REAL(KIND=WP), intent(INOUT) :: sLenE(:,:), sSigE(:,:), &
!                                   sSigF(:,:), sLenF(:),&
!                                   sDelF(:) 

!   character(32_IP),  intent(in)  :: zUndType
!   INTEGER(KIND=IP), intent(INOUT) :: iRNX,iRNY

!   LOGICAL, intent(OUT) :: qOK







! !     Matching 1st beam only for now.....

!   do ic = 1, nbeams

!     if (qMatchA(ic)) then

!       CALL MatchBeam(srho,sEmit_n(ic),sKbeta, &
!                      sFF,sEta,sKappa, &
!                      iNNE(ic,:),sLenE(ic,:),sSigE(ic,:), &
!                      zUndType,qOKL)

!     end if  

!   end do

!   if (qMatchFieldMesh) then

!     call matchMesh(match to 1st beam sigmas)

!     ! Match to TOTAL length of beam
!     ! Usually want inner mesh to 
!     ! have 1-2 MPs in each mesh element
!     ! Or if doing ditributed beam with random
!     ! transverse pos's, then the mesh element length
!     ! specified here will equal the beam radius...


!   end if



!   IF (.NOT. qOKL) GOTO 1000
  
!   qOK = .TRUE.
  
!   GOTO 2000

! 1000 CALL Error_log('Error in setupcalcs:matchbeam',tErrorLog_G)

! 2000 CONTINUE
                     
! END SUBROUTINE MatchBeams

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! SUBROUTINE MatchBeam(srho,sEmit_n,sKbeta, &
!                       sFF,sEta,sKappa, &
!                       iNNE,sLenE,sSigE, &
!                       zUndType,qOK)

! ! Subroutine which matches the beam in x and y
! ! and defines the inner field nodes to use in
! ! the linear solver calculation
! !
! !         ARGUMENTS
! !
! ! srho                FEL parameter
! ! sEmit_n             Scaled emmittance
! ! saw                 RMS undulator parameter
! ! sgamma_r            Relativistic factor for beam energy 
! ! sFF                 Focussing factor
! ! sUndPer             Undulator period
! ! ux, uy              Undulator polarization
! ! sLenE               Length of electron pulse in each dimension 
! !                     (x,y,z2,px,py,p2)
! ! sSigE               Electron pulse standard deviation in each
! !                     dimension (x,y,z2,px,py,p2)
! ! qOK                 Error flag

!   REAL(KIND=WP), INTENT(IN) :: srho,sEmit,sKbeta, &
!                                  sFF,sEta, sKappa

!   INTEGER(KIND=IP), INTENT(IN) :: iNNF(:),iNNE(:)

!   REAL(KIND=WP), INTENT(INOUT) :: sLenE(:), sSigE(:), &
!                                   sSigF(:,:), sLenF(:),&
!                                   sDelF(:) 

!   character(32_IP),  intent(in)  :: zUndType

!   INTEGER(KIND=IP), INTENT(INOUT) :: iRNX,iRNY

!   LOGICAL, INTENT(OUT) :: qOK

! !     LOCAL ARGS:-
! ! qOKL               Local error flag

!   LOGICAL :: qOKL
  
! !     Set error flag

!   qOK = .FALSE.

! !     Get matched beam sigma in x and y








!   CALL GetMBParams(srho,sEmit_n,sKbeta,&
!                    sFF,sEta,sKappa,sLenE,sSigE, &
!                    sSigF,zUndType,qOKL)

!   IF (.NOT. qOKL) GOTO 1000

! !     Define inner 'active' node set based on max
! !     transverse radius of beam...

! !     in x...

!   IF(tProcInfo_G%qRoot) THEN

!     PRINT*, 'Matching field grid sampling to beam sampling in xbar...'

!   END IF

!   CALL GetInnerNodes(iNNF(iX_CG),iNNE(iX_CG),&
!                      sLenE(iX_CG),sLenF(iX_CG),&
!                      sDelF(iX_CG),iRNX,&
!                      qOKL)

!   IF (.NOT. qOKL) GOTO 1000
  
! !     ...and y.

!   IF(tProcInfo_G%qRoot) THEN

!     PRINT*, 'Matching field grid sampling to beam sampling in ybar...'

!   END IF


!   CALL GetInnerNodes(iNNF(iY_CG),iNNE(iY_CG),&
!                      sLenE(iY_CG),sLenF(iY_CG),&
!                      sDelF(iY_CG),iRNY,&
!                      qOKL)

!   IF (.NOT. qOKL) GOTO 1000
  
!   qOK = .TRUE.
  
!   GOTO 2000

! 1000 CALL Error_log('Error in setupcalcs:matchbeam',tErrorLog_G)

! 2000 CONTINUE
                     
! END SUBROUTINE MatchBeam

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! SUBROUTINE GetInnerNodes(iNodes,iNumElectrons,&
!                          sLenEPulse,sWigglerLength,&
!                          sLengthOfElm,iRedNodes,&
!                          sMNum, qOK)

! IMPLICIT NONE

! ! This subroutine calculates the number of inner 'active'
! ! radiation field nodes, and the element (sampling) length,
! ! so that the element length is matched to the electron
! ! macroparticle sampling length. It assumes the electron
! ! pulse transverse radius is matched to the undulator.
! !
! !        ARGUMENTS
! !
! ! iNodes                 Number of radiation field nodes in
! !                        each dimension (x,y,z2)
! ! iNumElectrons          Number of macroparticles in each 
! !                        dimension (x,y,z2,px,py,pz2)
! ! sLenEPulse             Electron pulse length in x,y,z2
! ! sLengthOfElm           Radiation field element length in
! !                        x,y,z2.

!   INTEGER(KIND=IP), INTENT(IN) :: iNodes,iNumElectrons
!   REAL(KIND=WP), INTENT(IN) :: sLenEPulse, sMNum
!   REAL(KIND=WP), INTENT(OUT) :: sWigglerLength,sLengthOfElm
!   INTEGER(KIND=IP), INTENT(INOUT) :: iRedNodes
!   LOGICAL, INTENT(OUT) :: qOK

! !        Local args:-

!   REAL(KIND=WP) :: dx,maxr,redwigglength
!   INTEGER(KIND=IP) :: nninner
!   LOGICAL :: qOKL

!   qOK = .FALSE.

! !     Match electron macroparticle element lengths to field
! !     element lengths.

!   dx = sLenEPulse / REAL(iNumElectrons,KIND=WP) !macroparticle dx


! !     Make length of radiation elm = len of macroparticle elm / nmps per elm

!   sLengthOfElm=dx / sMNum











! !     Max matched radius of beam
                                   
!   maxr = sLenEPulse * SQRT(2.0_WP) 

! !     Num inner nodes

!   nninner = ceiling(maxr / sLengthOfElm) + 1_IP

!   redwigglength = dx*REAL((nninner-1_IP),KIND=WP)

!   sWigglerLength = REAL(iNodes-1,KIND=WP)*dx

!   IF(tProcInfo_G%qRoot) THEN

!     PRINT*, ' electron macroparticle spacing = ', sLenEPulse/iNumElectrons
!     PRINT*, ' field node spacing = ', sWigglerLength/REAL(iNodes-1,KIND=WP)

!   END IF

! !     Ensure the reduced node set in the transverse plane is at least
! !     large enough to track all the macroparticles in the case
! !     of a mono-energetic matched beam...

!   IF (iRedNodes < NINT(redwigglength/sLengthOfElm)+1_IP) THEN

!     iRedNodes=NINT(redwigglength/sLengthOfElm)+1_IP

!   END IF

! !     Ensure if number of electron macroparticles in x and y are even, then 
! !     so is the number of 'matched' elements (so that each macroparticle is 
! !     initialized in the center of each element in the transverse plane.)... 
 
!   IF (MOD(iNodes,2)==0) THEN

!     IF (MOD(iRedNodes,2)==1) iRedNodes=iRedNodes+1  

!   END IF
 
!   IF (MOD(iNodes,2)==1) THEN

!     IF (MOD(iRedNodes,2)==0) iRedNodes=iRedNodes+1  

!   END IF
  
!   qOK = .TRUE.
  
!   GOTO 2000

! 1000 CALL Error_log('Error in setupcalcs:GetInnerNodes',tErrorLog_G)

! 2000 CONTINUE

! END SUBROUTINE GetInnerNodes

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! SUBROUTINE GetMBParams(srho,sEmit_n,k_beta,sFF,sEta,sKappa,sLenE, &
!                        sSigE,qOK)

!     IMPLICIT NONE

! ! Calculate matched beam sigma and adjust sampling lengths
! !
! !               ARGUMENTS
! !
! ! srho               FEL parameter
! ! sEmit_n            Scaled beam emittance
! ! saw                RMS undulator parameter
! ! sgamma_r           Relativistic factor of beam energy
! ! sFF                Focussing factor - sqrt(2) for natural helical
! !                    wiggler
! ! sUndPer            Wiggler wavelength
! ! sLenE              Length of electron Pulse in x,y,z2 direction
! ! sSigE              Sigma spread of electron beam gaussian distribution
! !                    in each dimension (x,y,z2,px,py,p2)
! ! sSigF              Seed field sigma spread for gaussian distribution
! !                    in each dimension (x,y,z2,px,py,p2)
! ! ux,uy              Polarization variables of undulator
! ! qOK                Error flag

!   REAL(KIND=WP), INTENT(IN)    :: srho	      
!   REAL(KIND=WP), INTENT(IN)    :: sEmit_n	      
!   REAL(KIND=WP), INTENT(IN)    :: k_beta	      
!   REAL(KIND=WP), INTENT(IN)    :: sFF,sEta,sKappa
!   REAL(KIND=WP), INTENT(INOUT) :: sLenE(:)	   
!   REAL(KIND=WP), INTENT(INOUT) :: sSigE(:)
!   LOGICAL,       INTENT(OUT)   :: qOK

! !          LOCAL VARS

!   INTEGER(KIND=IP) :: error

! !     Set error flag to false

!   qOK = .FALSE.

!   if (tProcInfo_G%qRoot) print*,''
!   if (tProcInfo_G%qRoot) print*, '---------------------'
!   IF (tProcInfo_G%qRoot) PRINT*, 'Matching transverse beam area to focusing channel...'


! !     Matched beam radius used for electron sigma spread        

!   sSigE(iX_CG:iY_CG) = MatchedBeamRadius(srho,&
!                  sEmit_n,k_beta)

! !     p spread

!   sSigE(iPX_CG:iPY_CG) = 18.0_WP * &
!             SQRT(sEta) / 2.0_wp / sKappa * sEmit_n / &
!             sSigE(iX_CG:iY_CG)

! !     The sigma values above are the rms radii
! !     Need to change to sigma of current distribution
! !     Here it is assumed lex=6sigma

!   sSigE(iX_CG:iY_CG)=sSigE(iX_CG:iY_CG)/3.0_WP/sqrt(2.0_WP)
!   sSigE(iPX_CG:iPY_CG)=sSigE(iPX_CG:iPY_CG)/3.0_WP/sqrt(2.0_WP)

! !     Length of electron pulse from new sigma, modelling to 6*sigma
         
!   sLenE(iX_CG:iY_CG)   = 6.0_WP * sSigE(iX_CG:iY_CG)
         
!   sLenE(iPX_CG:iPY_CG) = 6.0_WP * sSigE(iPX_CG:iPY_CG)


!   IF (tProcInfo_G%qRoot) PRINT*, 'New Gaussian sigma of electron beam in x is ',sSigE(iX_CG)
!   IF (tProcInfo_G%qRoot) PRINT*, '...so total sampled length of beam in x is ', sLenE(iX_CG)
!   if (tProcInfo_G%qRoot) print*,''
!   IF (tProcInfo_G%qRoot) PRINT*, 'New Gaussian sigma of e-beam in px is ', sSigE(iPX_CG)
!   IF (tProcInfo_G%qRoot) PRINT*, 'New Gaussian sigma of e-beam in py is ', sSigE(iPY_CG)


!   IF (tProcInfo_G%qRoot) PRINT*, 'Scaled betatron wavelength (in gain lengths) = ', 2.0_WP*pi/k_beta

! ! Set error flag and exit         

!   qOK = .TRUE.				    

!   GOTO 2000     

! ! Error Handler

! 1000 CALL Error_log('Error in DFunctions:CalculateMatchedBeamParameters',tErrorLog_G)

! 2000 CONTINUE

! END SUBROUTINE GetMBParams

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
      'THERE MAY BE TOO MUCH DIFFRACTION IN X'
  
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
