!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE setupcalcs

USE Paratype
USE ParallelInfoType
USE TransformInfoType
USE IO
USE ArrayFunctions
USE extra
USE typesAndConstants
USE Globals
USE electronInit
USE gMPsFromDists
use avwrite

IMPLICIT NONE

CONTAINS



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE passToGlobals(rho,aw,gamr,lam_w,iNN, &
                         sRNX,sRNY, &
                         sElmLen,&
                         fx,fy,sFocusFactor,taper,sFiltFrac, &
                         dStepFrac,sBeta,zUndType,qFormatted, qSwitch,qOK)

    IMPLICIT NONE

! Subroutine to pass all the temporary variables to global
! vars used in the integration loop.
!
!                    ARGUMENTS
!
! rho                Pierce parameter
! eta                scaled z-velocity
! kbeta              scaled betatron wavenumber
! iNN                number of nodes in each dimension (x,y,z2)
! sRNX,sRNY          number of nodes in the inner reduced 'active'
!                    set of nodes
! sElmLen            Length of ONE element in each dimension (x,y,z2)
! fx, fy             specifies undulator polarization
! qSwitch            If letting electrons evolve, field evolve,
!                    diffraction and gaussian field
! qOK                Error flag

    REAL(KIND=WP),     INTENT(IN)    :: rho,aw,gamr, lam_w
    INTEGER(KIND=IP),  INTENT(IN)    :: sRNX,sRNY
    INTEGER(KIND=IP),  INTENT(IN)    :: iNN(:)
    REAL(KIND=WP),     INTENT(IN)    :: sElmLen(:)	
    REAL(KIND=WP),     INTENT(IN)    :: fx,fy,sFocusFactor, taper
    REAL(KIND=WP),     INTENT(IN)    :: sFiltFrac, dStepFrac, sBeta
    LOGICAL,           INTENT(IN)    :: qSwitch(nSwitches_CG), qFormatted
    character(32_ip),  intent(in)    :: zUndType
    LOGICAL,           INTENT(OUT)   :: qOK

!                    LOCAL ARGS
!
! lam_r_bar          Resonant wavelength in scaled units
! qOKL               Local error flag

    REAL(KIND=WP) :: lam_r_bar, LenZ2, modfact1, sbetaz
    LOGICAL :: qOKL

    qOK = .FALSE.

!                  Pass to globals

    NX_G  = iNN(iX_CG)
    NY_G  = iNN(iY_CG)
    NZ2_G = iNN(iZ2_CG)

    IF (NX_G == 1 .AND. NY_G == 1) THEN
    
       iNodesPerElement_G = 2_IP
    
    ELSE
    
       iNodesPerElement_G = 8_IP
    
    END IF




    ReducedNX_G=sRNX
    ReducedNY_G=sRNY

    IF (NX_G*NY_G==1) THEN
      ReducedNX_G=1_IP
      ReducedNY_G=1_IP
    END IF
    
    outnodex_G=NX_G-ReducedNX_G
    outnodey_G=NY_G-ReducedNY_G

!     Set up the length of ONE element globally

    sLengthOfElmX_G  = sElmLen(iX_CG)
    sLengthOfElmY_G  = sElmLen(iY_CG)
    sLengthOfElmZ2_G = sElmLen(iZ2_CG)

    delta_G = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G

!     Filter fraction to frequency in Fourier space

    Lenz2 = sLengthOfElmZ2_G * NZ2_G
    lam_r_bar = 4*pi*rho
    sFilt = Lenz2 / lam_r_bar * sFiltFrac

!     Set up parameters
    if (qMod_G) then

      modfact1 = mf(1)

    else 

      modfact1 = 1.0_WP

    end if

    n2col = modfact1
    n2col0 = n2col
    sz0 = 0.0_WP
    undgrad = taper

    sRho_save_G = rho

    sRho_G = sRho_save_G ! * modfact1

    sbetaz = SQRT(gamr**2.0_WP - 1.0_WP - (aw)**2.0_WP) / &
             gamr

    sEta_G = (1.0_WP - sbetaz) / sbetaz
    sKBeta_G = aw / 2.0_WP / sFocusFactor / rho / gamr

    sFocusfactor_save_G = sFocusFactor

    sFocusfactor_G = sFocusfactor_save_G ! * modfact1


    fx_G = fx
    fy_G = fy

    !sAw_save_G = ((1 / (2.0_WP*rho*sFocusFactor*kbeta)**2) *   &
    !        (1.0_WP - (1.0_WP / ( 1.0_WP + eta )**2) ) - 1.0_WP) ** (-0.5_WP)



    sAw_save_G = aw
    sAw_G = sAw_save_G ! * modfact1

    sGammaR_G = gamr


    lam_w_G = lam_w
    lam_r_G = lam_w * sEta_G

    lg_G = lam_w_G / 4.0_WP / pi / rho
    lc_G = lam_r_G / 4.0_WP / pi / rho



    diffstep = dStepFrac * 4.0_WP * pi * rho
    sBeta_G = sBeta

    NBX_G = 16_IP   ! Nodes used in boundaries
    NBY_G = 16_IP

    NBZ2_G = 37_IP


    zUndType_G = zUndType

    kx_und_G = SQRT(sEta_G/(8.0_WP*sRho_G**2))
    ky_und_G = SQRT(sEta_G/(8.0_WP*sRho_G**2))

!     Get the number of nodes

    iNumberNodes_G = iNN(iX_CG)*iNN(iY_CG)*iNN(iZ2_CG)

    IF(iNumberNodes_G <= 0_IP) THEN
       CALL Error_log('iNumberNodes_G <=0.',tErrorLog_G)
       GOTO 1000    
    END IF

!     Get global node numbers

    IF (.NOT. tTransInfo_G%qOneD) THEN
       ALLOCATE(iNodCodA_G(ReducedNX_G,ReducedNY_G,NZ2_G))
       ALLOCATE(iGloNumA_G(8*(ReducedNX_G-1)*(ReducedNY_G-1)*(NZ2_G-1)))
       CALL GL_NUM(ReducedNX_G,ReducedNY_G,NZ2_G,iNodCodA_G,iGloNumA_G)
    END IF

    qElectronsEvolve_G       = qSwitch(iElectronsEvolve_CG)
    qFieldEvolve_G           = qSwitch(iFieldEvolve_CG)
    qElectronFieldCoupling_G = qSwitch(iElectronFieldCoupling_CG)
    qDiffraction_G           = qSwitch(iDiffraction_CG)
    qFocussing_G             = qSwitch(iFocussing_CG)
    qResume_G                = qSwitch(iResume_CG)
    qDump_G                  = qSwitch(iDump_CG)



    call initPFile(tPowF, qFormatted) ! initialize power file type
    


    tArrayE(:)%tFileType%qFormatted = qFormatted
    tArrayA(:)%tFileType%qFormatted = qFormatted
    tArrayZ%tFileType%qFormatted = qFormatted





!     Set error flag and exit

    qOK = .TRUE.				    

    GOTO 2000

1000 CALL Error_log('Error in setupCalcs:setupParams',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE passToGlobals

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            
SUBROUTINE SetUpInitialValues(nseeds, freqf, SmeanZ2, qFlatTopS, sSigmaF, &
                              sLengthOfElm, sA0_x, sA0_y, sX0, sY0, sZ20, &
                              sV, sA, qOK)

    IMPLICIT NONE
!
! Set up the initial macroparticle and field values
!
!                   ARGUMENTS
!
! sSeed              INPUT    Descriptions of the sSeed field
! qInitialGauss      INPUT    If to use gauss
! sA0_Re             INPUT    Initial field value (real)
! sA0_Im             INPUT    Initial field value (imaginary)
! sX0                INPUT    Initial X values
! sY0                INPUT    Initial Y values
! sZ20               INPUT    Initial z2 values
! sV                 UPDATE   Initial values of variables to
!                                      be integrated
! qOK                OUTPUT   Error flag

    INTEGER(KIND=IP), INTENT(IN) :: nseeds
    REAL(KIND=WP), INTENT(IN)    :: sSigmaF(:,:), SmeanZ2(:), freqf(:)
    LOGICAL, INTENT(IN) :: qFlatTopS(:)
    REAL(KIND=WP), INTENT(IN)    :: sLengthOfElm(:)
    REAL(KIND=WP), INTENT(IN)    :: sA0_x(:)
    REAL(KIND=WP), INTENT(IN)    :: sA0_y(:)
    REAL(KIND=WP), INTENT(IN)    :: sX0(:)      
    REAL(KIND=WP), INTENT(IN)    :: sY0(:)      
    REAL(KIND=WP), INTENT(IN)    :: sz20(:)      
    REAL(KIND=WP), INTENT(INOUT) :: sV(:)
    REAL(KIND=WP), INTENT(INOUT) :: sA(:)
    LOGICAL,       INTENT(OUT)   :: qOK

!                LOCAL ARGS
! 
! qOKL         Local error flag
! iZ2          Number of nodes in Z2
! iXY          Number of nodes in XY plane
! sA0gauss_Re  Initial field over all planes

    LOGICAL           :: qOKL
    LOGICAL           :: qInitialGauss
    INTEGER(KIND=IP)  :: iZ2,iXY,i,lowind,highind,error,NN(3)
    REAL(KIND=WP)     :: z2bar,rho
    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE :: sAx_mag,sAy_mag,&
                                              sAreal,sAimag

!     Set error flag to false

    qOK = .FALSE. 

    iZ2 = NZ2_G
    iXY = NX_G*NY_G

    NN(iX_CG) = NX_G
    NN(iY_CG) = NY_G
    NN(iZ2_CG) = NZ2_G
    
    ALLOCATE(sAreal(iXY*iZ2),sAimag(iXY*iZ2))
   
    CALL getSeeds(NN,sSigmaF,SmeanZ2,sA0_x,sA0_y,qFlatTopS,sRho_G,freqf, &
                  nseeds,sLengthOfElm,sAreal,sAimag)

    sA(1:iXY*iZ2) = sAreal
    sA(iXY*iZ2 + 1:2*iXY*iZ2) = sAimag

    DEALLOCATE(sAreal,sAimag)

!     Set up initial electron perp values

    CALL PutValueInVector(iRe_PPerp_CG, sEl_PX0Position_G,&
         sV, qOKL)
    IF (.NOT. qOKL) GOTO 1000

    CALL PutValueInVector(iIm_PPerp_CG,-sEl_PY0Position_G,&
         sV, qOKL)		
    IF (.NOT. qOKL) GOTO 1000

!     Set up initial electron Q values

    CALL PutValueInVector(iRe_Q_CG, sEl_PZ20Position_G,&
         sV, qOKL)
    IF (.NOT. qOKL) GOTO 1000

!     Set up initial electron starting positions in z2 direction

    CALL PutValueInVector(iRe_Z2_CG, sz20, sV, qOKL)
    IF (.NOT. qOKL) GOTO 1000

!     Set up initial electron starting positions in x direction

    CALL PutValueInVector(iRe_X_CG, sX0, sV, qOKL)
    IF (.NOT. qOKL) GOTO 1000

!     Set up initial electron starting positions in y direction

    CALL PutValueInVector(iRe_Y_CG, sY0, sV, qOKL)
    IF (.NOT. qOKL) GOTO 1000

!     Set error flag and exit

    qOK = .TRUE.				    
    GOTO 2000

1000 CALL Error_log('Error in FEMethod:SetUpInitialValues',tErrorLog_G)
    PRINT*,'Error in FEMethod:SetUpInitialValues'
2000 CONTINUE

END SUBROUTINE SetUpInitialValues

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE PopMacroElectrons(qSimple, fname, sQe,NE,noise,Z,LenEPulse,&
                             sigma, beamCenZ2,gamma_d,eThresh, &
                             chirp,nbeams, &
                             sV,qOK)

!                     ARGUMENTS

    logical, intent(in) :: qSimple
    character(*), intent(in) :: fname(:)
    REAL(KIND=WP),     INTENT(IN)    :: sQe(:), gamma_d(:), chirp(:)
    INTEGER(KIND=IP),  INTENT(IN)    :: NE(:,:),nbeams
    LOGICAL,           INTENT(IN)    :: noise
    REAL(KIND=WP),     INTENT(IN)    :: Z
    REAL(KIND=WP),     INTENT(INOUT) :: LenEPulse(:,:)
    REAL(KIND=WP),     INTENT(INOUT) :: sigma(:,:)
    REAL(KIND=WP),     INTENT(INOUT) :: beamCenZ2(:)
    REAL(KIND=WP),     INTENT(IN)    :: eThresh
    REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sV(:)
    LOGICAL,           INTENT(OUT)   :: qOK     

!                   LOCAL ARGS

    INTEGER(KIND=IPL) :: NMacroE
    REAL(KIND=WP)     :: sQOneE
    REAL(KIND=WP), ALLOCATABLE  :: RealE(:)
    INTEGER(KIND=IP) :: j,error, req, lrank, rrank
    INTEGER(KIND=IPL) :: sendbuff, recvbuff
    INTEGER sendstat(MPI_STATUS_SIZE)
    INTEGER recvstat(MPI_STATUS_SIZE)
    LOGICAL :: qOKL

    sQOneE = 1.60217656535E-19

    qOK = .FALSE.

    IF (qSimple) ALLOCATE(RealE(nbeams))

!     Print a reminder to check whether shot-noise is
!     being modelled or not

    IF (tProcInfo_G%qROOT) THEN
       IF (noise) THEN
          PRINT *, 'SHOT-NOISE TURNED ON'
       ELSE
          PRINT *, 'SHOT-NOISE TURNED OFF'
       ENDIF
    ENDIF

!     Number of real electrons

    IF (qSimple) RealE = sQe / sQOneE


!     Change sig_gamma / gamma to sig_gamma

    IF (qSimple) LenEPulse(:,iPZ2_CG) = gamma_d(:) * sGammaR_G * LenEPulse(:,iPZ2_CG)
    IF (qSimple) sigma(:,iPZ2_CG) = gamma_d(:) * sGammaR_G * sigma(:,iPZ2_CG)

!     Setup electrons




    if (qSimple) then

      CALL electron_grid(RealE,NE,noise, &
                         Z,nbeams, LenEPulse,sigma, beamCenZ2, gamma_d, &
                         eThresh,tTransInfo_G%qOneD, &
                         chirp,sV,qOKL)
      IF (.NOT. qOKL) GOTO 1000

    else 

      call getMPs(fname, nbeams, Z, noise, eThresh)

    end if

    IF(iGloNumElectrons_G <= 0_IPL) THEN
       CALL Error_log('iGloNumElectrons_G <=0.',tErrorLog_G)
       GOTO 1000    
    END IF

    IF (tProcInfo_G%qROOT) PRINT *,&
         'TOTAL NUM OF MACROPARTICLES = ', iGloNumElectrons_G

    allocate(sV(iNumberElectrons_G * 6_IP))

!    Set up the array describing the number of electrons
!    on each processor

    IF (tProcInfo_G%rank == tProcInfo_G%size-1) THEN
       rrank = 0
       lrank = tProcInfo_G%rank-1
    ELSE IF (tProcInfo_G%rank==0) THEN
       rrank = tProcInfo_G%rank+1
       lrank = tProcInfo_G%size-1
    ELSE
       rrank = tProcInfo_G%rank+1
       lrank = tProcInfo_G%rank-1
    END IF

    ALLOCATE(procelectrons_G(tProcInfo_G%size))

    procelectrons_G(1) = iNumberElectrons_G

    sendbuff = iNumberElectrons_G
    recvbuff = iNumberElectrons_G

!     When the following loop is complete, the array
!     procelectrons_G will contain a record of the number
!     of macroparticles on each process. The first element,
!     procelectrons_G(1), will contain this processes local
!     macroparticle number. The array then cycles through each
!     process in ascending order.

    DO j=2,tProcInfo_G%size
       CALL MPI_ISSEND( sendbuff,1,MPI_INT_HIGH,rrank,&
            0,tProcInfo_G%comm,req,error )
       CALL MPI_RECV( recvbuff,1,MPI_INT_HIGH,lrank,&
            0,tProcInfo_G%comm,recvstat,error )
       CALL MPI_WAIT( req,sendstat,error )
       procelectrons_G(j) = recvbuff
       sendbuff=recvbuff
    END DO

    IF (iNumberElectrons_G==0) qEmpty=.TRUE. 

    if (qSimple) DEALLOCATE(RealE)

!    Set error flag and exit

    qOK = .TRUE.

    GOTO 2000

1000 CALL Error_log('Error in SetupCalcs:PopMacroElectrons',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE PopMacroElectrons

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE getSeeds(NN,sigs,cens,magxs,magys,qFTs,rho,frs,nSeeds,dels,xfieldt,yfieldt)

!             ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: NN(:)
  REAL(KIND=WP), INTENT(IN) :: sigs(:,:), cens(:), rho, frs(:), &
                               magxs(:), magys(:), dels(:)
  LOGICAL, INTENT(IN) :: qFTs(:)
  INTEGER(KIND=IP), INTENT(IN) :: nSeeds
  REAL(KIND=WP), INTENT(OUT) :: xfieldt(:), yfieldt(:)

!            LOCAL ARGS

  INTEGER(KIND=IP) :: ind
  REAL(KIND=WP) :: xfield(size(xfieldt)), yfield(size(yfieldt))


  xfieldt = 0.0_WP
  yfieldt = 0.0_WP

  DO ind = 1, nSeeds
  
    CALL getSeed(NN(:),sigs(ind,:),cens(ind),magxs(ind),magys(ind),qFTs(ind),rho,frs(ind), &
                   dels,xfield,yfield)

    xfieldt = xfieldt + xfield
    yfieldt = yfieldt + yfield
    
  END DO

END SUBROUTINE getSeeds

!*****************************************************

SUBROUTINE getSeed(NN,sig,cen,magx,magy,qFT,rho,fr,dels,xfield,yfield)

  IMPLICIT NONE

!             ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: NN(:)
  REAL(KIND=WP), INTENT(IN) :: sig(:), cen, rho, fr, magx, magy, dels(:)
  LOGICAL, INTENT(IN) :: qFT
  REAL(KIND=WP), INTENT(OUT) :: xfield(:), yfield(:)

!             LOCAL ARGS

  REAL(KIND=WP) :: xnds(NN(iX_CG)), ynds(NN(iY_CG)), &
                   z2nds(NN(iZ2_CG)), &
                   xenv(NN(iX_CG)), yenv(NN(iY_CG)), &
                   z2env(NN(iZ2_CG)), oscx(NN(iZ2_CG)), &
                   oscy(NN(iZ2_CG))
 
  REAL(KIND=WP) :: lx, ly, lz2
                   
  INTEGER(KIND=IP) :: ind1, ind2, ind3, gind

!     Sample length of the field in each dimension

  lx = dels(iX_CG) * (NN(iX_CG) - 1_IP)
  ly = dels(iY_CG) * (NN(iY_CG) - 1_IP)
  lz2 = dels(iZ2_CG) * (NN(iZ2_CG) - 1_IP)

!     Coordinates of field nodes in x, y and z2 (sample points)

  IF (NN(iX_CG) == 1_IP) THEN
    xnds = 1_WP
  ELSE
    xnds = linspace(-lx/2_WP, lx/2_WP, NN(iX_CG))
  END IF  


  IF (NN(iY_CG) == 1_IP) THEN
    ynds = 1_WP
  ELSE
    ynds = linspace(-ly/2_WP, ly/2_WP, NN(iY_CG))
  END IF  

  z2nds = linspace(0.0_WP,lz2,NN(iZ2_CG))

!     Profile in each dimension

  IF (NN(iX_CG) == 1_IP) THEN
    xenv = 1_WP
  ELSE
    xenv = gaussian(xnds,0.0_WP,sig(iX_CG))
  END IF

  
  IF (NN(iY_CG) == 1_IP) THEN
    yenv = 1_WP
  ELSE
    yenv = gaussian(ynds,0.0_WP,sig(iY_CG))    
  END IF
   

  IF (qFT) THEN
    
    WHERE (z2nds < (cen - sig(iZ2_CG)))

      z2env = 0.0_WP

    ELSEWHERE (z2nds > (cen + sig(iZ2_CG)))

        z2env = 0.0_WP

    ELSEWHERE

      z2env = 1.0_WP

    END WHERE

  ELSE
  
    z2env = gaussian(z2nds,cen,sig(iZ2_CG))

  END IF


!     x and y polarized fields in z2

  oscx = -z2env * cos(fr * z2nds / (2.0_WP * rho))
  oscy = z2env * sin(fr * z2nds / (2.0_WP * rho))


!     Full 3D field

  do ind1 = 1, NN(iZ2_CG)
    do ind2 = 1,NN(iY_CG)
      do ind3 = 1,NN(iX_CG)
        
        gind = ind3 + NN(iX_CG)*(ind2-1) + NN(iX_CG)*NN(iY_CG)*(ind1-1)
        xfield(gind) = magx * xenv(ind3) * yenv(ind2) * oscx(ind1)
        yfield(gind) = magy * xenv(ind3) * yenv(ind2) * oscy(ind1)
      
      end do
    end do     
  end do

END SUBROUTINE getSeed

END MODULE setupcalcs
