MODULE ElectronInit

USE paratype
USE ParallelInfoType
USE TransformInfoType
USE DerivsGlobals
USE Functions
USE particleFunctions
USE MacrosGen

IMPLICIT NONE

CONTAINS

  SUBROUTINE electron_grid(i_RealE, &
       iNMP, &
       q_noise, &
       sZ, &
       nbeams, &
       samLenE, &
       sigE, &
       beamCenZ2, &
       gamma_d, &
       sElectronThreshold, &
       qOneD, &
       chirp, &
       sV, &
       qOK)

! Calculate the electron grid positions
!
!                ARGUMENTS
!
! i_RealE             INPUT   number of real electrons
! iNMP(:)          INPUT   number of macro electrons
!                             in all direction
! q_noise             INPUT   if allowing shot noise
! sZ                  INPUT   initial Z position
! samLenE(:)	      UPDATE  Length of elecgtron pulse in x,y,z2
! sigE(:)   INPUT   Sigma spread of electron gaussian
!                             distribution
! sElectronThreshold  INTPUT  Beyond this threshold level,
!                             electrons are removed
! sx0_offset          OUTPUT  Offset from centre
! sy0_offset          OUTPUT  Offset from centre
! qOK                 OUTPUT  Error flag

    IMPLICIT NONE

    REAL(KIND=WP), INTENT(IN)   :: i_RealE(:)
    REAL(KIND=WP), INTENT(INOUT):: beamCenZ2(:)
    INTEGER(KIND=IP), INTENT(IN):: iNMP(:,:)
    LOGICAL, INTENT(IN)         :: q_noise
    REAL(KIND=WP), INTENT(IN)	:: sZ, chirp(:)

    INTEGER(KIND=IP), INTENT(IN) :: nbeams
    REAL(KIND=WP), INTENT(INOUT):: samLenE(:,:)    
    
    REAL(KIND=WP), INTENT(INOUT)   :: sigE(:,:)
    REAL(KIND=WP), INTENT(IN)   :: gamma_d(:)
    REAL(KIND=WP), INTENT(IN)   :: sElectronThreshold

    LOGICAL,         INTENT(IN):: qOneD

    REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sV(:)
    LOGICAL,         INTENT(OUT):: qOK

!              LOCAL ARGUMENTS
!
! qOKL               Local error flag
! i                  Loop counter
! sSmall             A small number
!
! Electron beam INFO
!
! Electrons co-ordinates locally (full size) in
! all 6 directions
!
!
! x_coord          y_coord          z2_coord
! px_vector        py_vector        pz2_vector
!
!
! TOTALMPS                Total number of macro particles
! chi_bar                 Chi-BAR = Chi * section volume of beam
! NormChi                 Chi = sElectron_Macro/
!                         (peak density * section volume of beam)
! sElectron_Macro         Real number of electrons per macro electron
! ilowerElectron          The lower limit of REAL electrons using the
!                         threshold limit

    LOGICAL  :: qOKL	
    INTEGER(KIND=IP) :: i

    INTEGER(KIND=IPL) :: TOTALMPS

    REAL(KIND=WP) :: ilowerElectron

    INTEGER(KIND=IPL), ALLOCATABLE :: totalmps_b(:)
	
    REAL(KIND=WP),ALLOCATABLE :: s_tmp_macro(:),&
                                 s_mean_number_macro(:)

    REAL(KIND=WP),ALLOCATABLE :: Tmp_chibar(:),Tmp_Normchi(:)

    REAL(KIND=WP),ALLOCATABLE :: x_tmpcoord(:),y_tmpcoord(:),&
         z2_tmpcoord(:)
    REAL(KIND=WP),ALLOCATABLE :: px_tmpvector(:),py_tmpvector(:),&
         pz2_tmpvector(:)

    REAL(KIND=WP),ALLOCATABLE :: s_tmp_Vk(:)

    REAL(KIND=WP), ALLOCATABLE :: s_tmp_max_av(:)
    REAL(KIND=WP) :: local_start, local_end, afact, um

    REAL(KIND=WP) :: offsets(6)

    REAL(KIND=WP), ALLOCATABLE :: Qchoff(:), tconv(:)

    INTEGER(KIND=IPL), ALLOCATABLE :: b_sts(:), b_ends(:)

    INTEGER(KIND=IP), ALLOCATABLE :: iNumLocalElectrons(:,:)

    INTEGER(KIND=IP)    :: rank, b_ind
    INTEGER(KIND=IP)    :: numproc
    INTEGER(KIND=IP)    :: error
    
!     Set error flag to false         

    qOK = .FALSE.
    
    
    rank = tProcInfo_G%rank
    numproc = tProcInfo_G%size

    ALLOCATE(totalmps_b(nbeams))
    ALLOCATE(iNumLocalElectrons(nbeams,6))

!     Split electrons across z2.

    CALL splitBeams(iNMP,samLenE,nBeams,numproc,rank,&
                    iNumLocalElectrons,totalmps_b)
                      
    TOTALMPS = SUM(INT(totalmps_b,KIND=IPL)) ! Total no of MPs

!     Allocate electrons position and momenta arrays

    ALLOCATE(x_tmpcoord(TOTALMPS))
    ALLOCATE(y_tmpcoord(TOTALMPS))
    ALLOCATE(z2_tmpcoord(TOTALMPS))
    ALLOCATE(px_tmpvector(TOTALMPS))
    ALLOCATE(py_tmpvector(TOTALMPS))
    ALLOCATE(pz2_tmpvector(TOTALMPS))	  
    ALLOCATE(s_tmp_macro(TOTALMPS))
    ALLOCATE(Tmp_chibar(TOTALMPS))
    ALLOCATE(Tmp_Normchi(TOTALMPS))
    ALLOCATE(s_tmp_Vk(TOTALMPS))

    ALLOCATE(b_sts(nbeams),b_ends(nbeams))

    ALLOCATE(s_tmp_max_av(nbeams))

    CALL getStEnd(nbeams,totalmps_b,b_sts,b_ends)


!!!!!!  TEMP
!!!!!!  CONVERT SIGPX -> SIX_{DX/DZ}

    um = sqrt(fx_G**2.0_WP + fy_G**2.0_WP)
    afact = sqrt(2.0_WP) * saw_G / um
    
!    sigE(:,iPX_CG) = sigE(:,iPX_CG) * afact * &
!                     sqrt((sEta_G * (sEta_G + 2.0_WP) / (1 + afact**2.0_WP)))
                     
!    sigE(:,iPY_CG) = sigE(:,iPY_CG) * afact * &
!                     sqrt((sEta_G * (sEta_G + 2.0_WP) / (1 + afact**2.0_WP)))                     

    sigE(:,iPX_CG) = sigE(:,iPX_CG) * afact * &
                     (1.0_WP + sEta_G) / sgammaR_G
                     
    sigE(:,iPY_CG) = sigE(:,iPY_CG) * afact * &
                     (1.0_WP + sEta_G) / sgammaR_G
    

!    samLenE(:,iPX_CG) = samLenE(:,iPX_CG) * afact * &
!                     sqrt((sEta_G * (sEta_G + 2.0_WP) / (1.0_WP + afact**2.0_WP))) 

!    samLenE(:,iPY_CG) = samLenE(:,iPY_CG) * afact * &
!                     sqrt((sEta_G * (sEta_G + 2.0_WP) / (1.0_WP + afact**2.0_WP))) 

     samLenE(:,iPX_CG) = sigE(:,iPX_CG) * 6.0_WP
     samLenE(:,iPY_CG) = sigE(:,iPY_CG) * 6.0_WP

    DO b_ind = 1,nbeams

      CALL genBeam(iNMP(b_ind,:),iNumLocalElectrons(b_ind,:),&
                    sigE(b_ind,:), gamma_d(b_ind), &
                    samLenE(b_ind,:),beamCenZ2(b_ind),numproc, rank, &
                    i_RealE(b_ind), q_noise, qOneD, sZ, &
                    x_tmpcoord(b_sts(b_ind):b_ends(b_ind)), &
                    y_tmpcoord(b_sts(b_ind):b_ends(b_ind)), &
                    z2_tmpcoord(b_sts(b_ind):b_ends(b_ind)), &
                    px_tmpvector(b_sts(b_ind):b_ends(b_ind)), &
                    py_tmpvector(b_sts(b_ind):b_ends(b_ind)), &
                    pz2_tmpvector(b_sts(b_ind):b_ends(b_ind)), &
                    s_tmp_max_av(b_ind), &
                    s_tmp_macro(b_sts(b_ind):b_ends(b_ind)), &
                    s_tmp_Vk(b_sts(b_ind):b_ends(b_ind)))
                      
    END DO


    CALL getChi(s_tmp_macro, s_tmp_Vk, maxval(s_tmp_max_av), &
                Tmp_chibar, Tmp_Normchi)
    
    DEALLOCATE(s_tmp_macro,s_tmp_Vk)

    CALL removeLow(Tmp_chibar, Tmp_Normchi, b_sts, b_ends, sElectronThreshold, &
 chirp,nbeams,x_tmpcoord,y_tmpcoord,z2_tmpcoord,px_tmpvector,&
                   py_tmpvector, pz2_tmpvector,totalmps_b,beamCenZ2)

    DEALLOCATE(x_tmpcoord)
    DEALLOCATE(y_tmpcoord)
    DEALLOCATE(z2_tmpcoord)
    DEALLOCATE(px_tmpvector)
    DEALLOCATE(py_tmpvector)
    DEALLOCATE(pz2_tmpvector)
    DEALLOCATE(Tmp_chibar)
    DEALLOCATE(Tmp_Normchi)

    DEALLOCATE(s_tmp_max_av)

    DEALLOCATE(iNumLocalElectrons)


!!!!!!! TEMP
!!!!!!! COVERT DX/DZ AND DY/DZ -> SCALED PX, PY AND ADD OFFSET
    ALLOCATE(tconv(size(sEl_PX0Position_G)))
    
    tconv = sEl_PX0Position_G**2.0_WP + sEl_PY0Position_G**2.0_WP



    sEl_PX0Position_G = sqrt((sEl_PZ20Position_G**2.0_WP - 1.0_WP) / &
                            (1.0_WP + tconv))  &
                        / afact * sEl_PX0Position_G

    sEl_PY0Position_G = sqrt((sEl_PZ20Position_G**2.0_WP - 1.0_WP) / &
                            (1.0_WP + tconv))  &
                        / afact * sEl_PY0Position_G


    DEALLOCATE(tconv)

    sEl_PX0Position_G = sEl_PX0Position_G + pxOffset(sZ, srho_G, fy_G)
    sEl_PY0Position_G = sEl_PY0Position_G + pyOffset(sZ, srho_G, fx_G)









!     We currently have gamma in the p2 position array -
!     need to change to p2

    sEl_PZ20Position_G = getP2(sEl_PZ20Position_G,sEl_PX0Position_G,&
                               sEl_PY0Position_G,sEta_G,sAw_G)

!     Allocate electron array

    ALLOCATE(sV(iNumberElectrons_G * 6_IPL))

!     Sum the local num of macroparticles to a global number

    call sum_mpi_int14(iNumberElectrons_G,iGloNumElectrons_G)

!     Set error flag and exit         

    qOK = .TRUE.				    
    GOTO 2000     

!     Error Handler

1000 CALL Error_log('Error in Chow:electron_grid',tErrorLog_G)
    PRINT*,'Error in Chow:electron_grid'
2000 CONTINUE
  END SUBROUTINE electron_grid

!********************************************************

  FUNCTION xOffSet(rho, &
       aw,  &
       gamma_r, &
       gamma_j, &
       eta, &
       k_beta,&
       ff, &
       px,&
       py,&
       ux,&
       uy,&
       sZ0)
!
! Calculate xOffset value
! Value of Range mid point offset from origin
!
! srho     - Pierce parameter, describe the strength
!            of the field
! saw      - Wiggler parameter
! sgammar  - Mean electron velocity at resonance
! sEpsilon - (1+aw^2)/(2*gammar^2) 
! sZ0      - Starting z position
    REAL(KIND=WP), INTENT(IN) :: rho,aw,gamma_r,gamma_j, &
         eta,px,py,k_beta,ff,ux,uy,sZ0
    REAL(KIND=WP) :: xOffSet, nc
    REAL(KIND=WP) ::srBcoeff,s_Sin_zOver2rho
    
    nc = 2.0_WP*aw**2/(ux**2 + uy**2)
    
    srBcoeff = 4.0_WP * sqrt(2.0) * ff * k_beta * & 
              rho**2.0_WP / sqrt(ux**2 + uy**2) / sqrt(eta)* &
              (gamma_r / sqrt(gamma_j**2 - (1.0_WP + nc*(px**2 + py**2)))) !!!TEMP!!!

    s_Sin_zOver2rho = SIN(sZ0 / (2.0_WP * rho))

    xOffSet = -srBcoeff * s_Sin_zOver2rho

  END FUNCTION xOffSet

!********************************************************

  FUNCTION yOffSet(rho, &
       aw,  &
       gamma_r, &
       gamma_j, &
       eta, &
       k_beta, &
       ff, &
       px, &
       py, &
       ux,&
       uy,&
       sZ0)
!
! Calculate xOffset value
! Value of Range mid point offset from origin
!
! ARGS:-
!
! srho     - Pierce parameter, describe the strength
!            of the field
! saw      - Wiggler parameter
! sgammar  - Mean electron velocity at resonance
! sEpsilon - (1+aw^2)/(2*gammar^2) 
! sZ0      - Starting z position
!	
    REAL(KIND=WP), INTENT(IN) :: rho,aw,gamma_r,gamma_j, &
         eta,px,py,k_beta,ff,ux,uy,sZ0
    REAL(KIND=WP) :: yOffSet, nc
    REAL(KIND=WP) ::srBcoeff,s_Cos_zOver2rho
!
    nc = 2.0_WP*aw**2/(ux**2 + uy**2)
    
    srBcoeff = 4.0_WP * sqrt(2.0_WP) * ff * k_beta * & 
              rho**2.0_WP / sqrt(ux**2 + uy**2) / sqrt(eta) * &
              (gamma_r / sqrt(gamma_j**2 - (1.0_WP + nc*(px**2 + py**2))))
          
    s_Cos_zOver2rho = COS(sZ0 / (2.0_WP * rho))	
! Initial values for the electron pulse in all direction
    yOffSet         = srBcoeff * s_Cos_zOver2rho
      
  END FUNCTION yOffSet
!********************************************************

  FUNCTION pxOffset(z, rho, uy)
  
! Equation for the initial electron px offset due to
! the undulator field.  
! 
!               ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: z, rho, uy

!                OUTPUT

    REAL(KIND=WP) :: pxOffset

    pxOffset = -uy*COS(z / (2.0_WP * rho))
  
  END FUNCTION pxOffset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  FUNCTION pyOffset(z, rho, ux)

! Equation for the initial electron py offset due to
! the undulator field.  
! 
!               ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: z, rho, ux

!                OUTPUT

    REAL(KIND=WP) :: pyOffset

    pyOffset = -ux * SIN(z / (2.0_WP * rho))
    
  END FUNCTION pyOffset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  FUNCTION pz2Offset(gamma, px, py, eta, aw)

! Equation for the initial electron py offset due to
! the undulator field.  
! 
!               ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: gamma, px, py, eta, aw

!                OUTPUT

    REAL(KIND=WP) :: pz2Offset
    
!              LOCAL ARGS

    REAL(KIND=WP) :: nc


    nc = 2.0_WP*aw**2/(fx_G**2 + fy_G**2)
           
          
    pz2Offset = ((gamma/SQRT(gamma**2 - 1.0_WP - &
                   nc*(px**2 + py**2)))-1.0_WP)/eta
    
  END FUNCTION pz2Offset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

  FUNCTION getGamma(px, py, p2, eta, aw)
  
! Return gamma, given p2, px, py and eta
! 
!           ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: px, py, p2, eta, aw

!            OUTPUT

    REAL(KIND=WP) :: getGamma
    
!          LOCAL ARGS

    REAL(KIND=WP) :: sl1


    sl1 = 2.0_WP*aw**2/(fx_G**2 + fy_G**2)      

    getGamma = SQRT((1.0_WP + ( sl1 * (px**2.0_WP + py**2.0_WP) )) * &
                  (1.0_WP + eta * p2 )**2.0_WP / &
                  ( eta * p2 * (eta * p2 + 2.0_WP) ) )
  
  END FUNCTION getGamma

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

  FUNCTION getP2(gamma, px, py, eta, aw)
  
! Return p2, given gamma, px, py and eta
! 
!           ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: px(:), py(:), gamma(:), eta, aw

!            OUTPUT

    REAL(KIND=WP), DIMENSION(SIZE(gamma)) :: getP2

!              LOCAL ARGS

    REAL(KIND=WP) :: nc


    nc = 2.0_WP*aw**2/(fx_G**2 + fy_G**2)

    getP2 = ((gamma/SQRT(gamma**2 - 1.0_WP - &
             nc*(px**2 + py**2)))-1.0_WP)/eta
  
  END FUNCTION getP2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

  SUBROUTINE getExtent(off,length,start,endm)

! Simple subroutine to calculate the extent, 
! or beginning and end of the model in a dimension,
! given the length and the offset from zero.
! 
!              ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: off, length
    REAL(KIND=WP), INTENT(OUT) :: start, endm
    
    start = -length/2.0_WP + off
    
    endm = start + length
  
  END SUBROUTINE getExtent
    
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  SUBROUTINE splitBeam(N, globLen, numproc, rank, &
                      locN, local_start, local_end)

! Get local beam macroparticle number
! and start and end points in z2.
! 
!           ARGUMENTS

    INTEGER(KIND=IP), INTENT(IN) :: N, numproc, rank
    REAL(KIND=WP), INTENT(IN) :: globLen
    
    INTEGER(KIND=IP), INTENT(OUT) :: locN
    REAL(KIND=WP), INTENT(OUT) :: local_start, local_end
    
!          LOCAL ARGS
    
    REAL(KIND=WP) :: frac, OneElmLength, upperLength, lowerLength
    INTEGER(KIND=IP) :: lowern, highern, remainder


    frac = REAL(N)/REAL(numproc)
    lowern = FLOOR(frac)
    highern = CEILING(frac)
    remainder = MOD(N,numproc)
     
    IF (remainder==0) THEN
       locN = lowern
    ELSE
       IF (rank < remainder) THEN
          locN = highern
       ELSE
          locN = lowern
       ENDIF
    ENDIF

!     Calculate the distance covered by one element of the grid in z2

    OneElmLength = globLen/N	

!     Calculate the local length of the electron pulse a local process
!     will hold, in both the lower and upper case.
    
    lowerLength = lowern*OneElmLength
    upperLength = highern*OneElmLength

!     Calculate local start and end values.

    IF (rank >= remainder) THEN
       local_start = (upperLength*remainder) + &
            ((rank-remainder)*lowerLength)
       local_end = (upperLength*remainder) +&
            (((rank+1)-remainder)*lowerLength)
    ELSE
       local_start = rank*upperLength
       local_end = (rank+1)*upperLength
    ENDIF
	
    
  END SUBROUTINE splitBeam

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE getIntTypes(iNMPs, samplens, sigmas, &
                     inttypes)


!                   ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN)  :: iNMPs(:)
  REAL(KIND=WP), INTENT(IN) :: samplens(:), sigmas(:)
  INTEGER(KIND=IP), INTENT(OUT)  :: inttypes(:)

!                  LOCAL ARGS

  INTEGER(KIND=IP) :: iLIT

    DO iLIT=1,6
       IF (iNMPs(iLIT) == 1_IP .OR. samplens(iLIT)<=1.0E-6_WP &
            .OR. samplens(iLIT)<6.0_WP*sigmas(iLIT) ) THEN
          inttypes(iLIT)=iTopHatDistribution_CG
          IF (tProcInfo_G%qROOT) PRINT *, 'tophat'
       ELSE
          inttypes(iLIT)=iGaussianDistribution_CG
          IF (tProcInfo_G%qROOT) PRINT *, 'gauss'
       END IF
    ENDDO

END SUBROUTINE getIntTypes

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE genGrid(inttype,gridtype,centre,sigma,length,&
                   iNMP,iNMP_loc,Grid,Integral,qParallel,qOK)

  IMPLICIT NONE

! gridtype == iLinear_CG
!
!          ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: inttype, gridtype
  REAL(KIND=WP), INTENT(IN) :: centre, sigma, length
  INTEGER(KIND=IP), INTENT(IN) :: iNMP, iNMP_loc
  REAL(KIND=WP), INTENT(INOUT) :: Grid(:), Integral(:)
  LOGICAL, INTENT(IN) :: qParallel
  LOGICAL, INTENT(INOUT) :: qOK

!         LOCAL VARS

  REAL(KIND=WP) :: start, final, local_start, local_fin, shift
  INTEGER(KIND=IP) :: locN
  LOGICAL :: qOKL

  qOK = .FALSE.

  IF (qParallel) THEN
  
    CALL splitBeam(iNMP, length, tProcInfo_G%size, tProcInfo_G%rank, &
                   locN, local_start, local_fin)

    shift = centre - (length / 2.0_WP) ! Amount the beam is shifted from 
                                       ! having the head at z2=0

    local_start = local_start + shift
    local_fin = local_fin + shift

    CALL PulseGrid(iGridType=gridtype, &
                   iNumMP=locN, &
                   sStart=local_start, &
                   sEnd=local_fin, &
                   sGrid=Grid, &
                   qOK = qOKL)

    IF (.NOT. qOKL) GOTO 1000
		  
    CALL DistributionIntegralZ2(inttype, &
                                locN, &
                                iNMP, &
                                Grid, &
                                centre, &
                                sigma, &
                                MPI_DOUBLE_PRECISION, &
                                MPI_SUM, &
                                Integral, &
                                qOKL)

  ELSE

    CALL getExtent(centre,length,start,final)

    CALL PulseGrid(iGridType=gridtype, &
                   iNumMP=iNMP, &
                   sStart=start, &
                   sEnd=final, &
                   sGrid=Grid, &
                   qOK = qOKL)

    IF (.NOT. qOKL) GOTO 1000
		  
    CALL DistributionIntegral(inttype, &
                              iNMP, &
                              Grid, &
                              centre, &
                              sigma, &
                              Integral, &
                              qOKL)
  
    IF (.NOT. qOKL) GOTO 1000

  END IF

!     Set error flag and exit         

  qOK = .TRUE.				    
  GOTO 2000     

!     Error Handler

1000 CALL Error_log('Error in electron_grid:genGrid',tErrorLog_G)
  PRINT*,'Error in electron_grid:genGrid'

2000 CONTINUE

END SUBROUTINE genGrid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE getOffsets(sZ,samLenE,sZ2_center,gamma_d,offsets)

  IMPLICIT NONE

!             ARGUMENTS

  REAL(KIND=WP), INTENT(IN) :: sZ, samLenE(:),gamma_d
  REAL(KIND=WP), INTENT(INOUT) :: sZ2_center
  REAL(KIND=WP), INTENT(INOUT) :: offsets(:)

!             LOCAL ARGS

  REAL(KIND=WP) :: spx_offset, spy_offset, sx_offset, sy_offset, &
                   sGamma_offset, sz2_offset


!     Get offsets

  spx_offset     = pxOffset(sZ, sRho_G, fy_G)
  
  spy_offset     = pyOffset(sZ, sRho_G, fx_G)
  
  sGamma_offset  = sGammaR_G * gamma_d
         
  sx_offset      = xOffSet(sRho_G, sAw_G,  sGammaR_G, sGamma_offset, &
                           sEta_G, sKBeta_G, sFocusfactor_G, &
                           spx_offset, spy_offset, &
                           fx_G,fy_G, sZ)
            
  sy_offset      = yOffSet(sRho_G, sAw_G,  sGammaR_G, sGamma_offset, &
                           sEta_G, sKBeta_G, sFocusfactor_G, &
                           spx_offset, spy_offset, &
                           fx_G,fy_G, sZ)
              
!  sz2_offset     = samLenE(iZ2_CG)/2.0_WP

  IF (sZ2_center < (samLenE(iZ2_CG) / 2.0_WP)) THEN

    sz2_offset     = samLenE(iZ2_CG) / 2.0_WP

    sZ2_center     = sz2_offset

  ELSE

    sz2_offset     = sZ2_center

  END IF

  offsets(iX_CG)    = sx_offset
  offsets(iY_CG)    = sy_offset
  offsets(iZ2_CG)   = sz2_offset
  offsets(iPX_CG)   = spx_offset
  offsets(iPY_CG)   = spy_offset
  offsets(iPZ2_CG)  = sGamma_offset

END SUBROUTINE getOffsets

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE genGrids(sigmas,offsets,slens,intType,iNMPs,iNMPs_loc, &
                    sx_grid, sy_grid, sz2_grid, &
                    spx_grid, spy_grid, spz2_grid, &
                    sX_integral, sY_integral, sz2_integral, &
                    sPX_integral, sPY_integral, sPZ2_integral)

  IMPLICIT NONE

!           ARGUMENTS

  REAL(KIND=WP), INTENT(IN) :: sigmas(:), offsets(:), slens(:)
  INTEGER(KIND=IP), INTENT(IN) :: iNMPs(:), iNMPs_loc(:), intType(:)
  REAL(KIND=WP), INTENT(INOUT) :: sx_grid(:), sy_grid(:), sz2_grid(:), &
                                  spx_grid(:), spy_grid(:), spz2_grid(:), &
                                  sX_integral(:), sY_integral(:), sz2_integral(:), &
                                  sPX_integral(:), sPY_integral(:), sPZ2_integral(:)

!         LOCAL ARGS
  
  INTEGER(KIND=IP) :: ind
  LOGICAL :: qOKL
  
! Generate grids and integrals in each dimension


!               below to go into electronGrid()  
!  sGamma_len = sgammaR_G / 2.0_WP * samLenE(iPZ2_CG) 
!  sGamma_sig = sgammaR_G / 2.0_WP * sigE(iPZ2_CG)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Need to use a type construct of pointers to do the below,
!   see Fortran book, Section 6.14, page 126 to implement 
!   this.

!  DO ind = 1_IP, 6_IP
!
!    IF (ind==iZ2_CG) THEN
!
!      CALL genGrid(intType(ind),iLinear_CG,offsets(ind), &
!                   sigmas(ind),slens(ind),&
!                   iNMPs(ind),grids(ind,:),integrals(ind,:),.TRUE., &
!                   qOKL)
!
!    ELSE
!    
!      CALL genGrid(intType(ind),iLinear_CG,offsets(ind), &
!                   sigmas(ind),slens(ind),&
!                   iNMPs(ind),grids(ind,:),integrals(ind,:),.FALSE., &
!                   qOKL)
!
!    END IF
!
!    IF (.NOT. qOKL) GOTO 1000
!
!  END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  CALL genGrid(intType(iX_CG),iLinear_CG,offsets(iX_CG), &
               sigmas(iX_CG),slens(iX_CG),&
               iNMPs(iX_CG),iNMPs_loc(iX_CG),sx_grid,sX_integral,.FALSE., &
               qOKL)

  CALL genGrid(intType(iY_CG),iLinear_CG,offsets(iY_CG), &
               sigmas(iY_CG),slens(iY_CG),&
               iNMPs(iY_CG),iNMPs_loc(iY_CG),sy_grid,sY_integral,.FALSE., &
               qOKL)

  CALL genGrid(intType(iZ2_CG),iLinear_CG,offsets(iZ2_CG), &
               sigmas(iZ2_CG),slens(iZ2_CG),&
               iNMPs(iZ2_CG),iNMPs_loc(iZ2_CG),sz2_grid,sZ2_integral,.TRUE., &
               qOKL)

  CALL genGrid(intType(iPX_CG),iLinear_CG,offsets(iPX_CG), &
               sigmas(iPX_CG),slens(iPX_CG),&
               iNMPs(iPX_CG),iNMPs_loc(iPX_CG),spx_grid,sPX_integral,.FALSE., &
               qOKL)

  CALL genGrid(intType(iPY_CG),iLinear_CG,offsets(iPY_CG), &
               sigmas(iPY_CG),slens(iPY_CG),&
               iNMPs(iPY_CG),iNMPs_loc(iPY_CG),spy_grid,sPY_integral,.FALSE., &
               qOKL)

  CALL genGrid(intType(iPZ2_CG),iLinear_CG,offsets(iPZ2_CG), &
               sigmas(iPZ2_CG),slens(iPZ2_CG),&
               iNMPs(iPZ2_CG),iNMPs_loc(iPZ2_CG),spz2_grid,sPZ2_integral,.FALSE., &
               qOKL)

! Each gamma has its own sig_px, sig_py....need to change above to 
! do this. For now, sigma_px and sigma_py are the same for all gamma_j.

!  sigpx(size(nMP_P2))
!  sigpy(size(nMP_P2))

!  sigpx(i) = sigpx_avg * gamma_j / gamma_r
!  sigpy(i) = sigpy_avg * gamma_j / gamma_r


END SUBROUTINE genGrids

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE genBeam(iNMP,iNMP_loc,sigE,gamma_d,samLenE,sZ2_center,numproc, rank, &
                   i_RealE, q_noise, qOneD, sZ, x_tmpcoord, &
                   y_tmpcoord,z2_tmpcoord,px_tmpvector,py_tmpvector,&
                   pz2_tmpvector,s_tmp_max_av,s_tmp_macro,s_tmp_Vk)

  IMPLICIT NONE

!                   ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: iNMP(:),iNMP_loc(:)
  REAL(KIND=WP), INTENT(IN) :: samLenE(:), sigE(:), i_realE, &
                               gamma_d, sZ
                               
  REAL(KIND=WP), INTENT(INOUT) ::  sZ2_center
  INTEGER, INTENT(IN) :: numproc, rank
  LOGICAL, INTENT(IN) :: q_noise, qOneD
  REAL(KIND=WP), INTENT(INOUT) :: x_tmpcoord(:), y_tmpcoord(:), z2_tmpcoord(:), &
                                  px_tmpvector(:),py_tmpvector(:),&
                                  pz2_tmpvector(:) , s_tmp_Vk(:), &
                                  s_tmp_max_av, s_tmp_macro(:)


!                  LOCAL ARGS
  
  REAL(KIND=WP), ALLOCATABLE :: sx_grid(:), sy_grid(:), sz2_grid(:), & 
                                spx_grid(:), spy_grid(:), spz2_grid(:), &
                                sx_integral(:), sy_integral(:), &
                                sz2_integral(:), &
                                spx_integral(:), spy_integral(:), &
                                spz2_integral(:)
                                

  INTEGER(KIND=IP) :: iNumLocalElectrons(6)
  INTEGER(KIND=IP), ALLOCATABLE :: iLocalIntegralType(:)
  REAL(KIND=WP) :: offsets(6)
  integer :: error

  ALLOCATE(sx_grid(iNMP_loc(iX_CG)+1))
  ALLOCATE(sy_grid(iNMP_loc(iY_CG)+1))
  ALLOCATE(sz2_grid(iNMP_loc(iZ2_CG)+1))
  ALLOCATE(spx_grid(iNMP_loc(iPX_CG)+1))
  ALLOCATE(spy_grid(iNMP_loc(iPY_CG)+1))   
  ALLOCATE(spz2_grid(iNMP_loc(iPZ2_CG)+1))

  ALLOCATE(sX_integral(iNMP_loc(iX_CG)))
  ALLOCATE(sY_integral(iNMP_loc(iY_CG)))
  ALLOCATE(sz2_integral(iNMP_loc(iZ2_CG)))
  ALLOCATE(sPX_integral(iNMP_loc(iPX_CG)))
  ALLOCATE(sPY_integral(iNMP_loc(iPY_CG)))
  ALLOCATE(sPZ2_integral(iNMP_loc(iPZ2_CG))) 

  ALLOCATE(iLocalIntegralType(6))

!     Work out which integral type to use

  CALL getIntTypes(iNMP, samLenE, sigE, &
                   iLocalIntegralType)

  CALL getOffsets(sZ,samLenE,sZ2_center,gamma_d,offsets)

!!!!!!!!!! TEMP
!!!!!!!!!! CENTERING BEAM IN DX/DZ, DY/DZ = 0

  offsets(iPX_CG) = 0.0_WP
  offsets(iPY_CG) = 0.0_WP
  
!    sZ2_center = offsets(iZ2_CG)
    
  CALL genGrids(sigE,offsets,samLenE,iLocalIntegralType,iNMP, iNMP_loc, &
                sx_grid, sy_grid, sz2_grid, spx_grid, spy_grid, spz2_grid, &
                sX_integral, sY_integral, sz2_integral, &
                sPX_integral, sPY_integral, sPZ2_integral)

!    TOTALMPS = PRODUCT(INT(iNumLocalElectrons,KIND=IPL)) ! Total no of MPs

!     Allocate electrons' position and momenta arrays


  IF (qOneD) THEN ! If 1D, only need z2 and p2 to generate macroparticles

    IF (iNMP(iPZ2_CG) == 1_IP) THEN ! Cold beam case (important for noise)

      CALL genMacros(i_total_electrons=i_RealE, &
           q_noise=q_noise,                & 
           x_1_grid=sz2_grid,               &
           x_1_integral=sZ2_integral,       &
           s_number_macro=s_tmp_macro,     & 
           s_vol_element=s_tmp_Vk,          &
           max_av=s_tmp_max_av,             &
           x_1_coord=z2_tmpcoord)

      pz2_tmpvector = offsets(iPZ2_CG)

    ELSE

      CALL genMacros(i_total_electrons=i_RealE, &
                     q_noise=q_noise,                & 
                     x_1_grid=sz2_grid,               &
                     x_1_integral=sZ2_integral,       &	
                     p_3_grid=spz2_grid,              &
                     p_3_integral=sPZ2_integral,      &
                     s_number_macro=s_tmp_macro,     &
                     s_vol_element=s_tmp_Vk,         &
                     max_av=s_tmp_max_av,            & 
                     x_1_coord=z2_tmpcoord,           &
                     p_3_vector=pz2_tmpvector)

    END IF

    x_tmpcoord  = offsets(iX_CG) 
    y_tmpcoord  = offsets(iY_CG)
    px_tmpvector = offsets(iPX_CG)
    py_tmpvector = offsets(iPY_CG)    

  ELSE ! 6D beam

    CALL genMacros(i_total_electrons=i_RealE, &
                   q_noise=q_noise,                     & 
                   x_1_grid=sx_grid,               &
                   x_1_integral=sX_integral,       &
                   x_2_grid=sy_grid,               &
                   x_2_integral=sY_integral,       & 
                   x_3_grid=sz2_grid,                   &
                   x_3_integral=sZ2_integral,      &	
                   p_1_grid=spx_grid,              &
                   p_1_integral=sPX_integral,      &	 
                   p_2_grid=spy_grid,              &
                   p_2_integral=sPY_integral,      &	
                   p_3_grid=spz2_grid,             &
                   p_3_integral=sPZ2_integral,     &
                   s_number_macro=s_tmp_macro,     &
                   s_vol_element=s_tmp_Vk,         &
                   max_av=s_tmp_max_av,            &
                   x_1_coord=x_tmpcoord,           &
                   x_2_coord=y_tmpcoord,           &
                   x_3_coord=z2_tmpcoord,          &
                   p_1_vector=px_tmpvector,        &
                   p_2_vector=py_tmpvector,        &
                   p_3_vector=pz2_tmpvector)
  END IF

  DEALLOCATE(iLocalIntegralType)
  DEALLOCATE(sx_grid, sy_grid, sz2_grid, spx_grid, spy_grid)
  DEALLOCATE(spz2_grid, sX_integral, sY_integral, sz2_integral)
  DEALLOCATE(sPX_integral, sPY_integral, sPZ2_integral)


END SUBROUTINE genBeam

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE splitBeams(iNMP,samLenE,nBeams,numproc,rank,&
                      iNumLocalElectrons,totalmps_b)

  IMPLICIT NONE
  
!                   ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: iNMP(:,:)
  REAL(KIND=WP), INTENT(IN) :: samLenE(:,:)
  INTEGER(KIND=IP), INTENT(IN) :: nBeams
  INTEGER, INTENT(IN) :: numproc, rank
  INTEGER(KIND=IP), INTENT(INOUT) :: iNumLocalElectrons(:,:)
  INTEGER(KIND=IPL), INTENT(INOUT) :: totalmps_b(:)
  
!                   LOCAL ARGS

  REAL(KIND=WP) :: local_start, local_end
  INTEGER(KIND=IP) :: ind

!     Split electron macroparticles across z2


  iNumLocalElectrons(:,iX_CG) = iNMP(:,iX_CG)
  iNumLocalElectrons(:,iY_CG) = iNMP(:,iY_CG)
  iNumLocalElectrons(:,iPX_CG) = iNMP(:,iPX_CG)
  iNumLocalElectrons(:,iPY_CG) = iNMP(:,iPY_CG)
  iNumLocalElectrons(:,iPZ2_CG) = iNMP(:,iPZ2_CG)

  DO ind = 1, nBeams

    CALL splitBeam(iNMP(ind,iZ2_CG), samLenE(ind,iZ2_CG), numproc, rank, &
                   iNumLocalElectrons(ind,iZ2_CG), local_start, local_end)
                   
!             Total no of MPs in this beam
    totalmps_b(ind) = PRODUCT(INT(iNumLocalElectrons(ind,:),KIND=IPL)) 

  END DO

END SUBROUTINE splitBeams

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE getKeepNum(s_tmp_macro,sElectronThreshold,TOTALMPS, &
                     ikeepnumber, iendnumber, ilowerElectron)

  IMPLICIT NONE

! Discard macroparticles with weights below a certain threshold.
! Return the number of macroparticles which we are keeping, and the 
! number of particles we are discarding.

!                 ARGUMENTS

  REAL(KIND=WP), INTENT(IN) :: s_tmp_macro(:)
  REAL(KIND=WP), INTENT(IN) :: sElectronThreshold
  INTEGER(KIND=IPL), INTENT(IN) :: TOTALMPS
  INTEGER(KIND=IPL), INTENT(INOUT) :: ikeepnumber, iendnumber
  REAL(KIND=WP), INTENT(INOUT) :: ilowerElectron

!                LOCAL ARGS

  REAL(KIND=WP) :: total_local_real_electrons, &
                   n_real_electrons
                   
  INTEGER :: error

  total_local_real_electrons = SUM(s_tmp_macro)
 
  CALL MPI_ALLREDUCE(total_local_real_electrons,&
       n_real_electrons, 1, MPI_DOUBLE_PRECISION, &
       MPI_SUM,tProcInfo_G%comm,error) 
			
  ilowerElectron=n_real_electrons/&
       REAL(TOTALMPS,KIND=WP)*(sElectronThreshold/100.0_WP)

  ikeepnumber=COUNT(s_tmp_macro>=ilowerElectron)
  iendnumber=TOTALMPS-ikeepnumber

END SUBROUTINE getKeepNum

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE getIndices(s_tmp_macro,ilowerElectron,TOTALMPS, &
                     ikeepos, iendpos)

  IMPLICIT NONE

! Discard macroparticles with weights below a certain threshold.
! Return indices of macroparticles which we are keeping, and indices
! of particles we are discarding.

!                 ARGUMENTS

  REAL(KIND=WP), INTENT(IN) :: s_tmp_macro(:)
  REAL(KIND=WP), INTENT(IN) :: ilowerElectron
  INTEGER(KIND=IPL), INTENT(IN) :: TOTALMPS
  INTEGER(KIND=IPL), INTENT(INOUT) :: ikeepos(:), iendpos(:)


!                LOCAL ARGS

  INTEGER(KIND=IPL) :: il, jl, kl


    jl=0
    kl=0
    
    DO il=1,TOTALMPS

       IF (s_tmp_macro(il)<ilowerElectron) THEN
          jl=jl+1_IPL
          iendpos(jl)=il
       ELSE
          kl=kl+1_IPL
          ikeepos(kl)=il 
       ENDIF
       
    ENDDO

END SUBROUTINE getIndices

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE getStEnd(nbeams,totalmps_b,b_sts,b_ends)

! This routine calculates the start and ending indices
! for each beam within the larger array.
!
!            ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: nBeams
  INTEGER(KIND=IPL), INTENT(IN) :: totalmps_b(nBeams)
  INTEGER(KIND=IPL), INTENT(INOUT) :: b_sts(nBeams),b_ends(nBeams)

!            LOCAL ARGS

  INTEGER(KIND=IP) :: b_ind

  b_sts(1) = 1_IPL
  b_ends(1) = totalmps_b(1)

  IF (nbeams > 1) THEN

    DO b_ind = 2,nbeams
 
      b_sts(b_ind) = b_ends(b_ind-1) + 1_IPL
      b_ends(b_ind) = b_ends(b_ind-1) + totalmps_b(b_ind)

    END DO

  END IF

  END SUBROUTINE getStEnd

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE removeLow(Tmp_chibar, Tmp_Normchi, b_sts,b_ends,sElectronThreshold, &
 chirp,nbeams,x_tmpcoord,y_tmpcoord,z2_tmpcoord,px_tmpvector,&
                     py_tmpvector, pz2_tmpvector,totalmps_b,&
                     sZ2_center)
                   
  IMPLICIT NONE
  
! Discard macroparticles with weights below a certain threshold.
! This subroutine assigns macroparticle values to global arrays,
! and removes macroparticles with a low weight in the process.

!                   ARGUMENTS

  REAL(KIND=WP), INTENT(IN) :: Tmp_chibar(:), Tmp_Normchi(:), &
                               x_tmpcoord(:), y_tmpcoord(:), &
                               z2_tmpcoord(:), px_tmpvector(:),&
                               py_tmpvector(:), pz2_tmpvector(:)
  INTEGER(KIND=IPL), INTENT(IN) :: b_sts(:), b_ends(:)
  INTEGER(KIND=IP), INTENT(IN) :: nbeams
  REAL(KIND=WP), INTENT(IN) :: chirp(:)
  REAL(KIND=WP), INTENT(IN) :: sElectronThreshold
  INTEGER(KIND=IPL), INTENT(IN) :: totalmps_b(:)
  REAL(KIND=WP), INTENT(IN) :: sZ2_center(:)

!                  LOCAL ARGS

  INTEGER(KIND=IPL), ALLOCATABLE :: ikeepos(:), iendpos(:), b_keepn(:),&
                                    b_neglectn(:)
  REAL(KIND=WP), ALLOCATABLE :: ilowerElectron(:), Qchoff(:)
  INTEGER(KIND=IPL) :: nsum, ist, ien, prev
  INTEGER(KIND=IP) :: b_ind

  ALLOCATE(b_keepn(nbeams),b_neglectn(nbeams),ilowerElectron(nbeams))
  
  DO b_ind=1, nbeams

    CALL getKeepNum(Tmp_chibar(b_sts(b_ind):b_ends(b_ind)),&
                    sElectronThreshold,totalmps_b(b_ind), &
                    b_keepn(b_ind), b_neglectn(b_ind), &
                    ilowerElectron(b_ind))

  END DO

  ALLOCATE(sEl_X0Position_G(SUM(b_keepn)))
  ALLOCATE(sEl_Y0Position_G(SUM(b_keepn)))
  ALLOCATE(sEl_Z20Position_G(SUM(b_keepn)))
  ALLOCATE(sEl_PX0Position_G(SUM(b_keepn)))
  ALLOCATE(sEl_PY0Position_G(SUM(b_keepn)))
  ALLOCATE(sEl_PZ20Position_G(SUM(b_keepn)))
  ALLOCATE(s_chi_bar_G(SUM(b_keepn)))
  ALLOCATE(s_Normalised_chi_G(SUM(b_keepn)))

  nsum = 0_IPL
  prev = 0_IPL

  DO b_ind=1, nbeams

    ALLOCATE(ikeepos(b_keepn(b_ind)), iendpos(b_neglectn(b_ind)))
    ALLOCATE(Qchoff(b_keepn((b_ind))))

    CALL getIndices(Tmp_chibar(b_sts(b_ind):b_ends(b_ind)), &
                    ilowerElectron(b_ind),totalmps_b(b_ind), &
                    ikeepos, iendpos)

    ist = 1_IPL + NSUM
    ien = NSUM + b_keepn(b_ind)

    sEl_X0Position_G(ist:ien)   = x_tmpcoord(ikeepos + prev)
    sEl_Y0Position_G(ist:ien)   = y_tmpcoord(ikeepos + prev)
    sEl_Z20Position_G(ist:ien)  = z2_tmpcoord(ikeepos + prev)
    sEl_PX0Position_G(ist:ien)  = px_tmpvector(ikeepos + prev)
    sEl_PY0Position_G(ist:ien)  = py_tmpvector(ikeepos + prev)
    sEl_PZ20Position_G(ist:ien) = pz2_tmpvector(ikeepos + prev)
    s_chi_bar_G(ist:ien)        = Tmp_chibar(ikeepos + prev)
    s_Normalised_chi_G(ist:ien) = Tmp_Normchi(ikeepos + prev)

    nsum = nsum + b_keepn(b_ind)

    DEALLOCATE(ikeepos,iendpos)

!     Add linear chirp to the beam....TEMP

    Qchoff = chirp(b_ind)*(sEl_Z20Position_G(ist:ien)-sZ2_center(b_ind))
    sEl_PZ20Position_G(ist:ien) = sEl_PZ20Position_G(ist:ien) + Qchoff

    DEALLOCATE(Qchoff)

    prev = prev + totalmps_b(b_ind)

  END DO

  iNumberElectrons_G = SUM(b_keepn)

END SUBROUTINE removeLow

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE beamReport(s_tmp_macro,sElectronThreshold,beam_no)

!                   ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: s_tmp_macro(:)
    REAL(KIND=WP), INTENT(IN) :: sElectronThreshold
    INTEGER(KIND=IP), INTENT(IN) :: beam_no

!                   LOCAL ARGS

    REAL(KIND=WP) :: total_local_real_electrons, n_real_electrons
    INTEGER(KIND=IP) :: error

    total_local_real_electrons = SUM(s_tmp_macro)

!     Sum local electron number to global value

    CALL MPI_ALLREDUCE(total_local_real_electrons,&
         n_real_electrons, 1, MPI_DOUBLE_PRECISION, &
         MPI_SUM,tProcInfo_G%comm,error) 

!     Print info to standard out

    IF (tProcInfo_G%qROOT) THEN
       PRINT*, 'generating beam number ', beam_no
       PRINT '(A22,ES11.5E2)', 'Total Real Electrons ',&
            n_real_electrons
       PRINT '(A19,F6.2,A2)', 'THRESHOLD LEVEL - ',&
            sElectronThreshold,'%'
       !PRINT '(A8,F6.4,A2)', 'LOST - ',&
       !     (totalmps-igloNumElectrons_G)*100.0_WP/&
       !     totalmps,'%'
    ENDIF

END SUBROUTINE beamReport

END MODULE ElectronInit
