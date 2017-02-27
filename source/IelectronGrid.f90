!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Top-level module for generating electron beam macroparticles in the simple
!> beam case. Can generate multiple simple beams with homogeneous distributions
!> in 6 dimensions.

MODULE ElectronInit

USE paratype
USE ParallelInfoType
USE Globals
USE MacrosGen
use parBeam
use remLow
use grids
use initConds
use parallelSetup
use gtop2
use addNoise
use pseqs


IMPLICIT NONE

CONTAINS

  SUBROUTINE electron_grid(i_RealE, &
       iNMP, &
       q_noise, &
       sZ, &
       nbeams, &
       samLenE, &
       sigE, &
       alphax, alphay, &
       emitx, emity, &
       beamCenZ2, &
       gamma_d, &
       sElectronThreshold, &
       qOneD, &
       chirp, &
       mag, fr, &
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

    REAL(KIND=WP), INTENT(IN)   :: i_RealE(:), alphax(:), alphay(:), &
                                   emitx(:), emity(:)
    REAL(KIND=WP), INTENT(INOUT):: beamCenZ2(:)
    INTEGER(KIND=IP), INTENT(IN):: iNMP(:,:)
    LOGICAL, INTENT(IN)         :: q_noise
    REAL(KIND=WP), INTENT(IN)   :: sZ
    REAL(KIND=WP), INTENT(INOUT) :: chirp(:), mag(:), fr(:)


    INTEGER(KIND=IP), INTENT(IN) :: nbeams
    REAL(KIND=WP), INTENT(INOUT):: samLenE(:,:)    
    
    REAL(KIND=WP), INTENT(INOUT)   :: sigE(:,:)
    REAL(KIND=WP), INTENT(IN)   :: gamma_d(:)
    REAL(KIND=WP), INTENT(IN)   :: sElectronThreshold

    LOGICAL,         INTENT(IN):: qOneD

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
    REAL(KIND=WP) :: local_start, local_end, afact, um, kx, ky

    REAL(KIND=WP) :: offsets(6), betax(nbeams), betay(nbeams)

    real(kind=wp) :: sigpx0, sigpy0
    
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

!    um = sqrt(fx_G**2.0_WP + fy_G**2.0_WP)
!    afact = sqrt(2.0_WP) * saw_G / um
    
    afact = saw_G

!    sigE(:,iPX_CG) = sigE(:,iPX_CG) * afact * &
!                     sqrt((sEta_G * (sEta_G + 2.0_WP) / (1 + afact**2.0_WP)))
                     
!    sigE(:,iPY_CG) = sigE(:,iPY_CG) * afact * &
!                     sqrt((sEta_G * (sEta_G + 2.0_WP) / (1 + afact**2.0_WP)))                     

!    sigE(:,iPX_CG) = sigE(:,iPX_CG) * afact * &
!                     (1.0_WP + sEta_G) / sgammaR_G
                     
!    sigE(:,iPY_CG) = sigE(:,iPY_CG) * afact * &
!                     (1.0_WP + sEta_G) / sgammaR_G
    

!    samLenE(:,iPX_CG) = samLenE(:,iPX_CG) * afact * &
!                     sqrt((sEta_G * (sEta_G + 2.0_WP) / (1.0_WP + afact**2.0_WP))) 

!    samLenE(:,iPY_CG) = samLenE(:,iPY_CG) * afact * &
!                     sqrt((sEta_G * (sEta_G + 2.0_WP) / (1.0_WP + afact**2.0_WP))) 

!     samLenE(:,iPX_CG) = sigE(:,iPX_CG) * 6.0_WP
!     samLenE(:,iPY_CG) = sigE(:,iPY_CG) * 6.0_WP









!                 rms px at x = 0 and py at y = 0

!    sigpx0 = gamma_d * sGammaR_G * sqrt(emitx / betax) / saw_G
!    sigpy0 = gamma_d * sGammaR_G * sqrt(emity / betay) / saw_G

    do b_ind = 1, nbeams

      if (emitx(b_ind) > 0.0_wp) then

!           'SI' beta!!! i.e. for dx/dz, not for px or py....

        betax(b_ind) = lg_G * sigE(b_ind,iX_CG)**2.0_wp / sRho_G / emitx(b_ind)

!     getting rms sigma pxbar (@ x=0) and pybar (@ y=0) from Twiss

        sigE(b_ind,iPX_CG) = gamma_d(b_ind) * sGammaR_G * sqrt(lg_G*lc_G) * &
                                    sigE(b_ind,iX_CG) / betax(b_ind) / saw_G

      else 
        betax(b_ind) = -1.0_wp
      end if

      if (emity(b_ind) > 0.0_wp) then
        
        betay(b_ind) = lg_G * sigE(b_ind,iY_CG)**2.0_wp / sRho_G / emity(b_ind)

        sigE(b_ind,iPY_CG) = gamma_d(b_ind) * sGammaR_G * sqrt(lg_G*lc_G) * &
                                    sigE(b_ind,iY_CG) / betay(b_ind) / saw_G

      else 
        betay(b_ind) = -1.0_wp
      end if

    end do

!    print*, 'hey there, I got betax = ', betax
!    print*, 'hey there, I got betay = ', betay
!    print*, 'hey there, I got emitx = ', emitx
!    print*, 'hey there, I got emity = ', emity
!    print*, 'hey there, I got sigpx = ', sigE(:,iPX_CG)
!    print*, 'hey there, I got sigpy = ', sigE(:,iPY_CG)



    DO b_ind = 1,nbeams

      CALL genBeam(iNMP(b_ind,:),iNumLocalElectrons(b_ind,:),&
                    sigE(b_ind,:), alphax(b_ind), betax(b_ind), &
                    alphay(b_ind), betay(b_ind), gamma_d(b_ind), &
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
                    s_tmp_Vk(b_sts(b_ind):b_ends(b_ind)), b_ind)
                      
    END DO

!    npk_bar_G = maxval(s_tmp_max_av) ! record peak density
    
    CALL getChi(s_tmp_macro, s_tmp_Vk, npk_bar_G, &
                Tmp_chibar, Tmp_Normchi)
    
    DEALLOCATE(s_tmp_macro,s_tmp_Vk)

    CALL removeLow(Tmp_chibar, Tmp_Normchi, b_sts, b_ends, sElectronThreshold, &
                   nbeams,x_tmpcoord,y_tmpcoord,z2_tmpcoord,px_tmpvector,&
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
!!!!!!! BECAUSE PXBAR OFFSET IS NOT DEPENDENT ON GAMMA, BUT SIGMA_PXBAR 
!!!!!!! IS - AND DXDZ OFFSET *IS* DEPENDANT ON GAMMA, BUT SIGMA_DXDZ
!!!!!!! IS NOT

    ALLOCATE(tconv(size(sElPX_G)))
    
!    tconv = sElPX_G**2.0_WP + sElPY_G**2.0_WP

!    sElPX_G = sqrt((sElGam_G**2.0_WP - 1.0_WP) / &
!                            (1.0_WP + tconv))  &
!                        / afact * sElPX_G

!    sElPY_G = sqrt((sElGam_G**2.0_WP - 1.0_WP) / &
!                            (1.0_WP + tconv))  &
!                        / afact * sElPY_G


    DEALLOCATE(tconv)


!     We currently have gamma -
!     need to change to gamma / gamma_r

    sElGam_G = sElGam_G / sGammaR_G

    sElPY_G = - sElPY_G

!     Sum the local num of macroparticles to a global number

    call sum_mpi_int14(iNumberElectrons_G,iGloNumElectrons_G)

    mag(:) = mag(:) / sGammaR_G
    chirp(:) = chirp(:) / sGammaR_G

    do b_ind = 1, nbeams
  
      call addChirp(sElGam_G(b_sts(b_ind):b_ends(b_ind)), &
                    sElZ2_G(b_sts(b_ind):b_ends(b_ind)), &
                    b_ends(b_ind) - b_sts(b_ind) + 1, beamCenZ2(b_ind), &
                    chirp(b_ind))
  
  
      call addModulation(sElGam_G(b_sts(b_ind):b_ends(b_ind)), &
                         sElZ2_G(b_sts(b_ind):b_ends(b_ind)), &
                         b_ends(b_ind) - b_sts(b_ind) + 1, &
                         mag(b_ind), fr(b_ind))
  

    end do



!     Set error flag and exit         

    qOK = .TRUE.				    
    GOTO 2000     

!     Error Handler

1000 CALL Error_log('Error in ElectronInit:electron_grid',tErrorLog_G)
    PRINT*,'Error in ElectronInit:electron_grid'
2000 CONTINUE
  END SUBROUTINE electron_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE genBeam(iNMP, iNMP_loc, sigE, alphax, betax, alphay, betay, &
                   gamma_d, samLenE, sZ2_center, numproc, rank, &
                   i_RealE, q_noise, qOneD, sZ, x_tmpcoord, &
                   y_tmpcoord,z2_tmpcoord,px_tmpvector,py_tmpvector,&
                   pz2_tmpvector,s_tmp_max_av,s_tmp_macro,s_tmp_Vk, b_num)

  IMPLICIT NONE

!                   ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: iNMP(:),iNMP_loc(:), b_num
  REAL(KIND=WP), INTENT(IN) :: samLenE(:), sigE(:), i_realE, &
                               gamma_d, sZ, alphax, betax, alphay, betay
                               
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
  REAL(KIND=WP) :: offsets(6), gxpx, gypy
  integer :: error
  integer(kind=ip) :: nseqparts
  real(kind=wp), allocatable :: xseq(:), yseq(:), pxseq(:), &
                                pyseq(:), gamseq(:)

  real(kind=wp), allocatable :: nktemp(:), z2base(:), vkt(:)

  integer(kind=ip) :: ij

  ALLOCATE(sx_grid(iNMP_loc(iX_CG)+1))
  ALLOCATE(sy_grid(iNMP_loc(iY_CG)+1))
  ALLOCATE(sz2_grid(iNMP_loc(iZ2_CG)+1))
  ALLOCATE(spx_grid(iNMP_loc(iPX_CG)+1))
  ALLOCATE(spy_grid(iNMP_loc(iPY_CG)+1))   
  ALLOCATE(spz2_grid(iNMP_loc(iGam_CG)+1))

  ALLOCATE(sX_integral(iNMP_loc(iX_CG)))
  ALLOCATE(sY_integral(iNMP_loc(iY_CG)))
  ALLOCATE(sz2_integral(iNMP_loc(iZ2_CG)))
  ALLOCATE(sPX_integral(iNMP_loc(iPX_CG)))
  ALLOCATE(sPY_integral(iNMP_loc(iPY_CG)))
  ALLOCATE(sPZ2_integral(iNMP_loc(iGam_CG))) 

  ALLOCATE(iLocalIntegralType(6))

!     Work out which integral type to use

  CALL getIntTypes(iNMP, samLenE, sigE, &
                   iLocalIntegralType)

  CALL getOffsets(sZ,samLenE,sZ2_center,gamma_d,offsets)

!!!!!!!!!! TEMP
!!!!!!!!!! CENTERING BEAM IN DX/DZ, DY/DZ = 0

  offsets(iX_CG) = 0.0_WP
  offsets(iY_CG) = 0.0_WP
  offsets(iPX_CG) = 0.0_WP
  offsets(iPY_CG) = 0.0_WP
  
!    sZ2_center = offsets(iZ2_CG)
    
  CALL genGrids(b_num, sigE,offsets,samLenE,iLocalIntegralType,iNMP, iNMP_loc, &
                sx_grid, sy_grid, sz2_grid, spx_grid, spy_grid, spz2_grid, &
                sX_integral, sY_integral, sz2_integral, &
                sPX_integral, sPY_integral, sPZ2_integral)

!    TOTALMPS = PRODUCT(INT(iNumLocalElectrons,KIND=IPL)) ! Total no of MPs

!     Allocate electrons' position and momenta arrays




! #####################################################################
!
!  For equispaced grids in EVERY dimension - this can be memory intensive,
!  and you may end up with more macroparticles than real electrons!!!
!  The noise statistics will be correct in every dimension - but you may be
!  better doing a one-to-one simulation in this case.
!


  IF (qOneD) THEN ! If 1D, only need z2 and p2 to generate macroparticles
  
    IF (iNMP(iGam_CG) == 1_IP) THEN ! Cold beam case (important for noise)
  
      CALL genMacros(i_total_electrons=i_RealE, &
           q_noise=q_noise,                & 
           x_1_grid=sz2_grid,               &
           x_1_integral=sZ2_integral,       &
           s_number_macro=s_tmp_macro,     & 
           s_vol_element=s_tmp_Vk,          &
           max_av=s_tmp_max_av,             &
           x_1_coord=z2_tmpcoord)
  
      pz2_tmpvector = offsets(iGam_CG)
  
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

    if (qEquiXY_G) then

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
    

    else    ! using sequences in transverse plane






! #####################################################################
!
!  For equispaced particles (before noise is added) in z2 only -
!  a random or low-discrepancy sequence will be used for every other 
!  dimension. The same sequences will be reused for every slice,
!  creating 'beamlets' in the 6D phase space in z2.
!
!  This ensures a quiet start in z2 only, and so the noise statistics
!  will only be correct in z2.
!
!  Currently, only random sequences in the other dimensions are used.
!  Halton/Hammersley or other low-discrepency sequences may be added 
!  later...

      nseqparts = nseqparts_G
 
      allocate(nktemp(iNMP_loc(iZ2_CG)), z2base(iNMP_loc(iZ2_CG)))
      allocate(vkt(iNMP_loc(iZ2_CG)))
   

!    if (tProcInfo_G%qRoot) then
!    print*, 'size z2 grid = ', size(sz2_grid)
!    print*, 'size z2 int = ', size(sZ2_integral)
!    print*, 'size nks = ', size(nktemp)
!    print*, 'size Vk', size(s_tmp_Vk)
!    print*, 'size z2 temp', size(z2_tmpcoord)
!    print*, 'sigmas are...', sigE
!    end if


!    call mpi_finalize(error)
!    stop

!   Create quiet 'base' beam in z2...

      CALL genMacros(i_total_electrons=i_RealE, &
                     q_noise=.false.,           & 
                     x_1_grid=sz2_grid,         &
                     x_1_integral=sZ2_integral, &
                     s_number_macro=nktemp,   & 
                     s_vol_element=vkt,       &
                     max_av=s_tmp_max_av,     &
                     x_1_coord=z2base)

!   ...then generate some random sequences for the other 5 dimensions...

      allocate(xseq(nseqparts), yseq(nseqparts), &
               pxseq(nseqparts), pyseq(nseqparts), &
               gamseq(nseqparts))  ! to store 'constant' sequences which will be replicated for each z2 slice

      call getSeqs(xseq, yseq, pxseq, pyseq, gamseq, sigE)

!   ...then, each particle in this 1D beam (which has perfectly equispaced particles)
!   is split into many particles with the same temporal/longitudinal coordinate
!   with transverse positions given by the random particle distributions

      gamseq = gamseq + offsets(iGam_CG)


      
!   Rotate phase space to Twiss params...

      if (betax > 0.0_wp) then
        gxpx = -alphax / betax
      else 
        gxpx = 0.0_wp
      end if

      if (betay > 0.0_wp) then
        gypy = -alphay / betay
      else
        gypy = 0.0_wp
      end if

      pxseq = pxseq + gamseq * gxpx * sqrt(lg_G * lc_G) * xseq / saw_G
      pyseq = pyseq + gamseq * gypy * sqrt(lg_G * lc_G) * yseq / saw_G


      do ij = 1, iNMP_loc(iZ2_CG)

        s_tmp_macro((ij-1)*nseqparts+1:(ij*nseqparts)) &
                         = nktemp(ij) / nseqparts   ! split charge evenly across MPs

        z2_tmpcoord((ij-1)*nseqparts+1:(ij*nseqparts)) = z2base(ij)
        x_tmpcoord((ij-1)*nseqparts+1:(ij*nseqparts)) = xseq(:)
        y_tmpcoord((ij-1)*nseqparts+1:(ij*nseqparts)) = yseq(:)
        px_tmpvector((ij-1)*nseqparts+1:(ij*nseqparts)) = pxseq(:)
        py_tmpvector((ij-1)*nseqparts+1:(ij*nseqparts)) = pyseq(:)
        pz2_tmpvector((ij-1)*nseqparts+1:(ij*nseqparts)) = gamseq(:)   ! Sequences constructed!!

        s_tmp_Vk((ij-1)*nseqparts+1:(ij*nseqparts)) = vkt(ij)

      ! Need to rename z2, x, y etc above
      ! to use z2_tmpcoord etc...
      ! Make a new temp z2 array to put into genMacros

      end do

      deallocate(xseq, yseq, pxseq, pyseq, gamseq)
      deallocate(nktemp, z2base)

      if (q_noise) call applyNoise(z2_tmpcoord, sz2_grid(2) - sz2_grid(1), s_tmp_macro)  ! add noise in z2

    end if 

  end if





  DEALLOCATE(iLocalIntegralType)
  DEALLOCATE(sx_grid, sy_grid, sz2_grid, spx_grid, spy_grid)
  DEALLOCATE(spz2_grid, sX_integral, sY_integral, sz2_integral)
  DEALLOCATE(sPX_integral, sPY_integral, sPZ2_integral)


END SUBROUTINE genBeam

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
