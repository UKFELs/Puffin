!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module remLow

! This module defines subroutines used to remove electron
! macroparticles with a low chi weighting factor from 
! the initially generated macroparticles.

use paratype
use Globals
use parallelSetup
use gtop2
use beamPrep

implicit none

contains

SUBROUTINE removeLowNC(Tmp_chibar, Tmp_Normchi, b_sts,b_ends,sElectronThreshold, &
                       npk,nbeams,x_tmpcoord,y_tmpcoord,z2_tmpcoord,px_tmpvector,&
                       py_tmpvector, pz2_tmpvector,totalmps_b)

! Discard macroparticles with weights below a certain threshold.
! This subroutine assigns macroparticle values to global arrays,
! and removes macroparticles with a low weight in the process.
! This is a modified version of the routine below,
! with the addition of the chirp onto the energy removed.
! Eventually, that routine sould be replaced with this one,
! and a new routine created which adds the chirp seperately.

!                   ARGUMENTS

  IMPLICIT NONE

  REAL(KIND=WP), INTENT(IN) :: Tmp_chibar(:), Tmp_Normchi(:), &
                               x_tmpcoord(:), y_tmpcoord(:), &
                               z2_tmpcoord(:), px_tmpvector(:),&
                               py_tmpvector(:), pz2_tmpvector(:)
  INTEGER(KIND=IPL), INTENT(IN) :: b_sts(:), b_ends(:)
  INTEGER(KIND=IP), INTENT(IN) :: nbeams
  REAL(KIND=WP), INTENT(IN) :: sElectronThreshold, npk
  INTEGER(KIND=IPL), INTENT(IN) :: totalmps_b(:)

!                  LOCAL ARGS

  INTEGER(KIND=IPL), ALLOCATABLE :: ikeepos(:), iendpos(:), b_keepn(:),&
                                    b_neglectn(:)
  REAL(KIND=WP), ALLOCATABLE :: ilowerElectron(:)
  INTEGER(KIND=IPL) :: nsum, ist, ien, prev
  INTEGER(KIND=IP) :: b_ind

  ALLOCATE(b_keepn(nbeams),b_neglectn(nbeams),ilowerElectron(nbeams))
  
  DO b_ind=1, nbeams

    CALL getKeepNum(npk*Tmp_chibar(b_sts(b_ind):b_ends(b_ind)),&
                    sElectronThreshold,totalmps_b(b_ind), &
                    b_keepn(b_ind), b_neglectn(b_ind), &
                    ilowerElectron(b_ind))

  END DO


  ALLOCATE(sElX_G(SUM(b_keepn)))
  ALLOCATE(sElY_G(SUM(b_keepn)))
  ALLOCATE(sElZ2_G(SUM(b_keepn)))
  ALLOCATE(sElPX_G(SUM(b_keepn)))
  ALLOCATE(sElPY_G(SUM(b_keepn)))
  ALLOCATE(sElPZ2_G(SUM(b_keepn)))
  ALLOCATE(s_chi_bar_G(SUM(b_keepn)))
  ALLOCATE(s_Normalised_chi_G(SUM(b_keepn)))

  nsum = 0_IPL
  prev = 0_IPL

  DO b_ind=1, nbeams

    ALLOCATE(ikeepos(b_keepn(b_ind)), iendpos(b_neglectn(b_ind)))

    CALL getIndices(npk * Tmp_chibar(b_sts(b_ind):b_ends(b_ind)), &
                    ilowerElectron(b_ind),totalmps_b(b_ind), &
                    ikeepos, iendpos)

    ist = 1_IPL + NSUM
    ien = NSUM + b_keepn(b_ind)

    sElX_G(ist:ien)   = x_tmpcoord(ikeepos + prev)
    sElY_G(ist:ien)   = y_tmpcoord(ikeepos + prev)
    sElZ2_G(ist:ien)  = z2_tmpcoord(ikeepos + prev)
    sElPX_G(ist:ien)  = px_tmpvector(ikeepos + prev)
    sElPY_G(ist:ien)  = py_tmpvector(ikeepos + prev)
    sElPZ2_G(ist:ien) = pz2_tmpvector(ikeepos + prev)
    s_chi_bar_G(ist:ien)        = Tmp_chibar(ikeepos + prev)
    s_Normalised_chi_G(ist:ien) = Tmp_Normchi(ikeepos + prev)

    nsum = nsum + b_keepn(b_ind)

    DEALLOCATE(ikeepos,iendpos)

    prev = prev + totalmps_b(b_ind)

  END DO

  iNumberElectrons_G = SUM(b_keepn)

  call sum_mpi_int14(iNumberElectrons_G,iGloNumElectrons_G)


!     We currently have gamma in the p2 position array -
!     need to change to p2

!    sElPZ2_G = getP2(sElPZ2_G, sElPX_G,&
!                     sElPY_G, sEta_G, sAw_G)
     
  sElPZ2_G = sElPZ2_G / sGammaR_G

END SUBROUTINE removeLowNC

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE removeLow(Tmp_chibar, Tmp_Normchi, b_sts,b_ends,sElectronThreshold, &
                     chirp,mag,fr,nbeams,x_tmpcoord,y_tmpcoord,z2_tmpcoord,px_tmpvector,&
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
  REAL(KIND=WP), INTENT(IN) :: chirp(:), mag(:), fr(:)
  REAL(KIND=WP), INTENT(IN) :: sElectronThreshold
  INTEGER(KIND=IPL), INTENT(IN) :: totalmps_b(:)
  REAL(KIND=WP), INTENT(IN) :: sZ2_center(:)

!                  LOCAL ARGS

  INTEGER(KIND=IPL), ALLOCATABLE :: ikeepos(:), iendpos(:), b_keepn(:),&
                                    b_neglectn(:)
  REAL(KIND=WP), ALLOCATABLE :: ilowerElectron(:)
  INTEGER(KIND=IPL) :: nsum, ist, ien, prev
  INTEGER(KIND=IP) :: b_ind

  ALLOCATE(b_keepn(nbeams),b_neglectn(nbeams),ilowerElectron(nbeams))
  
  DO b_ind=1, nbeams

    CALL getKeepNum(Tmp_chibar(b_sts(b_ind):b_ends(b_ind)),&
                    sElectronThreshold,totalmps_b(b_ind), &
                    b_keepn(b_ind), b_neglectn(b_ind), &
                    ilowerElectron(b_ind))

  END DO

  ALLOCATE(sElX_G(SUM(b_keepn)))
  ALLOCATE(sElY_G(SUM(b_keepn)))
  ALLOCATE(sElZ2_G(SUM(b_keepn)))
  ALLOCATE(sElPX_G(SUM(b_keepn)))
  ALLOCATE(sElPY_G(SUM(b_keepn)))
  ALLOCATE(sElPZ2_G(SUM(b_keepn)))
  ALLOCATE(s_chi_bar_G(SUM(b_keepn)))
  ALLOCATE(s_Normalised_chi_G(SUM(b_keepn)))

  nsum = 0_IPL
  prev = 0_IPL

  DO b_ind=1, nbeams

    ALLOCATE(ikeepos(b_keepn(b_ind)), iendpos(b_neglectn(b_ind)))

    CALL getIndices(Tmp_chibar(b_sts(b_ind):b_ends(b_ind)), &
                    ilowerElectron(b_ind),totalmps_b(b_ind), &
                    ikeepos, iendpos)

    ist = 1_IPL + NSUM
    ien = NSUM + b_keepn(b_ind)

    sElX_G(ist:ien)   = x_tmpcoord(ikeepos + prev)
    sElY_G(ist:ien)   = y_tmpcoord(ikeepos + prev)
    sElZ2_G(ist:ien)  = z2_tmpcoord(ikeepos + prev)
    sElPX_G(ist:ien)  = px_tmpvector(ikeepos + prev)
    sElPY_G(ist:ien)  = py_tmpvector(ikeepos + prev)
    sElPZ2_G(ist:ien) = pz2_tmpvector(ikeepos + prev)
    s_chi_bar_G(ist:ien)        = Tmp_chibar(ikeepos + prev)
    s_Normalised_chi_G(ist:ien) = Tmp_Normchi(ikeepos + prev)

    nsum = nsum + b_keepn(b_ind)

    DEALLOCATE(ikeepos,iendpos)

!     Add linear chirp to the beam....TEMP


    call addChirp(sElPZ2_G(ist:ien), sElZ2_G(ist:ien), &
                  b_keepn(b_ind), sZ2_center(b_ind), chirp(b_ind))


    call addModulation(sElPZ2_G(ist:ien), sElZ2_G(ist:ien), &
                       b_keepn(b_ind), mag(b_ind), fr(b_ind))

    prev = prev + totalmps_b(b_ind)

  END DO

  iNumberElectrons_G = SUM(b_keepn)

END SUBROUTINE removeLow

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

end module remLow