! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Module to calculate d/dz of the field values and electron macroparticle
!> coordinates.

module rhs

use paratype
use ArrayFunctions
use Globals
use Functions
use TransformInfoType
use ParallelInfoType
use Equations
use wigglerVar
use FiElec1D
use FiElec
use gtop2
use ParaField
use bfields


implicit none

contains

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Calculate d/dz of radiation field and electron macroparticle coordinates in
!> 6D phase space
!> @param[in] sz zbar
!> @param[in] sAr Real (x) component of A_perp
!> @param[in] sAi Imaginary (-y) component of A_perp
!> @param[in] sx scaled electron x coordinates
!> @param[in] sy scaled electron y coordinates
!> @param[in] sz2 scaled electron z2 coordinates
!> @param[in] spr scaled real (px) p_perp electron coordinates
!> @param[in] spi scaled real (-py) p_perp electron coordinates
!> @param[in] sgam scaled energy (gamma) electron coordinates
!> @param[out] sdx d/dz of scaled electron x coordinates
!> @param[out] sdy d/dz of scaled electron y coordinates
!> @param[out] sdz2 d/dz of scaled electron z2 coordinates
!> @param[out] sdpr d/dz of scaled real (px) p_perp electron coordinates
!> @param[out] sdpi d/dz of scaled real (-py) p_perp electron coordinates
!> @param[out] sdgam d/dz of scaled energy (gamma) electron coordinates
!> @param[out] sDADzr d/dz of real (x) component of A_perp
!> @param[out] sDADzi d/dz of real (-y) component of A_perp
!> @param[out] qOK Error flag

  subroutine getrhs(sz, &
                    sAr, sAi, &
                    sx, sy, sz2, &
                    spr, spi, sgam, &
                    sdx, sdy, sdz2, &
                    sdpr, sdpi, sdgam, &
                    sDADzr, sDADzi, &
                    qOK)

  use rhs_vars

  implicit none

!> Inputs %%%
!
! sZ - Propagation distance
! sA - current radiation field vals
! sy - current electron coordinates in all dimensions
!
! Output
! sb  - d/dz of electron phase space positions
! sDADz - RHS of field source term

  real(kind=wp), intent(in) :: sz
  real(kind=wp), contiguous, intent(in) :: sAr(:), sAi(:)
  real(kind=wp), contiguous, intent(in)  :: sx(:), sy(:), sz2(:), &
                                            spr(:), spi(:), sgam(:)


  real(kind=wp), contiguous, intent(inout)  :: sdx(:), sdy(:), sdz2(:), &
                                   sdpr(:), sdpi(:), sdgam(:)

  real(kind=wp), contiguous,  intent(inout) :: sDADzr(:), sDADzi(:) !!!!!!!
  logical, intent(inout) :: qOK

  integer(kind=ipl) :: i, z2node
  integer :: error
  logical qOKL

!     Begin

  qOK = .false.
  qOKL = .false.

!     SETUP AND INITIALISE THE PARTICLE'S POSITION
!     ALLOCATE THE ARRAYS

!  allocate(Lj(iNumberElectrons_G))
  allocate(p_nodes(iNumberElectrons_G))
!  allocate(p_nodes2(ispt))
!  allocate(tmp1(500000))
!  allocate(tmp2(ispt))
  
  call alct_e_srtcts(iNumberElectrons_G)

  if (tTransInfo_G%qOneD) then
    allocate(lis_GR(2,iNumberElectrons_G))
  else
    allocate(lis_GR(8,iNumberElectrons_G))
  end if

!     Initialise right hand side to zero

  sField4ElecReal = 0.0_WP
  sField4ElecImag = 0.0_WP

  call rhs_tmsavers(sz)  ! This can be moved later...

!     Adjust undulator tuning

  call getAlpha(sZ)
  call adjUndPlace(sZ)



!$OMP PARALLEL

! !$OMP SIMD
!     do i = 1, iNumberElectrons_G
!       tmp1(i) = sz2(i) * 4.2_wp ! + 1_IP - (fz2-1)
!     end do
! !$OMP END SIMD

  call getP2(sp2, sgam, spr, spi, sEta_G, sGammaR_G, saw_G)




  if (tTransInfo_G%qOneD) then



    p_nodes = int(sz2 / dz2, kind=ip) + 1_IP - (fz2-1)
! !$OMP WORKSHARE
!!$OMP SIMD
!    do i = 1, iNumberElectrons_G
!      tmp1(i) = sz2(i) * 4.2_wp ! + 1_IP - (fz2-1)
!    end do
!!$OMP END SIMD
!    tmp1 = sz2 * 4.2_wp
    !!$OMP SIMD
!    do i = 1, iNumberElectrons_G
!      p_nodes(i) = int(sz2(i) / dz2, kind=ip) + 1_IP - (fz2-1)
!    end do
    !!$OMP END SIMD

! !$OMP END WORKSHARE

  else

!$OMP WORKSHARE

!    p_nodes = (floor( (sx+halfx)  / dx)  + 1_IP) + &
!              (floor( (sy+halfy)  / dy) * ReducedNX_G )  + &   !  y 'slices' before primary node
!              (ReducedNX_G * ReducedNY_G * &
!                              floor(sz2  / dz2) ) - &
!                              (fz2-1)*ntrnds_G  ! transverse slices before primary node

    p_nodes = (int( (sx+halfx)  / dx, kind=ip)  + 1_IP) + &
              (int( (sy+halfy)  / dy, kind=ip) * nspinDX )  + &   !  y 'slices' before primary node
              (nspinDX * nspinDY * &
                              int(sz2  / dz2, kind=ip) ) - &
                              (fz2-1)*ntrndsi_G  ! transverse slices before primary node

!$OMP END WORKSHARE

  end if






  if (tTransInfo_G%qOneD) then

    call getInterps_1D(sz2)
    if (qPArrOK_G) then
      call getFFelecs_1D(sAr, sAi)
      call getSource_1D(sDADzr, sDADzi,  spr, spi, sgam, sEta_G)
    end if

  else

    call getInterps_3D(sx, sy, sz2)
    if ((qPArrOK_G) .and. (qInnerXYOK_G)) then
      call getFFelecs_3D(sAr, sAi)
      call getSource_3D(sDADzr, sDADzi, spr, spi, sgam, sEta_G)
    end if

  end if



!    IF (ioutside>0) THEN
!       Print*, 'WARNING: ',ioutside,&
!            ' electrons are outside the inner driving core'
!    END IF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      Calculate electron d/dz of electron equations - if needed

  if (.not. qElectronFieldCoupling_G) then
    sField4ElecReal = 0.0_WP
    sField4ElecImag = 0.0_WP
  end if


    if (qElectronsEvolve_G) then

        call getBFields(sx, sy, sz, &
                        bxu, byu, bzu)

!     z2

        CALL dz2dz_f(sx, sy, sz2, spr, spi, sgam, &
                     sdz2, qOKL)
        !if (.not. qOKL) goto 1000

!     X

        call dxdz_f(sx, sy, sz2, spr, spi, sgam, &
                    sdx, qOKL)
        !if (.not. qOKL) goto 1000

!     Y

        call dydz_f(sx, sy, sz2, spr, spi, sgam, &
                    sdy, qOKL)
        !if (.not. qOKL) goto 1000


!     PX (Real pperp)

        call dppdz_r_f(sx, sy, sz2, spr, spi, sgam, sZ, &
                       sdpr, qOKL)
        !if (.not. qOKL) goto 1000


!     -PY (Imaginary pperp)

        call dppdz_i_f(sx, sy, sz2, spr, spi, sgam, sz, &
                       sdpi, qOKL)
        !if (.not. qOKL) goto 1000


!     P2

        call dgamdz_f(sx, sy, sz2, spr, spi, sgam, &
                     sdgam, qOKL)
        !if (.not. qOKL) goto 1000

    end if



!$OMP END PARALLEL



!    if (qFieldEvolve_G) then

!     Sum dadz from different MPI processes together

!        call sum2RootArr(sDADz,ReducedNX_G*ReducedNY_G*NZ2_G*2,0)

!     Boundary condition dadz = 0 at head of field

!        if (tProcInfo_G%qRoot) sDADz(1:ReducedNX_G*ReducedNY_G) = 0.0_WP
!        if (tProcInfo_G%qRoot) sDADz(ReducedNX_G*ReducedNY_G*NZ2_G + 1: &
!                                     ReducedNX_G*ReducedNY_G*NZ2_G + &
!                                     ReducedNX_G*ReducedNY_G) = 0.0_WP

        !if (tTransInfo_G%qOneD) then
        !  if (tProcInfo_G%qRoot) sDADz=sDADz !sDADz=6.0_WP*sDADz
        !else
        !   if (tProcInfo_G%qRoot) sDADz=sDADz !216.0_WP/8.0_WP*sDADz
        !end if

!    end if

!     Switch field off

    if (.not. qFieldEvolve_G) then
       sDADzr = 0.0_WP
       sDADzi = 0.0_WP
    end if

!     if electrons not allowed to evolve then

    if (.not. qElectronsEvolve_G) then
       sdpr = 0.0_wp
       sdpi = 0.0_wp
       sdgam = 0.0_wp
       sdx   = 0.0_wp
       sdy   = 0.0_wp
       sdz2 = 0.0_wp
    end if

!     Deallocate arrays

!    deallocate(i_n4e,N,iNodeList_Re,iNodeList_Im,i_n4ered)
!    deallocate(sField4ElecReal,sField4ElecImag,Lj,dp2f)
    !deallocate(Lj)
    deallocate(lis_GR)
    deallocate(p_nodes)
    call dalct_e_srtcts()


    ! Set the error flag and exit

    qOK = .true.

    goto 2000

1000 call Error_log('Error in rhs:getrhs',tErrorLog_G)
    print*,'Error in rhs:getrhs'
2000 continue

  end subroutine getrhs



!        #########################################


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Initialize data used in the calculation of d/dz of electron beam + radiation
!> field quantities
!> @param[in] sz zbar

subroutine rhs_tmsavers(sz)

use rhs_vars

real(kind=wp), intent(in) :: sz

  ioutside=0


!     Define the size of each element

  dx = sLengthOfElmX_G
  dy = sLengthOfElmY_G
  dz2 = sLengthOfElmZ2_G

  dV3 = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G


!     Time savers

  sInv2rho    = 1.0_WP/(2.0_WP * sRho_G)

  ZOver2rho   = sz * sInv2rho
  salphaSq    = (2.0_WP * sGammaR_G * sRho_G / sAw_G)**2

  un = sqrt(fx_G**2.0_WP + fy_G**2.0_WP)


!     number of transverse nodes

  ntrans = NX_G * NY_G

!     Diff between real and imaginary nodes in the reduced system

  retim = nspinDX*nspinDY*nZ2_G

  econst = sAw_G/(sRho_G*sqrt(2.0_WP*(fx_G**2.0_WP+fy_G**2.0_WP)))

  nc = 2.0_WP*saw_G**2/(fx_G**2.0_WP + fy_G**2.0_WP)

  nb = 2.0_WP * sRho_G / ((fx_G**2.0_WP+fy_G**2.0_WP)*sEta_G)

  maxEl = maxval(procelectrons_G)
  qoutside=.FALSE.
  iOutside=0_IP

!  halfx = ((ReducedNX_G-1) / 2.0_WP) * sLengthOfElmX_G
!  halfy = ((ReducedNY_G-1) / 2.0_WP) * sLengthOfElmY_G

  halfx = ((nspinDX-1) / 2.0_WP) * sLengthOfElmX_G
  halfy = ((nspinDY-1) / 2.0_WP) * sLengthOfElmY_G


end subroutine rhs_tmsavers

end module rhs
