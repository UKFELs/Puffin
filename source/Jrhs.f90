!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module rhs

! Module to calculate the RHS of the field source equation
! and d/dz of electron equations.
!

use paratype
use ArrayFunctions
use Globals
use Functions
use TransformInfoType
use ParallelInfoType
use stiffness
use Equations
use wigglerVar
use FiElec1D
use FiElec
use gtop2


implicit none

contains

  subroutine getrhs(sz, &
                    sA, &
                    sx, sy, sz2, &
                    spr, spi, sgam, &
                    sdx, sdy, sdz2, &
                    sdpr, sdpi, sdgam, &                    
                    sDADz, &
                    qOK)

  use rhs_vars

  implicit none

! Inputs %%%
!
! sZ - Propagation distance
! sA - current radiation field vals
! sy - current electron coordinates in all dimensions
! 
! Output
! sb  - d/dz of electron phase space positions
! sDADz - RHS of field source term

  real(kind=wp), intent(in) :: sz
  real(kind=wp), intent(in) :: sA(:)
  real(kind=wp), intent(in)  :: sx(:), sy(:), sz2(:), &
                                spr(:), spi(:), sgam(:)

  
  real(kind=wp), intent(inout)  :: sdx(:), sdy(:), sdz2(:), &
                                   sdpr(:), sdpi(:), sdgam(:)

  real(kind=wp), intent(inout) :: sDADz(:) !!!!!!!
  logical, intent(inout) :: qOK

  integer(kind=ipl) :: i, z2node
  logical qOKL

!     Begin

  qOK = .false.
  qOKL = .false.
    
!     SETUP AND INITIALISE THE PARTICLE'S POSITION
!     ALLOCATE THE ARRAYS

!  allocate(Lj(iNumberElectrons_G)) 
  allocate(p_nodes(iNumberElectrons_G))
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


!     Calculate Lj term

!  Lj = sqrt((1.0_WP - (1.0_WP / ( 1.0_WP + (sEta_G * sp2)) )**2.0_WP) &
!             / (1.0_WP + nc* ( spr**2.0_wp  +  &
!                               spi**2.0_wp )   )) &
!          * (1.0_WP + sEta_G *  sp2) * sGammaR_G


  call getP2(sp2, sgam, spr, spi, sEta_G, sGammaR_G, saw_G)




  
  if (tTransInfo_G%qOneD) then
 
    p_nodes = floor(sz2 / dz2) + 1_IP

  else

    p_nodes = (floor( (sx+halfx)  / dx)  + 1_IP) + &
              (floor( (sy+halfy)  / dy) * ReducedNX_G )  + &   !  y 'slices' before primary node
              (ReducedNX_G * ReducedNY_G * &
                              floor(sz2  / dz2) )  ! transverse slices before primary node


  end if  



  if (tTransInfo_G%qOneD) then

    call getInterps_1D(sz2)
    call getFFelecs_1D(sA)
    call getSource_1D(sDADz, spr, spi, sgam, sEta_G)

  else

    call getInterps_3D(sx, sy, sz2)
    call getFFelecs_3D(sA)    
    call getSource_3D(sDADz, spr, spi, sgam, sEta_G)

  end if

  deallocate(p_nodes)



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

!     z2

        CALL dz2dz_f(sx, sy, sz2, spr, spi, sgam, &
                     sdz2, qOKL)
        if (.not. qOKL) goto 1000

!     X

        call dxdz_f(sx, sy, sz2, spr, spi, sgam, &
                    sdx, qOKL)
        if (.not. qOKL) goto 1000             

!     Y

        call dydz_f(sx, sy, sz2, spr, spi, sgam, &
                    sdy, qOKL)
        if (.not. qOKL) goto 1000


!     PX (Real pperp)
       
        call dppdz_r_f(sx, sy, sz2, spr, spi, sgam, &
                       sdpr, qOKL)
        if (.not. qOKL) goto 1000


!     -PY (Imaginary pperp)

        call dppdz_i_f(sx, sy, sz2, spr, spi, sgam, &
                       sdpi, qOKL)
        if (.not. qOKL) goto 1000

!     P2

        call dgamdz_f(sx, sy, sz2, spr, spi, sgam, &
                     sdgam, qOKL)
        if (.not. qOKL) goto 1000
 
    end if 






    if (qFieldEvolve_G) then

!     Sum dadz from different MPI processes together

        call sum2RootArr(sDADz,ReducedNX_G*ReducedNY_G*NZ2_G*2,0)

!     Boundary condition dadz = 0 at head of field

        if (tProcInfo_G%qRoot) sDADz(1:ReducedNX_G*ReducedNY_G) = 0.0_WP
 
        !if (tTransInfo_G%qOneD) then
        !  if (tProcInfo_G%qRoot) sDADz=sDADz !sDADz=6.0_WP*sDADz
        !else
        !   if (tProcInfo_G%qRoot) sDADz=sDADz !216.0_WP/8.0_WP*sDADz
        !end if

    end if
    
!     Switch field off

    if (.not. qFieldEvolve_G) then
       sDADz = 0.0_WP
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
    call dalct_e_srtcts()

    ! Set the error flag and exit

    qOK = .true.

    goto 2000 

1000 call Error_log('Error in rhs:getrhs',tErrorLog_G)
    print*,'Error in rhs:getrhs'
2000 continue

  end subroutine getrhs



!        #########################################



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

  kbeta = sAw_G / (2.0_WP * sFocusFactor_G * sRho_G * sGammaR_G)
  un = sqrt(fx_G**2.0_WP + fy_G**2.0_WP)


!     number of transverse nodes

  ntrans = ReducedNX_G * ReducedNY_G

!     Diff between real and imaginary nodes in the reduced system

  retim = ReducedNX_G*ReducedNY_G*nZ2_G


  fkb= sFocusfactor_G * kbeta

  econst = sAw_G/(sRho_G*sqrt(2.0_WP*(fx_G**2.0_WP+fy_G**2.0_WP)))

  nc = 2.0_WP*saw_G**2/(fx_G**2.0_WP + fy_G**2.0_WP)
    
  nd = sqrt((fx_G**2.0_WP+fy_G**2.0_WP)*(sEta_G))/(2.0_WP*sqrt(2.0_WP)* &
                             fkb*sRho_G)
    
  nb = 2.0_WP * sRho_G / ((fx_G**2.0_WP+fy_G**2.0_WP)*sEta_G)
    
  maxEl = maxval(procelectrons_G)
  qoutside=.FALSE.
  iOutside=0_IP

  halfx = ((ReducedNX_G-1) / 2.0_WP) * sLengthOfElmX_G
  halfy = ((ReducedNY_G-1) / 2.0_WP) * sLengthOfElmY_G



end subroutine rhs_tmsavers





end module rhs
