!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE rhs

! Module to calculate the RHS of the field source equation
! and d/dz of electron equations.
!

USE paratype
USE ArrayFunctions
USE Globals
USE Functions
USE extra
USE basis_fn
USE TransformInfoType
USE ParallelInfoType
USE stiffness
USE Equations
use wigglerVar


IMPLICIT NONE

CONTAINS

  SUBROUTINE getrhs(sz, &
                    sA, &
                    sx, sy, sz2, &
                    spr, spi, sp2, &
                    sdx, sdy, sdz2, &
                    sdpr, sdpi, sdp2, &                    
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
                                spr(:), spi(:), sp2(:)

  
  real(kind=wp), intent(inout)  :: sdx(:), sdy(:), sdz2(:), &
                                   sdpr(:), sdpi(:), sdp2(:)

  real(kind=wp), intent(inout) :: sDADz(:) !!!!!!!
  logical, intent(inout) :: qOK



! i
! dx,dy,dz2 - step size in x y z2
! xx,yy,zz2 - arrays of the principle node for each electron
! sa1_elxnew,sa1_elynew,sa1_elz2new - ARRAYS STORING THE
! PARTICLE'S POSITION X,Y,Z2
! s_Lex,s_Ley,s_Lez2 - co-ordinates of electrons locally
! N - storing the interploation function
! i_n4e - temporary storing the global nodes number 
! iNodeList_Re
! iNodeList_Im
! sInv2rho - 1/2rho
! sInv4rho - 1/4rho
! sTheta - z_2/2rho
! ZOver2rho - z/2rho
! salphaSq - alpha^2
! iAstartR - A_real index
! iAstartI - A_imag index	
! spPerpSq_i
! sBetaz_i	
! sField4ElecReal
! sField4ElecImag
! sBetaz_i         - Beta z for each electron
! sPPerp_Re        - Real PPerp value for ithelectron
! sPPerp_Im        - Imaginary PPerp value for ithelectron
! sQ_Re            - Real Q value for ithelectron
! qOKL             - Local error flag

!  INTEGER(KIND=IP) :: icheck
!  REAL(KIND=WP) :: dx,dy,dz2
!  REAL(KIND=WP) :: dV3
!  INTEGER(KIND=IP) :: xx,yy,xred,yred,zz2
!  REAL(KIND=WP) :: s_Lex,s_Ley,s_Lez2
!  INTEGER(KIND=IP),DIMENSION(:),ALLOCATABLE ::&
!              i_n4e,iNodeList_Re,iNodeList_Im,&
!              i_n4ered
!  REAL(KIND=WP),DIMENSION(:),ALLOCATABLE :: N
!  REAL(KIND=WP) :: sInv2rho,sInv4rho
!  REAL(KIND=WP) :: ZOver2rho,salphaSq
!!  REAL(KIND=WP),DIMENSION(:),ALLOCATABLE ::&
!!       sField4ElecReal,sField4ElecImag
!  INTEGER(KIND=IP) :: iAstartR,&
!       iAstartI,NN
!  REAL(KIND=WP) :: spPerpSq			   
!  REAL(KIND=WP),ALLOCATABLE :: Lj(:)! , dp2f(:)
!  REAL(KIND=WP) :: sBetaz_i,sInvGamma_i
!
!  REAL(KIND=WP) :: z2test 
!  REAL(KIND=WP) :: FieldConst,econst
!  REAL(KIND=WP) :: stheta, kbeta, un, nc, nd, nb, fkb
!  REAL(KIND=WP),DIMENSION(6) :: sendbuff, recvbuff 
!  INTEGER(KIND=IP) :: x_inc, y_inc, z2_inc, istart, iend
!  INTEGER(KIND=IP) :: iNodesX,iNodesZ2,iNodesY, j, ntrans
!  INTEGER(KIND=IPL) :: maxEl,i
!  INTEGER(KIND=IP) :: local_z2_start, local_nz2, index, ti
!  INTEGER(KIND=IP) :: iOutside
!  INTEGER :: stat,req,error,lrank,rrank
!  REAL(KIND=WP),DIMENSION(10)	:: couple 
!  INTEGER(KIND=IP) :: retim, xnode, ynode, z2node 
!  integer(kind=ip) :: x_in1, x_in2, y_in1, y_in2, z2_in1, z2_in2
!  integer(kind=ip), allocatable :: p_nodes(:)
!  REAL(KIND=WP) :: halfx, halfy, dadzRInst, dadzIInst
!  real(kind=wp) :: li1, li2, li3, li4, li5, li6, li7, li8, locx, locy, locz2
!
!  REAL(KIND=WP) :: time1, start_time
!  LOGICAL :: qOKL,qoutside
  logical qOKL

!     Begin

  qOK = .false.
  qOKL = .false.
    
!     SETUP AND INITIALISE THE PARTICLE'S POSITION
!     ALLOCATE THE ARRAYS

  ALLOCATE(Lj(iNumberElectrons_G))!,dp2f(iNumberElectrons_G))
  allocate(p_nodes(iNumberElectrons_G))
  call alct_e_srtcts(iNumberElectrons_G)
  allocate(lis_GR(8,iNumberElectrons_G))



!     Initialise right hand side to zero

  sField4ElecReal = 0.0_WP
  sField4ElecImag = 0.0_WP


  call rhs_tmsavers(sz)  ! This can be moved later...

!     Adjust undulator tuning

  call getAlpha(sZ)


!     Calculate Lj term

! !$OMP PARALLEL WORKSHARE

  Lj = sqrt((1.0_WP - (1.0_WP / ( 1.0_WP + (sEta_G * sp2)) )**2.0_WP) &
             / (1.0_WP + nc* ( spr**2.0_wp  +  &
                               spi**2.0_wp )   )) &
          * (1.0_WP + sEta_G *  sp2) * sGammaR_G

! !$OMP END PARALLEL WORKSHARE



  
  if (tTransInfo_G%qOneD) then

! !DIR$ SIMD
 
    p_nodes = floor(sz2 / dz2) + 1_IP

! !DIR$ END SIMD


  else

! !$OMP PARALLEL WORKSHARE

    p_nodes = (floor( (sx+halfx)  / dx)  + 1_IP) + &
              (floor( (sy+halfy)  / dy) * ReducedNX_G )  + &   !  y 'slices' before primary node
              (ReducedNX_G * ReducedNY_G * &
                              floor(sz2  / dz2) )  ! transverse slices before primary node
! !$OMP END PARALLEL WORKSHARE

  end if  



  if (tTransInfo_G%qOneD) then

!    do i = 1, maxEl
!      IF (i<=procelectrons_G(1)) THEN 
!    
!        !   Get surrounding nodes 
!    
!        !   Interpolate
!    
!        !   
!    
!        dadzRInst = ((s_chi_bar_G(i)/dV3) * Lj(i) &
!                          * spr(i) )
!    
!        dadzIInst = ((s_chi_bar_G(i)/dV3) * Lj(i) &
!                          * spi(i) )
!
!
!    
!        z2node = floor(sz2(i)  / dz2)  + 1_IP
!        locz2 = sz2(i) - REAL(z2node  - 1_IP, kind=wp) * sLengthOfElmZ2_G
!    
!        li1 = (1.0_wp - locz2/sLengthOfElmZ2_G)
!        li2 = 1 - li1
!
!        sField4ElecReal(i) = li1 * sA(p_nodes(i)) + sField4ElecReal(i)
!        sField4ElecReal(i) = li2 * sA(p_nodes(i) + 1_ip) + sField4ElecReal(i)
!    
!        sField4ElecImag(i) = li1 * sA(p_nodes(i) + retim) + sField4ElecImag(i)
!        sField4ElecImag(i) = li2 * sA(p_nodes(i) + retim + 1_ip) + sField4ElecImag(i)
!    
!        sDADz(p_nodes(i)) =         li1 * dadzRInst + sDADz(p_nodes(i))
!        sDADz(p_nodes(i) + 1_ip) =  li2 * dadzRInst + sDADz(p_nodes(i) + 1_ip)                
!    
!        sDADz(p_nodes(i) + retim) =         li1 * dadzIInst + sDADz(p_nodes(i) + retim)                        
!        sDADz(p_nodes(i) + 1_ip + retim) =  li2 * dadzIInst + sDADz(p_nodes(i) + 1_ip + retim)           
!    
!      end if
!    end do


  else

    call getFFelecs(sA)
    call getInterps(sx, sy, sz2)
    call getSource(sDADz, spr, spi)

    !do i = 1, maxEl
    !  IF (i<=procelectrons_G(1)) THEN 


!                  Get surrounding nodes 

!        xnode = floor( (sx(i) + halfx ) / dx)  + 1_IP
!        locx = sx(i) + halfx - REAL(xnode  - 1_IP, kind=wp) * sLengthOfElmX_G
!        ynode = floor( (sy(i) + halfy )  / dy)  + 1_IP
!        locy = sy(i) + halfy - REAL(ynode  - 1_IP, kind=wp) * sLengthOfElmY_G
!        z2node = floor(sz2(i)  / dz2)  + 1_IP
!        locz2 = sz2(i) - REAL(z2node  - 1_IP, kind=wp) * sLengthOfElmZ2_G
!    
!
!!                  Get weights for interpolant
!    
!        x_in1  = (1.0_wp - locx/sLengthOfElmX_G)
!        x_in2  = 1 - x_in1
!        y_in1  = (1.0_wp - locy/sLengthOfElmY_G)
!        y_in2  = 1 - y_in1
!        z2_in1 = (1.0_wp - locz2/sLengthOfElmZ2_G)
!        z2_in2 = 1 - z2_in1
!
!        li1 = x_in1 * y_in1 * z2_in1
!        li2 = x_in2 * y_in1 * z2_in1
!        li3 = x_in1 * y_in2 * z2_in1
!        li4 = x_in2 * y_in2 * z2_in1
!        li5 = x_in1 * y_in1 * z2_in2
!        li6 = x_in2 * y_in1 * z2_in2
!        li7 = x_in1 * y_in2 * z2_in2
!        li8 = x_in2 * y_in2 * z2_in2
    

!    !               Interpolate field to electron spatial position
!    
!        sField4ElecReal(i) = lis_GR(1,i) * sA(p_nodes(i)) + sField4ElecReal(i)
!        sField4ElecReal(i) = lis_GR(2,i) * sA(p_nodes(i) + 1_ip) + sField4ElecReal(i)
!        sField4ElecReal(i) = lis_GR(3,i) * sA(p_nodes(i) + ReducedNX_G) + sField4ElecReal(i)
!        sField4ElecReal(i) = lis_GR(4,i) * sA(p_nodes(i) + ReducedNX_G + 1_ip) + sField4ElecReal(i)
!        sField4ElecReal(i) = lis_GR(5,i) * sA(p_nodes(i) + ntrans) + sField4ElecReal(i)
!        sField4ElecReal(i) = lis_GR(6,i) * sA(p_nodes(i) + ntrans + 1_ip) + sField4ElecReal(i)
!        sField4ElecReal(i) = lis_GR(7,i) * sA(p_nodes(i) + ntrans + ReducedNX_G) + sField4ElecReal(i)
!        sField4ElecReal(i) = lis_GR(8,i) * sA(p_nodes(i) + ntrans + ReducedNX_G + 1) + sField4ElecReal(i)
!    
!        sField4ElecImag(i) = lis_GR(1,i) * sA(p_nodes(i) + retim) + sField4ElecImag(i)
!        sField4ElecImag(i) = lis_GR(2,i) * sA(p_nodes(i) + retim + 1_ip) + sField4ElecImag(i)
!        sField4ElecImag(i) = lis_GR(3,i) * sA(p_nodes(i) + retim + ReducedNX_G) + sField4ElecImag(i)
!        sField4ElecImag(i) = lis_GR(4,i) * sA(p_nodes(i) + retim + ReducedNX_G + 1_ip) + sField4ElecImag(i)
!        sField4ElecImag(i) = lis_GR(5,i) * sA(p_nodes(i) + retim + ntrans) + sField4ElecImag(i)
!        sField4ElecImag(i) = lis_GR(6,i) * sA(p_nodes(i) + retim + ntrans + 1_ip) + sField4ElecImag(i)
!        sField4ElecImag(i) = lis_GR(7,i) * sA(p_nodes(i) + retim + ntrans + ReducedNX_G) + sField4ElecImag(i)
!        sField4ElecImag(i) = lis_GR(8,i) * sA(p_nodes(i) + retim + ntrans + ReducedNX_G + 1) + sField4ElecImag(i)


!                  Get 'instantaneous' dAdz

!        dadzRInst = ((s_chi_bar_G(i)/dV3) * Lj(i) &
!                          * spr(i) )
!    
!        sDADz(p_nodes(i)) =                            lis_GR(1,i) * dadzRInst + sDADz(p_nodes(i))
!        sDADz(p_nodes(i) + 1_ip) =                     lis_GR(2,i) * dadzRInst + sDADz(p_nodes(i) + 1_ip)                
!        sDADz(p_nodes(i) + ReducedNX_G) =              lis_GR(3,i) * dadzRInst + sDADz(p_nodes(i) + ReducedNX_G)          
!        sDADz(p_nodes(i) + ReducedNX_G + 1_ip) =       lis_GR(4,i) * dadzRInst + sDADz(p_nodes(i) + ReducedNX_G + 1_ip)   
!        sDADz(p_nodes(i) + ntrans) =                   lis_GR(5,i) * dadzRInst + sDADz(p_nodes(i) + ntrans)               
!        sDADz(p_nodes(i) + ntrans + 1_ip) =            lis_GR(6,i) * dadzRInst + sDADz(p_nodes(i) + ntrans + 1_ip)         
!        sDADz(p_nodes(i) + ntrans + ReducedNX_G) =     lis_GR(7,i) * dadzRInst + sDADz(p_nodes(i) + ntrans + ReducedNX_G)   
!        sDADz(p_nodes(i) + ntrans + ReducedNX_G + 1) = lis_GR(8,i) * dadzRInst + sDADz(p_nodes(i) + ntrans + ReducedNX_G + 1)
!
!        dadzIInst = ((s_chi_bar_G(i)/dV3) * Lj(i) &
!                          * spi(i) ) 
!    
!        sDADz(p_nodes(i) + retim) =                             lis_GR(1,i) * dadzIInst + sDADz(p_nodes(i) + retim)                        
!        sDADz(p_nodes(i) + 1_ip + retim) =                      lis_GR(2,i) * dadzIInst + sDADz(p_nodes(i) + 1_ip + retim)           
!        sDADz(p_nodes(i) + ReducedNX_G + retim) =               lis_GR(3,i) * dadzIInst + sDADz(p_nodes(i) + ReducedNX_G + retim)           
!        sDADz(p_nodes(i) + ReducedNX_G + 1_ip + retim) =        lis_GR(4,i) * dadzIInst + sDADz(p_nodes(i) + ReducedNX_G + 1_ip + retim)    
!        sDADz(p_nodes(i) + ntrans + retim) =                    lis_GR(5,i) * dadzIInst + sDADz(p_nodes(i) + ntrans + retim)               
!        sDADz(p_nodes(i) + ntrans + 1_ip + retim) =             lis_GR(6,i) * dadzIInst + sDADz(p_nodes(i) + ntrans + 1_ip + retim)       
!        sDADz(p_nodes(i) + ntrans + ReducedNX_G + retim) =      lis_GR(7,i) * dadzIInst + sDADz(p_nodes(i) + ntrans + ReducedNX_G + retim)  
!        sDADz(p_nodes(i) + ntrans + ReducedNX_G + 1 + retim) =  lis_GR(8,i) * dadzIInst + sDADz(p_nodes(i) + ntrans + ReducedNX_G + 1 & 
!                                                                       + retim)

  
    !  end if
    !end do

! !$OMP END PARALLEL DO

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

        CALL dz2dz_f(sx, sy, sz2, spr, spi, sp2, &
                   sdx, sdy, sdz2, sdpr, sdpi, sdp2, &
                   Lj,qOKL)
        if (.not. qOKL) goto 1000

!     X


        call dxdz_f(sx, sy, sz2, spr, spi, sp2, &
                  sdx, sdy, sdz2, sdpr, sdpi, sdp2, &
                  Lj,qOKL)
        if (.not. qOKL) goto 1000             

!     Y

        call dydz_f(sx, sy, sz2, spr, spi, sp2, &
                  sdx, sdy, sdz2, sdpr, sdpi, sdp2, &
                  Lj,qOKL)
        if (.not. qOKL) goto 1000

!     dp2f is the focusing correction for dp2/dz

        call caldp2f_f(sx, sy, sdx, sdy, sp2, kbeta, qOKL)
        if (.not. qOKL) goto 1000

!     PX (Real pperp)
       
        call dppdz_r_f(sx, sy, sz2, spr, spi, sp2, &
                     sdx, sdy, sdz2, sdpr, sdpi, sdp2, &
                     Lj,qOKL)
        if (.not. qOKL) goto 1000

!     -PY (Imaginary pperp)

        call dppdz_i_f(sx, sy, sz2, spr, spi, sp2, &
                     sdx, sdy, sdz2, sdpr, sdpi, sdp2, &
                     Lj,qOKL)
        if (.not. qOKL) goto 1000

!     P2

        call dp2dz_f(sx, sy, sz2, spr, spi, sp2, &
                   sdx, sdy, sdz2, sdpr, sdpi, sdp2, &
                   Lj,qOKL)
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
       sdp2 = 0.0_wp
       sdx   = 0.0_wp
       sdy   = 0.0_wp
       sdz2 = 0.0_wp
    end if

!     Deallocate arrays

!    deallocate(i_n4e,N,iNodeList_Re,iNodeList_Im,i_n4ered)
!    deallocate(sField4ElecReal,sField4ElecImag,Lj,dp2f)
    deallocate(Lj)
    deallocate(lis_GR)
    call dalct_e_srtcts()


    ! Set the error flag and exit

    qOK = .true.

    goto 2000 

1000 call Error_log('Error in rhs:getrhs',tErrorLog_G)
    print*,'Error in rhs:getrhs'
2000 continue

  END SUBROUTINE getrhs



!      ##################################################


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

  econst = sAw_G/(sRho_G*SQRT(2.0_WP*(fx_G**2.0_WP+fy_G**2.0_WP)))

  nc = 2.0_WP*saw_G**2/(fx_G**2.0_WP + fy_G**2.0_WP)
    
  nd = SQRT((fx_G**2.0_WP+fy_G**2.0_WP)*(sEta_G))/(2.0_WP*SQRT(2.0_WP)* &
                             fkb*sRho_G)
    
  nb = 2.0_WP * sRho_G / ((fx_G**2.0_WP+fy_G**2.0_WP)*sEta_G)
    
  maxEl = maxval(procelectrons_G)
  qoutside=.FALSE.
  iOutside=0_IP

  halfx = ((ReducedNX_G-1) / 2.0_WP) * sLengthOfElmX_G
  halfy = ((ReducedNY_G-1) / 2.0_WP) * sLengthOfElmY_G



end subroutine rhs_tmsavers



!      ##################################################



subroutine getInterps(sx, sy, sz2)

use rhs_vars

real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:)
integer(kind=ip) :: xnode, ynode, z2node

real(kind=wp) :: locx, locy, locz2, &
                 x_in1, x_in2, y_in1, y_in2, z2_in1, z2_in2


!$OMP PARALLEL DO PRIVATE(xnode, ynode, z2node, locx, locy, locz2, &
!$OMP x_in1, x_in2, y_in1, y_in2, z2_in1, z2_in2)
  do i = 1, maxEl
    if (i<=procelectrons_G(1)) then 


!                  Get surrounding nodes 

      xnode = floor( (sx(i) + halfx ) / dx)  + 1_IP
      locx = sx(i) + halfx - real(xnode  - 1_IP, kind=wp) * dx
      x_in2 = locx / dx
      x_in1 = (1.0_wp - x_in2)

      ynode = floor( (sy(i) + halfy )  / dy)  + 1_IP
      locy = sy(i) + halfy - real(ynode  - 1_IP, kind=wp) * dy
      y_in2 = locy / dy
      y_in1 = (1.0_wp - y_in2)

      z2node = floor(sz2(i)  / dz2)  + 1_IP
      locz2 = sz2(i) - real(z2node  - 1_IP, kind=wp) * dz2
      z2_in2 = locz2 / dz2
      z2_in1 = (1.0_wp - z2_in2)


!                  Get weights for interpolant

      lis_GR(1,i) = x_in1 * y_in1 * z2_in1
      lis_GR(2,i) = x_in2 * y_in1 * z2_in1
      lis_GR(3,i) = x_in1 * y_in2 * z2_in1
      lis_GR(4,i) = x_in2 * y_in2 * z2_in1
      lis_GR(5,i) = x_in1 * y_in1 * z2_in2
      lis_GR(6,i) = x_in2 * y_in1 * z2_in2
      lis_GR(7,i) = x_in1 * y_in2 * z2_in2
      lis_GR(8,i) = x_in2 * y_in2 * z2_in2

    end if
  end do
!$OMP END PARALLEL DO


end subroutine getInterps






!      ##################################################




subroutine getFFelecs(sA)


use rhs_vars

real(kind=wp), intent(in) :: sA(:)
integer(kind=ip) :: i

!$OMP PARALLEL DO
  do i = 1, maxEl
  
    if (i<=procelectrons_G(1)) then

      sField4ElecReal(i) = lis_GR(1,i) * sA(p_nodes(i)) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(2,i) * sA(p_nodes(i) + 1_ip) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(3,i) * sA(p_nodes(i) + ReducedNX_G) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(4,i) * sA(p_nodes(i) + ReducedNX_G + 1_ip) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(5,i) * sA(p_nodes(i) + ntrans) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(6,i) * sA(p_nodes(i) + ntrans + 1_ip) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(7,i) * sA(p_nodes(i) + ntrans + ReducedNX_G) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(8,i) * sA(p_nodes(i) + ntrans + ReducedNX_G + 1) + sField4ElecReal(i)
  
      sField4ElecImag(i) = lis_GR(1,i) * sA(p_nodes(i) + retim) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(2,i) * sA(p_nodes(i) + retim + 1_ip) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(3,i) * sA(p_nodes(i) + retim + ReducedNX_G) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(4,i) * sA(p_nodes(i) + retim + ReducedNX_G + 1_ip) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(5,i) * sA(p_nodes(i) + retim + ntrans) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(6,i) * sA(p_nodes(i) + retim + ntrans + 1_ip) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(7,i) * sA(p_nodes(i) + retim + ntrans + ReducedNX_G) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(8,i) * sA(p_nodes(i) + retim + ntrans + ReducedNX_G + 1) + sField4ElecImag(i)

    end if
  
  end do 
!$OMP END PARALLEL DO

end subroutine getFFelecs



!         ########################################################



subroutine getSource(sDADz, spr, spi)


use rhs_vars

real(kind=wp), intent(inout) :: sDADz(:)
real(kind=wp), intent(in) :: spr(:), spi(:)

integer(kind=ip) :: i
real(kind=wp) :: dadzRInst, dadzIInst


!$OMP PARALLEL DO PRIVATE(dadzRInst, dadzIInst)
  do i = 1, maxEl
  
    if (i<=procelectrons_G(1)) then


!                  Get 'instantaneous' dAdz

      dadzRInst = ((s_chi_bar_G(i)/dV3) * Lj(i) &
                        * spr(i) )

      !$OMP ATOMIC
      sDADz(p_nodes(i)) =                            lis_GR(1,i) * dadzRInst + sDADz(p_nodes(i))
      !$OMP ATOMIC
      sDADz(p_nodes(i) + 1_ip) =                     lis_GR(2,i) * dadzRInst + sDADz(p_nodes(i) + 1_ip)
      !$OMP ATOMIC              
      sDADz(p_nodes(i) + ReducedNX_G) =              lis_GR(3,i) * dadzRInst + sDADz(p_nodes(i) + ReducedNX_G)
      !$OMP ATOMIC    
      sDADz(p_nodes(i) + ReducedNX_G + 1_ip) =       lis_GR(4,i) * dadzRInst + sDADz(p_nodes(i) + ReducedNX_G + 1_ip)
      !$OMP ATOMIC
      sDADz(p_nodes(i) + ntrans) =                   lis_GR(5,i) * dadzRInst + sDADz(p_nodes(i) + ntrans)
      !$OMP ATOMIC  
      sDADz(p_nodes(i) + ntrans + 1_ip) =            lis_GR(6,i) * dadzRInst + sDADz(p_nodes(i) + ntrans + 1_ip)
      !$OMP ATOMIC
      sDADz(p_nodes(i) + ntrans + ReducedNX_G) =     lis_GR(7,i) * dadzRInst + sDADz(p_nodes(i) + ntrans + ReducedNX_G)
      !$OMP ATOMIC
      sDADz(p_nodes(i) + ntrans + ReducedNX_G + 1) = lis_GR(8,i) * dadzRInst + sDADz(p_nodes(i) + ntrans + ReducedNX_G + 1)

      dadzIInst = ((s_chi_bar_G(i)/dV3) * Lj(i) &
                        * spi(i) ) 
      
      !$OMP ATOMIC
      sDADz(p_nodes(i) + retim) =                             lis_GR(1,i) * dadzIInst + sDADz(p_nodes(i) + retim)
      !$OMP ATOMIC
      sDADz(p_nodes(i) + 1_ip + retim) =                      lis_GR(2,i) * dadzIInst + sDADz(p_nodes(i) + 1_ip + retim)
      !$OMP ATOMIC
      sDADz(p_nodes(i) + ReducedNX_G + retim) =               lis_GR(3,i) * dadzIInst + sDADz(p_nodes(i) + ReducedNX_G + retim)
      !$OMP ATOMIC
      sDADz(p_nodes(i) + ReducedNX_G + 1_ip + retim) =        lis_GR(4,i) * dadzIInst + sDADz(p_nodes(i) + ReducedNX_G + 1_ip + retim)
      !$OMP ATOMIC
      sDADz(p_nodes(i) + ntrans + retim) =                    lis_GR(5,i) * dadzIInst + sDADz(p_nodes(i) + ntrans + retim)
      !$OMP ATOMIC
      sDADz(p_nodes(i) + ntrans + 1_ip + retim) =             lis_GR(6,i) * dadzIInst + sDADz(p_nodes(i) + ntrans + 1_ip + retim)
      !$OMP ATOMIC
      sDADz(p_nodes(i) + ntrans + ReducedNX_G + retim) =      lis_GR(7,i) * dadzIInst + sDADz(p_nodes(i) + ntrans + ReducedNX_G + retim)
      !$OMP ATOMIC
      sDADz(p_nodes(i) + ntrans + ReducedNX_G + 1 + retim) =  lis_GR(8,i) * dadzIInst + sDADz(p_nodes(i) + ntrans + ReducedNX_G + 1 &
                                                                       + retim)
    end if
  
  end do 
!$OMP END PARALLEL DO

end subroutine getSource


end module rhs
