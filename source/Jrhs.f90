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

  SUBROUTINE getrhs(sz,&
                    sA,&
                    sx, sy, sz2, &
                    spx, spy, sz2, &
                    sdx, sdy, sdz2, &
                    sdpx, sdpy, sdz2, &
                    sb,&
                    sDADz,&
                    qOK)

  IMPLICIT NONE

! Inputs %%%
!
! sZ - Propagation distance
! sA - current radiation field vals
! sy - current electron coordinates in all dimensions
! 
! Output
! sb  - d/dz of electron phase space positions
! sDADz - RHS of field source term

  REAL(KIND=WP),INTENT(IN) :: sz
  REAL(KIND=WP),INTENT(IN) :: sA(:)
  real(kind=wp), intent(in)  :: sx(:), sdx(:)
  real(kind=wp), intent(in)  :: sy(:), sdy(:)
  real(kind=wp), intent(in)  :: sz2(:), sdz2(:)
  real(kind=wp), intent(in)  :: spr(:), sdpr(:)
  real(kind=wp), intent(in)  :: spi(:), sdpi(:)
  real(kind=wp), intent(in)  :: sp2(:), sdp2(:)
  REAL(KIND=WP), INTENT(INOUT) :: sDADz(:) !!!!!!!
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

  INTEGER(KIND=IP) :: icheck
  REAL(KIND=WP) :: dx,dy,dz2
  REAL(KIND=WP) :: dV3
  INTEGER(KIND=IP) :: xx,yy,xred,yred,zz2
  REAL(KIND=WP) :: s_Lex,s_Ley,s_Lez2
  INTEGER(KIND=IP),DIMENSION(:),ALLOCATABLE ::&
              i_n4e,iNodeList_Re,iNodeList_Im,&
              i_n4ered
  REAL(KIND=WP),DIMENSION(:),ALLOCATABLE :: N
  REAL(KIND=WP) :: sInv2rho,sInv4rho
  REAL(KIND=WP) :: ZOver2rho,salphaSq
  REAL(KIND=WP),DIMENSION(:),ALLOCATABLE ::&
       sField4ElecReal,sField4ElecImag
  INTEGER(KIND=IP) :: iAstartR,&
       iAstartI,NN
  REAL(KIND=WP) :: spPerpSq			   
  REAL(KIND=WP),ALLOCATABLE :: Lj(:), dp2f(:)
  REAL(KIND=WP) :: sBetaz_i,sInvGamma_i
  REAL(KIND=WP) :: sPPerp_Re
  REAL(KIND=WP) :: sPPerp_Im
  REAL(KIND=WP) :: sQ_Re 
  REAL(KIND=WP) :: sXcoord 
  REAL(KIND=WP) :: sYcoord
  REAL(KIND=WP) :: sZ2coord,z2test 
  REAL(KIND=WP) :: FieldConst,econst
  REAL(KIND=WP) :: stheta, kbeta, un, nc, nd, nb, fkb
  REAL(KIND=WP),DIMENSION(6) :: sendbuff, recvbuff 
  INTEGER(KIND=IP) :: x_inc, y_inc, z2_inc, istart, iend
  INTEGER(KIND=IP) :: iNodesX,iNodesZ2,iNodesY, j, ntrans
  INTEGER(KIND=IPL) :: maxEl,i
  INTEGER(KIND=IP) :: local_z2_start, local_nz2, index, ti
  INTEGER(KIND=IP) :: iOutside
  INTEGER :: stat,req,error,lrank,rrank
  REAL(KIND=WP),DIMENSION(10)	:: couple 
  INTEGER(KIND=IP) :: retim, xnode, ynode, z2node 
  integer(kind=ip) :: x_in1, x_in2, y_in1, y_in2, z2_in1, z2_in2
  integer(kind=ip), allocatable :: p_nodes(:)
  REAL(KIND=WP) :: halfx, halfy, dadzRInst, dadzIInst
  real(kind=wp) :: li1, li2, li3, li4, li5, li6, li7, li8, locx, locy, locz2

  REAL(KIND=WP) :: time1, start_time
  LOGICAL :: qOKL,qoutside

!     Begin

  qOK = .false.
  qOKL = .FALSE.
    
!     SETUP AND INITIALISE THE PARTICLE'S POSITION
!     ALLOCATE THE ARRAYS

  ALLOCATE(i_n4e(iNodesPerElement_G),N(iNodesPerElement_G),&
           iNodeList_Re(iNodesPerElement_G),&
           iNodeList_Im(iNodesPerElement_G))
  AlLOCATE(i_n4ered(iNodesPerElement_G))
  ALLOCATE(sField4ElecReal(iNumberElectrons_G),&
           sField4ElecImag(iNumberElectrons_G))
  ALLOCATE(Lj(iNumberElectrons_G),dp2f(iNumberElectrons_G))

  ioutside=0

!     Set up Pointers to the field equations

  iAstartR = iBStartPosition_G(iRe_A_CG)
  iAstartI = iBStartPosition_G(iIm_A_CG)

!     Define the size of each element

  dx  = sLengthOfElmX_G
  dy  = sLengthOfElmY_G
  dz2 = sLengthOfElmZ2_G
  dV3 = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G

!     Time savers

  sInv2rho    = 1.0_WP/(2.0_WP * sRho_G)
  sInv4rho    = 1.0_WP/(4.0_WP * sRho_G)
  ZOver2rho   = sz * sInv2rho
  salphaSq    = (2.0_WP * sGammaR_G * sRho_G / sAw_G)**2

  kbeta = sAw_G / (2.0_WP * sFocusFactor_G * sRho_G * sGammaR_G)
  un = sqrt(fx_G**2.0_WP + fy_G**2.0_WP)

!     Nodes in X, Y and Z2 dimensions

  iNodesX = NX_G
  iNodesY=NY_G
  iNodesZ2 = NZ2_G
  ntrans = ReducedNX_G * ReducedNY_G

!     Diff between real and imaginary nodes in the reduced system

  retim = ReducedNX_G*ReducedNY_G*nZ2_G

!     Initialise right hand side to zero

  sb = 0.0_WP
  sField4ElecReal = 0.0_WP
  sField4ElecImag = 0.0_WP


!     Adjust undulator tuning (linear taper)

!    n2col = n2col0 * (1 + undgrad*(sz - sz0))
  call getAlpha(sZ)

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




! Calculate Lj term



  Lj = sqrt((1.0_WP - (1.0_WP / ( 1.0_WP + (sEta_G * sp2)) )**2.0_WP) &
             / (1.0_WP + nc* ( spr**2.0_wp  +  &
                               spi**2.0_wp )   )) &
          * (1.0_WP + sEta_G *  sp2) * sGammaR_G






  allocate(p_nodes(iNumberElectrons_G))
  
  if (tTransInfo_G%qOneD) then
 
    p_nodes = floor(sz2 / dz2) + 1_IP

  else

    p_nodes = ((floor( (sx+halfx)  / dx)  + 1_IP) + &
              ( (floor( (sy+halfy)  / dy)  + 1_IP)   - 1) * (ReducedNX_G - 1) + &
              (ReducedNX_G - 1) * (ReducedNY_G - 1) * &
                              ( (floor(sz2  / dz2)  + 1_IP)  -   1))

  end if  


  if (tTransInfo_G%qOneD) then

    do i = 1, maxEl
      IF (i<=procelectrons_G(1)) THEN 
    
        !   Get surrounding nodes 
    
        !   Interpolate
    
        !   
    
        dadzRInst = ((s_chi_bar_G(i)/dV3) * Lj(i) &
                          * spr(i) )
    
        dadzIInst = ((s_chi_bar_G(i)/dV3) * Lj(i) &
                          * spi(i) )


    
        z2node = floor(sz2(i)  / dz2)  + 1_IP
        locz2 = sz2(i) - REAL(z2node  - 1_IP, kind=wp) * sLengthOfElmZ2_G
    
        li1 = (1.0_wp - locz2/sLengthOfElmZ2_G)
        li2 = 1 - li1
    
        sField4ElecReal(i) = li1 * sA(p_nodes(i)) + sField4ElecReal(i)
        sField4ElecReal(i) = li2 * sA(p_nodes(i) + 1_ip) + sField4ElecReal(i)
    
        sField4ElecImag(i) = li1 * sA(p_nodes(i) + retim) + sField4ElecImag(i)
        sField4ElecImag(i) = li2 * sA(p_nodes(i) + retim + 1_ip) + sField4ElecImag(i)
    
        sDADz(p_nodes(i)) =         li1 * dadzRInst + sDADz(p_nodes(i))
        sDADz(p_nodes(i) + 1_ip) =  li2 * dadzRInst + sDADz(p_nodes(i) + 1_ip)                
    
        sDADz(p_nodes(i) + retim) =         li1 * dadzIInst + sDADz(p_nodes(i) + retim)                        
        sDADz(p_nodes(i) + 1_ip + retim) =  li2 * dadzIInst + sDADz(p_nodes(i) + 1_ip + retim)           
    
      end if
    end do

  else

    do i = 1, maxEl
      IF (i<=procelectrons_G(1)) THEN 
  
      !   Get surrounding nodes 
  
      !   Interpolate
  
      !   
  
        dadzRInst = ((s_chi_bar_G(i)/dV3) * Lj(i) &
                          * spr(i) )
    
        dadzIInst = ((s_chi_bar_G(i)/dV3) * Lj(i) &
                          * spi(i) )
    
    
    
        xnode = floor( (sx(i) + halfx ) / dx)  + 1_IP
        locx = sx(i) + halfx - REAL(xnode  - 1_IP, kind=wp) * sLengthOfElmX_G
        ynode = floor( (sy(i) + halfy )  / dy)  + 1_IP
        locy = sy(i) + halfy - REAL(ynode  - 1_IP, kind=wp) * sLengthOfElmY_G
        z2node = floor(sz2(i)  / dz2)  + 1_IP
        locz2 = sz2(i) - REAL(z2node  - 1_IP, kind=wp) * sLengthOfElmZ2_G
    
    
    
        x_in1  = (1.0_wp - locx/sLengthOfElmX_G)
        x_in2  = 1 - x_in1
        y_in1  = (1.0_wp - locy/sLengthOfElmY_G)
        y_in2  = 1 - y_in1
        z2_in1 = (1.0_wp - locz2/sLengthOfElmZ2_G)
        z2_in2 = 1 - z2_in1
    
        li1 = x_in1 * y_in1 * z2_in1
        li2 = x_in2 * y_in1 * z2_in1
        li3 = x_in1 * y_in2 * z2_in1
        li4 = x_in2 * y_in2 * z2_in1
        li5 = x_in1 * y_in1 * z2_in2
        li6 = x_in2 * y_in1 * z2_in2
        li7 = x_in1 * y_in2 * z2_in2
        li8 = x_in2 * y_in2 * z2_in2
    
    
    
    
        sField4ElecReal(i) = li1 * sA(p_nodes(i)) + sField4ElecReal(i)
        sField4ElecReal(i) = li2 * sA(p_nodes(i) + 1_ip) + sField4ElecReal(i)
        sField4ElecReal(i) = li3 * sA(p_nodes(i) + ReducedNX_G) + sField4ElecReal(i)
        sField4ElecReal(i) = li4 * sA(p_nodes(i) + ReducedNX_G + 1_ip) + sField4ElecReal(i)
        sField4ElecReal(i) = li5 * sA(p_nodes(i) + ntrans) + sField4ElecReal(i)
        sField4ElecReal(i) = li6 * sA(p_nodes(i) + ntrans + 1_ip) + sField4ElecReal(i)
        sField4ElecReal(i) = li7 * sA(p_nodes(i) + ntrans + ReducedNX_G) + sField4ElecReal(i)
        sField4ElecReal(i) = li8 * sA(p_nodes(i) + ntrans + ReducedNX_G + 1) + sField4ElecReal(i)
    
        sField4ElecImag(i) = li1 * sA(p_nodes(i) + retim) + sField4ElecImag(i)
        sField4ElecImag(i) = li2 * sA(p_nodes(i) + retim + 1_ip) + sField4ElecImag(i)
        sField4ElecImag(i) = li3 * sA(p_nodes(i) + retim + ReducedNX_G) + sField4ElecImag(i)
        sField4ElecImag(i) = li4 * sA(p_nodes(i) + retim + ReducedNX_G + 1_ip) + sField4ElecImag(i)
        sField4ElecImag(i) = li5 * sA(p_nodes(i) + retim + ntrans) + sField4ElecImag(i)
        sField4ElecImag(i) = li6 * sA(p_nodes(i) + retim + ntrans + 1_ip) + sField4ElecImag(i)
        sField4ElecImag(i) = li7 * sA(p_nodes(i) + retim + ntrans + ReducedNX_G) + sField4ElecImag(i)
        sField4ElecImag(i) = li8 * sA(p_nodes(i) + retim + ntrans + ReducedNX_G + 1) + sField4ElecImag(i)
    
    
        sDADz(p_nodes(i)) =                            li1 * dadzRInst + sDADz(p_nodes(i))
        sDADz(p_nodes(i) + 1_ip) =                     li2 * dadzRInst + sDADz(p_nodes(i) + 1_ip)                
        sDADz(p_nodes(i) + ReducedNX_G) =              li3 * dadzRInst + sDADz(p_nodes(i) + ReducedNX_G)          
        sDADz(p_nodes(i) + ReducedNX_G + 1_ip) =       li4 * dadzRInst + sDADz(p_nodes(i) + ReducedNX_G + 1_ip)   
        sDADz(p_nodes(i) + ntrans) =                   li5 * dadzRInst + sDADz(p_nodes(i) + ntrans)               
        sDADz(p_nodes(i) + ntrans + 1_ip) =            li6 * dadzRInst + sDADz(p_nodes(i) + ntrans + 1_ip)         
        sDADz(p_nodes(i) + ntrans + ReducedNX_G) =     li7 * dadzRInst + sDADz(p_nodes(i) + ntrans + ReducedNX_G)   
        sDADz(p_nodes(i) + ntrans + ReducedNX_G + 1) = li8 * dadzRInst + sDADz(p_nodes(i) + ntrans + ReducedNX_G + 1)
    
        sDADz(p_nodes(i) + retim) =                             li1 * dadzIInst + sDADz(p_nodes(i) + retim)                        
        sDADz(p_nodes(i) + 1_ip + retim) =                      li2 * dadzIInst + sDADz(p_nodes(i) + 1_ip + retim)           
        sDADz(p_nodes(i) + ReducedNX_G + retim) =               li3 * dadzIInst + sDADz(p_nodes(i) + ReducedNX_G + retim)           
        sDADz(p_nodes(i) + ReducedNX_G + 1_ip + retim) =        li4 * dadzIInst + sDADz(p_nodes(i) + ReducedNX_G + 1_ip + retim)    
        sDADz(p_nodes(i) + ntrans + retim) =                    li5 * dadzIInst + sDADz(p_nodes(i) + ntrans + retim)               
        sDADz(p_nodes(i) + ntrans + 1_ip + retim) =             li6 * dadzIInst + sDADz(p_nodes(i) + ntrans + 1_ip + retim)       
        sDADz(p_nodes(i) + ntrans + ReducedNX_G + retim) =      li7 * dadzIInst + sDADz(p_nodes(i) + ntrans + ReducedNX_G + retim)  
        sDADz(p_nodes(i) + ntrans + ReducedNX_G + 1 + retim) =  li8 * dadzIInst + sDADz(p_nodes(i) + ntrans + ReducedNX_G + 1 & 
                                                                       + retim)
  
      end if
    end do

  end if

  deallocate(p_nodes)



!    IF (ioutside>0) THEN 
!       Print*, 'WARNING: ',ioutside,&
!            ' electrons are outside the inner driving core'
!    END IF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      Calculate electron d/dz of electron equations - if needed

    if (qElectronsEvolve_G) then   

!     z2

        CALL dz2dz(sy, sb, qOKL) 
        if (.not. qOKL) goto 1000

!     X

        call dxdz(sy, Lj, nd, sb, qOKL) 
        if (.not. qOKL) goto 1000             

!     Y

        call dydz(sy, Lj, nd, sb, qOKL)
        if (.not. qOKL) goto 1000


!     dp2f is the focusing correction for dp2/dz

        call caldp2f(kbeta, sy, sb, dp2f, qOKL)	
        if (.not. qOKL) goto 1000



!     PX (Real pperp)
       
        call dppdz_r(sInv2rho,ZOver2rho,salphaSq,sField4ElecReal,nd,Lj,kbeta,sb,sy,dp2f,qOKL)
        if (.not. qOKL) goto 1000


!     -PY (Imaginary pperp)

        call dppdz_i(sInv2rho,ZOver2rho,salphaSq,sField4ElecImag,nd,Lj,kbeta,sb,sy,dp2f,qOKL)
        if (.not. qOKL) goto 1000

!     P2

        call dp2dz(sInv2rho,ZOver2rho,salphaSq,sField4ElecImag,sField4ElecReal,nd,Lj,kbeta,sb,sy,dp2f,nb,qOKL)
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
       sb(iPXs:iPXe) = 0.0_wp
       sb(iPYs:iPYe) = 0.0_wp
       sb(iP2s:iP2e) = 0.0_wp
       sb(iXs:iXe)   = 0.0_wp
       sb(iYs:iYe)   = 0.0_wp
       sb(iZ2s:iZ2e) = 0.0_wp
    end if

!     Deallocate arrays

    deallocate(i_n4e,N,iNodeList_Re,iNodeList_Im,i_n4ered)
    deallocate(sField4ElecReal,sField4ElecImag,Lj,dp2f)


    ! Set the error flag and exit

    qOK = .true.

    goto 2000 

1000 call Error_log('Error in rhs:getrhs',tErrorLog_G)
    print*,'Error in rhs:getrhs'
2000 continue

  END SUBROUTINE getrhs

END MODULE rhs
