MODULE masks

! This module contains routines and functions
! to construct various masks used in the simulation
! of the absorbing boundaries used in the FEL
! simulation code Puffin.
!
! A useful reference for the boundaries employed 
! here is REF.
!
! Dr Lawrence Campbell
! Center for Free Electron Laser Science (CFEL)
! c/o DESY, building 99
! Luruper Chaussee 149
! 22761 Hamburg
! Germany
!  

USE paratype
USE typesAndConstants
USE Functions

IMPLICIT NONE

CONTAINS

SUBROUTINE getMask(nX,nY,dx,dy,nbx,nby,mask)

! Construct the mask for use in the absorption
! step. This outputs a 2D mask (in the transverse
! plane) in a 1D array.
!
!                ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: nX,nY,nbx,nby
  REAL(KIND=WP), INTENT(IN) :: dx,dy

  REAL(KIND=WP), INTENT(OUT) :: mask(nX*nY)

!                LOCAL ARGS

  REAL(KIND=WP) :: lenx, leny
  REAL(KIND=WP) :: x(nx), y(ny), maskt(nx,ny)

!                  Begin

  lenx  = (nX-1) * dx
  leny  = (nY-1) * dy

  x  = linspace(-lenx/2,lenx/2,nx)
  y  = linspace(-leny/2,leny/2,ny)

  maskt = Mask2D(x,y,dx,dy,nX,nY,nbx,nby)

  !IF (tProcInfo_G%qRoot) PRINT*, maskt(32,:)



  mask = RESHAPE( maskt, (/ nx*ny /))

  !IF (tProcInfo_G%qRoot) PRINT*, '  '

!  IF (tProcInfo_G%qRoot) PRINT*, mask



END SUBROUTINE getMask

!**********************************************************

FUNCTION Mask1D(x,dx,nx,nb)

! This function calculates a simple 1D mask
! to define the absoribing boundary in Puffin.
!
!                Author:
!            Lawrence Campbell
!       University of Hamburg / DESY
!                  2013
! 

  REAL(KIND=WP), INTENT(IN) :: x(:), dx
  INTEGER(KIND=IP), INTENT(IN) :: nb, nx
  REAL(KIND=WP) :: Mask1D(nx)

  REAL(KIND=WP) :: Lb, x0

!     Get the length of the boundary, Lb

  Lb = (nb-1) * dx

!     Get the center if of the boundary, x0

  x0 = x(nx) - Lb/2.0_WP

!     Mask initialized as a cos^2 at each end
  
  Mask1D(nx-nb+1_IP:nx) = cos(pi * (x(nx-nb+1_IP:nx)-x0) / Lb) ** 2.0_WP

  Mask1D(1_IP:nb) = cos(pi * (x(1_IP:nb) + x0) / Lb) ** 2.0_WP

  Mask1D( nb + 1_IP : nx-nb ) = 0.0_WP   !   Should now have 1D mask

END FUNCTION Mask1D

!!!!!!!!!!!!******************************!!!!!!!!!!!!
!!!!!!!!!!!!******************************!!!!!!!!!!!!
!!!!!!!!!!!!******************************!!!!!!!!!!!!



FUNCTION Mask2D(x,y,dx,dy,nx,ny,nbx,nby)

! This function constructs the 2D mask to be applied
! to the absorbing boundary of the radiation field nodes
! in the transverse plane. The mask should decrease from 
! unity at the center of the transverse boundaries
! to zero at the edges of the boundary. Here, a cos^2
! function peaked at the center of the boundary is used.
!
!                ARGUMENTS
!
!    x, y             Coordinates of each node in x and y (1D)
!
!    nx, ny           Num of nodes in x and y
!
!    nbx, nby         Num of nodes to be used in the transverse
!                     boundary
!
!    Mask2D           The 2D mask

  REAL(KIND=WP), INTENT(IN) :: x(:), y(:), dx, dy
  INTEGER(KIND=IP), INTENT(IN) :: nbx, nby
  INTEGER(KIND=IP), INTENT(IN) :: nx ,ny
  REAL(KIND=WP) :: Mask2D(nx,ny)

  REAL(KIND=WP) :: maskx(nx), masky(ny), maskx2(nx), masky2(ny)
  INTEGER(KIND=IP) :: ix, iy

!     Mask1D for x and y

  maskx = Mask1D2(x,dx,nx,nbx)
  masky = Mask1D2(y,dy,ny,nby)

! IF (tProcInfo_G%qRoot) PRINT*, masky

!     Maskf1 for y

  masky2 = Maskf3(y,dy,ny,nby)

!     maskf2 for x

  maskx2 = Maskf4(x,dx,nx,nbx)

!     Combine in some cunning way

  DO iy = 1,ny
  	DO ix = 1,nx

  	  Mask2D(ix,iy) = masky2(iy)*maskx(ix) + maskx2(ix)*masky(iy)
    
    END DO
  END DO

END FUNCTION Mask2D

!******************************************************

FUNCTION Maskf1(x,dx,nx,nb)

! Constructs a prticular kind of mask, more
! an envelope for the mask which is eventually used.
!
!               ARGUMENTS:-

  REAL(KIND=WP), INTENT(IN) :: x(:), dx
  INTEGER(KIND=IP), INTENT(IN) :: nb,nx

  REAL(KIND=WP) :: Maskf1(nx)

  REAL(KIND=WP) :: Lb, x0

!     Get the length of the boundary, Lb

  Lb = (nb-1) * dx

!     Get the center if of the boundary, x0

  x0 = x(nx) - Lb/2.0_WP

!     Mask initialized as a cos^2 at each end
  
  Maskf1(nx-nb+1_IP:nx) = cos(pi * (x(nx-nb+1_IP:nx)-x0) / Lb) ** 2.0_WP

  Maskf1(1_IP:nb) = cos(pi * (x(1_IP:nb) + x0) / Lb) ** 2.0_WP

  Maskf1( nb/2 + 1_IP : nx - nb/2 ) = 1.0_WP   !   Should now have mask type 1

END FUNCTION Maskf1

!******************************************************

FUNCTION Maskf2(x,dx,nx,nb)

! Constructs a prticular kind of mask, more
! an envelope for the mask which is eventually used.
!
!               ARGUMENTS:-


  REAL(KIND=WP), INTENT(IN) :: x(:), dx
  INTEGER(KIND=IP), INTENT(IN) :: nb,nx

  REAL(KIND=WP) :: Maskf2(nx)

  REAL(KIND=WP) :: Lb, x0

!     Get the length of the boundary, Lb

  Lb = (nb-1) * dx

!     Get the center if of the boundary, x0

  x0 = x(nx) - Lb/2.0_WP

!     Mask initialized as a cos^2 at each end
  
  Maskf2(nx-nb+1_IP:nx) = cos(pi * (x(nx-nb+1_IP:nx)-x0) / Lb + pi/2.0_WP) ** 2.0_WP
  Maskf2(1_IP:nb) = cos(pi * (x(1_IP:nb) + x0) / Lb + pi/2.0_WP) ** 2.0_WP

  Maskf2( 1_IP : nb/2 ) = 0.0_WP
  Maskf2(nx - nb/2 + 1_IP : nx) = 0.0_WP
  Maskf2( nb + 1_IP : nx-nb ) = 1.0_WP   !   Should now have mask type 2

END FUNCTION Maskf2


!******************************************************



FUNCTION Mask1D2(x,dx,nx,nb)

! This function calculates a simple 1D mask
! to define the absoribing boundary in Puffin.
!
!                Author:
!            Lawrence Campbell
!       University of Hamburg / DESY
!                  2013
! 

  REAL(KIND=WP), INTENT(IN) :: x(:), dx
  INTEGER(KIND=IP), INTENT(IN) :: nb, nx
  REAL(KIND=WP) :: Mask1D2(nx)

  REAL(KIND=WP) :: Lb, x0, x01, x02

!     Get the length of the boundary, Lb

  Lb = (nb-1) * dx

!     Get the center if of the boundary, x0

!  x0 = x(nx) - Lb/2.0_WP
   x01 = x(nx)
   x02 = x(1)

!     Mask initialized as a cos^2 at each end
  
  Mask1D2(nx-nb+1_IP:nx) = cos(pi * (x(nx-nb+1_IP:nx)-x01) / (2.0_WP * Lb)) ** 2.0_WP

  Mask1D2(1_IP:nb) = cos(pi * (x(1_IP:nb) - x02) / (2.0_WP * Lb)) ** 2.0_WP

  Mask1D2( nb + 1_IP : nx-nb ) = 0.0_WP   !   Should now have 1D mask


END FUNCTION Mask1D2


!******************************************************

FUNCTION Maskf3(x,dx,nx,nb)

! Constructs a prticular kind of mask, more
! an envelope for the mask which is eventually used.
!
!               ARGUMENTS:-


  REAL(KIND=WP), INTENT(IN) :: x(:), dx
  INTEGER(KIND=IP), INTENT(IN) :: nb,nx

  REAL(KIND=WP) :: Maskf3(nx)

!     Mask initialized as unity everywhere
  
  Maskf3(:) = 1.0_WP


END FUNCTION Maskf3

!******************************************************

FUNCTION Maskf4(x,dx,nx,nb)

! Constructs a prticular kind of mask, more
! an envelope for the mask which is eventually used.
!
!               ARGUMENTS:-


  REAL(KIND=WP), INTENT(IN) :: x(:), dx
  INTEGER(KIND=IP), INTENT(IN) :: nb,nx

  REAL(KIND=WP) :: Maskf4(nx)

  REAL(KIND=WP) :: Lb, x01, x02

!     Get the length of the boundary, Lb

  Lb = (nb-1) * dx

!     Get the center if of the boundary, x0

  x01 = x(nx - nb + 1_IP)
  x02 = x(nb)

!     Mask initialized as a cos^2 at each end
  
  Maskf4(nx-nb+1_IP:nx) = cos(pi * (x(nx-nb+1_IP:nx)-x01) / (2.0_WP * Lb)) ** 2.0_WP
  Maskf4(1_IP:nb) = cos(pi * (x(1_IP:nb) - x02) / (2.0_WP * Lb)) ** 2.0_WP

  Maskf4( nb + 1_IP : nx-nb ) = 1.0_WP   !   Should now have mask type 2


END FUNCTION Maskf4

END MODULE masks