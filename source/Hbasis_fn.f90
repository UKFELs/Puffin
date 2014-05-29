!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE basis_fn

USE paratype

IMPLICIT NONE 

Private intpln_fn1D
Private intpln_fn3D 

INTERFACE intpl_fn
   MODULE PROCEDURE intpln_fn1D, intpln_fn3D   
END INTERFACE

CONTAINS

  SUBROUTINE intpln_fn3D(x,y,z2,iOneElm,lx,ly,lz2,N)
	
    IMPLICIT NONE
!
! Work out the interpolation function
!	
! ARGS:-

! x,y,z2 - position (co-ordinate) of each electron 
!          within its element
! iOneElm - NUMBER OF NODES IN ONE ELEMENT
! lx - length of element in x direction
! ly - length of element in y direction
! lz2 - length of element in z2 direction
!
! OUTPUT:-
! N - Each electron shared its energy to 8 nodes
!     according to the interpolation fucntion
    REAL(KIND=WP),INTENT(IN)    :: x,y,z2
    INTEGER(KIND=IP),INTENT(IN)	:: iOneElm
    REAL(KIND=WP),INTENT(IN)    :: lx,ly,lz2
    REAL(KIND=WP),INTENT(OUT)   :: N(iOneElm)
! Local vars:-    
    REAL(KIND=WP) :: updateX,updateY,updateZ2
! BEGIN:-
    updateX=x/lx
    updateY=y/ly
    updateZ2=z2/lz2	
	
    N(1)=(1.0_WP-updateX)*(1.0_WP-updateY)*(1.0_WP-updateZ2)
    N(2)=updateX*(1.0_WP-updateY)*(1.0_WP-updateZ2)
    N(3)=(1.0_WP-updateX)*updateY*(1.0_WP-updateZ2)    
    N(4)=updateX*updateY*(1.0_WP-updateZ2)

    N(5)=(1.0_WP-updateX)*(1.0_WP-updateY)*updateZ2
    N(6)=updateX*(1.0_WP-updateY)*updateZ2
    N(7)=(1.0_WP-updateX)*updateY*updateZ2
    N(8)=updateX*updateY*updateZ2

  END SUBROUTINE intpln_fn3D

!********************************************************

  SUBROUTINE intpln_fn1D(z2,iOneElm,lz2,N)
	
    IMPLICIT NONE
	
! Calculates the interpolation function in 1D
!
! ARGS:-
!
! x,y,z2 	- position (co-ordinate) of each electron within its element
! iOneElm - NUMBER OF NODES IN ONE ELEMENT
! lx - length of element in x direction
! ly - length of element in y direction
! lz2 - length of element in z2 direction
!
! Output %%%
! N - Each electron shared its energy to 8 nodes
!     according to the interpolation fucntion
    REAL(KIND=WP),INTENT(IN)    :: z2
    INTEGER(KIND=IP),INTENT(IN)	:: iOneElm
    REAL(KIND=WP),INTENT(IN)    :: lz2
    REAL(KIND=WP),INTENT(OUT)   :: N(iOneElm)
    
    REAL(KIND=WP) :: updateX,updateY,updateZ2

    updateZ2=z2/lz2	
    
    N(1)=(1.0_WP-updateZ2)
    N(2)=updateZ2
	
  END SUBROUTINE intpln_fn1D
!********************************************************
  SUBROUTINE Rsquared(x,y,z2,iOneElm,lx,ly,lz2,RSq)
	
    IMPLICIT NONE
!
! ARGS:-
! x,y,z2 - position (co-ordinate) of each electron within its element
! iOneElm - NUMBER OF NODES IN ONE ELEMENT
! w - length of element in x direction
! h - length of element in y direction
! d - length of element in z2 direction
!
!Output %%%
! N - Each electron shared its energy to 8 nodes
!     according to the interpolation fucntion
    REAL(KIND=WP),INTENT(IN) :: x,y,z2
    INTEGER(KIND=IP),INTENT(IN) :: iOneElm
    REAL(KIND=WP),INTENT(IN) :: lx,ly,lz2
    REAL(KIND=WP),INTENT(OUT) :: RSq(iOneElm)
    REAL(KIND=WP) :: updateX,updateY,updateZ2
!--------------------------------------------------------
! BEGIN:-
    updateX  = x/lx
    updateY  = y/ly
    updateZ2 = z2/lz2	
	
    RSq(1) = (updateX)**2 + (updateY)**2 + (updateZ2)**2
    RSq(2) = (updateX)**2 + (1.0_WP-updateY)**2 + (updateZ2)**2
    RSq(3) = (1.0_WP-updateX)**2 + (updateY)**2 + (updateZ2)**2
	
    RSq(4) = (1.0_WP-updateX)**2 + (1.0_WP-updateY)**2 + (updateZ2)**2
    RSq(5) = (updateX)**2 + (updateY)**2 + (1.0_WP-updateZ2)**2
    RSq(6) = (updateX)**2 + (1.0_WP-updateY)**2 + (1.0_WP-updateZ2)**2
	
    RSq(7) = (1.0_WP-updateX)**2 + (updateY)**2 + (1.0_WP-updateZ2)**2
    RSq(8) = (1.0_WP-updateX)**2 + (1.0_WP-updateY)**2 + (1.0_WP-updateZ2)**2
		
  END SUBROUTINE Rsquared

END MODULE basis_fn
