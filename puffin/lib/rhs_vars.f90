! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module rhs_vars

use puffin_kinds, only: WP, IPL, IP

implicit none (type, external)
private

public :: bxu, byu, bzu, dV3, dx, dy, dz2, econst, halfx, halfy, iOutside, IP, IPL, lis_GR, maxEl, &
           nb, nc, ntrans, p_nodes, qoutside, retim, salphaSq, sField4ElecImag, sField4ElecReal, &
           sInv2rho, sp2, un, WP, ZOver2rho


!!!!!!!!!!!




  integer(kind=ip) :: iOutside   ! Not *currently* used

!     Loop counters

  integer(kind=ipl) :: maxEl

!    For index referencing

  integer(kind=ip) :: retim, ntrans
  integer(kind=ip), allocatable :: p_nodes(:)


!     For interpolation

  real(kind=wp) :: halfx, halfy



!    Shortcuts

  real(kind=wp) :: nc, nb, ZOver2rho, &
                   salphaSq, sInv2rho, econst, un
  real(kind=wp), allocatable :: lis_GR(:,:)
  real(kind=wp) :: dV3, dx, dy, dz2


real(kind=wp), allocatable :: sp2(:), sField4ElecReal(:), &
                              sField4ElecImag(:)


real(kind=wp), allocatable :: bxu(:), byu(:), bzu(:)

!    Error flags

  logical :: qoutside

!!!!!!!!!!!!!




!   INTEGER(KIND=IP) :: icheck
!   REAL(KIND=WP) :: dx,dy,dz2
!
!   INTEGER(KIND=IP) :: xx,yy,xred,yred,zz2
!   REAL(KIND=WP) :: s_Lex,s_Ley,s_Lez2
!   INTEGER(KIND=IP),DIMENSION(:),ALLOCATABLE ::&
!               i_n4e,iNodeList_Re,iNodeList_Im,&
!               i_n4ered
!   REAL(KIND=WP),DIMENSION(:),ALLOCATABLE :: N
!
!
!
!
!
!
!   INTEGER(KIND=IP) :: iNodesX,iNodesZ2,iNodesY, j, ntrans
!
!
!   INTEGER :: stat,req,error,lrank,rrank
!   REAL(KIND=WP),DIMENSION(10)	:: couple
!
!
!   REAL(KIND=WP) :: time1, start_time





end module rhs_vars
