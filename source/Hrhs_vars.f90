module rhs_vars

use paratype

!!!!!!!!!!!




  integer(kind=ip) :: iOutside   ! Not *currently* used

!     Loop counters

  integer(kind=ipl) :: maxEl,i

!    For index referencing

  integer(kind=ip) :: retim, xnode, ynode, z2node, ntrans
  integer(kind=ip), allocatable :: p_nodes(:)


!     For interpolation

  integer(kind=ip) :: x_in1, x_in2, y_in1, y_in2, z2_in1, z2_in2
  real(kind=wp) :: li1, li2, li3, li4, li5, li6, li7, li8, locx, locy, locz2
  REAL(kind=wp) :: halfx, halfy, dadzRInst, dadzIInst



!    Shortcuts

  real(kind=wp) :: kbeta, nc, nd, nb, ZOver2rho, &
                   salphaSq, sInv2rho, fkb, econst, un
  real(kind=wp), allocatable :: Lj(:)
  real(kind=wp) :: dV3, dx, dy, dz2

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





end module