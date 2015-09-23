module rhs_vars

use paratype

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

  real(kind=wp) :: kbeta, nc, nd, nb, ZOver2rho, &
                   salphaSq, sInv2rho, fkb, econst, un
  real(kind=wp), allocatable :: Lj(:), lis_GR(:,:)
  real(kind=wp) :: dV3, dx, dy, dz2


real(kind=wp), allocatable :: dp2f(:), sField4ElecReal(:), &
                              sField4ElecImag(:)

                              
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