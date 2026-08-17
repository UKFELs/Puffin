! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module puffin_mpiInfo
   use puffin_kinds, only: ip
   implicit none

!*****************************************************
! Originally created by Cynthia Nam

! Updated- Lawrence Campbell
! Place -  University of Strathclyde
!          Glasgow
!          Scotland
! Date -   24/11/2008
!*****************************************************

   type puffin_mpiInfoType
      integer           :: comm = 0_ip
      integer           :: rank = 0_ip
      integer           :: size = 0_ip
      logical           :: qroot = .false.
   end type puffin_mpiInfoType

   type(puffin_mpiInfoType) :: tProcInfo_G
   save tProcInfo_G

end module puffin_mpiInfo
