! ###############################################
! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause
! ###############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Module defining the type for the MPI communicator info in Puffin

module typempicomm

  use paratype
		
  implicit none

  type fMPIComm
 	  integer           :: comm = 0_IP
	  integer           :: rank = 0_IP
	  integer           :: size = 0_IP
	  logical           :: qroot = .false.
  end type fMPIComm

! Define Global variables
!
! tProcInfo_G - Parallel processing information

  type(fMPIComm) :: tProcInfo_G
  save tProcInfo_G

end module typempicomm
