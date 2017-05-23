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
!> Module for calculating sequences used in the macroparticle generation.

module pseqs

use paratype
use typempicomm
use parallelsetup
use globals
use randomGauss

implicit none

contains


subroutine genRSNorm(com, nparts)

! Generate random sequence with normal
! distribution - zero mean and unit variance

  real(kind=wp), intent(inout) :: com(:)
  integer(kind=ip), intent(in) :: nparts

  integer(kind=ip) :: ij

  do ij = 1, nparts

    com(ij) = random_normal()

  end do

end subroutine genRSNorm


subroutine modVar(com, var)

! Widen / shorten unit variance sequence
! to given variance

  real(kind=wp), intent(inout) :: com(:)
  real(kind=wp), intent(in) :: var

  com = com * var

end subroutine modVar


subroutine genGSeq(com, nparts, sigma)

! Generate random sequence with normal distribution,
! then modify to give random gaussian distribution 
! specified with input rms sigma

  real(kind=wp), intent(inout) :: com(:)
  real(kind=wp), intent(in) :: sigma
  integer(kind=ip), intent(in) :: nparts

  call genRSNorm(com, nparts)
  call modVar(com, sigma)

end subroutine genGSeq


subroutine getSeqs(xcom, ycom, pxcom, pycom, gcom, sigE)

  real(kind=wp), intent(inout) :: xcom(:), ycom(:), &
                                pxcom(:), pycom(:), &
                                gcom(:)

  real(kind=wp), intent(in) :: sigE(:)


!  Keep these local for now...add more sequences later

  integer(kind=ip) :: iTrLoad
  integer(kind=ip), parameter :: iRandSeq = 1_ip
  integer :: error


  iTrLoad = iRandSeq  ! ....for now

!  gen sequences on root process only...

  CALL init_random_seed()

  if (tProcInfo_G%qRoot) then

    if (iTrLoad == iRandSeq) then

      call genGSeq(xcom, nseqparts_G, sigE(iX_CG))
      call genGSeq(ycom, nseqparts_G, sigE(iY_CG))
      call genGSeq(pxcom, nseqparts_G, sigE(iPX_CG))
      call genGSeq(pycom, nseqparts_G, sigE(iPY_CG))
      call genGSeq(gcom, nseqparts_G, sigE(iGam_CG))

    end if

  end if

!  Share sequences to all other processes

  call mpi_bcast(xcom, nseqparts_G, mpi_double_precision, 0, &
               tProcInfo_G%comm, error )

  call mpi_bcast(ycom, nseqparts_G, mpi_double_precision, 0, &
               tProcInfo_G%comm, error )
    
  call mpi_bcast(pxcom, nseqparts_G, mpi_double_precision, 0, &
               tProcInfo_G%comm, error )
  
  call mpi_bcast(pycom, nseqparts_G, mpi_double_precision, 0, &
               tProcInfo_G%comm, error )

  call mpi_bcast(gcom, nseqparts_G, mpi_double_precision, 0, &
               tProcInfo_G%comm, error )

  
end subroutine getSeqs

end module pseqs