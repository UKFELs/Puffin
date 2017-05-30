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

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Generate random sequence with normal
!> distribution - zero mean and unit variance
!> @param[inout] com Sequence to modify (of width = 1)
!> @param[in] nparts Length of sequence sequence

subroutine genRSNorm(com, nparts)

  real(kind=wp), intent(inout) :: com(:)
  integer(kind=ip), intent(in) :: nparts

  integer(kind=ip) :: ij

  do ij = 1, nparts

    com(ij) = random_normal()

  end do

end subroutine genRSNorm

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Widen / shorten unit variance sequence
!> to given variance
!> @param[inout] com Sequence to modify (of width = 1)
!> @param[in] sig r.m.s. sigma of desired Gaussian distribution

subroutine modVar(com, sig)

  real(kind=wp), contiguous, intent(inout) :: com(:)
  real(kind=wp), intent(in) :: sig

  com = com * sig

end subroutine modVar


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Generate random sequence with normal distribution,
!> then modify to give random gaussian distribution 
!> specified with input rms sigma
!> @param[inout] com Array to output sequence
!> @param[in] nparts Length of sequence
!> @param[in] sig r.m.s. sigma of desired Gaussian distribution

subroutine genGSeq(com, nparts, sigma)

  real(kind=wp), contiguous, intent(inout) :: com(:)
  real(kind=wp), intent(in) :: sigma
  integer(kind=ip), intent(in) :: nparts

  call genRSNorm(com, nparts)
  call modVar(com, sigma)

end subroutine genGSeq


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Generate sequences for 5-D phase space for Puffin. The 5 dimensions
!> are 4 transverse (x,y,px,py) and energy. 
!> @param[inout] xcom Sequence in x dimension
!> @param[inout] ycom Sequence in y dimension
!> @param[inout] pxcom Sequence in px dimension
!> @param[inout] pycom Sequence in py dimension
!> @param[inout] gcom Sequence in energy dimension
!> @param[in] sigE 6 element array of requested gaussian r.m.s. width 
!> in the Puffin order (x,y,z2,px,py,gamma).

subroutine getSeqs(xcom, ycom, pxcom, pycom, gcom, sigE, iTrLoad)

  real(kind=wp), contiguous, intent(inout) :: xcom(:), ycom(:), &
                                pxcom(:), pycom(:), &
                                gcom(:)

  real(kind=wp), intent(in) :: sigE(:)
  integer(kind=ip), intent(in) :: iTrLoad

!  Keep these local for now...add more sequences later

  integer(kind=ip), parameter :: iRandSeq = 1_ip
  integer(kind=ip), parameter :: iHaltonSeq = 2_ip
  integer :: error


!  iTrLoad = iHaltonSeq  ! ....for now

!  gen sequences on root process only...

  CALL init_random_seed()

  if (tProcInfo_G%qRoot) then

    if (iTrLoad == iRandSeq) then

      call genGSeq(xcom, nseqparts_G, sigE(iX_CG))
      call genGSeq(ycom, nseqparts_G, sigE(iY_CG))
      call genGSeq(pxcom, nseqparts_G, sigE(iPX_CG))
      call genGSeq(pycom, nseqparts_G, sigE(iPY_CG))
      call genGSeq(gcom, nseqparts_G, sigE(iGam_CG))

    else if (iTrLoad == iHaltonSeq) then

      call genHSeq(xcom, nseqparts_G, 2_ip, sigE(iX_CG))
      call genHSeq(ycom, nseqparts_G, 3_ip, sigE(iY_CG))
      call genHSeq(pxcom, nseqparts_G, 5_ip, sigE(iPX_CG))
      call genHSeq(pycom, nseqparts_G, 7_ip, sigE(iPY_CG))
      call genHSeq(gcom, nseqparts_G, 11_ip, sigE(iGam_CG))

!      print*, xcom

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


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to generate a Halton sequence of length size nparts
!> projected over a Gaussian of specified width
!> @param[inout] seq Array to output sequence
!> @param[in] nparts Length of sequence
!> @param[in] basis Base of sequence
!> @param[in] sig r.m.s. sigma of desired Gaussian distribution

subroutine genHSeq(seq, nparts, basis, sig)

  real(kind=wp), contiguous, intent(inout) :: seq(:)
  integer(kind=ip), intent(in) :: nparts
  integer(kind=ip), intent(in) :: basis
  real(kind=wp), intent(in) :: sig

  integer(kind=ip) :: ic

  do ic = 1, nparts
    seq(ic) = halton(ic, basis) ! sequence value between -1 and 1
  end do

  call projectRSeq(seq)  ! Project sequence to Gaussian of variance = 1

  call modVar(seq, sig)

end subroutine genHSeq

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Function which calculates the nth element in the bit-reversed Halton 
!> sequence of specified base.
!> @param[in] ind Nth element of sequence
!> @param[in] ibase Index of sequence

function halton(ind, ibase) result(y)

  integer(kind=ip), intent(in) :: ind, ibase

  real(kind=wp) :: ft, t1
  real(kind=wp) :: y
  integer(kind=ip) :: it

  ft = 1.0_wp
  y = 0.0_wp
  it = ind

  do while (it>0)

    ft = ft / real(ibase, kind=wp)
    y = y + ft * real(mod(it, ibase), kind=wp)
    it = it / ibase

  end do

end function halton


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to project given distribution to a Gaussian of unit variance.
!> @param[inout] seq Random/semi-random sequence to be projected.

subroutine projectRSeq(seq)
  
  use functions
  use error_fn
  
  real(kind=wp), intent(inout) :: seq(:)

  integer(kind=ip) :: is, ic1
  integer(kind=ip) :: iMeth

  integer(kind=ip), parameter :: GS = 1_ip
  integer(kind=ip), parameter :: JP = 2_ip
  integer(kind=ip), parameter :: iIntMesh = 2000_ip

  real(kind=wp), allocatable :: tseq(:), yf(:), cdf(:)

  iMeth = GS

  is = size(seq)

! can do general sequence (so no need to know inverse - will need to numerically 
! calculate 'real' sequence)

  if (iMeth == GS) then

    allocate(yf(iIntMesh), cdf(iIntMesh))
    allocate(tseq(is))

    yf = linspace(-4.0_wp, 4.0_wp, iIntMesh)

    do ic1 = 1, iIntMesh
      cdf(ic1) = 0.5_wp * (1.0_wp + erf(yf(ic1) / sqrt(2.0_wp)))  ! CDF of Gaussian
    end do
    
    ! interpolate as MATLAB interp1(cdf, yf, seq)
!    print*, cdf
    call interps(cdf, yf, seq, tseq)

    seq = tseq

! ...or could do joint probability projection a-la Genesis...

  else if (iMeth == JP) then
    
!    rsq = 
    
  end if

  deallocate(tseq)

end subroutine projectRSeq


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Interpolate values of x -> f(x) using linear interpolants. Values of x to be 
!> interpolated do not have to be in order, and the grid in x does not have to 
!> be equispaced.
!> @param[in] x Mesh in x
!> @param[in] func f(x) - the function to be interpolated to.
!> @param[in] smplsx The values or samples in x to be interpolated. (finding 
!> f(x) at these points)
!> @param[out] smpls_interp The output interpolated values.

subroutine interps(x, func, smplsx, smpls_interp)

  real(kind=wp), intent(in) :: func(:), x(:), smplsx(:)
  real(kind=wp), intent(out):: smpls_interp(:)

  integer(kind=ip) :: nsamples, ic

  nsamples = size(smplsx)  

  do ic = 1, nsamples

    smpls_interp(ic) = interp1(x, func, smplsx(ic))

  end do
  
end subroutine interps

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Interpolate single value of x -> f(x) using linear interpolants. The grid in 
!> x does not have to be equispaced.
!> @param[in] x Mesh in x
!> @param[in] func f(x) - the function to be interpolated to.
!> @param[in] smplx The value or sample in x to be interpolated. (i.e. finding 
!> f(smplx))

function interp1(x, func, smplx) result(smpl_interp)

  real(kind=wp), intent(in) :: func(:), x(:), smplx
  real(kind=wp) :: smpl_interp

  integer(kind=ip) :: is, ic, ifl, ifu
  real(kind=wp) :: dx, locx

  is = size(func)
  ic = 1_ip

  if ((smplx > x(1)) .and. (smplx < x(is))) then
    
    do while (smplx > x(ic)) 
      ic = ic + 1_ip
    end do

    ifl = ic
    ifu = ic + 1_ip

    dx = x(ifu) - x(ifl)
    locx = smplx - x(ifl)

!    smpl_interp = func(ifu) ! snap to lower
    smpl_interp = func(ifl) * (1.0_wp - locx/dx)
    smpl_interp = smpl_interp + func(ifu) * locx / dx

  else if (smplx <= x(1)) then
    
    smpl_interp = func(1)
!    print*, 'eh, why here? func(1) = ', func(1), 'smplx = ', smplx

  else if (smplx >= x(is)) then
    
    smpl_interp = func(is)
!    print*, 'eh, why here? func(1) = ', func(1), 'smplx = ', smplx

  end if

end function interp1

end module pseqs

