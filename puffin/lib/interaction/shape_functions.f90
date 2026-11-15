! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> B-spline macroparticle shape functions for the field mesh.
!>
!> Puffin's deposition is linear (a degree-1 B-spline, the hat function).  That
!> is only C0: as a macroparticle crosses a node the source term it scatters has
!> a kink, and RK4 cannot integrate through a kink at fourth order.  The step
!> size at which that starts to dominate is set by how many cells a particle
!> crosses per step, not by the undulator quiver - see
!> benchmark/convergence/README.md.
!>
!> Raising the degree smooths the crossing.  The degree-2 (quadratic) spline is
!> C1 and the degree-3 (cubic) spline is C2, so the source seen by the
!> integrator gains a continuous first, then second, derivative.
!>
!> All three are B-splines of the same family, so they share the properties the
!> deposition relies on: the weights are non-negative, they sum to one (charge
!> is conserved), and they reproduce a linear function exactly (the weighted
!> mean node position is the particle position, so the deposited first moment
!> is right).  The same weights are used to gather the field back to the
!> particle, which is what keeps the coupling symmetric and free of self-force.

module shape_functions

use puffin_kinds, only: WP, IP

implicit none (type, external)
private

public :: getShapeWeights, shapeStencilWidth, shapeLeftReach, shapeBuffExtra, &
          qShapeOrderOK, iShapeMaxWidth

!> Widest stencil any supported degree needs, for sizing local arrays.

integer(kind=ip), parameter :: iShapeMaxWidth = 4_ip


contains


!> Number of nodes, per dimension, touched by a macroparticle.

  pure function shapeStencilWidth(iOrder) result(nwid)

    integer(kind=ip), intent(in) :: iOrder  !< B-spline degree, 1, 2 or 3
    integer(kind=ip) :: nwid

    nwid = iOrder + 1_ip

  end function shapeStencilWidth


!> How many cells further to the left of the linear stencil this degree reaches.
!>
!> The linear stencil starts at node floor(u)+1.  Both the quadratic and the
!> cubic can start one node earlier, so a particle sitting in the first cell of
!> a rank's slice would otherwise scatter into the rank to its left.  The
!> parallel layout uses this to shift which particles a rank owns - see
!> rearrElecs and getFStEnd in para_field.f90.

  pure function shapeLeftReach(iOrder) result(ishft)

    integer(kind=ip), intent(in) :: iOrder
    integer(kind=ip) :: ishft

    if (iOrder > 1_ip) then
      ishft = 1_ip
    else
      ishft = 0_ip
    end if

  end function shapeLeftReach


!> Extra nodes the right-hand halo needs beyond what the linear stencil needs.
!>
!> calcBuff sizes the halo from nint() of the predicted drift, which can land a
!> node below floor(), so this is deliberately one wider than the stencil
!> strictly requires.  Zero for the linear case, which leaves the default halo
!> untouched.

  pure function shapeBuffExtra(iOrder) result(nextra)

    integer(kind=ip), intent(in) :: iOrder
    integer(kind=ip) :: nextra

    if (iOrder > 1_ip) then
      nextra = iOrder
    else
      nextra = 0_ip
    end if

  end function shapeBuffExtra


!> Is this a degree the deposition knows how to build?

  pure function qShapeOrderOK(iOrder) result(qOK)

    integer(kind=ip), intent(in) :: iOrder
    logical :: qOK

    qOK = ((iOrder >= 1_ip) .and. (iOrder <= 3_ip))

  end function qShapeOrderOK


!> Weights, and the first node they apply to, for a particle at u.
!>
!> u is the particle position in units of the cell size, so that the node with
!> 1-based index n sits at u = n-1.  On return the weights w(1:iOrder+1) belong
!> to the consecutive nodes n0, n0+1, ..., n0+iOrder, where n0 is 1-based and
!> may be outside the mesh at its edges - the caller deals with that.
!>
!> Degree 1 reproduces the arithmetic of the original inline weights exactly,
!> so the default path is bit-for-bit unchanged.

  pure subroutine getShapeWeights(iOrder, u, n0, w)

    integer(kind=ip), intent(in) :: iOrder    !< B-spline degree, 1, 2 or 3
    real(kind=wp), intent(in) :: u            !< position in cell widths
    integer(kind=ip), intent(out) :: n0       !< 1-based index of first node
    real(kind=wp), intent(out) :: w(:)        !< weights, filled 1:iOrder+1

    real(kind=wp) :: t, d, uf

    select case (iOrder)

    case (1_ip)

!     Linear (hat).  Nodes floor(u)+1 and floor(u)+2.

      uf = floor(u)
      t = u - uf
      n0 = int(uf, kind=ip) + 1_ip

      w(1) = 1.0_wp - t
      w(2) = t

    case (2_ip)

!     Quadratic.  Centred on the NEAREST node, so the stencil is symmetric
!     about the particle: nodes floor(u+1/2), floor(u+1/2)+1, floor(u+1/2)+2
!     in 1-based indexing.  d is the offset from the centre node, in [-1/2,1/2).

      uf = floor(u + 0.5_wp)
      d = u - uf
      n0 = int(uf, kind=ip)

      w(1) = 0.5_wp * (0.5_wp - d)**2
      w(2) = 0.75_wp - d*d
      w(3) = 0.5_wp * (0.5_wp + d)**2

    case (3_ip)

!     Cubic.  Symmetric about the cell the particle is in: one node behind it
!     and two ahead, so nodes floor(u), floor(u)+1, floor(u)+2, floor(u)+3
!     in 1-based indexing.

      uf = floor(u)
      t = u - uf
      n0 = int(uf, kind=ip)

      w(1) = ((1.0_wp - t)**3) / 6.0_wp
      w(2) = (3.0_wp*t**3 - 6.0_wp*t*t + 4.0_wp) / 6.0_wp
      w(3) = (-3.0_wp*t**3 + 3.0_wp*t*t + 3.0_wp*t + 1.0_wp) / 6.0_wp
      w(4) = (t**3) / 6.0_wp

    case default

!     Unreachable - the degree is validated when the input file is read.  Fall
!     back to depositing everything on one node rather than leaving the weights
!     undefined.

      n0 = int(floor(u), kind=ip) + 1_ip
      w(:) = 0.0_wp
      w(1) = 1.0_wp

    end select

  end subroutine getShapeWeights

end module shape_functions
