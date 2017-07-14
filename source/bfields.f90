! ################################################
! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause
! ################################################
!
!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Module containing the b-field generator type for the udulator modules.


module bfields

use paratype
!use globals


private iUndStart, iUndEnd, iUndMain

integer(kind=ip), parameter :: iUndStart = 1_ip, &
                               iUndEnd = 2_ip, &
                               iUndMain = 0_ip

! actually needs to extend abstract type...
type, public :: undFields  

!     These describe the physical module:

  real(kind=wp) :: ux, uy ! undulator polarization

  real(kind=wp) :: kux, kuy  ! undulator field x and y variation (from curved
                             ! poles, if used)

  real(kind=wp) :: kbx, kby  ! Scaled betatron wavenumbers in x and y from
                             ! in-undulator 'strong' focusing

  integer(kind=ip) :: Nw     ! Number of periods

  real(kind=wp), private :: n2col, n2col0   ! alpha, or undulator tuning
  real(kind=wp) :: taper   ! gradient for linear taper. Tuning is then 
                           ! n2col = n2col0 + taper * zbar

  logical :: qUndEnds   ! If simulating undulator ends
  logical :: qFocussing  ! if using additional strong intra-undulator focusing
  character(32_ip) :: zundtype

  logical :: qOneD

  real(kind=wp) :: sZFS, sZFE ! Start and end of the 'main' undulator,
                                ! when ignoring the ends (local - so with
                                ! no ends, sZFS = 0, and sZFE = module length).

  integer(kind=ip), private :: iUndPlace  ! Tracks if we are in the ends or not.
                                          ! Can take values iUndStart, iUndEnd,
                                          ! or iUndMain.

!        These describe numerical integration: (may spin out to seperate type...)
!        (in fact they definitely don't belong here now...)

  real(kind=wp) :: delmz           ! Integration step size
  integer(kind=ip) :: isteps4diff  ! Steps per diffraction step
  integer(kind=ip) :: nsteps  ! Total number of steps

contains 

  procedure :: getBfields
  procedure :: getAlpha
  procedure :: adjUndPlace
  procedure, private :: getBXfield, getBYfield, getBZfield

end type undFields


contains

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Calculate the b-fields. Calls bx, by and bz subroutines
!> @param[in] tScaling Fortran type describing system scaling.
!> @param[in] sx Macroparticle coordinates in xbar.
!> @param[in] sy Macroparticle coordinates in ybar.
!> @param[in] sz current scaled distance though undulator, zbar.
!> @param[out] bxj scaled b-field in x direction for given positions.
!> @param[out] byj scaled b-field in y direction for given positions.
!> @param[out] bzj scaled b-field in z direction for given positions.

  subroutine getBFields(self, tScaling, sx, sy, sz, &
                        bxj, byj, bzj)

    use typeScale

    class(undFields), intent(inout) :: self
    type(fScale), intent(in) :: tScaling
    real(kind=wp), contiguous, intent(in) :: sx(:), sy(:)
    real(kind=wp), intent(in) :: sz

    real(kind=wp), contiguous, intent(out) :: bxj(:), byj(:), bzj(:)


    call self%adjUndPlace(sZ)
    call self%getAlpha(sZ)

    call self%getBXfield(tScaling, sx, sy, sz, bxj)
    call self%getBYfield(tScaling, sx, sy, sz, byj)
    call self%getBZfield(tScaling, sx, sy, sz, bzj)

  end subroutine getBFields



!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Calculate the bx magnetic fields at specified x, y, and z coords.
!> @param[in] tScaling Fortran type describing system scaling.
!> @param[in] sx Macroparticle coordinates in xbar.
!> @param[in] sy Macroparticle coordinates in ybar.
!> @param[in] sz current scaled distance though undulator, zbar.
!> @param[out] bxj scaled b-field in x direction for given positions.

subroutine getBXfield(self, tScaling, sx, sy, sz, bxj)

  use typeScale

  class(undFields), intent(in) :: self
  type(fScale), intent(in) :: tScaling
  real(kind=wp), contiguous, intent(in) :: sx(:), sy(:)
  real(kind=wp), intent(in) :: sz
  real(kind=wp), contiguous, intent(out) :: bxj(:)

!    Local vars:-

  real(kind=wp) :: szt, rho, alph, sZFE, sZFS

  rho = tScaling%rho
  alph = self%n2col
  sZFE = self%sZFE

!      cc1 = sqrt(tScaling%eta) / (2_wp * rho * self%kuy)

  szt = sZ
  szt = szt / 2_wp / rho


!  ####################################################
!    Curved pole case - planar wiggler with focusing
!    in both x and y (electron wiggles in x)

  if (self%zUndType == 'curved') then

    if (self%iUndPlace == iUndStart) then

!$OMP WORKSHARE
      bxj = self%kux / self%kuy * sinh(self%kux * sx) &
            * sinh(self%kuy * sy) &
            * szt / 4_wp / pi * sin(szt) * alph
!$OMP END WORKSHARE

    else if (self%iUndPlace == iUndEnd) then

      szt = sZ - sZFE
      szt = szt / 2_wp / rho

!$OMP WORKSHARE
      bxj = self%kux / self%kuy * sinh(self%kux * sx) &
            * sinh(self%kuy * sy) &
            * (-szt / 4_wp / pi + 1_wp) * sin(szt) * alph
!$OMP END WORKSHARE

    else if (self%iUndPlace == iUndMain) then

!$OMP WORKSHARE
      bxj = self%kux / self%kuy * sinh(self%kux * sx) &
            * sinh(self%kuy * sy) &
            * sin(szt) * alph
!$OMP END WORKSHARE

    end if

!    END curved pole field description
!  ####################################################



!  ####################################################
!    Plane-pole case - planar wiggler with focusing
!    only in y (and electron will wiggle in x)

  else if (self%zUndType == 'planepole')  then

    if (self%iUndPlace == iUndStart) then

!$OMP WORKSHARE
      bxj = 0_wp
!$OMP END WORKSHARE

    else if (self%iUndPlace == iUndEnd) then

      szt = sZ - sZFE
      szt = szt / 2_wp / rho

!$OMP WORKSHARE
      bxj = 0_wp
!$OMP END WORKSHARE

    else if (self%iUndPlace == iUndMain) then

!$OMP WORKSHARE
      bxj = 0_wp
!$OMP END WORKSHARE

    end if

!    END plane pole undulator field description
!  ####################################################







!  ####################################################
!    Helical case - helical wiggler with focusing
!    in x and y (and electron will wiggle in x and y)

  else if (self%zUndType == 'helical')  then

    if (self%iUndPlace == iUndStart) then


!      bxj = sin(szt / 8_wp) * &
!               cos(szt / 8_wp) * sin(szt) / 4_wp   &
!             +  sin(szt/8_wp)**2_wp  * cos(szt)
!$OMP WORKSHARE
      bxj = (sZ - pi * rho) / (6_wp * pi*rho) * cos(szt) * alph
!$OMP END WORKSHARE

      if (sZ < pi * rho) then 
!$OMP WORKSHARE
        bxj = 0.0_wp
!$OMP END WORKSHARE
      else if (sZ > 7_wp * pi * rho) then
!$OMP WORKSHARE
        bxj = cos(szt) * alph
!$OMP END WORKSHARE
      end if

    else if (self%iUndPlace == iUndEnd) then

      szt = sZ - sZFE
!      szt = szt / 2_wp / rho


!      bxj = - cos(szt / 8_wp) * &
!              sin(szt / 8_wp) * sin(szt)  / 4_wp  &
!            +  cos(szt/8_wp)**2_wp  * cos(szt)

!$OMP WORKSHARE
      bxj =  - (szt - 7.0_wp * pi * rho) / (6_wp * pi * rho) * &
               cos(szt / 2_wp / rho) * alph
!$OMP END WORKSHARE

      if (sZt < pi * rho) then 
!$OMP WORKSHARE
        bxj = cos(szt / 2_wp / rho) * alph
!$OMP END WORKSHARE
      else if (sZt > 7_wp * pi * rho) then
!$OMP WORKSHARE
        bxj = 0.0_wp
!$OMP END WORKSHARE
      end if

    else if (self%iUndPlace == iUndMain) then

!$OMP WORKSHARE
      bxj = cos(szt) * alph
!$OMP END WORKSHARE

    end if

!    END helical undulator field description
!  ####################################################






  else






!  ####################################################
!    'puffin' elliptical undulator...
!    with variable x and y polarization...

    if (self%iUndPlace == iUndStart) then


!      bxj = self%ux * sin(szt / 8_wp) * &
!               cos(szt / 8_wp) * sin(szt) / 4_wp   &
!             +  sin(szt/8_wp)**2_wp  * cos(szt)
!$OMP WORKSHARE
      bxj = self%ux * (sZ - pi * rho) / (6_wp * pi*rho) * cos(szt) * alph
!$OMP END WORKSHARE

      if (sZ < pi * rho) then 
!$OMP WORKSHARE
        bxj = 0.0_wp
!$OMP END WORKSHARE
      else if (sZ > 7_wp * pi * rho) then
!$OMP WORKSHARE
        bxj = self%ux * cos(szt) * alph
!$OMP END WORKSHARE
      end if

    else if (self%iUndPlace == iUndEnd) then

      szt = sZ - sZFE
      !szt = szt / 2_wp / rho

!$OMP WORKSHARE
      bxj =  -self%ux * (szt - 7.0_wp * pi * rho) / (6_wp * pi * rho) * &
               cos(szt / 2_wp / rho) * alph
!$OMP END WORKSHARE

      if (sZt < pi * rho) then 
!$OMP WORKSHARE
        bxj = self%ux * cos(szt / 2_wp / rho) * alph
!$OMP END WORKSHARE
      else if (sZt > 7_wp * pi * rho) then
!$OMP WORKSHARE
        bxj = 0.0_wp
!$OMP END WORKSHARE
      end if
      

    else if (self%iUndPlace == iUndMain) then

!$OMP WORKSHARE
      bxj = self%ux * cos(szt) * alph
!$OMP END WORKSHARE

    end if

!    END elliptical undulator description
!  ####################################################




  end if


!   Focusing component (non-physical)

    if (self%qFocussing) then

!$OMP WORKSHARE
        bxj = sqrt(tScaling%eta) * self%kbx**2.0_wp / tScaling%kappa &
              * sy + bxj
!$OMP END WORKSHARE

    end if


  end subroutine getBXfield


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Calculate the bx magnetic fields at specified x, y, and z coords.
!> @param[in] tScaling Fortran type describing system scaling.
!> @param[in] sx Macroparticle coordinates in xbar.
!> @param[in] sy Macroparticle coordinates in ybar.
!> @param[in] sz current scaled distance though undulator, zbar.
!> @param[out] bxj scaled b-field in x direction for given positions.


  subroutine getBYfield(self, tScaling, sx, sy, sz, byj)

  use typeScale

  class(undFields), intent(in) :: self
  type(fScale), intent(in) :: tScaling
  real(kind=wp), contiguous, intent(in) :: sx(:), sy(:)
  real(kind=wp), intent(in) :: sz
  real(kind=wp), contiguous, intent(out) :: byj(:)

!    Local vars:-

  real(kind=wp) :: szt, rho, alph

  rho = tScaling%rho
  alph = self%n2col

  szt = sZ
  szt = szt / 2_wp / rho


!  ####################################################
!    Curved pole case - planar wiggler with focusing
!    in both x and y (electron wiggles in x)


  if (self%zUndType == 'curved') then

    if (self%iUndPlace == iUndStart) then

!$OMP WORKSHARE
      byj = cosh(self%kux * sx) &
            * cosh(self%kuy * sy) &
            *  szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

    else if (self%iUndPlace == iUndEnd) then

      szt = sZ - sZFE
      szt = szt / 2_wp / rho

!$OMP WORKSHARE
      byj = cosh(self%kux * sx) &
            * cosh(self%kuy * sy) &
            * (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (self%iUndPlace == iUndMain) then

!$OMP WORKSHARE
      byj = cosh(self%kux * sx) &
            * cosh(self%kuy * sy) &
            * sin(szt)
!$OMP END WORKSHARE

    end if

!    END curved pole field description
!  ####################################################







!  ####################################################
!    Plane-pole case - planar wiggler with focusing
!    only in y (and electron will wiggle in x)



  else if (self%zUndType == 'planepole')  then

    if (self%iUndPlace == iUndStart) then

!$OMP WORKSHARE
!      byj = cosh( sqrt(tScaling%eta) / 2_wp / rho * sy) * &
!            (  (- sin(szt / 8_wp) * &
!               cos(szt / 8_wp) * cos(szt) / 4_wp   &
!             +  sin(szt/8_wp)**2_wp  * sin(szt) )  )


      !byj = sin(szt/8_wp)**2_wp * sin(szt)
      byj = szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

!    print*, "hehehe"

    else if (self%iUndPlace == iUndEnd) then

      szt = sZ - sZFE
      szt = szt / 2_wp / rho

!$OMP WORKSHARE
!      byj = cosh( sqrt(tScaling%eta) / 2_wp / rho * sy) * &
!            (  cos(szt / 8_wp) * &
!              sin(szt / 8_wp) * cos(szt)  / 4_wp  &
!            +  cos(szt/8_wp)**2_wp  * sin(szt)  )
            
      byj = (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (self%iUndPlace == iUndMain) then

!$OMP WORKSHARE
!      byj = cosh( sqrt(tScaling%eta) / 2_wp / rho * sy) &
!            * sin(szt)
      byj = sin(szt)
!$OMP END WORKSHARE

    end if

!    END plane pole undulator field description
!  ####################################################









!  ####################################################
!    Helical case - helical wiggler with focusing
!    in x and y (and electron will wiggle in x and y)

  else if (self%zUndType == 'helical')  then

    if (self%iUndPlace == iUndStart) then

!$OMP WORKSHARE
      byj = szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

    else if (self%iUndPlace == iUndEnd) then

      szt = sZ - sZFE
      szt = szt / 2_wp / rho

!$OMP WORKSHARE
      byj = (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (self%iUndPlace == iUndMain) then

!$OMP WORKSHARE
      byj = sin(szt)
!$OMP END WORKSHARE

    end if

!    END helical undulator field description
!  ####################################################








  else






!  ####################################################
!    'puffin' elliptical undulator...
!    with variable x and y polarization...


    if (self%iUndPlace == iUndStart) then

!$OMP WORKSHARE
      byj = self%uy * szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

    else if (self%iUndPlace == iUndEnd) then

      szt = sZ - sZFE
      szt = szt / 2_wp / rho

!$OMP WORKSHARE
      byj = self%uy * (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (self%iUndPlace == iUndMain) then

!$OMP WORKSHARE
      byj = self%uy * sin(szt)
!$OMP END WORKSHARE

    end if

!    END elliptical undulator description
!  ####################################################




  end if

!   Focusing component (non-physical)

    if (self%qFocussing) then

!$OMP WORKSHARE
      byj = -sqrt(tScaling%eta) * self%kby**2.0_wp / tScaling%kappa &
            * sx + byj
!$OMP END WORKSHARE

    end if

  end subroutine getBYfield



!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Calculate the bx magnetic fields at specified x, y, and z coords.
!> @param[in] tScaling Fortran type describing system scaling.
!> @param[in] sx Macroparticle coordinates in xbar.
!> @param[in] sy Macroparticle coordinates in ybar.
!> @param[in] sz current scaled distance though undulator, zbar.
!> @param[out] bxj scaled b-field in x direction for given positions.


subroutine getBZfield(self, tScaling, sx, sy, sz, bzj)

  use typeScale

  class(undFields), intent(in) :: self
  type(fScale), intent(in) :: tScaling
  real(kind=wp), contiguous, intent(in) :: sx(:), sy(:)
  real(kind=wp), intent(in) :: sz
  real(kind=wp), contiguous, intent(out) :: bzj(:)

!    Local vars:-

  real(kind=wp) :: szt, rho, alph

  rho = tScaling%rho
  alph = self%n2col

  szt = sZ
  szt = szt / 2_wp / rho


  if (self%qOneD) then

!  ####################################################
!    1D case - no z-component of magnetic field

!$OMP WORKSHARE
    bzj = 0_wp
!$OMP END WORKSHARE

  else







!  ####################################################
!    Curved pole case - planar wiggler with focusing
!    in both x and y (electron wiggles in x)


    if (self%zUndType == 'curved') then

      if (self%iUndPlace == iUndStart) then

!$OMP WORKSHARE
        bzj = sqrt(tScaling%eta) / 2 / rho / self%kuy &
                  * cosh(self%kux * sx) &
              * sinh(self%kuy * sy) &
              * (szt / 4_wp / pi) * cos(szt) * alph
!$OMP END WORKSHARE

      else if (self%iUndPlace == iUndEnd) then

        szt = sZ - sZFE
        szt = szt / 2_wp / rho

!$OMP WORKSHARE
        bzj = sqrt(tScaling%eta) / 2_wp / rho / self%kux &
              * cosh(self%kux * sx) * sinh(self%kuy * sy) &
              * (-szt / 4_wp / pi + 1_wp) * cos(szt) * alph
!$OMP END WORKSHARE

      else if (self%iUndPlace == iUndMain) then


!$OMP WORKSHARE
        bzj = sqrt(tScaling%eta) / 2_wp / rho / self%kux * &
             cosh(self%kux * sx) * sinh(self%kuy * sy) &
              * cos(szt) * alph
!$OMP END WORKSHARE

      end if

!    END curved pole field description
!  ####################################################






!  ####################################################
!    Plane-pole case - planar wiggler with focusing
!    only in y (and electron will wiggle in x)

    else if (self%zUndType == 'planepole')  then

      if (self%iUndPlace == iUndStart) then

!$OMP WORKSHARE
        bzj = sinh( sqrt(tScaling%eta) / 2_wp / rho * sy) * &
              (szt / 4_wp / pi) * cos(szt) * alph
!$OMP END WORKSHARE

      else if (self%iUndPlace == iUndEnd) then

        szt = sZ - sZFE
        szt = szt / 2_wp / rho

!$OMP WORKSHARE
        bzj = sinh( sqrt(tScaling%eta) / 2_wp / rho * sy) * &
              (-szt / 4_wp / pi + 1_wp) * cos(szt) * alph
!$OMP END WORKSHARE

      else if (self%iUndPlace == iUndMain) then

!$OMP WORKSHARE
        bzj = sinh( sqrt(tScaling%eta) / 2_wp / rho * sy) &
              * cos(szt) * alph
!$OMP END WORKSHARE

      end if

!    END plane pole undulator field description
!  ####################################################






!  ####################################################
!    Helical case - helical wiggler with focusing
!    in x and y (and electron will wiggle in x and y)

    else if (self%zUndType == 'helical')  then

      if (self%iUndPlace == iUndStart) then

! ...from x-comp:

!$OMP WORKSHARE
        bzj = - sqrt(tScaling%eta) / 2 / rho * (sZ - pi * rho) / (6_wp * pi*rho) &
              * sx * sin(szt) * alph
!$OMP END WORKSHARE

        if (sZ < pi * rho) then 
!$OMP WORKSHARE
          bzj = 0.0_wp
!$OMP END WORKSHARE
        else if (sZ > 7_wp * pi * rho) then
!$OMP WORKSHARE
          bzj = - sqrt(tScaling%eta) / 2 / rho * sx * sin(szt) * alph
!$OMP END WORKSHARE
        end if
      

! ...and from y-comp:

!$OMP WORKSHARE
        bzj = bzj + szt / 4_wp / pi * sqrt(tScaling%eta) / 2 / rho * sy * cos(szt) * alph
!$OMP END WORKSHARE

!  !$OMP WORKSHARE
!        bzj = sqrt(tScaling%eta) / 2 / rho * (     &
!              sx *  sin(szt) )    + &
!              sy * ( -1/32_wp * cos(szt/4_wp) * cos(szt) + &
!                      1/4_wp * sin(szt/4_wp) * sin(szt) + &
!                      sin(szt/8_wp)**2 * cos(szt) ) )
!  !$OMP END WORKSHARE

      else if (self%iUndPlace == iUndEnd) then

        szt = sZ - sZFE
      !szt = szt / 2_wp / rho

! ...from x-comp:

!$OMP WORKSHARE
        bzj = - sqrt(tScaling%eta) / 2 / rho * (szt - 7.0_wp * pi * rho) / &
                (6_wp * pi * rho) * sx * sin(szt / 2_wp / rho) * alph
!$OMP END WORKSHARE

        if (szt < pi * rho) then
!$OMP WORKSHARE
          bzj = - sqrt(tScaling%eta) / 2 / rho * sx * sin(szt / 2_wp / rho) * alph
!$OMP END WORKSHARE
        else if (szt > 7_wp * pi * rho) then
!$OMP WORKSHARE
          bzj = 0.0_wp
!$OMP END WORKSHARE
        end if


! ...and from y-comp:

!$OMP WORKSHARE
        bzj = bzj + (-szt / 8_wp / pi / rho + 1_wp) * &
              sqrt(tScaling%eta) / 2 / rho * sy * cos(szt / 2_wp / rho) * alph
!$OMP END WORKSHARE

      else if (self%iUndPlace == iUndMain) then

!$OMP WORKSHARE
        bzj = sqrt(tScaling%eta) / 2 / rho * &
              ( -sx * sin(szt)  + sy * cos(szt) ) * alph
!$OMP END WORKSHARE

      end if

!    END helical undulator field description
!  ####################################################

    else

!  ####################################################
!    'puffin' elliptical undulator...
!    with variable x and y polarization...

      if (self%iUndPlace == iUndStart) then

! ...from x-comp:

!$OMP WORKSHARE
        bzj = - sqrt(tScaling%eta) / 2 / rho * (sZ - pi * rho) / (6_wp * pi*rho) &
              * self%ux * sx * sin(szt) * alph
!$OMP END WORKSHARE

        if (sZ < pi * rho) then 
!$OMP WORKSHARE
          bzj = 0.0_wp
!$OMP END WORKSHARE
        else if (sZ > 7_wp * pi * rho) then
!$OMP WORKSHARE
          bzj = - sqrt(tScaling%eta) / 2 / rho * self%ux * sx * sin(szt) * alph
!$OMP END WORKSHARE
        end if


! ...and from y-comp:

!$OMP WORKSHARE
        bzj = bzj + szt / 4_wp / pi * sqrt(tScaling%eta) / 2 / rho * self%uy * sy * &
                cos(szt) * alph
!$OMP END WORKSHARE


      else if (self%iUndPlace == iUndEnd) then

        szt = sZ - sZFE
!      szt = szt / 2_wp / rho

! ...from x-comp:

!$OMP WORKSHARE
        bzj = - sqrt(tScaling%eta) / 2 / rho * (szt - 7.0_wp * pi * rho) / &
                (6_wp * pi * rho) * self%ux * sx * sin(szt / 2_wp / rho) * alph
!$OMP END WORKSHARE

        if (szt < pi * rho) then 
!$OMP WORKSHARE
          bzj = - sqrt(tScaling%eta) / 2 / rho * self%ux * sx * sin(szt / 2_wp / rho) &
                * alph
!$OMP END WORKSHARE
        else if (szt > 7_wp * pi * rho) then
!$OMP WORKSHARE
          bzj = 0.0_wp
!$OMP END WORKSHARE
        end if
      

! ...and from y-comp:

!$OMP WORKSHARE
        bzj = bzj + (-szt / 8_wp / pi / rho + 1_wp) * &
              sqrt(tScaling%eta) / 2 / rho * self%uy * sy * cos(szt / 2_wp / rho) * alph
!$OMP END WORKSHARE


      else if (self%iUndPlace == iUndMain) then

!$OMP WORKSHARE
        bzj = sqrt(tScaling%eta) / 2 / rho * &
              ( -self%ux*sx * sin(szt)  + self%uy*sy * cos(szt) ) * alph
!$OMP END WORKSHARE

      end if

!    END elliptical undulator description
!  ####################################################

    end if

  end if

end subroutine getBZfield


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Update the undulator tuning at given (local to this element) z coord.
!> @param[in] sz current scaled distance though undulator, zbar.

subroutine getAlpha(self, sZ)

  class(undFields), intent(inout) :: self
  real(kind=wp), intent(in) :: sZ

  if ((sZ >= self%sZFS) .and. (sZ <= self%sZFE)) then

    self%n2col = self%n2col0  + self%taper*(sz - self%sZFS)  ! simple linear taper

  else if (sz < self%sZFS) then
    
    self%n2col = self%n2col0

  else if (sZ > self%sZFE) then

    self%n2col0 = self%n2col

  end if

end subroutine getAlpha

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Update whether we are in the 'main' undulator section, or if we are
!> in the ends (front or back).
!> @param[in] szl current scaled distance though undulator, zbar.

subroutine adjUndPlace(self, szl)
    
  class(undFields), intent(inout) :: self
  real(kind=wp) :: szl

    if (self%qUndEnds) then

      if (szl < 0_wp) then

        print*, 'undulator section not recognised, sz < 0!!'
        stop

      else if ((sZl <= self%sZFS) .and. (szl >= 0_wp) ) then 

        self%iUndPlace = iUndStart

      else if (sZl >= sZFE) then
 
        self%iUndPlace = iUndEnd

      else if ((sZl > sZFS) .and. (sZl < sZFE)) then

        self%iUndPlace = iUndMain

      else 

        print*, 'undulator section not recognised, sz > sZFE!!'
        print*, 'sZl = ', szl
        stop

      end if

    else

      self%iUndPlace = iUndMain

    end if

end subroutine adjUndPlace



end module bfields
