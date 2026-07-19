! ################################################
! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause
! ################################################
!
!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Module containing the routines which calculate the scaled magnetic b-fields
!> for the Lorentz force in Puffin


module bfields

use puffin_kinds, only: WP
use globals, only: iUndStart_G, iUndEnd_G, iUndMain_G, qFocussing_G, qOneD_G, pi
use GlobalTypes, only: tUndulator, tFELFrame

implicit none (type, external)
private

public :: getbfields


contains

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Calculate the b-fields. Calls bx, by and bz subroutines
!> @param[in] sx Macroparticle coordinates in xbar
!> @param[in] sy Macroparticle coordinates in ybar
!> @param[in] sz current scaled distance though undulator, zbar
!> @param[out] bxj scaled b-field in x direction for macroparticles
!> @param[out] byj scaled b-field in y direction for macroparticles
!> @param[out] bzj scaled b-field in z direction for macroparticles
!> @param[in] und undulator parameters (type fields replace globals)
!> @param[in] frame FEL frame parameters (rho, eta, kappa etc.)

  subroutine getBFields(sx, sy, sZ, &
                        bxj, byj, bzj, und, frame)

!   subroutine to calculate the scaled magnetic fields
!   at a given zbar

  real(kind=wp), contiguous, intent(in) :: sx(:), sy(:)
  real(kind=wp), intent(in) :: sz

  real(kind=wp), contiguous, intent(out) :: bxj(:), byj(:), bzj(:)
  type(tUndulator), intent(in) :: und
  type(tFELFrame), intent(in) :: frame

  call getBXfield(sx, sy, sz, bxj, und, frame)
  call getBYfield(sx, sy, sz, byj, und, frame)
  call getBZfield(sx, sy, sz, bzj, und, frame)

  end subroutine getBFields




subroutine getBXfield(sx, sy, sz, bxj, und, frame)

  real(kind=wp), contiguous, intent(in) :: sx(:), sy(:)
  real(kind=wp), intent(in) :: sz
  real(kind=wp), contiguous, intent(out) :: bxj(:)
  type(tUndulator), intent(in) :: und
  type(tFELFrame), intent(in) :: frame

!    Local vars:-

  real(kind=wp) :: szt

  szt = sZ
  szt = szt / 2_wp / frame%rho


!  ####################################################
!    Curved pole case - planar wiggler with focusing
!    in both x and y (electron wiggles in x)

  if (und%undulator_type == "curved") then

    if (und%undulator_position == iUndStart_G) then

!$OMP WORKSHARE
      bxj = und%kx_undulator / und%ky_undulator * sinh(und%kx_undulator * sx) &
            * sinh(und%ky_undulator * sy) &
            * szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndEnd_G) then

      szt = sZ - und%z_end_undulator
      szt = szt / 2_wp / frame%rho

!$OMP WORKSHARE
      bxj = und%kx_undulator / und%ky_undulator * sinh(und%kx_undulator * sx) &
            * sinh(und%ky_undulator * sy) &
            * (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndMain_G) then

!$OMP WORKSHARE
      bxj = und%kx_undulator / und%ky_undulator * sinh(und%kx_undulator * sx) &
            * sinh(und%ky_undulator * sy) &
            * sin(szt)
!$OMP END WORKSHARE

    end if

!    END curved pole field description
!  ####################################################



!  ####################################################
!    Plane-pole case - planar wiggler with focusing
!    only in y (and electron will wiggle in x)

  else if (und%undulator_type == "planepole")  then

    if (und%undulator_position == iUndStart_G) then

!$OMP WORKSHARE
      bxj = 0_wp
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndEnd_G) then

      szt = sZ - und%z_end_undulator
      szt = szt / 2_wp / frame%rho

!$OMP WORKSHARE
      bxj = 0_wp
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndMain_G) then

!$OMP WORKSHARE
      bxj = 0_wp
!$OMP END WORKSHARE

    end if

!    END plane pole undulator field description
!  ####################################################



!  ####################################################
!    Helical case - helical wiggler with focusing
!    in x and y (and electron will wiggle in x and y)

  else if (und%undulator_type == "helical")  then

    if (und%undulator_position == iUndStart_G) then

!$OMP WORKSHARE
      bxj = (sZ - pi * frame%rho) / (6_wp * pi*frame%rho) * cos(szt)
!$OMP END WORKSHARE

      if (sZ < pi * frame%rho) then
!$OMP WORKSHARE
        bxj = 0.0_wp
!$OMP END WORKSHARE
      else if (sZ > 7_wp * pi * frame%rho) then
!$OMP WORKSHARE
        bxj = cos(szt)
!$OMP END WORKSHARE
      end if

    else if (und%undulator_position == iUndEnd_G) then

      szt = sZ - und%z_end_undulator

!$OMP WORKSHARE
      bxj =  - (szt - 7.0_wp * pi * frame%rho) / (6_wp * pi * frame%rho) * &
               cos(szt / 2_wp / frame%rho)
!$OMP END WORKSHARE

      if (sZt < pi * frame%rho) then
!$OMP WORKSHARE
        bxj = cos(szt / 2_wp / frame%rho)
!$OMP END WORKSHARE
      else if (sZt > 7_wp * pi * frame%rho) then
!$OMP WORKSHARE
        bxj = 0.0_wp
!$OMP END WORKSHARE
      end if

    else if (und%undulator_position == iUndMain_G) then

!$OMP WORKSHARE
      bxj = cos(szt)
!$OMP END WORKSHARE

    end if

!    END helical undulator field description
!  ####################################################


  else


!  ####################################################
!    'puffin' elliptical undulator...
!    with variable x and y polarization...

    if (und%undulator_position == iUndStart_G) then

!$OMP WORKSHARE
      bxj = und%fx * (sZ - pi * frame%rho) / (6_wp * pi*frame%rho) * cos(szt)
!$OMP END WORKSHARE

      if (sZ < pi * frame%rho) then
!$OMP WORKSHARE
        bxj = 0.0_wp
!$OMP END WORKSHARE
      else if (sZ > 7_wp * pi * frame%rho) then
!$OMP WORKSHARE
        bxj = und%fx * cos(szt)
!$OMP END WORKSHARE
      end if

    else if (und%undulator_position == iUndEnd_G) then

      szt = sZ - und%z_end_undulator

!$OMP WORKSHARE
      bxj =  -und%fx * (szt - 7.0_wp * pi * frame%rho) / (6_wp * pi * frame%rho) * &
               cos(szt / 2_wp / frame%rho)
!$OMP END WORKSHARE

      if (sZt < pi * frame%rho) then
!$OMP WORKSHARE
        bxj = und%fx * cos(szt / 2_wp / frame%rho)
!$OMP END WORKSHARE
      else if (sZt > 7_wp * pi * frame%rho) then
!$OMP WORKSHARE
        bxj = 0.0_wp
!$OMP END WORKSHARE
      end if


    else if (und%undulator_position == iUndMain_G) then

!$OMP WORKSHARE
      bxj = und%fx*cos(szt)
!$OMP END WORKSHARE

    end if

!    END elliptical undulator description
!  ####################################################


  end if


!   Focusing component (non-physical)

    if (qFocussing_G) then

!$OMP WORKSHARE
        bxj = sqrt(frame%eta) * und%k_beta_y_sf**2.0_wp / frame%kappa &
              * sy + bxj
!$OMP END WORKSHARE

    end if


  end subroutine getBXfield



  subroutine getBYfield(sx, sy, sz, byj, und, frame)

  real(kind=wp), contiguous, intent(in) :: sx(:), sy(:)
  real(kind=wp), intent(in) :: sz
  real(kind=wp), contiguous, intent(out) :: byj(:)
  type(tUndulator), intent(in) :: und
  type(tFELFrame), intent(in) :: frame

!    Local vars:-

  real(kind=wp) :: szt

  szt = sZ
  szt = szt / 2_wp / frame%rho


!  ####################################################
!    Curved pole case - planar wiggler with focusing
!    in both x and y (electron wiggles in x)


  if (und%undulator_type == "curved") then

    if (und%undulator_position == iUndStart_G) then

!$OMP WORKSHARE
      byj = cosh(und%kx_undulator * sx) &
            * cosh(und%ky_undulator * sy) &
            *  szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndEnd_G) then

      szt = sZ - und%z_end_undulator
      szt = szt / 2_wp / frame%rho

!$OMP WORKSHARE
      byj = cosh(und%kx_undulator * sx) &
            * cosh(und%ky_undulator * sy) &
            * (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndMain_G) then

!$OMP WORKSHARE
      byj = cosh(und%kx_undulator * sx) &
            * cosh(und%ky_undulator * sy) &
            * sin(szt)
!$OMP END WORKSHARE

    end if

!    END curved pole field description
!  ####################################################



!  ####################################################
!    Plane-pole case - planar wiggler with focusing
!    only in y (and electron will wiggle in x)



  else if (und%undulator_type == "planepole")  then

    if (und%undulator_position == iUndStart_G) then

!$OMP WORKSHARE
      byj = szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndEnd_G) then

      szt = sZ - und%z_end_undulator
      szt = szt / 2_wp / frame%rho

!$OMP WORKSHARE
      byj = (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndMain_G) then

!$OMP WORKSHARE
      byj = sin(szt)
!$OMP END WORKSHARE

    end if

!    END plane pole undulator field description
!  ####################################################




!  ####################################################
!    Helical case - helical wiggler with focusing
!    in x and y (and electron will wiggle in x and y)

  else if (und%undulator_type == "helical")  then

    if (und%undulator_position == iUndStart_G) then

!$OMP WORKSHARE
      byj = szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndEnd_G) then

      szt = sZ - und%z_end_undulator
      szt = szt / 2_wp / frame%rho

!$OMP WORKSHARE
      byj = (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndMain_G) then

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


    if (und%undulator_position == iUndStart_G) then

!$OMP WORKSHARE
      byj = und%fy * szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndEnd_G) then

      szt = sZ - und%z_end_undulator
      szt = szt / 2_wp / frame%rho

!$OMP WORKSHARE
      byj = und%fy * (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndMain_G) then

!$OMP WORKSHARE
      byj = und%fy * sin(szt)
!$OMP END WORKSHARE

    end if

!    END elliptical undulator description
!  ####################################################


  end if

!   Focusing component (non-physical)

    if (qFocussing_G) then

!$OMP WORKSHARE
      byj = -sqrt(frame%eta) * und%k_beta_x_sf**2.0_wp / frame%kappa &
            * sx + byj
!$OMP END WORKSHARE

    end if

  end subroutine getBYfield




subroutine getBZfield(sx, sy, sz, bzj, und, frame)

  real(kind=wp), contiguous, intent(in) :: sx(:), sy(:)
  real(kind=wp), intent(in) :: sz
  real(kind=wp), contiguous, intent(out) :: bzj(:)
  type(tUndulator), intent(in) :: und
  type(tFELFrame), intent(in) :: frame

!    Local vars:-

  real(kind=wp) :: szt



  szt = sZ
  szt = szt / 2_wp / frame%rho


  if (qOneD_G) then

!  ####################################################
!    1D case - no z-component of magnetic field

!$OMP WORKSHARE
    bzj = 0_wp
!$OMP END WORKSHARE

  else



!  ####################################################
!    Curved pole case - planar wiggler with focusing
!    in both x and y (electron wiggles in x)


  if (und%undulator_type == "curved") then

    if (und%undulator_position == iUndStart_G) then

!$OMP WORKSHARE
      bzj = sqrt(frame%eta) / 2 / frame%rho / und%ky_undulator &
                * cosh(und%kx_undulator * sx) &
            * sinh(und%ky_undulator * sy) &
            * (szt / 4_wp / pi) * cos(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndEnd_G) then

      szt = sZ - und%z_end_undulator
      szt = szt / 2_wp / frame%rho

!$OMP WORKSHARE
      bzj = sqrt(frame%eta) / 2_wp / frame%rho / und%kx_undulator &
            * cosh(und%kx_undulator * sx) * sinh(und%ky_undulator * sy) &
            * (-szt / 4_wp / pi + 1_wp) * cos(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndMain_G) then


!$OMP WORKSHARE
      bzj = sqrt(frame%eta) / 2_wp / frame%rho / und%kx_undulator * &
           cosh(und%kx_undulator * sx) * sinh(und%ky_undulator * sy) &
            * cos(szt)
!$OMP END WORKSHARE

    end if

!    END curved pole field description
!  ####################################################



!  ####################################################
!    Plane-pole case - planar wiggler with focusing
!    only in y (and electron will wiggle in x)

  else if (und%undulator_type == "planepole")  then

    if (und%undulator_position == iUndStart_G) then

!$OMP WORKSHARE
      bzj = sinh( sqrt(frame%eta) / 2_wp / frame%rho * sy) * &
            (szt / 4_wp / pi) * cos(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndEnd_G) then

      szt = sZ - und%z_end_undulator
      szt = szt / 2_wp / frame%rho

!$OMP WORKSHARE
      bzj = sinh( sqrt(frame%eta) / 2_wp / frame%rho * sy) * &
            (-szt / 4_wp / pi + 1_wp) * cos(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndMain_G) then

!$OMP WORKSHARE
      bzj = sinh( sqrt(frame%eta) / 2_wp / frame%rho * sy) &
            * cos(szt)
!$OMP END WORKSHARE

    end if

!    END plane pole undulator field description
!  ####################################################



!  ####################################################
!    Helical case - helical wiggler with focusing
!    in x and y (and electron will wiggle in x and y)

  else if (und%undulator_type == "helical")  then

    if (und%undulator_position == iUndStart_G) then

! ...from x-comp:

!$OMP WORKSHARE
      bzj = - sqrt(frame%eta) / 2 / frame%rho * (sZ - pi * frame%rho) / (6_wp * pi*frame%rho) &
            * sx * sin(szt)
!$OMP END WORKSHARE

      if (sZ < pi * frame%rho) then
!$OMP WORKSHARE
        bzj = 0.0_wp
!$OMP END WORKSHARE
      else if (sZ > 7_wp * pi * frame%rho) then
!$OMP WORKSHARE
        bzj = - sqrt(frame%eta) / 2 / frame%rho * sx * sin(szt)
!$OMP END WORKSHARE
      end if


! ...and from y-comp:

!$OMP WORKSHARE
      bzj = bzj + szt / 4_wp / pi * sqrt(frame%eta) / 2 / frame%rho * sy * cos(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndEnd_G) then

      szt = sZ - und%z_end_undulator

! ...from x-comp:

!$OMP WORKSHARE
      bzj = - sqrt(frame%eta) / 2 / frame%rho * (szt - 7.0_wp * pi * frame%rho) / &
              (6_wp * pi * frame%rho) * sx * sin(szt / 2_wp / frame%rho)
!$OMP END WORKSHARE

      if (szt < pi * frame%rho) then
!$OMP WORKSHARE
        bzj = - sqrt(frame%eta) / 2 / frame%rho * sx * sin(szt / 2_wp / frame%rho)
!$OMP END WORKSHARE
      else if (szt > 7_wp * pi * frame%rho) then
!$OMP WORKSHARE
        bzj = 0.0_wp
!$OMP END WORKSHARE
      end if


! ...and from y-comp:

!$OMP WORKSHARE
      bzj = bzj + (-szt / 8_wp / pi / frame%rho + 1_wp) * &
            sqrt(frame%eta) / 2 / frame%rho * sy * cos(szt / 2_wp / frame%rho)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndMain_G) then

!$OMP WORKSHARE
      bzj = sqrt(frame%eta) / 2 / frame%rho * &
            ( -sx * sin(szt)  + sy * cos(szt) )
!$OMP END WORKSHARE

    end if

!    END helical undulator field description
!  ####################################################


  else



!  ####################################################
!    'puffin' elliptical undulator...
!    with variable x and y polarization...

    if (und%undulator_position == iUndStart_G) then

! ...from x-comp:

!$OMP WORKSHARE
      bzj = - sqrt(frame%eta) / 2 / frame%rho * (sZ - pi * frame%rho) / (6_wp * pi*frame%rho) &
            * und%fx * sx * sin(szt)
!$OMP END WORKSHARE

      if (sZ < pi * frame%rho) then
!$OMP WORKSHARE
        bzj = 0.0_wp
!$OMP END WORKSHARE
      else if (sZ > 7_wp * pi * frame%rho) then
!$OMP WORKSHARE
        bzj = - sqrt(frame%eta) / 2 / frame%rho * und%fx * sx * sin(szt)
!$OMP END WORKSHARE
      end if


! ...and from y-comp:

!$OMP WORKSHARE
      bzj = bzj + szt / 4_wp / pi * sqrt(frame%eta) / 2 / frame%rho * und%fy * sy * cos(szt)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndEnd_G) then

      szt = sZ - und%z_end_undulator

! ...from x-comp:

!$OMP WORKSHARE
      bzj = - sqrt(frame%eta) / 2 / frame%rho * (szt - 7.0_wp * pi * frame%rho) / &
              (6_wp * pi * frame%rho) * und%fx * sx * sin(szt / 2_wp / frame%rho)
!$OMP END WORKSHARE

      if (szt < pi * frame%rho) then
!$OMP WORKSHARE
        bzj = - sqrt(frame%eta) / 2 / frame%rho * und%fx * sx * sin(szt / 2_wp / frame%rho)
!$OMP END WORKSHARE
      else if (szt > 7_wp * pi * frame%rho) then
!$OMP WORKSHARE
        bzj = 0.0_wp
!$OMP END WORKSHARE
      end if


! ...and from y-comp:

!$OMP WORKSHARE
      bzj = bzj + (-szt / 8_wp / pi / frame%rho + 1_wp) * &
            sqrt(frame%eta) / 2 / frame%rho * und%fy * sy * cos(szt / 2_wp / frame%rho)
!$OMP END WORKSHARE

    else if (und%undulator_position == iUndMain_G) then

!$OMP WORKSHARE
      bzj = sqrt(frame%eta) / 2 / frame%rho * &
            ( -und%fx*sx * sin(szt)  + und%fy*sy * cos(szt) )
!$OMP END WORKSHARE

    end if

!    END elliptical undulator description
!  ####################################################


  end if

  end if

end subroutine getBZfield

end module bfields
