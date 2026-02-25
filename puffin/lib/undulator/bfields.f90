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

use puffin_kinds
use globals

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

  subroutine getBFields(sx, sy, sZ, &
                        bxj, byj, bzj)

!   subroutine to calculate the scaled magnetic fields
!   at a given zbar

  real(kind=wp), contiguous, intent(in) :: sx(:), sy(:)
  real(kind=wp), intent(in) :: sz

  real(kind=wp), contiguous, intent(out) :: bxj(:), byj(:), bzj(:)

  call getBXfield(sx, sy, sz, bxj)
  call getBYfield(sx, sy, sz, byj)
  call getBZfield(sx, sy, sz, bzj)

  end subroutine getBFields







subroutine getBXfield(sx, sy, sz, bxj)

  real(kind=wp), contiguous, intent(in) :: sx(:), sy(:)
  real(kind=wp), intent(in) :: sz
  real(kind=wp), contiguous, intent(out) :: bxj(:)

!    Local vars:-

  real(kind=wp) :: szt

!      cc1 = sqrt(sEta_G) / (2_wp * sRho_G * ky_und_G)




  szt = sZ
  szt = szt / 2_wp / sRho_G


!  ####################################################
!    Curved pole case - planar wiggler with focusing
!    in both x and y (electron wiggles in x)

  if (zUndType_G == 'curved') then

    if (iUndPlace_G == iUndStart_G) then

!$OMP WORKSHARE
      bxj = kx_und_G / ky_und_G * sinh(kx_und_G * sx) &
            * sinh(ky_und_G * sy) &
            * szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndEnd_G) then

      szt = sZ - sZFE
      szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE
      bxj = kx_und_G / ky_und_G * sinh(kx_und_G * sx) &
            * sinh(ky_und_G * sy) &
            * (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE
      bxj = kx_und_G / ky_und_G * sinh(kx_und_G * sx) &
            * sinh(ky_und_G * sy) &
            * sin(szt)
!$OMP END WORKSHARE

    end if

!    END curved pole field description
!  ####################################################








!  ####################################################
!    Plane-pole case - planar wiggler with focusing
!    only in y (and electron will wiggle in x)

  else if (zUndType_G == 'planepole')  then

    if (iUndPlace_G == iUndStart_G) then

!$OMP WORKSHARE
      bxj = 0_wp
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndEnd_G) then

      szt = sZ - sZFE
      szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE
      bxj = 0_wp
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE
      bxj = 0_wp
!$OMP END WORKSHARE

    end if

!    END plane pole undulator field description
!  ####################################################







!  ####################################################
!    Helical case - helical wiggler with focusing
!    in x and y (and electron will wiggle in x and y)

  else if (zUndType_G == 'helical')  then

    if (iUndPlace_G == iUndStart_G) then


!      bxj = sin(szt / 8_wp) * &
!               cos(szt / 8_wp) * sin(szt) / 4_wp   &
!             +  sin(szt/8_wp)**2_wp  * cos(szt)
!$OMP WORKSHARE
      bxj = (sZ - pi * sRho_G) / (6_wp * pi*sRho_G) * cos(szt)
!$OMP END WORKSHARE

      if (sZ < pi * sRho_G) then 
!$OMP WORKSHARE
        bxj = 0.0_wp
!$OMP END WORKSHARE
      else if (sZ > 7_wp * pi * sRho_G) then
!$OMP WORKSHARE
        bxj = cos(szt)
!$OMP END WORKSHARE
      end if

    else if (iUndPlace_G == iUndEnd_G) then

      szt = sZ - sZFE
!      szt = szt / 2_wp / sRho_G


!      bxj = - cos(szt / 8_wp) * &
!              sin(szt / 8_wp) * sin(szt)  / 4_wp  &
!            +  cos(szt/8_wp)**2_wp  * cos(szt)

!$OMP WORKSHARE
      bxj =  - (szt - 7.0_wp * pi * sRho_G) / (6_wp * pi * sRho_G) * &
               cos(szt / 2_wp / sRho_G)
!$OMP END WORKSHARE

      if (sZt < pi * sRho_G) then 
!$OMP WORKSHARE
        bxj = cos(szt / 2_wp / sRho_G)
!$OMP END WORKSHARE
      else if (sZt > 7_wp * pi * sRho_G) then
!$OMP WORKSHARE
        bxj = 0.0_wp
!$OMP END WORKSHARE
      end if

    else if (iUndPlace_G == iUndMain_G) then

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

    if (iUndPlace_G == iUndStart_G) then


!      bxj = fx_G * sin(szt / 8_wp) * &
!               cos(szt / 8_wp) * sin(szt) / 4_wp   &
!             +  sin(szt/8_wp)**2_wp  * cos(szt)
!$OMP WORKSHARE
      bxj = fx_G * (sZ - pi * sRho_G) / (6_wp * pi*sRho_G) * cos(szt)
!$OMP END WORKSHARE

      if (sZ < pi * sRho_G) then 
!$OMP WORKSHARE
        bxj = 0.0_wp
!$OMP END WORKSHARE
      else if (sZ > 7_wp * pi * sRho_G) then
!$OMP WORKSHARE
        bxj = fx_G * cos(szt)
!$OMP END WORKSHARE
      end if

    else if (iUndPlace_G == iUndEnd_G) then

      szt = sZ - sZFE
      !szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE
      bxj =  -fx_G * (szt - 7.0_wp * pi * sRho_G) / (6_wp * pi * sRho_G) * &
               cos(szt / 2_wp / sRho_G)
!$OMP END WORKSHARE

      if (sZt < pi * sRho_G) then 
!$OMP WORKSHARE
        bxj = fx_G * cos(szt / 2_wp / sRho_G)
!$OMP END WORKSHARE
      else if (sZt > 7_wp * pi * sRho_G) then
!$OMP WORKSHARE
        bxj = 0.0_wp
!$OMP END WORKSHARE
      end if
      

    else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE
      bxj = fx_G*cos(szt)
!$OMP END WORKSHARE

    end if

!    END elliptical undulator description
!  ####################################################




  end if


!   Focusing component (non-physical)

    if (qFocussing_G) then

!$OMP WORKSHARE
        bxj = sqrt(sEta_G) * sKBetaYSF_G**2.0_wp / sKappa_G &
              * sy + bxj
!$OMP END WORKSHARE

    end if


  end subroutine getBXfield



  subroutine getBYfield(sx, sy, sz, byj)

  real(kind=wp), contiguous, intent(in) :: sx(:), sy(:)
  real(kind=wp), intent(in) :: sz
  real(kind=wp), contiguous, intent(out) :: byj(:)

!    Local vars:-

  real(kind=wp) :: szt

  szt = sZ
  szt = szt / 2_wp / sRho_G


!  ####################################################
!    Curved pole case - planar wiggler with focusing
!    in both x and y (electron wiggles in x)


  if (zUndType_G == 'curved') then

    if (iUndPlace_G == iUndStart_G) then

!$OMP WORKSHARE
      byj = cosh(kx_und_G * sx) &
            * cosh(ky_und_G * sy) &
            *  szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndEnd_G) then

      szt = sZ - sZFE
      szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE
      byj = cosh(kx_und_G * sx) &
            * cosh(ky_und_G * sy) &
            * (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE
      byj = cosh(kx_und_G * sx) &
            * cosh(ky_und_G * sy) &
            * sin(szt)
!$OMP END WORKSHARE

    end if

!    END curved pole field description
!  ####################################################







!  ####################################################
!    Plane-pole case - planar wiggler with focusing
!    only in y (and electron will wiggle in x)



  else if (zUndType_G == 'planepole')  then

    if (iUndPlace_G == iUndStart_G) then

!$OMP WORKSHARE
!      byj = cosh( sqrt(sEta_G) / 2_wp / sRho_G * sy) * &
!            (  (- sin(szt / 8_wp) * &
!               cos(szt / 8_wp) * cos(szt) / 4_wp   &
!             +  sin(szt/8_wp)**2_wp  * sin(szt) )  )


      !byj = sin(szt/8_wp)**2_wp * sin(szt)
      byj = szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

!    print*, "hehehe"

    else if (iUndPlace_G == iUndEnd_G) then

      szt = sZ - sZFE
      szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE
!      byj = cosh( sqrt(sEta_G) / 2_wp / sRho_G * sy) * &
!            (  cos(szt / 8_wp) * &
!              sin(szt / 8_wp) * cos(szt)  / 4_wp  &
!            +  cos(szt/8_wp)**2_wp  * sin(szt)  )
            
      byj = (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE
!      byj = cosh( sqrt(sEta_G) / 2_wp / sRho_G * sy) &
!            * sin(szt)
      byj = sin(szt)
!$OMP END WORKSHARE

    end if

!    END plane pole undulator field description
!  ####################################################









!  ####################################################
!    Helical case - helical wiggler with focusing
!    in x and y (and electron will wiggle in x and y)

  else if (zUndType_G == 'helical')  then

    if (iUndPlace_G == iUndStart_G) then

!$OMP WORKSHARE
      byj = szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndEnd_G) then

      szt = sZ - sZFE
      szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE
      byj = (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndMain_G) then

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


    if (iUndPlace_G == iUndStart_G) then

!$OMP WORKSHARE
      byj = fy_G * szt / 4_wp / pi * sin(szt)
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndEnd_G) then

      szt = sZ - sZFE
      szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE
      byj = fy_G * (-szt / 4_wp / pi + 1_wp) * sin(szt)
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE
      byj = fy_G * sin(szt)
!$OMP END WORKSHARE

    end if

!    END elliptical undulator description
!  ####################################################




  end if

!   Focusing component (non-physical)

    if (qFocussing_G) then

!$OMP WORKSHARE
      byj = -sqrt(sEta_G) * sKBetaXSF_G**2.0_wp / sKappa_G &
            * sx + byj
!$OMP END WORKSHARE

    end if

  end subroutine getBYfield




subroutine getBZfield(sx, sy, sz, bzj)

  real(kind=wp), contiguous, intent(in) :: sx(:), sy(:)
  real(kind=wp), intent(in) :: sz
  real(kind=wp), contiguous, intent(out) :: bzj(:)

!    Local vars:-

  real(kind=wp) :: szt



  szt = sZ
  szt = szt / 2_wp / sRho_G


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


  if (zUndType_G == 'curved') then

    if (iUndPlace_G == iUndStart_G) then

!$OMP WORKSHARE
      bzj = sqrt(sEta_G) / 2 / sRho_G / ky_und_G &
                * cosh(kx_und_G * sx) &
            * sinh(ky_und_G * sy) &
            * (szt / 4_wp / pi) * cos(szt)
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndEnd_G) then

      szt = sZ - sZFE
      szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE
      bzj = sqrt(sEta_G) / 2_wp / sRho_G / kx_und_G &
            * cosh(kx_und_G * sx) * sinh(ky_und_G * sy) &
            * (-szt / 4_wp / pi + 1_wp) * cos(szt)
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndMain_G) then


!$OMP WORKSHARE
      bzj = sqrt(sEta_G) / 2_wp / sRho_G / kx_und_G * &
           cosh(kx_und_G * sx) * sinh(ky_und_G * sy) &
            * cos(szt)
!$OMP END WORKSHARE

    end if

!    END curved pole field description
!  ####################################################






!  ####################################################
!    Plane-pole case - planar wiggler with focusing
!    only in y (and electron will wiggle in x)

  else if (zUndType_G == 'planepole')  then

    if (iUndPlace_G == iUndStart_G) then

!$OMP WORKSHARE
      bzj = sinh( sqrt(sEta_G) / 2_wp / sRho_G * sy) * &
            (szt / 4_wp / pi) * cos(szt)
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndEnd_G) then

      szt = sZ - sZFE
      szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE
      bzj = sinh( sqrt(sEta_G) / 2_wp / sRho_G * sy) * &
            (-szt / 4_wp / pi + 1_wp) * cos(szt) 
!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE
      bzj = sinh( sqrt(sEta_G) / 2_wp / sRho_G * sy) &
            * cos(szt)
!$OMP END WORKSHARE

    end if

!    END plane pole undulator field description
!  ####################################################






!  ####################################################
!    Helical case - helical wiggler with focusing
!    in x and y (and electron will wiggle in x and y)

  else if (zUndType_G == 'helical')  then

    if (iUndPlace_G == iUndStart_G) then

! ...from x-comp:

!$OMP WORKSHARE
      bzj = - sqrt(sEta_G) / 2 / sRho_G * (sZ - pi * sRho_G) / (6_wp * pi*sRho_G) &
            * sx * sin(szt)            
!$OMP END WORKSHARE

      if (sZ < pi * sRho_G) then 
!$OMP WORKSHARE
        bzj = 0.0_wp
!$OMP END WORKSHARE
      else if (sZ > 7_wp * pi * sRho_G) then
!$OMP WORKSHARE
        bzj = - sqrt(sEta_G) / 2 / sRho_G * sx * sin(szt)
!$OMP END WORKSHARE
      end if
      

! ...and from y-comp:

!$OMP WORKSHARE
      bzj = bzj + szt / 4_wp / pi * sqrt(sEta_G) / 2 / sRho_G * sy * cos(szt)
!$OMP END WORKSHARE

!  !$OMP WORKSHARE
!        bzj = sqrt(sEta_G) / 2 / sRho_G * (     &
!              sx *  sin(szt) )    + &
!              sy * ( -1/32_wp * cos(szt/4_wp) * cos(szt) + &
!                      1/4_wp * sin(szt/4_wp) * sin(szt) + &
!                      sin(szt/8_wp)**2 * cos(szt) ) )
!  !$OMP END WORKSHARE

    else if (iUndPlace_G == iUndEnd_G) then

      szt = sZ - sZFE
      !szt = szt / 2_wp / sRho_G

! ...from x-comp:

!$OMP WORKSHARE
      bzj = - sqrt(sEta_G) / 2 / sRho_G * (szt - 7.0_wp * pi * sRho_G) / &
              (6_wp * pi * sRho_G) * sx * sin(szt / 2_wp / sRho_G)            
!$OMP END WORKSHARE

      if (szt < pi * sRho_G) then 
!$OMP WORKSHARE
        bzj = - sqrt(sEta_G) / 2 / sRho_G * sx * sin(szt / 2_wp / sRho_G)
!$OMP END WORKSHARE
      else if (szt > 7_wp * pi * sRho_G) then
!$OMP WORKSHARE
        bzj = 0.0_wp
!$OMP END WORKSHARE
      end if
      

! ...and from y-comp:

!$OMP WORKSHARE
      bzj = bzj + (-szt / 8_wp / pi / sRho_G + 1_wp) * &
            sqrt(sEta_G) / 2 / sRho_G * sy * cos(szt / 2_wp / sRho_G)
!$OMP END WORKSHARE












! !$OMP WORKSHARE
!       bzj = sqrt(sEta_G) / 2 / sRho_G * (     &
!             sx * ( -1/32_wp * cos(szt/4_wp) * sin(szt) - &
!                     1/4_wp * sin(szt/4_wp) * cos(szt) - &
!                     cos(szt/8_wp)**2 * sin(szt) )    + &
!             sy * ( 1/32_wp * cos(szt/4_wp) * cos(szt) - &
!                     1/4_wp * sin(szt/4_wp) * sin(szt) + &
!                     cos(szt/8_wp)**2 * cos(szt) ) )
! !$OMP END WORKSHARE

    else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE
      bzj = sqrt(sEta_G) / 2 / sRho_G * &
            ( -sx * sin(szt)  + sy * cos(szt) )
!$OMP END WORKSHARE

    end if

!    END helical undulator field description
!  ####################################################





  else




!  ####################################################
!    'puffin' elliptical undulator...
!    with variable x and y polarization...

    if (iUndPlace_G == iUndStart_G) then

! ...from x-comp:

!$OMP WORKSHARE
      bzj = - sqrt(sEta_G) / 2 / sRho_G * (sZ - pi * sRho_G) / (6_wp * pi*sRho_G) &
            * fx_G * sx * sin(szt)            
!$OMP END WORKSHARE

      if (sZ < pi * sRho_G) then 
!$OMP WORKSHARE
        bzj = 0.0_wp
!$OMP END WORKSHARE
      else if (sZ > 7_wp * pi * sRho_G) then
!$OMP WORKSHARE
        bzj = - sqrt(sEta_G) / 2 / sRho_G * fx_G * sx * sin(szt)
!$OMP END WORKSHARE
      end if
      

! ...and from y-comp:

!$OMP WORKSHARE
      bzj = bzj + szt / 4_wp / pi * sqrt(sEta_G) / 2 / sRho_G * fy_G * sy * cos(szt)
!$OMP END WORKSHARE





!!$OMP WORKSHARE
!      bzj = sqrt(sEta_G) / 2 / sRho_G * (     &
!        fx_G*sx * ( 1/32_wp * cos(szt/4_wp) * sin(szt) + &
!                    1/4_wp * sin(szt/4_wp) * cos(szt) - &
!                    sin(szt/8_wp)**2 * sin(szt) )    + &
!        fy_G*sy * ( -1/32_wp * cos(szt/4_wp) * cos(szt) + &
!                    1/4_wp * sin(szt/4_wp) * sin(szt) + &
!                    sin(szt/8_wp)**2 * cos(szt) ) )
!!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndEnd_G) then

      szt = sZ - sZFE
!      szt = szt / 2_wp / sRho_G








! ...from x-comp:

!$OMP WORKSHARE
      bzj = - sqrt(sEta_G) / 2 / sRho_G * (szt - 7.0_wp * pi * sRho_G) / &
              (6_wp * pi * sRho_G) * fx_G * sx * sin(szt / 2_wp / sRho_G)            
!$OMP END WORKSHARE

      if (szt < pi * sRho_G) then 
!$OMP WORKSHARE
        bzj = - sqrt(sEta_G) / 2 / sRho_G * fx_G * sx * sin(szt / 2_wp / sRho_G)
!$OMP END WORKSHARE
      else if (szt > 7_wp * pi * sRho_G) then
!$OMP WORKSHARE
        bzj = 0.0_wp
!$OMP END WORKSHARE
      end if
      

! ...and from y-comp:

!$OMP WORKSHARE
      bzj = bzj + (-szt / 8_wp / pi / sRho_G + 1_wp) * &
            sqrt(sEta_G) / 2 / sRho_G * fy_G * sy * cos(szt / 2_wp / sRho_G)
!$OMP END WORKSHARE













!!$OMP WORKSHARE
!      bzj = sqrt(sEta_G) / 2 / sRho_G * (     &
!        fx_G*sx * ( -1/32_wp * cos(szt/4_wp) * sin(szt) - &
!                    1/4_wp * sin(szt/4_wp) * cos(szt) - &
!                    cos(szt/8_wp)**2 * sin(szt) )    + &
!        fy_G*sy * ( 1/32_wp * cos(szt/4_wp) * cos(szt) - &
!                    1/4_wp * sin(szt/4_wp) * sin(szt) + &
!                    cos(szt/8_wp)**2 * cos(szt) ) )
!!$OMP END WORKSHARE

    else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE
      bzj = sqrt(sEta_G) / 2 / sRho_G * &
            ( -fx_G*sx * sin(szt)  + fy_G*sy * cos(szt) )
!$OMP END WORKSHARE

    end if

!    END elliptical undulator description
!  ####################################################




  end if

  end if

end subroutine getBZfield

end module bfields
