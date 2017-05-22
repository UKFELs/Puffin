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
!> This module contains the type definition to hold the scaling constants
!> describing the z2 radiation frame used in Puffin.

module typeScale

  use paratype
		
  implicit none
      
  type fScale
  
    real(kind=wp) :: eta
    real(kind=wp) :: rho
    real(kind=wp) :: aw
    real(kind=wp) :: lambda_w
    real(kind=wp) :: gamma_r
    
  end type fScale
  
end module typeScale
