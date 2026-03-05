! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

MODULE InitDataType

  use puffin_kinds
		
  IMPLICIT NONE
      
  type cInitData

    real(kind=wp) :: zbarTotal
    real(kind=wp) :: Zbarinter
    real(kind=wp) :: zbarlocal
    integer(kind=ip) :: iCsteps
    integer(kind=ip) :: iStep
    integer(kind=ip) :: iUnd_cr
    integer(kind=ip) :: iChic_cr
    integer(kind=ip) :: iDrift_cr
    integer(kind=ip) :: iQuad_cr
    integer(kind=ip) :: iModulation_cr
    integer(kind=ip) :: iL
    integer(kind=ip) :: igwr

  end type cInitData

end module InitDataType
