!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE InitDataType

  USE paratype
		
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

  end type cInitData

end module InitDataType
