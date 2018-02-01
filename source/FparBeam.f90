! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module parBeam

! This module is used to determine how the electron beam
! macroparticles in Puffin are spread across MPI processes.

use paratype
use typesandconstants
use globals

implicit none

contains

  SUBROUTINE splitBeam(N, globLen, numproc, rank, &
                      locN, local_start, local_end)

! Get local beam macroparticle number
! and start and end points in z2.
! 
!           ARGUMENTS

    INTEGER(KIND=IP), INTENT(IN) :: N, numproc, rank
    REAL(KIND=WP), INTENT(IN) :: globLen
    
    INTEGER(KIND=IP), INTENT(OUT) :: locN
    REAL(KIND=WP), INTENT(OUT) :: local_start, local_end
    
!          LOCAL ARGS
    
    REAL(KIND=WP) :: frac, OneElmLength, upperLength, lowerLength
    INTEGER(KIND=IP) :: lowern, highern, remainder


    frac = REAL(N)/REAL(numproc)
    lowern = FLOOR(frac)
    highern = CEILING(frac)
    remainder = MOD(N,numproc)
     
    IF (remainder==0) THEN
       locN = lowern
    ELSE
       IF (rank < remainder) THEN
          locN = highern
       ELSE
          locN = lowern
       ENDIF
    ENDIF

!     Calculate the distance covered by one element of the grid in z2

    OneElmLength = globLen/N  

!     Calculate the local length of the electron pulse a local process
!     will hold, in both the lower and upper case.
    
    lowerLength = lowern*OneElmLength
    upperLength = highern*OneElmLength

!     Calculate local start and end values.

    IF (rank >= remainder) THEN
       local_start = (upperLength*remainder) + &
            ((rank-remainder)*lowerLength)
       local_end = (upperLength*remainder) +&
            (((rank+1)-remainder)*lowerLength)
    ELSE
       local_start = rank*upperLength
       local_end = (rank+1)*upperLength
    ENDIF
  
    
  END SUBROUTINE splitBeam

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE splitBeams(iNMP,samLenE,nBeams,numproc,rank,&
                      iNumLocalElectrons,totalmps_b)

  IMPLICIT NONE
  
!                   ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: iNMP(:,:)
  REAL(KIND=WP), INTENT(IN) :: samLenE(:,:)
  INTEGER(KIND=IP), INTENT(IN) :: nBeams
  INTEGER, INTENT(IN) :: numproc, rank
  INTEGER(KIND=IP), INTENT(INOUT) :: iNumLocalElectrons(:,:)
  INTEGER(KIND=IPL), INTENT(INOUT) :: totalmps_b(:)
  
!                   LOCAL ARGS

  REAL(KIND=WP) :: local_start, local_end
  INTEGER(KIND=IP) :: ind

!     Split electron macroparticles across z2


  iNumLocalElectrons(:,iX_CG) = iNMP(:,iX_CG)
  iNumLocalElectrons(:,iY_CG) = iNMP(:,iY_CG)
  iNumLocalElectrons(:,iPX_CG) = iNMP(:,iPX_CG)
  iNumLocalElectrons(:,iPY_CG) = iNMP(:,iPY_CG)
  iNumLocalElectrons(:,iGam_CG) = iNMP(:,iGam_CG)

  DO ind = 1, nBeams

    CALL splitBeam(iNMP(ind,iZ2_CG), samLenE(ind,iZ2_CG), numproc, rank, &
                   iNumLocalElectrons(ind,iZ2_CG), local_start, local_end)
                   
!             Total no of MPs in this beam

    if (qEquiXY_G) then

      totalmps_b(ind) = PRODUCT(INT(iNumLocalElectrons(ind,:),KIND=IPL)) 

    else

      totalmps_b(ind) = INT(iNumLocalElectrons(ind,iZ2_CG) * &
                               nseqparts_G,KIND=IPL)

    end if

  END DO

END SUBROUTINE splitBeams



subroutine divMPs(ndpts, numproc, rank, &
                  locN, local_start, local_end)

! Get local number of nodes and start and global
! indices of start and end points. 
! 
!           ARGUMENTS

  integer(kind=ip), intent(in) :: ndpts, numproc, rank
  
  integer(kind=ip), intent(out) :: locN
  integer(kind=ip), intent(out) :: local_start, local_end
  
!          LOCAL ARGS
  
  real(kind=wp) :: frac
  integer(kind=ip) :: lowern, highern, remainder


  frac = REAL(ndpts,kind=wp)/REAL(numproc,kind=wp)
  lowern = FLOOR(frac)
  highern = CEILING(frac)
  remainder = MOD(ndpts,numproc)
   
  IF (remainder==0) THEN
     locN = lowern
  ELSE
     IF (rank < remainder) THEN
        locN = highern
     ELSE
        locN = lowern
     ENDIF
  ENDIF


!     Calculate local start and end values.

  IF (rank >= remainder) THEN
    
    local_start = (remainder*highern) + ((rank-remainder) * lowern) + 1
    local_end = local_start + locN - 1

  ELSE
     
    local_start = rank*locN + 1
    local_end = local_start + locN - 1
  
  ENDIF

  
end subroutine divMPs



end module parBeam
