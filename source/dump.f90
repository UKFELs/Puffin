!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE dumpFiles


! This module contains the subroutines for dumping 
! the data in Puffin. The dumping is very dirty, and
! creates a file for each variable, for each process.
! This enables Puffin to resume from these dump files
! in the even of a crash. This is the only purpose
! of these dump files, and they are not intended for
! any post-processing.


  USE paratype
  USE typesandconstants
  USE Globals
  USE ParallelInfoType
  USE sddsPuffin

  IMPLICIT NONE

  CONTAINS




SUBROUTINE DUMPDATA(sA,sV,rank,nnodes,nelectrons,sz,istep,page)

 REAL(KIND=WP),DIMENSION(:),INTENT(IN) :: sA
 REAL(KIND=WP),DIMENSION(:),INTENT(IN) :: sV
 INTEGER(KIND=IP),INTENT(IN) :: rank,nnodes,istep,page
 INTEGER(KIND=IPL), INTENT(IN) :: nelectrons
 REAL(KIND=WP),INTENT(IN) :: sz

 CHARACTER(32_IP) :: FileName

! FIELD

if (rank==0) then

! Real part
 FileName = 'reA' // TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sA(1:nnodes)
 CLOSE(UNIT=213,STATUS='KEEP')
! Imaginary part
 FileName = 'imA' // TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sA(nnodes+1:2*nnodes)
 CLOSE(UNIT=213,STATUS='KEEP') 

end if

! ELECTRONS

if (nelectrons > 0) then

! re pperp
 FileName = 'rePPerp'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sV(1:nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! Im pperp
 FileName = 'imPPerp'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sV(nelectrons+1:2*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! Q 
 FileName = 'Q'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sV(2*nelectrons+1:3*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! Z2 
 FileName = 'Z2-'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sV(3*nelectrons+1:4*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! X 
 FileName = 'X'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sV(4*nelectrons+1:5*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP')
! Y
 FileName = 'Y'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sV(5*nelectrons+1:6*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 

end if
 
! step

if (rank==0) then

 FileName = 'step'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) istep
 CLOSE(UNIT=213,STATUS='KEEP') 

 ! Z
 FileName = 'Z'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sz
 CLOSE(UNIT=213,STATUS='KEEP')  

end if

 ! nelectrons
 FileName = 'nelectrons'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) nelectrons
 CLOSE(UNIT=213,STATUS='KEEP') 
 
! page

if (rank==0) then

 FileName = 'page'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) page
 CLOSE(UNIT=213,STATUS='KEEP') 

end if
        
END SUBROUTINE DUMPDATA




SUBROUTINE DUMPCHIDATA(chibar,normchi,rank)

 REAL(KIND=WP),DIMENSION(:),INTENT(IN) :: chibar
 REAL(KIND=WP),DIMENSION(:),INTENT(IN) :: normchi
 INTEGER(KIND=IP),INTENT(IN) :: rank

 CHARACTER(32_IP) :: FileName

! FIELD
! Real part
 FileName = 'chibar' // TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) chibar
 CLOSE(UNIT=213,STATUS='KEEP')
! Imaginary part
 FileName = 'normchi' // TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) normchi
 CLOSE(UNIT=213,STATUS='KEEP') 

        
END SUBROUTINE DUMPCHIDATA






end module dumpFiles