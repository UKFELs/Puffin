MODULE extra

USE paratype
USE IO

IMPLICIT NONE

CONTAINS
!********************************************************
  SUBROUTINE GL_NUM(iSizeX,iSizeY,iSizeZ2,iNodCodA,&
       iGloNumA)

    IMPLICIT NONE

! Each element has 8-nodes, assign all elements with
! local node number, 
! global node number and element number.
! Each node has its co-ordinates in the system
! 
! iSizeX         - INPUT     
! iSizeY         - INPUT
! iSizeZ2        - INPUT     - Number of elements in all
!                              direction
! iNodCodA       - OUTPUT    - CO-ORDINATES OF THE NODES
! iLocNumA       - OUTPUT    - LOCAL NODE NUMBER FOR
!                              EACH NODE
! iGloNumA       - OUTPUT    - GLOBAL NODE NUMBER FOR
!                              EACH NODE
! iElmNumA       - OUTPUT    - ELEMENT NUMBER FOR EACH
!                              NODE
!
    INTEGER(KIND=IP),INTENT(IN) :: iSizeX,iSizeY,iSizeZ2
								
    INTEGER(KIND=IP),INTENT(OUT) :: iGloNumA(:)
    INTEGER(KIND=IP),INTENT(OUT) :: iNodCodA(:,:,:)
!
! Local Indices
! ia,ib,ic,ie,ii,ij,ik  - Loop counter
! ix,iy,iz2             - coordinates counter  
! il                    - Local node counter
! inode                 - counter
! iCount_all            - Global node counter

    INTEGER(KIND=IP) :: iCount_all,ie,ik,ij,ii,ix, &
         iy,iz2,il,ia,ib,ic,inode
!--------------------------------------------------------
! BEGIN:-
! Label Node with a coordinates
    inode=0
    DO ic=1,iSizeZ2
       DO ib=1,iSizeY
          DO ia=1,iSizeX
             inode=inode+1
             iNodCodA(ia,ib,ic)=inode
          ENDDO
       ENDDO
    ENDDO

    iCount_all=0
    ie=0

! Assign with element with an element number, node
! number and global number
    DO ik=1,iSizeZ2-1
       DO ij=1,iSizeY-1
          DO ii=1,iSizeX-1
!*****************************
! Information for Local Node 1
!*****************************

             !++++++++++++++
             ! Co-ordinates+
             !++++++++++++++
             ix=ii
             iy=ij
             iz2=ik
		
             !+++++++++++++++++++
             ! Label Global node+
             !+++++++++++++++++++
             iCount_all=iCount_all+1

             !+++++++++++++++++++++++++++++++++++++++++
             ! Assign Global node to this coordinates +
             !+++++++++++++++++++++++++++++++++++++++++
             iGloNumA(iCount_all)=iNodCodA(ix,iy,iz2)

             !*****************************
             ! Information for Local Node 2
             !*****************************

             !++++++++++++++
             ! Co-ordinates+
             !++++++++++++++
             ix=ii+1
             iy=ij
             iz2=ik

             !+++++++++++++++++++
             ! Label Global node+
             !+++++++++++++++++++
             iCount_all=iCount_all+1

             !+++++++++++++++++++++++++++++++++++++++++
             ! Assign Global node to this coordinates +
             !+++++++++++++++++++++++++++++++++++++++++
             iGloNumA(iCount_all)=iNodCodA(ix,iy,iz2)

             !*****************************
             ! Information for Local Node 3
             !*****************************

             !++++++++++++++
             ! Co-ordinates+
             !++++++++++++++
             ix=ii
             iy=ij+1
             iz2=ik

             !+++++++++++++++++++
             ! Label Global node+
             !+++++++++++++++++++
             iCount_all=iCount_all+1

             !+++++++++++++++++++++++++++++++++++++++++
             ! Assign Global node to this coordinates +
             !+++++++++++++++++++++++++++++++++++++++++
             iGloNumA(iCount_all)=iNodCodA(ix,iy,iz2)
   
             !*****************************
             ! Information for Local Node 4
             !*****************************

             !++++++++++++++
             ! Co-ordinates+
             !++++++++++++++
             ix=ii+1
             iy=ij+1
             iz2=ik

             !+++++++++++++++++++
             ! Label Global node+
             !+++++++++++++++++++
             iCount_all=iCount_all+1

             !+++++++++++++++++++++++++++++++++++++++++
             ! Assign Global node to this coordinates +
             !+++++++++++++++++++++++++++++++++++++++++
             iGloNumA(iCount_all)=iNodCodA(ix,iy,iz2)
	  
             !*****************************
             ! Information for Local Node 5
             !*****************************

             !++++++++++++++
             ! Co-ordinates+
             !++++++++++++++
             ix=ii
             iy=ij
             iz2=ik+1

             !+++++++++++++++++++
             ! Label Global node+
             !+++++++++++++++++++
             iCount_all=iCount_all+1

             !+++++++++++++++++++++++++++++++++++++++++
             ! Assign Global node to this coordinates +
             !+++++++++++++++++++++++++++++++++++++++++
             iGloNumA(iCount_all)=iNodCodA(ix,iy,iz2)

             !*****************************
             ! Information for Local Node 6
             !*****************************

             !++++++++++++++
             ! Co-ordinates+
             !++++++++++++++
             ix=ii+1
             iy=ij
             iz2=ik+1

             !+++++++++++++++++++
             ! Label Global node+
             !+++++++++++++++++++
             iCount_all=iCount_all+1

             !+++++++++++++++++++++++++++++++++++++++++
             ! Assign Global node to this coordinates +
             !+++++++++++++++++++++++++++++++++++++++++
             iGloNumA(iCount_all)=iNodCodA(ix,iy,iz2)

             !*****************************
             ! Information for Local Node 7
             !*****************************

             !++++++++++++++
             ! Co-ordinates+
             !++++++++++++++
             ix=ii
             iy=ij+1
             iz2=ik+1
             
             !+++++++++++++++++++
             ! Label Global node+
             !+++++++++++++++++++
             iCount_all=iCount_all+1
   
             !+++++++++++++++++++++++++++++++++++++++++
             ! Assign Global node to this coordinates +
             !+++++++++++++++++++++++++++++++++++++++++
             iGloNumA(iCount_all)=iNodCodA(ix,iy,iz2)

             !*****************************
             ! Information for Local Node 8
             !*****************************

             !++++++++++++++
             ! Co-ordinates+
             !++++++++++++++
             ix=ii+1
             iy=ij+1
             iz2=ik+1

             !+++++++++++++++++++
             ! Label Global node+
             !+++++++++++++++++++
             iCount_all=iCount_all+1

             !+++++++++++++++++++++++++++++++++++++++++
             ! Assign Global node to this coordinates +
             !+++++++++++++++++++++++++++++++++++++++++
             iGloNumA(iCount_all)=iNodCodA(ix,iy,iz2)

          ENDDO
       ENDDO
    ENDDO
    
  END SUBROUTINE GL_NUM
!********************************************************
  SUBROUTINE principal(iSizeX,iSizeY,iOneElm,iGloNumA,&
       iNodCodA,xx,yy,zz2,i_n4e)
    
    IMPLICIT NONE
!
! Given the co-ordinate of a particle, determine the
! principal node and the relevant nodes
!
! iSizeX       - INPUT   
! iSizeY       - INPUT
! iSizeZ2      - INPUT   - Number of elements in all
!                          direction
! iOneElm      - INPUT   - NUMBER OF NODES IN ONE ELEMENT
! iGloNumA     - INPUT   - GLOBAL NODE NUMBER FOR EACH NODE
! iNodCodA     - INPUT   - CO-ORDINATES OF THE NODES
! xx,yy,zz2    - INPUT   - Coordinates of the principal
!                          node for each electron
! i_n4e        - OUTPUT  - All global node numbers of
!                          the element which electrons
!                           stays in 
!
    INTEGER(KIND=IP),INTENT(IN) :: iSizeX,iSizeY,iOneElm
    INTEGER(KIND=IP),INTENT(IN) :: iGloNumA(:),iNodCodA(:,:,:) 
    INTEGER(KIND=IP),INTENT(IN) :: xx,yy,zz2
    INTEGER(KIND=IP),INTENT(OUT) :: i_n4e(iOneElm)
!
! Local Indices	
! ix,iy,iz2             - temporary store princpal
!                         coordinate of electron
! i_Element_Principle   - element number of the principal
!                         node
! temp2                 - Global number of the principal
!                         node
! i,j                   - Loop counter
!
    INTEGER(KIND=IP) :: ix,iy,iz2,i_Element_Principle,&
         temp2,i,j

    ix=xx
    iy=yy
    iz2=zz2
	
    i_Element_Principle=(ix+(iy-1)*(iSizeX-1)+&
         (iSizeX-1)*(iSizeY-1)*(iz2-1))

    temp2=i_Element_Principle*iOneElm-7
	
    j=1
    i_n4e(j)=iGloNumA(temp2)
	
    IF (iGloNumA(temp2)==iNodCodA(ix,iy,iz2)) THEN
       DO i=(temp2+1),(i_Element_Principle*iOneElm)
          j=j+1
          i_n4e(j)=iGloNumA(i)
       ENDDO
    ELSE 
       PRINT *, "Program has stopped Executing. AExtra.f90:principal",&
            ix,iy,iz2,xx,yy,zz2
       STOP 
    ENDIF
  END SUBROUTINE principal
!********************************************************
  SUBROUTINE principal2(iSizeX,iSizeY,iOneElm,xx,yy,&
       zz2,i_n4e,qoutside)

    IMPLICIT NONE
!
! Given the co-ordinate of a particle, determine the
! principal node and the relevant nodes
!
! iSizeX       - INPUT   
! iSizeY       - INPUT
! iSizeZ2      - INPUT   - Number of elements in all
!                          directions
! iOneElm      - INPUT   - NUMBER OF NODES IN ONE ELEMENT
! xx,yy,zz2    - INPUT   - Coordinates of the principal
!                          node for each electron
! i_n4e        - OUTPUT  - All global node numbers of
!                          the element which the 
!                          electron stays in 
!
    INTEGER(KIND=IP),INTENT(IN) :: iSizeX,iSizeY,iOneElm
    INTEGER(KIND=IP),INTENT(IN) :: xx,yy,zz2
    INTEGER(KIND=IP),INTENT(OUT):: i_n4e(iOneElm)
    LOGICAL,INTENT(INOUT)       :: qoutside
!
! Local Indices	
! ix,iy,iz2             - temporary store princpal 
!                         coordinate of electron
! i_Element_Principle   - element number of the
!                         principal node
! temp2                 - Global number of the
!                         principal node
! i,j                   - Loop counter
!
    INTEGER(KIND=IP) :: ix,iy,iz2,i_Element_Principle,&
         temp2,i,j,error

    qoutside=.false.

    ix=xx !new x
    iy=yy  !new y
    iz2=zz2 ! same z2
	
    IF (ix<1 .OR. iy<1 .OR. iz2<1) THEN
       qoutside=.true.
    END IF
    i_n4e(1)=(ix+(iy-1)*(iSizeX)+(iSizeX)*(iSizeY)*(iz2-1))	
    i_n4e(2)=((ix+1)+(iy-1)*(iSizeX)+(iSizeX)*(iSizeY)*(iz2-1))
    i_n4e(3)=(ix+((iy+1)-1)*(iSizeX)+(iSizeX)*(iSizeY)*(iz2-1))
    i_n4e(4)=((ix+1)+((iy+1)-1)*(iSizeX)+(iSizeX)*(iSizeY)*(iz2-1))
	
    i_n4e(5)=(ix+(iy-1)*(iSizeX)+(iSizeX)*(iSizeY)*((iz2+1)-1))
    i_n4e(6)=((ix+1)+(iy-1)*(iSizeX)+(iSizeX)*(iSizeY)*((iz2+1)-1))
    i_n4e(7)=(ix+((iy+1)-1)*(iSizeX)+(iSizeX)*(iSizeY)*((iz2+1)-1))
    i_n4e(8)=((ix+1)+((iy+1)-1)*(iSizeX)+(iSizeX)*(iSizeY)*((iz2+1)-1))
	
  END SUBROUTINE principal2
!********************************************************
END MODULE extra
