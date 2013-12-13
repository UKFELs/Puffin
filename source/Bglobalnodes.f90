!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE globalnodes
USE paratype

IMPLICIT NONE

 CONTAINS
 
 SUBROUTINE cal_nodes(iSizeX,iSizeY,iSizeZ2,iOneElm,iNodeX,iNodeY,iNodeZ2,iTotNod,iTotElm,iNodPln,iAtuNod,iOneElmSq)
 
 IMPLICIT NONE
 
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!	Inputs %%%
!	iSizeX,iSizeY and iSizeZ2 - THE ELEMENT SIZE OF THE SYSTEM
!	iOneElm - NUMBER OF NODES IN ONE ELEMENT
!
!	Outputs %%%
!	iNodeX,iNodeY,iNodeZ2 - NUMBER OF NODES ON X,Y,Z2 
!	iTotNod - TOTAL NUMBER OF NODES IN THE SYSTEM
!	iTotElm - TOTAL NUMBER OF ELEMENTS IN THE SYSTEM
!	iNodPln - NUMBER OF NODES ON ONE PLANE(Z2) 
!	iAtuNod - actual size of the nodes
!	iOneElmSq - (NUMBER OF ELEMENT NODES) ^2 ("SQUARED")
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
 
 
 INTEGER(KIND=IP),INTENT(IN)		::	iSizeX,iSizeY,iSizeZ2,iOneElm
 INTEGER(KIND=IP),INTENT(OUT)		::	iNodeX,iNodeY,iNodeZ2,iTotNod,iTotElm,iNodPln,iAtuNod,iOneElmSq
 

 
 !~~~Enter the size of the problem~~~

	iNodeX=iSizeX+1
	iNodeY=iSizeY+1
	iNodeZ2=iSizeZ2+1

!~~~Calculate important constants for the system~~~

	iTotNod=GetTotalNodes(iSizeX, iSizeY, iSizeZ2)
	iTotElm=GetTotalElements(iSizeX, iSizeY, iSizeZ2)
	iNodPln=GetNodesInPlane(iSizeX, iSizeY)
	iAtuNod=iOneElm*iTotElm
	iOneElmSq=iOneElm**2

 END SUBROUTINE cal_nodes
!--------------------------------------------------------------------------------	
!--------------------------------------------------------------------------------	
      FUNCTION GetTotalNodes(nX, nY, nZ) 
!
!********************************************************************
! Calculate the total number of nodes
!********************************************************************
!
! nX       - INPUT  - number of elements in X direction
! nY       - INPUT  - number of elements in Y direction
! nZ       - INPUT  - number of elements in Z direction
!
!====================================================================
! Define local variables
!=====================================================================
!
      INTEGER(KIND=IP),       INTENT(IN)                 :: nX	
      INTEGER(KIND=IP),       INTENT(IN)                 :: nY	
      INTEGER(KIND=IP),       INTENT(IN)                 :: nZ	
      INTEGER(KIND=IP)                                   :: GetTotalNodes
!
      GetTotalNodes = (nX + 1) * (nY + 1) * (nZ + 1)
!      
!--------------------------------------------------------------------------------
! Error Handler
!--------------------------------------------------------------------------------
!
!1000    call Error_log('Error: In (IO (InitialiseSDDSFile)) ',tErrorLog)	
!        Return
      END FUNCTION GetTotalNodes  
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
      FUNCTION GetTotalElements (nX, nY, nZ) 
!
!********************************************************************
! Calculate the total number of elements
!********************************************************************
!
! nX       - INPUT  - number of elements in X direction
! nY       - INPUT  - number of elements in Y direction
! nZ       - INPUT  - number of elements in Z direction
!
!====================================================================
! Define local variables
!=====================================================================
!
      INTEGER(KIND=IP),       INTENT(IN)                 :: nX	
      INTEGER(KIND=IP),       INTENT(IN)                 :: nY	
      INTEGER(KIND=IP),       INTENT(IN)                 :: nZ	
      INTEGER(KIND=IP)                                   :: GetTotalElements
!
      GetTotalElements = nX  * nY  * nZ 
!      
!--------------------------------------------------------------------------------
! Error Handler
!--------------------------------------------------------------------------------
!
!1000    call Error_log('Error: In (IO (InitialiseSDDSFile)) ',tErrorLog)	
!        Return
      END FUNCTION GetTotalElements 
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
      FUNCTION GetNodesInPlane  (nX, nY) 
!
!********************************************************************
! Nodes in plane
!********************************************************************
!
! nX       - INPUT  - number of elements in X direction
! nY       - INPUT  - number of elements in Y direction
!
!====================================================================
! Define local variables
!=====================================================================
!
      INTEGER(KIND=IP),       INTENT(IN)                 :: nX	
      INTEGER(KIND=IP),       INTENT(IN)                 :: nY	
      INTEGER(KIND=IP)                                   :: GetNodesInPlane  
!
      GetNodesInPlane   = (nX + 1)  * (nY +1)  
!      
!--------------------------------------------------------------------------------
! Error Handler
!--------------------------------------------------------------------------------
!
!1000    call Error_log('Error: In (IO (InitialiseSDDSFile)) ',tErrorLog)	
!        Return
      END FUNCTION GetNodesInPlane  
END MODULE globalnodes
