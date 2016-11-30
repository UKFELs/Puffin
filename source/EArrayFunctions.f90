!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Module to retrieve and populate array


Module ArrayFunctions



  USE paratype
  USE ParallelInfoType
  USE FileType
  USE IO
  USE ParallelSetup
!      
  IMPLICIT NONE
!
!
!        
!         Global Parameters
!
! iMtxStartPosition_G           Array holding pointers to equations in matrix
! iMtxEndPosition_G             Array holding pointers to equations in matrix
! iBStartPosition_G             Array holding pointers to equations in array B (rhs)
! iBEndPosition_G               Array holding pointers to equations in array B (rhs)
!
! nArrayVariables_CG            Number of variables held in array
! iRe_A_CG                      Position in pointer array of Real A values
! iIm_A_CG                      Position in pointer array of Imaginary A values
! iRe_PPerp_CG                  Position in pointer array of Real pperp values
! iIm_PPerp_CG                  Position in pointer array of Imaginary pperp values
! iRe_Gam_CG                    Position in pointer array of gamma / gamma_r values
! iRe_z2_CG                     Position in pointer array of z2 values
!
! nWaveEquations_CG             Number of wave equations (one real plus one imaginary)
! nElectronEquations_CG         Number of electron equations (real p, imaginary p, q, z2)
!

  INTEGER(KIND=IP), PARAMETER    :: nArrayVariables_CG = 8_IP
  INTEGER(KIND=IP), PARAMETER    :: iRe_PPerp_CG       = 1_IP
  INTEGER(KIND=IP), PARAMETER    :: iIm_PPerp_CG       = 2_IP
  INTEGER(KIND=IP), PARAMETER    :: iRe_Gam_CG           = 3_IP
  INTEGER(KIND=IP), PARAMETER    :: iRe_z2_CG          = 4_IP
  INTEGER(KIND=IP), PARAMETER    :: iRe_X_CG           = 5_IP
  INTEGER(KIND=IP), PARAMETER    :: iRe_Y_CG           = 6_IP
      
  INTEGER(KIND=IP), PARAMETER    :: nFieldEquations_CG = 2_IP
  INTEGER(KIND=IP), PARAMETER    :: iRe_A_CG	       = 1_IP
  INTEGER(KIND=IP), PARAMETER    :: iIm_A_CG	       = 2_IP
 
  INTEGER(KIND=IP), PARAMETER    :: nElectronEquations_CG  = 6_IP


!=====================!

  INTEGER(KIND=IP)    :: iXs, iXe, iYs, iYe, iZ2s, iZ2e, &
                         iPXs, iPXe, iPYs, iPYe, iP2s, iP2e






  
  LOGICAL  :: qEmpty

!                 Define type cArraySegment
!
! iStart         Start position of variable in large array
! iEnd           End position of variable in large array
! qWrite         If writing this variables data out to file
! zVariable      Name of variable
! tFileType      File information if writing data to file

  TYPE cArraySegment
    LOGICAL             :: qWrite = .FALSE.
    CHARACTER(32_IP)    :: zVariable = ''
    TYPE(cFileType)     :: tFileType
  END TYPE



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

End Module ArrayFunctions
