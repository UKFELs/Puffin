! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module grids

! This module contains the subroutines used to 
! define and create the grids in each phase space
! dimension which define the positions of the 
! electron beam macroparticles.

use paratype 
use particleFunctions
use parallelsetup
use parBeam
use globals

implicit none

contains

  SUBROUTINE getExtent(off,length,start,endm)

! Simple subroutine to calculate the extent, 
! or beginning and end of the model in a dimension,
! given the length and the offset from zero.
! 
!              ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: off, length
    REAL(KIND=WP), INTENT(OUT) :: start, endm
    
    start = -length/2.0_WP + off
    
    endm = start + length
  
  END SUBROUTINE getExtent

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE getIntTypes(iNMPs, samplens, sigmas, &
                       inttypes)


!                   ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN)  :: iNMPs(:)
  REAL(KIND=WP), INTENT(IN) :: samplens(:), sigmas(:)
  INTEGER(KIND=IP), INTENT(OUT)  :: inttypes(:)

!                  LOCAL ARGS

  INTEGER(KIND=IP) :: iLIT

    DO iLIT=1,6
       IF (iNMPs(iLIT) == 1_IP .OR. samplens(iLIT)<=1.0E-6_WP &
            .OR. samplens(iLIT)<6.0_WP*sigmas(iLIT) ) THEN
          inttypes(iLIT)=iTopHatDistribution_CG
          !IF (tProcInfo_G%qROOT) PRINT *, 'tophat'
       ELSE
          inttypes(iLIT)=iGaussianDistribution_CG
          !IF (tProcInfo_G%qROOT) PRINT *, 'gauss'
       END IF
    ENDDO

END SUBROUTINE getIntTypes

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE genGrids(ib, sigmas,offsets,slens,intType,iNMPs,iNMPs_loc, &
                    sx_grid, sy_grid, sz2_grid, &
                    spx_grid, spy_grid, spz2_grid, &
                    sX_integral, sY_integral, sz2_integral, &
                    sPX_integral, sPY_integral, sPZ2_integral)

  IMPLICIT NONE

!           ARGUMENTS

  REAL(KIND=WP), INTENT(IN) :: sigmas(:), offsets(:), slens(:)
  INTEGER(KIND=IP), INTENT(IN) :: iNMPs(:), iNMPs_loc(:), intType(:), ib
  REAL(KIND=WP), INTENT(INOUT) :: sx_grid(:), sy_grid(:), sz2_grid(:), &
                                  spx_grid(:), spy_grid(:), spz2_grid(:), &
                                  sX_integral(:), sY_integral(:), sz2_integral(:), &
                                  sPX_integral(:), sPY_integral(:), sPZ2_integral(:)

!         LOCAL ARGS
  
  INTEGER(KIND=IP) :: ind
  LOGICAL :: qOKL
  
! Generate grids and integrals in each dimension

  CALL genGrid(ib,intType(iX_CG),iLinear_CG,offsets(iX_CG), &
               sigmas(iX_CG),slens(iX_CG),&
               iNMPs(iX_CG),iNMPs_loc(iX_CG),sx_grid,sX_integral,.FALSE., &
               qOKL)

  CALL genGrid(ib,intType(iY_CG),iLinear_CG,offsets(iY_CG), &
               sigmas(iY_CG),slens(iY_CG),&
               iNMPs(iY_CG),iNMPs_loc(iY_CG),sy_grid,sY_integral,.FALSE., &
               qOKL)

  CALL genGrid(ib,intType(iZ2_CG),iLinear_CG,offsets(iZ2_CG), &
               sigmas(iZ2_CG),slens(iZ2_CG),&
               iNMPs(iZ2_CG),iNMPs_loc(iZ2_CG),sz2_grid,sZ2_integral,.TRUE., &
               qOKL)

  CALL genGrid(ib,intType(iPX_CG),iLinear_CG,offsets(iPX_CG), &
               sigmas(iPX_CG),slens(iPX_CG),&
               iNMPs(iPX_CG),iNMPs_loc(iPX_CG),spx_grid,sPX_integral,.FALSE., &
               qOKL)

  CALL genGrid(ib,intType(iPY_CG),iLinear_CG,offsets(iPY_CG), &
               sigmas(iPY_CG),slens(iPY_CG),&
               iNMPs(iPY_CG),iNMPs_loc(iPY_CG),spy_grid,sPY_integral,.FALSE., &
               qOKL)

  CALL genGrid(ib,intType(iGam_CG),iLinear_CG,offsets(iGam_CG), &
               sigmas(iGam_CG),slens(iGam_CG),&
               iNMPs(iGam_CG),iNMPs_loc(iGam_CG),spz2_grid,sPZ2_integral,.FALSE., &
               qOKL)

! Each gamma has its own sig_px, sig_py....need to change above to 
! do this. For now, sigma_px and sigma_py are the same for all gamma_j.

!  sigpx(size(nMP_P2))
!  sigpy(size(nMP_P2))

!  sigpx(i) = sigpx_avg * gamma_j / gamma_r
!  sigpy(i) = sigpy_avg * gamma_j / gamma_r


END SUBROUTINE genGrids

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE genGrid(b_num, inttype,gridtype,centre,sigma,length,&
                   iNMP,iNMP_loc,Grid,Integral,qParallel,qOK)

  IMPLICIT NONE

! gridtype == iLinear_CG
!
!          ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: inttype, gridtype, b_num
  REAL(KIND=WP), INTENT(IN) :: centre, sigma, length
  INTEGER(KIND=IP), INTENT(IN) :: iNMP, iNMP_loc
  REAL(KIND=WP), INTENT(INOUT) :: Grid(:), Integral(:)
  LOGICAL, INTENT(IN) :: qParallel
  LOGICAL, INTENT(INOUT) :: qOK

!         LOCAL VARS

  REAL(KIND=WP) :: start, final, local_start, local_fin, shift, &
                   sige, flat_len
  INTEGER(KIND=IP) :: locN
  LOGICAL :: qOKL

  qOK = .FALSE.

  IF (qParallel) THEN

    if ((qRndEj_G(b_num)) .and. &
           (inttype == iTopHatDistribution_CG) ) then !!!  If rounding edges of flat-top
  
      CALL splitBeam(iNMP, length, tProcInfo_G%size, tProcInfo_G%rank, &
                     locN, local_start, local_fin)

      shift = centre - (length / 2.0_WP) ! Amount the beam is shifted from 
                                         ! having the head at z2=0

      local_start = local_start + shift
      local_fin = local_fin + shift

      CALL PulseGrid(iGridType=gridtype, &
                     iNumMP=locN, &
                     sStart=local_start, &
                     sEnd=local_fin, &
                     sGrid=Grid, &
                     qOK = qOKL)

      IF (.NOT. qOKL) GOTO 1000
      
!     CALL DistributionIntegralZ2(inttype, &
!                                 locN, &
!                                 iNMP, &
!                                 Grid, &
!                                 centre, &
!                                 sigma, &
!                                 MPI_DOUBLE_PRECISION, &
!                                 MPI_SUM, &
!                                 Integral, &
!                                 qOKL)


      ! sige = 4.0_wp * 4.0_wp * pi * sRho_G
      flat_len = length - (sSigEj_G(b_num) * gExtEj_G)
      call flattop2(sSigEj_G(b_num), flat_len, Grid, iNMP, iNMP_loc, .true., Integral)



    else 

      CALL splitBeam(iNMP, length, tProcInfo_G%size, tProcInfo_G%rank, &
                     locN, local_start, local_fin)
  
      shift = centre - (length / 2.0_WP) ! Amount the beam is shifted from 
                                         ! having the head at z2=0
  
      local_start = local_start + shift
      local_fin = local_fin + shift
  
      CALL PulseGrid(iGridType=gridtype, &
                     iNumMP=locN, &
                     sStart=local_start, &
                     sEnd=local_fin, &
                     sGrid=Grid, &
                     qOK = qOKL)
  
      IF (.NOT. qOKL) GOTO 1000
        
      CALL DistributionIntegralZ2(inttype, &
                                  locN, &
                                  iNMP, &
                                  Grid, &
                                  centre, &
                                  sigma, &
                                  MPI_DOUBLE_PRECISION, &
                                  MPI_SUM, &
                                  Integral, &
                                  qOKL)


    end if




  ELSE

    CALL getExtent(centre,length,start,final)

    CALL PulseGrid(iGridType=gridtype, &
                   iNumMP=iNMP, &
                   sStart=start, &
                   sEnd=final, &
                   sGrid=Grid, &
                   qOK = qOKL)

    IF (.NOT. qOKL) GOTO 1000
		  
    CALL DistributionIntegral(inttype, &
                              iNMP, &
                              Grid, &
                              centre, &
                              sigma, &
                              Integral, &
                              qOKL)
  
    IF (.NOT. qOKL) GOTO 1000

  END IF

!     Set error flag and exit         

  qOK = .TRUE.				    
  GOTO 2000     

!     Error Handler

1000 CALL Error_log('Error in electron_grid:genGrid',tErrorLog_G)
  PRINT*,'Error in electron_grid:genGrid'

2000 CONTINUE

END SUBROUTINE genGrid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE getStEnd(nbeams,totalmps_b,b_sts,b_ends)

! This routine calculates the start and ending indices
! for each beam within the larger array.
!
!            ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: nBeams
  INTEGER(KIND=IPL), INTENT(IN) :: totalmps_b(nBeams)
  INTEGER(KIND=IPL), INTENT(INOUT) :: b_sts(nBeams),b_ends(nBeams)

!            LOCAL ARGS

  INTEGER(KIND=IP) :: b_ind

  b_sts(1) = 1_IPL
  b_ends(1) = totalmps_b(1)

  IF (nbeams > 1) THEN

    DO b_ind = 2,nbeams
 
      b_sts(b_ind) = b_ends(b_ind-1) + 1_IPL
      b_ends(b_ind) = b_ends(b_ind-1) + totalmps_b(b_ind)

    END DO

  END IF

  END SUBROUTINE getStEnd

end module grids