module Recoil


use paratype
use ArrayFunctions
use Globals
use rhs_vars

implicit none



!private real(kind=wp), allocatable :: dp2f(:), sField4ElecReal(:), &
!                                      sField4ElecImag(:) !, &
!                                      !Lj(:)



!real(kind=wp), allocatable :: dp2f(:), sField4ElecReal(:), &
!                              sField4ElecImag(:) !, &
                              !Lj(:)



contains


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine sig_avgloss(sgam, &
                      spread_loss)

    implicit none


    real(kind=wp), contiguous, intent(in) ::  sgam(:)

    real(kind=wp), contiguous, intent(out) :: spread_loss(:)
    real(kind=wp) TwoPi,aw_mean,FK,AvLossPrm,EnSpPrm
    integer(kind=ip) :: iSteps4Diff

    !logical, intent(inout) :: qOK

    !LOGICAL :: qOKL
    ! Calculate number of steps per period
    iSteps4Diff = nint(diffStep / sStepSize)
    ! Calculate mean value of Aw - to be consistent with used equations (TBCh)
    aw_mean=sAw_G/DSQRT(2.0_WP)
    ! Define 2*Pi
    TwoPi=2.0_WP*3.14159265359_WP
    ! Define constant factor F(K) for sigma gamma.
    FK=(1.7_WP*aw_mean+(1_WP/(1_WP+1.88_WP*aw_mean+0.8_WP*aw_mean**2_WP)))

    !qOK = .false.

    ! Check switches for energy spread and average loss. Trigger set to true activates the
    ! selected part of equation (loss or spread) while the other is multiplied by zero
    if (qAvgLoss_G) then
      AvLossPrm = 1.0_WP
    else
      AvLossPrm = 0.0_WP
    endif

    if (qEnergySpread_G) then
      EnSpPrm = 1.0_WP
    else
      EnSpPrm = 0.0_WP
    endif
    !print *,RanNumRecoil_G


  allocate(RanNumRecoil_G(iNumberElectrons_G))
  CALL RANDOM_SEED()
  CALL RANDOM_NUMBER(RanNumRecoil_G)
  RanNumRecoil_G = (2.0_WP*RanNumRecoil_G)-1.0_WP

!$OMP WORKSHARE

    spread_loss = &
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           + ((lam_w_g/iSteps4Diff)*(1.0_WP/sGammaR_G)*(-2.0_WP/3.0_WP)*2.818E-15*(((sgam*sGammaR_G)*(TwoPi/lam_w_g)*aw_mean) &
            **2.0_WP)) * AvLossPrm &
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           ! Energy spread increase
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           + (RanNumRecoil_G*(1.0_WP/sGammaR_G)*DSQRT((TwoPi/lam_w_g)**3.0_WP*aw_mean**2.0_WP*(sgam*sGammaR_G)**4.0_WP &
           * FK*1.015E-27*(lam_w_g/iSteps4Diff))*DSQRT(3.0_WP) * EnSpPrm)
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!$OMP END WORKSHARE
!print *,spread_loss
deallocate(RanNumRecoil_G)
!print *,lg_G,sGammaR_G,lam_w_g,aw_mean
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !Diagnostic prints for avg and sig - genuine equations used - no scaling, just for test !
    !print *,'avg=',((-2.0_WP/3.0_WP)*2.818E-15*(((sGammaR_G)*(TwoPi/lam_w_g)*aw_mean)**2.0_WP)*lam_w_g)
    !print *,'sig=',(DSQRT((TwoPi/lam_w_g)**3.0_WP*aw_mean**2.0_WP*(sGammaR_G)**4.0_WP &
    !       * FK*1.015E-27*lam_w_g)*DSQRT(3.0_WP))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    ! Set the error flag and exit

    !qOK = .true.

    !goto 2000

    !1000 call Error_log('Error in equations:dp2dz',tErrorLog_G)

    !print*,'Error in equations:dp2dz'

    !2000 continue


  end subroutine sig_avgloss
end module Recoil