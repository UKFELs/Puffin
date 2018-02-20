! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Various routines to setup and scale the constants in the simulation.

module setupcalcs

USE Paratype
USE ParallelInfoType
USE TransformInfoType
USE IO
USE ArrayFunctions
USE typesAndConstants
USE Globals
USE electronInit
USE gMPsFromDists
use avwrite
use MASPin
use h5in
use parafield
use scale

implicit none

contains

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to pass all the temporary variables to global
!> vars used in the integration loop.
!> @param[in] iNN Number of nodes in field mesh in x, y and z2
!> @param[in] iNMPs Number of macroparticles use to sample beam in each dimension
!> @param[in] fx Polarization parameter for the Puffin elliptical wiggler (see docs)
!> @param[in] fy Polarization parameter for the Puffin elliptical wiggler (see docs)
!> @param[in] taper Undulator taper d alpha / d zbar
!> @param[in] sFiltFrac Cutoff for high pass filter in diffraction stage, in un`its
!> of the resonant frequency specified by the reference parameters.
!> @param[in] dStepFrac Diffraction step size in units of undulator period
!> @param[in] sBeta Absorption constant for boundaries
!> @param[in] qFormatted Whether writing formatted data or not (only for SDDS files)
!> @param[in] qSwitch Array of switches for different simulation options
!> @param[inout] qOK Error flag.

SUBROUTINE passToGlobals(tScale, iNN, sElmLen, qSimple, iNMPs, &
                         taper, sFSigX, sFSigY, sFiltFrac, &
                         dStepFrac, sBeta, &
                         qFormatted, qSwitch, qOK)

    use typeScale
    implicit none

    type(fScale), intent(in) :: tScale
    integer(kind=ip),  intent(in)    :: iNN(:), iNMPs(:,:)

    real(kind=wp),     intent(in)    :: sElmLen(:), sFSigX, sFSigY
    real(kind=wp),     intent(in)    :: taper

    real(kind=wp),     intent(in)    :: sFiltFrac, dStepFrac, sBeta
    logical,           intent(in)    :: qSwitch(nSwitches_CG), qFormatted, &
                                        qSimple
    logical,           intent(out)   :: qOK


    real(kind=wp) :: LenZ2, modfact1, sbetaz, aw_rms
    logical :: qOKL

    qOK = .false.

!                  Pass to globals

    NX_G  = iNN(iX_CG)
    NY_G  = iNN(iY_CG)
    NZ2_G = iNN(iZ2_CG)

    ntrnds_G = NX_G * NY_G

    ata_G = 2.0_wp * pi * sFSigX * sFSigY

!    nspinDX = 31_ip
!    nspinDY = 31_ip

!    if ( mod(nx_g, 2) .ne. mod(nspinDX, 2) ) then
!
!      nspinDX =  nspinDX + 1
!
!    end if
!
!    if ( mod(ny_g, 2) .ne. mod(nspinDY, 2) ) then
!
!      nspinDY =  nspinDY + 1
!
!    end if


    qInnerXYOK_G = .true.


    ntrndsi_G = nspinDX * nspinDY

    IF (NX_G == 1 .AND. NY_G == 1) THEN

       iNodesPerElement_G = 2_IP

    ELSE

       iNodesPerElement_G = 8_IP

    END IF



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!      Define reduced node set in x and y,
!!      which the electron beam is initialized
!!      within, transversely.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



    if (NX_G*NY_G==1) then


!            1D case


      iRedNodesX_G = 1_IP
      iRedNodesY_G = 1_IP

      nspinDX = 1_ip
      nspinDY = 1_ip
      ntrndsi_G = 1_ip

    else


!            3D case


      if (iRedNodesX_G < 0_ip) then

!        If iRedNodes was not defined, set to default


        if (qSimple) then

          iRedNodesX_G = 20_ip

        else

          iRedNodesX_G = 20_ip

        end if

      end if



      if (iRedNodesY_G < 0_ip) then

        if (qSimple) then

          iRedNodesY_G = 20_ip

        else

          iRedNodesY_G = 20_ip

        end if

      end if

    end if


    outnodex_G=NX_G-iRedNodesX_G
    outnodey_G=NY_G-iRedNodesY_G

!     Set up the length of ONE element globally

    sLengthOfElmX_G  = sElmLen(iX_CG)
    sLengthOfElmY_G  = sElmLen(iY_CG)
    sLengthOfElmZ2_G = sElmLen(iZ2_CG)

    delta_G = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G

!     Filter fraction to frequency in Fourier space

    Lenz2 = sLengthOfElmZ2_G * NZ2_G
    sFilt = Lenz2 / tScale%lrbar * sFiltFrac

!     Set up parameters
    if (qMod_G) then

      modfact1 = mf(1)

    else

      modfact1 = 1.0_WP

    end if

    n2col = modfact1
    n2col0 = n2col
    sz0 = 0.0_WP
    undgrad = taper












    cf1_G = tScale%eta / tScale%kappa**2


    diffstep = dStepFrac * tScale%lrbar
    sBeta_G = sBeta

    NBX_G = 16_IP   ! Nodes used in boundaries
    NBY_G = 16_IP

    NBZ2_G = 37_IP


!    if (qSwitch(iOneD_CG)) then
!      zUndType_G = ''
!    else
      zUndType_G = tScale%undType
!    end if





!  Get focusing for 'reference' beam energy

    if (tScale%undType == 'curved') then

      kx_und_G = SQRT(tScale%eta/(8.0_WP*tScale%rho**2)) ! Giving equal focusing for now....
      ky_und_G = SQRT(tScale%eta/(8.0_WP*tScale%rho**2))

      sKBetaX_G = tScale%aw / sqrt(2.0_wp * tScale%eta) / tScale%gamma0 * kx_und_G
      sKBetaY_G = tScale%aw / sqrt(2.0_wp * tScale%eta) / tScale%gamma0 * ky_und_G

    else if (tScale%undType == 'planepole') then

      kx_und_G = 0.0_wp
      ky_und_G = 0.0_wp

      sKBetaX_G = 0.0_wp
      sKBetaY_G = tScale%aw / 2.0_wp / sqrt(2.0_wp) / tScale%rho / tScale%gamma0

    else if (tScale%undType == 'helical') then

      sKBetaX_G = tScale%aw / 2.0_wp / sqrt(2.0_wp) / tScale%rho / tScale%gamma0
      sKBetaY_G = tScale%aw / 2.0_wp / sqrt(2.0_wp) / tScale%rho / tScale%gamma0

    else

      sKBetaX_G = tScale%aw / 2.0_wp / sqrt(2.0_wp) / tScale%rho / tScale%gamma0
      sKBetaY_G = tScale%aw / 2.0_wp / sqrt(2.0_wp) / tScale%rho / tScale%gamma0

    end if

!    sKBetaXSF_G = 10.0_wp
!    sKBetaYSF_G = 10.0_wp



    qOneD_G                  = qSwitch(iOneD_CG)
    qElectronsEvolve_G       = qSwitch(iElectronsEvolve_CG)
    qFieldEvolve_G           = qSwitch(iFieldEvolve_CG)
    qElectronFieldCoupling_G = qSwitch(iElectronFieldCoupling_CG)
    qDiffraction_G           = qSwitch(iDiffraction_CG)
    qFocussing_G             = qSwitch(iFocussing_CG)
    qResume_G                = qSwitch(iResume_CG)
    qDump_G                  = qSwitch(iDump_CG)


!    if ((sKBetaXSF_G <= 0) .and. (sKBetaYSF_G <= 0) ) then
!      qFocussing_G = .false.
!    end if


    if (qFocussing_G) then

      if (sKBetaXSF_G > 0) then
        sKBetaX_G = sKBetaXSF_G
      else
        sKBetaXSF_G = 0.0_wp
        if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 0)) then
          print*, 'No in-undulator strong focusing in x'
        end if
      end if

      if (sKBetaYSF_G > 0) then
        sKBetaY_G = sKBetaYSF_G
      else
        sKBetaYSF_G = 0.0_wp
        if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 0)) then
          print*, 'No in-undulator strong focusing in y'
        end if
      end if

    end if


    sMNum_G = 1_wp



    if (sRedistLen_G < 0) sRedistLen_G = 4.0_wp

    if (iRedistStp_G < 0) iRedistStp_G = 60_ip



!     Get the number of nodes

    iNumberNodes_G = int(iNN(iX_CG), kind=IPN) * &
                       int(iNN(iY_CG), kind=IPN) * &
                         int(iNN(iZ2_CG), kind=IPN)




!     Get n_pk_bar

    npk_bar_G = tScale%npkbar

    IF(iNumberNodes_G <= 0_IPN) THEN
       CALL Error_log('iNumberNodes_G <= 0.',tErrorLog_G)
       GOTO 1000
    END IF


    dz2_I_G = tScale%lrbar
    call getCurrNpts(dz2_I_G, npts_I_G)


    tArrayE(:)%tFileType%qFormatted = qFormatted
    tArrayA(:)%tFileType%qFormatted = qFormatted
    tArrayZ%tFileType%qFormatted = qFormatted


    sRho_G = tScale%rho
    saw_G = tScale%aw
    sEta_G = tScale%eta
    sKappa_G = tScale%kappa
    sGammaR_G = tScale%gamma0
    lc_g = tScale%lc
    lg_g = tScale%lg
    lam_w_g = tScale%lambda_w
    lam_r_g = tScale%lambda_r

!    qEquiXY_G = .false.
!    nseqparts_G = 1000_ip


!     Set error flag and exit

    qOK = .TRUE.

    GOTO 2000

1000 CALL Error_log('Error in setupCalcs:setupParams',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE passToGlobals

! ***************************************************************

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to recalculate the beam charge to match the value of rho
!> supplied.
!> @param[out] sQb Beam charge
!> @param[in] sSigz2 Beam standard deviation in z2
!> @param[in] sLenz2 Beam total length in z2
!> @param[in] sSigTails Standard deviation used in z2 for the current profile
!> tail-off, if used
!> @param[in] qTails If tapering off the current profile with gaussian tails.
!> @param[in] sSigX Standard deviation of beam current density profile in xbar
!> @param[in] sSigY Standard deviation of beam current density profile in ybar

subroutine fixCharge(sQb, sSigz2, sLenz2, sSigTails, qTails, &
                    sSigX, sSigY)

  real(kind=wp), intent(out) :: sQb
  real(kind=wp), intent(in) :: sSigz2, sLenz2, sSigTails, &
                                 sSigX, sSigY

  logical, intent(in) :: qTails

  real(kind=wp) :: sLArea, sTArea
  logical :: qOneD

  qOneD = qOneD_G

  sTArea = sqrt(2.0_wp*pi) * sSigX &
            * sqrt(2.0_wp*pi) * sSigY

  call getLBArea(sLArea, sSigz2, sLenz2, sSigTails, qTails)

  call getQFmNpk(sQb, sTarea, sLarea, qOneD)

  if ((tProcInfo_G%qroot) .and. (ioutInfo_G > 0)) print*, 'FIXING CHARGE '
  if ((tProcInfo_G%qroot) .and. (ioutInfo_G > 0)) print*, 'Q =  ', sQb

end subroutine fixCharge

! ***************************************************************

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Calculates the area under the current profile in z2.
!> @param[out] sLArea Area under the curve of the current distribution.
!> @param[in] sSigz2 Beam standard deviation in z2
!> @param[in] sLenz2 Beam total length in z2
!> @param[in] sSigTails Standard deviation used in z2 for the current profile
!> tail-off, if used
!> @param[in] qTails If tapering off the current profile with gaussian tails.

subroutine getLBArea(sLArea, sSigz2, sLenz2, sSigTails, qTails)

  real(kind=wp), intent(out) :: sLArea
  real(kind=wp), intent(in) :: sSigz2, sLenz2, sSigTails
  logical, intent(in) :: qTails

  real(kind=wp) :: sEndsLen, sMainLen
  logical :: qFlatTop

  qFlatTop = .false.

  if (sSigZ2 >= 1e6) qFlatTop = .true.

  sEndsLen = 0.0_wp

  if (qFlatTop) then
    if (qTails) then
      sEndsLen = gExtEj_G * sSigTails
      sMainLen = sLenz2 - sEndsLen
      sLArea = sMainLen + sqrt(2*pi)*sSigTails
    else
      sLArea = sLenz2
    end if
  else
    sLArea = sqrt(2*pi) * sSigz2
  end if

end subroutine getLBArea

! ***************************************************************

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Calculates the area under the current profile in z2.
!> @param[out] sLArea Area under the curve of the current distribution.
!> @param[in] sSigz2 Beam standard deviation in z2
!> @param[in] sLenz2 Beam total length in z2
!> @param[in] sSigTails Standard deviation used in z2 for the current profile
!> tail-off, if used
!> @param[in] qTails If tapering off the current profile with gaussian tails.

subroutine getQFmNpk(sQb, sTarea, sLarea, qOneD)

  real(kind=wp), intent(out) :: sQb
  real(kind=wp), intent(in) :: sTArea, sLArea
  logical, intent(in) :: qOneD

  real(kind=wp) :: sVol, sNe


!  Set phase space volume

    if (qOneD) then
      sVol = sLArea * ata_G
      if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 0)) print*, 'TRANS AREA = ', ata_G
    else
      sVol = sLArea * sTArea
    end if

! Number of electrons

    sNe = npk_bar_G * sVol
    sQb = q_e * sNe

end subroutine getQFmNpk




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



SUBROUTINE SetUpInitialValues(nseeds, freqf, ph_sh, SmeanZ2, sFiltFrac, &
                              qFlatTopS, sSigmaF, &
                              sA0_x, sA0_y, qOK)

    IMPLICIT NONE
!
! Set up the initial macroparticle and field values
!
!                   ARGUMENTS
!
! sSeed              INPUT    Descriptions of the sSeed field
! qInitialGauss      INPUT    If to use gauss
! sA0_Re             INPUT    Initial field value (real)
! sA0_Im             INPUT    Initial field value (imaginary)
! sX0                INPUT    Initial X values
! sY0                INPUT    Initial Y values
! sZ20               INPUT    Initial z2 values
! sV                 UPDATE   Initial values of variables to
!                                      be integrated
! qOK                OUTPUT   Error flag

    INTEGER(KIND=IP), INTENT(IN) :: nseeds
    REAL(KIND=WP), INTENT(IN)    :: sSigmaF(:,:), SmeanZ2(:), &
                                    freqf(:), ph_sh(:)
    LOGICAL, INTENT(IN) :: qFlatTopS(:)
    REAL(KIND=WP), INTENT(IN)    :: sA0_x(:)
    REAL(KIND=WP), INTENT(IN)    :: sA0_y(:)
    real(kind=wp), intent(in)    :: sFiltFrac
!    REAL(KIND=WP), INTENT(INOUT) :: sA(:)
    LOGICAL,       INTENT(OUT)   :: qOK

!                LOCAL ARGS
!
! qOKL         Local error flag
! iZ2          Number of nodes in Z2
! iXY          Number of nodes in XY plane
! sA0gauss_Re  Initial field over all planes

    LOGICAL           :: qOKL
    LOGICAL           :: qInitialGauss
    INTEGER(KIND=IP)  :: iZ2,iXY,i,lowind,highind,error,NN(3)
    REAL(KIND=WP)     :: z2bar,rho
    REAL(KIND=WP)     :: sLengthOfElm(3)
!    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE :: sAx_mag,sAy_mag,&
!                                              sAreal,sAimag

!     Set error flag to false

    qOK = .FALSE.

    sZi_G = 0.0_wp
    sZlSt_G = 0.0_wp

    iZ2 = NZ2_G
    iXY = NX_G*NY_G

    NN(iX_CG) = NX_G
    NN(iY_CG) = NY_G
    NN(iZ2_CG) = NZ2_G

    sLengthOfElm(iX_CG) = sLengthOfElmX_G
    sLengthOfElm(iY_CG) = sLengthOfElmY_G
    sLengthOfElm(iZ2_CG) = sLengthOfElmZ2_G

!    ALLOCATE(sAreal(iXY*iZ2),sAimag(iXY*iZ2))

!    CALL getSeeds(NN,sSigmaF,SmeanZ2,sA0_x,sA0_y,qFlatTopS,sRho_G,freqf, &
!                  ph_sh, nseeds,sLengthOfElm,sAreal,sAimag)
!   print*,'It is seedy, but what type...'
!   print*,iFieldSeedType_G



    call getPaSeeds(NN,sSigmaF,SmeanZ2,sA0_x,sA0_y,qFlatTopS,sRho_G,&
                    freqf,ph_sh,nseeds,sLengthOfElm)

!    sA(1:iXY*iZ2) = sAreal
!    sA(iXY*iZ2 + 1:2*iXY*iZ2) = sAimag

!    DEALLOCATE(sAreal,sAimag)


!     Set error flag and exit

    qOK = .TRUE.
    GOTO 2000

1000 CALL Error_log('Error in FEMethod:SetUpInitialValues',tErrorLog_G)
    PRINT*,'Error in FEMethod:SetUpInitialValues'
2000 CONTINUE

END SUBROUTINE SetUpInitialValues

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Scale the variables from the simple beam input file, the seed input file,
!> and the radiation mesh. Accepts the data in S.I. units, and converts them
!> in-place to the Puffin scaled data equivalents (see the Puffin user manual
!> for more info on scaled variables)
!> @param[in] tScale Custom Fortran type describing scaling (see typeScale.f90).
!> @param[inout] sEleSig 2D real array of size (nbeams, 6), containing the rms
!> standard deviation of electron beam in each of the 6 spatial and momenta
!> dimensions. The first \f$ i \f$ dimension indicates the values of the
!> \f$ i^{th} \f$ beam, the second dimension is in order of
!> \f$(x, y, t, \frac{dx}{dz}, \frac{dy}{dz}, \Gamma)\f$. On output, the values
!> are in scaled dimensionless units
!> \f$(\bar{x}, \bar{y}, \bar{z}_2, \bar{p}_x, \bar{p}_y, \Gamma)\f$.
!> @param[inout] sLenEPulse Total sampled length of the beam in each dimension,
!> for each beam. Layout order and units same as sEleSig.
!> @param[inout] sSigEdge Standard deviation of Gaussian tails in the temporal
!> dimension if using a flat-top beam in that dimension. In \f$ s \f$ on input,
!> in \f$ \bar{z}_2 \f$ on output.
!> @param[inout] beamCenZ2 Mean position of beam in \f$ t \f$ in seconds on
!> input, and in scaled dimensionless units of \f$ \bar{z}_2 \f$ on output.
!> @param[inout] chirp Energy chirp of beam, in units of
!> \f$ \frac{1}{gamma c}\frac{d\gamma}{dt} \f$ on input, and units of
!> \f$ \frac{d\gamma}{d\bar{z}_2} \f$ on output
!> @param[inout] emitx Emittance of beam in x. Geometric emittance on input,
!> scaled \f$ \bar{\epsilon}_x \f$ on output.
!> @param[inout] emity Emittance of beam in y. Geometric emittance on input,
!> scaled \f$ \bar{\epsilon}_y \f$ on output.
!> @param[inout] gamFrac Energy of beam as a fraction of reference energy
!> @param[inout] sFieldModelLength Total length of field mesh in each dimension,
!> in order x, y, z, in \f$ x, y, t \f$ in meters, meters, and seconds on input, 
!> converted to dimensionless \f$ \bar{x}, \bar{y}, \bar{z}_2\f$ on output
!> @param[inout] sLengthofElm Size of each element in the mesh in each dimension,
!> in order x, y, z, in \f$ x, y, t \f$ in meters, meters, and seconds on input, 
!> converted to dimensionless \f$ \bar{x}, \bar{y}, \bar{z}_2\f$ on output
!> @param[inout] sSeedSigma Standard deviation of each seed, in each dimension.
!> @param[inout] sAx Intensity of x-polarized field. In units of \f$ Wm^{-2} \f$
!> on input, in scaled $\|A_x\|^2$ on output.
!> @param[inout] sAy Intensity of y-polarized field. In units of \f$ Wm^{-2} \f$
!> on input, in scaled $\|A_y\|^2$ on output.
!> @param[inout] scr Mean value of radiation seed in \f$ t \f$ in seconds on
!> input, and in dimensionless \f$ \bar{z}_2 \f$ on output.

subroutine scaleParams(tScale, sEleSig, sLenEPulse, sSigEdge, &
                       beamCenZ2, chirp, emitx, emity, gamFrac, &
                       sFieldModelLength, sLengthofElm, &
                       sSeedSigma, sAx, sAy, scr)

    use typeScale
    
    implicit none

    type(fScale), intent(in) :: tScale

    real(kind=wp), intent(inout) :: sEleSig(:,:), sLenEPulse(:,:), &
                                    sSigEdge(:), beamCenZ2(:), &
                                    chirp(:), &
                                    sFieldModelLength(:), &
                                    sLengthofElm(:), &
                                    sSeedSigma(:,:), &
                                    emitx(:), emity(:), &
                                    sAx(:), sAy(:), scr(:)

    real(kind=wp), intent(in) :: gamFrac(:)


    integer(kind=ip) :: nbeams, nseeds, ib, is

    nbeams = size(sEleSig(:,1))

    do ib = 1, nbeams

      call tScale%scaleX(sEleSig(ib,iX_CG))
      call tScale%scaleX(sEleSig(ib,iY_CG))
      call tScale%scalePx(gamFrac(ib), sEleSig(ib,iPX_CG))
      call tScale%scalePx(gamFrac(ib), sEleSig(ib,iPY_CG))
      call tScale%scaleT(sEleSig(ib,iZ2_CG))

      call tScale%scaleX(sLenEPulse(ib,iX_CG))
      call tScale%scaleX(sLenEPulse(ib,iY_CG))
      call tScale%scalePx(gamFrac(ib), sLenEPulse(ib,iPX_CG))
      call tScale%scalePx(gamFrac(ib), sLenEPulse(ib,iPY_CG))
      call tScale%scaleT(sLenEPulse(ib,iZ2_CG))

      call tScale%scaleT(sSigEdge(ib))
      call tScale%scaleT(beamCenZ2(ib))
      chirp(ib) = chirp(ib) * tScale%gamma0 * tScale%lc
      call tScale%scaleEmit(emitx(ib))
      call tScale%scaleEmit(emity(ib))

    end do

    call tScale%scaleX(sFieldModelLength(iX_CG))
    call tScale%scaleX(sFieldModelLength(iY_CG))
    call tScale%scaleT(sFieldModelLength(iZ2_CG))
    call tScale%scaleX(sLengthofElm(iX_CG))
    call tScale%scaleX(sLengthofElm(iY_CG))
    call tScale%scaleT(sLengthofElm(iZ2_CG))

    nseeds = size(sSeedSigma(:,1))

    do is = 1, nseeds

      call tScale%scaleX(sSeedSigma(is,iX_CG))
      call tScale%scaleX(sSeedSigma(is,iY_CG))
      call tScale%scaleT(sSeedSigma(is,iZ2_CG))
      call tScale%scaleIntensity(sAx(is))
      call tScale%scaleIntensity(sAy(is))
      call tScale%scaleT(scr(is))
      call tScale%scaleT(sSigFj_G(is))

    end do

end subroutine scaleParams






subroutine calcScaling(tScale, srho, saw, sgamr, slam_w, &
                       zUndType, sfx, sfy, qOneD)

  use typeScale
  implicit none

  type(fScale), intent(inout) :: tScale

  real(kind=wp), intent(in) :: srho, saw, sgamr, slam_w
  
  real(kind=wp), intent(inout) :: sfx, sfy

  character(32_IP), intent(in) :: zUndType

  logical, intent(in) :: qOneD

  real(kind=wp) :: saw_rms, sBetaz

  call tScale%init(srho, saw, sgamr, slam_w, &
                   zUndType, sfx, sfy, qOneD)

  if (zUndType == 'curved') then

    kx_und_G = SQRT(tScale%eta/(8.0_WP*tScale%rho**2)) ! Giving equal focusing for now....
    ky_und_G = SQRT(tScale%eta/(8.0_WP*tScale%rho**2))

    sKBetaX_G = saw / sqrt(2.0_wp * tScale%eta) / tScale%gamma0 * kx_und_G
    sKBetaY_G = saw / sqrt(2.0_wp * tScale%eta) / tScale%gamma0 * ky_und_G

    sfx = 0.0_wp
    sfy = 1.0_wp

  else if (zUndType == 'planepole') then

    sfx = 0.0_wp
    sfy = 1.0_wp

  else if (zUndType == 'helical') then

    sfx = 1.0_wp
    sfy = 1.0_wp

  end if

end subroutine calcScaling





subroutine calcSamples(tScale, sFieldModelLength, iNumNodes, sLengthOfElm, &
                       sStepSize, stepsPerPeriod, nSteps, &
                       nperiods, nodesperlambda, sGamFrac, &
                       sLenEPulse, iNumElectrons, qsimple)

  use typeScale

  implicit none

  type(fScale), intent(in) :: tScale
  real(kind=wp), intent(inout) :: sFieldModelLength(:), sLenEPulse(:,:)

  real(kind=wp), intent(in) :: sGamFrac(:)

  integer(kind=ip), intent(in) :: nperiods, nodesperlambda, &
                                  stepsPerPeriod

  real(kind=wp), intent(out) :: sLengthOfElm(:), sStepSize



  integer(kind=ip), intent(inout) :: iNumNodes(:), iNumElectrons(:,:)
  integer(kind=ip), intent(out) :: nSteps
  logical, intent(in) :: qsimple

  real(kind=wp), allocatable :: smeanp2(:), fmlensTmp(:)
  real(kind=wp) :: dz2, szbar, fmlenTmp
  integer(kind=ip) :: minENum, minESample



  dz2 = tScale%lrbar / real(nodesperlambda-1_IP,kind=wp)

  iNumNodes(iZ2_CG) = ceiling(sFieldModelLength(iZ2_CG) / dz2) + 1_IP

  if (fieldMesh == iPeriodic) then
  
    if (sPerWaves_G < 0.0_wp) then

      sLengthOfElm(iZ2_CG) = dz2
      sFieldModelLength(iZ2_CG) = real(iNumNodes(iZ2_CG) - 1_ip, kind=wp) * dz2
      sperwaves_G = sFieldModelLength(iZ2_CG) / tScale%lrbar
      sLenEPulse(1,iZ2_CG) = sFieldModelLength(iZ2_CG)

    else

!           Field mesh length is then number of waves times scaled wavelength

      sFieldModelLength(iZ2_CG) = sperwaves_G * tScale%lrbar
      sLengthOfElm(iZ2_CG) = dz2

!            For now, keeping dz2 to give an integer number of nodes per 
!           scaled wavelength, and rounding total mesh length to nearest
!                           integer number of nodes

      iNumNodes(iZ2_CG) = nint((sFieldModelLength(iZ2_CG) / dz2), kind=ip) + 1_IP
      sFieldModelLength(iZ2_CG) = real(iNumNodes(iZ2_CG) - 1_ip, kind=wp) * dz2
      
      sLenEPulse(1,iZ2_CG) = sFieldModelLength(iZ2_CG)

    end if

  end if
  

  if (iNumNodes(iX_CG) <= 1_ip) then

    sLengthOfElm(iX_CG) = 1_wp

  else

    sLengthOfElm(iX_CG) = sFieldModelLength(iX_CG) / &
                      real((iNumNodes(iX_CG) - 1_ip), kind=wp)

  end if




  if (iNumNodes(iY_CG) <= 1_ip) then

    sLengthOfElm(iY_CG) = 1_wp

  else

    sLengthOfElm(iY_CG) = sFieldModelLength(iY_CG) / &
                      real((iNumNodes(iY_CG) - 1_ip), kind=wp)

  end if


!  if (iNumNodes(iZ2_CG) <= 1_ip) then

!    sLengthOfElm(iZ2_CG) = sFieldModelLength(iZ2_CG)
 !   print*, 'WARNING, only one node in Z2!!'

!  else

    sLengthOfElm(iZ2_CG) = sFieldModelLength(iZ2_CG) / &
                      real((iNumNodes(iZ2_CG) - 1_ip), kind=wp)

!  end if



  if (stepsPerPeriod >= 1) then


    sStepSize = tScale%lrbar / real(stepsPerPeriod,kind=wp)
    nSteps = nperiods * stepsPerPeriod

  else

    if (ioutInfo_G > 0) print*, 'less than one step per period!!'

  end if

!  if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 1)) then
!    print*, 'step size is --- ', sStepSize
!  end if





  szbar = nperiods * tScale%lrbar



  minESample = 4_ip   ! minimum MP's per wavelength
  !dztemp = slamr / minESample

  if (iInputType_G == iGenHom_G) then


    minENum = ceiling(sLenEPulse(1,iZ2_CG) / (tScale%lrbar / real(minESample, kind=wp)) )


    if ((iNumElectrons(1,iZ2_CG) < 0) .or. (iNumElectrons(1,iZ2_CG) < minENum) ) then

      if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 0)) then
        print*, '******************************'
        print*, ''
        print*, 'WARNING - e-beam macroparticles sampling &
                                        & in z2 not fine enough - fixing...'
      end if

      iNumElectrons(1,3) = minENum

      if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 0)) then
        print*, 'num MPs in z2 now = ', &
                          iNumElectrons(1,iZ2_CG)
      end if

    end if




!   MAX P2 -

    if (qsimple) then

    allocate(smeanp2(size(sGamFrac)), fmlensTmp(size(sGamFrac)))
    smeanp2 = 1.0_wp / sGamFrac**2.0_wp  ! Estimate of p2...

    fmlensTmp = sLenEPulse(:,iZ2_CG) + (smeanp2(:) * szbar)
    fmlenTmp = maxval(fmlensTmp)

    if (fieldMesh == itemporal) then

      if (sFieldModelLength(iZ2_CG) <= fmlenTmp + 1.0_wp) then


        if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 0)) then
          print*, '******************************'
          print*, ''
          print*, 'WARNING - field mesh may not be large &
                                   &enough in z2 - fixing....'
        end if

        sFieldModelLength(iZ2_CG) = fmlenTmp + 10.0_wp  ! Add buffer 10 long for
                                                        ! extra security...

        if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 0)) then
          print*, 'Field mesh length in z2 now = ', &
                                sFieldModelLength(iZ2_CG)
          print*, ''
        end if

      end if

    end if

  end if

  deallocate(smeanp2, fmlensTmp)

  end if

  dz2 = tScale%lrbar / real(nodesperlambda-1_IP,kind=wp)

  iNumNodes(iZ2_CG) = ceiling(sFieldModelLength(iZ2_CG) / dz2) + 1_IP

!   if (fieldMesh == iPeriodic) then
!   
!     if (sPerWaves_G < 0.0_wp) then
! 
!       sLengthOfElm(iZ2_CG) = dz2
!       sFieldModelLength(iZ2_CG) = real(iNumNodes(iZ2_CG) - 1_ip, kind=wp) * dz2
!       sperwaves_G = sFieldModelLength(iZ2_CG) / (4.0_WP * pi * sRho_G)
! 
!       sLenEPulse(1,iZ2_CG) = sFieldModelLength(iZ2_CG)
! 
!     else
! 
! !           Field mesh length is then number of waves times scaled wavelength
! 
!       sFieldModelLength(iZ2_CG) = sperwaves_G * (4.0_WP * pi * sRho_G)
!       sLengthOfElm(iZ2_CG) = dz2
! 
! !            For now, keeping dz2 to give an integer number of nodes per 
! !           scaled wavelength, and rounding total mesh length to nearest
! !                           integer number of nodes
! 
!       iNumNodes(iZ2_CG) = nint((sFieldModelLength(iZ2_CG) / dz2), kind=ip) + 1_IP
!       sFieldModelLength(iZ2_CG) = real(iNumNodes(iZ2_CG) - 1_ip, kind=wp) * dz2
!       
!       sLenEPulse(1,iZ2_CG) = sFieldModelLength(iZ2_CG)
! 
!     end if
! 
!   end if
 
 
  if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 1)) then
    print*, '******************************'
    print*, ''
    print*, 'number of nodes in z2 --- ', iNumNodes(iZ2_CG)
  end if



end subroutine calcSamples





SUBROUTINE PopMacroElectrons(qSimple, fname, sQe, NE, noise, Z, LenEPulse, &
                             sigma, alphax, alphay, emitx, emity, &
                             beamCenZ2, gamma_d, eThresh, &
                             chirp, mag, fr, nbeams, qOK)

!                     ARGUMENTS

    logical, intent(in) :: qSimple
    character(*), intent(in) :: fname(:)
    REAL(KIND=WP),     INTENT(IN)    :: sQe(:), gamma_d(:)
    REAL(KIND=WP),     INTENT(INOUT)    ::  chirp(:)
    REAL(KIND=WP),     INTENT(INOUT)    :: mag(:), fr(:)
    INTEGER(KIND=IP),  INTENT(IN)    :: NE(:,:),nbeams
    LOGICAL,           INTENT(IN)    :: noise
    REAL(KIND=WP),     INTENT(IN)    :: Z
    REAL(KIND=WP),     INTENT(INOUT) :: LenEPulse(:,:)
    REAL(KIND=WP),     INTENT(INOUT) :: sigma(:,:)
    real(kind=wp),     intent(in)    :: alphax(:), alphay(:), emitx(:), emity(:)
    REAL(KIND=WP),     INTENT(INOUT) :: beamCenZ2(:)
    REAL(KIND=WP),     INTENT(IN)    :: eThresh
    LOGICAL,           INTENT(OUT)   :: qOK

!                   LOCAL ARGS

    INTEGER(KIND=IPL) :: NMacroE
    REAL(KIND=WP)     :: sQOneE, totNk_glob, totNk_loc
    REAL(KIND=WP), ALLOCATABLE  :: RealE(:)
    INTEGER(KIND=IP) :: j,error, req, lrank, rrank
    INTEGER(KIND=IPL) :: sendbuff, recvbuff
    INTEGER sendstat(MPI_STATUS_SIZE)
    INTEGER recvstat(MPI_STATUS_SIZE)
    character(1024) :: fname_temp
    LOGICAL :: qOKL

    sQOneE = 1.60217656535E-19

    qOK = .FALSE.

    IF (qSimple) ALLOCATE(RealE(nbeams))

!     Print a reminder to check whether shot-noise is
!     being modelled or not

    IF ((tProcInfo_G%qROOT) .and. (ioutInfo_G > 1)) then
       IF (noise) THEN
          PRINT *, 'SHOT-NOISE TURNED ON'
       ELSE
          PRINT *, 'SHOT-NOISE TURNED OFF'
       ENDIF
    ENDIF

!     Number of real electrons

    IF (qSimple) RealE = sQe / sQOneE


!     Change sig_gamma / gamma to sig_gamma

    IF (qSimple) LenEPulse(:,iGam_CG) = gamma_d(:) * sGammaR_G * LenEPulse(:,iGam_CG)
    IF (qSimple) sigma(:,iGam_CG) = gamma_d(:) * sGammaR_G * sigma(:,iGam_CG)

!     Setup electrons

    if (iInputType_G == iGenHom_G) then

      CALL electron_grid(RealE,NE,noise, &
                         Z,nbeams, LenEPulse,sigma, alphax, alphay, &
                         emitx, emity, beamCenZ2, gamma_d, &
                         eThresh,tTransInfo_G%qOneD, &
                         chirp,mag,fr,qOKL)
      IF (.NOT. qOKL) GOTO 1000

    else if (iInputType_G == iReadDist_G) then

      call getMPs(fname, nbeams, Z, noise, eThresh)

    else if (iInputType_G == iReadMASP_G) then

      fname_temp = fname(1)
      call readMASPfile(fname_temp)

    else if (iInputType_G == iReadH5_G) then
      fname_temp = fname(1)
      call readH5beamfile(fname_temp)
!    print *,"Rank ", tProcInfo_G%Rank
    else

      if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 0)) then
        print*, 'No beam input type specified....'
        print*, 'Exiting...'
      end if
      call UnDefineParallelLibrary(qOKL)
      stop

    end if

    if (iGloNumElectrons_G <= 0_IPL) then
       call Error_log('iGloNumElectrons_G <=0.',tErrorLog_G)
       goto 1000
    end if

    if (iNumberElectrons_G>0_IPL) then
      totNk_loc = sum(s_chi_bar_G) * npk_bar_G
    else
      totNk_loc = 0._WP
    end if
    
    if (qOneD_G) totNk_loc = totNk_loc * ata_g
!    print *,"Rank ", tProcInfo_G%Rank, " sum ",totNk_loc
    CALL MPI_ALLREDUCE(totNk_loc, totNk_glob, 1, MPI_DOUBLE_PRECISION, &
                       MPI_SUM, MPI_COMM_WORLD, error)


    if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 0)) then


      print*, ''

      print*, '-----------------------------------------'
      print*, 'Generated beam....'

      if (ioutInfo_G > 1) then
        print*, 'Total number of macroparticles = ', iGloNumElectrons_G

        print*, 'Avg num of real electrons per macroparticle Nk = ', &
                                    totNk_glob / iGloNumElectrons_G

        print*, 'Total number of real electrons modelled = ', &
                        totNk_glob
      end if


    end if

!    Set up the array describing the number of electrons
!    on each processor

    IF (tProcInfo_G%rank == tProcInfo_G%size-1) THEN
       rrank = 0
       lrank = tProcInfo_G%rank-1
    ELSE IF (tProcInfo_G%rank==0) THEN
       rrank = tProcInfo_G%rank+1
       lrank = tProcInfo_G%size-1
    ELSE
       rrank = tProcInfo_G%rank+1
       lrank = tProcInfo_G%rank-1
    END IF

    ALLOCATE(procelectrons_G(tProcInfo_G%size))

    procelectrons_G(1) = iNumberElectrons_G

    sendbuff = iNumberElectrons_G
    recvbuff = iNumberElectrons_G

!     When the following loop is complete, the array
!     procelectrons_G will contain a record of the number
!     of macroparticles on each process. The first element,
!     procelectrons_G(1), will contain this processes local
!     macroparticle number. The array then cycles through each
!     process in ascending order.

    if (tProcInfo_G%size > 1_ip) then

      DO j=2,tProcInfo_G%size
         CALL MPI_ISSEND( sendbuff,1,MPI_INT_HIGH,rrank,&
              0,tProcInfo_G%comm,req,error )
         CALL MPI_RECV( recvbuff,1,MPI_INT_HIGH,lrank,&
              0,tProcInfo_G%comm,recvstat,error )
         CALL MPI_WAIT( req,sendstat,error )
         procelectrons_G(j) = recvbuff
         sendbuff=recvbuff
      END DO

    end if

!    print*, 'procelectrons = ', procelectrons_G
!    stop
    IF (iNumberElectrons_G==0) qEmpty=.TRUE.

    if (qSimple) DEALLOCATE(RealE)


    if ( (nspinDX<0) .or. (nspinDY<0) ) then
      if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 1)) then
        print*, ''
        print*, ''
        print*, '----------------'
        print*, 'Getting inner node set for MPI communication'
      end if

      call getInNode()

      if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 1)) then
        print*, '...'
        print*, 'inner nx = ', nspinDX
        print*, 'inner ny = ', nspinDY
        print*, 'inner ntransnodes = ', ntrndsi_G
      end if
    end if








!    Set error flag and exit

    qOK = .TRUE.

    GOTO 2000

1000 CALL Error_log('Error in SetupCalcs:PopMacroElectrons',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE PopMacroElectrons

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




subroutine getPaSeeds(NN,sigs,cens,magxs,magys,qFTs,rho,&
                    frs,ph_sh,nSeeds,dels)



!             ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: NN(:)
  REAL(KIND=WP), INTENT(IN) :: sigs(:,:), cens(:), rho, frs(:), &
                               ph_sh(:), magxs(:), magys(:), dels(:)
  LOGICAL, INTENT(IN) :: qFTs(:)
  INTEGER(KIND=IP), INTENT(IN) :: nSeeds
  integer :: error


!  1st gen front seed if present



  if ((ffe_GGG > 0) .and. (ffe-ffs+1 > 0) ) then

    call getSeeds(NN,sigs,cens,magxs,magys,qFTs,rho,&
                  frs,ph_sh,nSeeds,dels,ffs, ffe, &
                  fr_rfield,fr_ifield)

  end if

!  CALL MPI_BARRIER(tProcInfo_G%comm,error)
!  call mpi_finalize(error)
!  stop


!  2nd gen active field seed


  call getSeeds(NN,sigs,cens,magxs,magys,qFTs,rho,&
                frs,ph_sh,nSeeds,dels,fz2, ez2, &
                ac_rfield,ac_ifield)




!  3rd gen back seed section if present


  if ((eee_GGG > 0) .and. (eee-ees+1 > 0) ) then

    call getSeeds(NN,sigs,cens,magxs,magys,qFTs,rho,&
                  frs,ph_sh,nSeeds,dels,ees, eee, &
                  bk_rfield,bk_ifield)

  end if



end subroutine getPaSeeds



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



SUBROUTINE getSeeds(NN,sigs,cens,magxs,magys,qFTs,rho,&
                    frs,ph_sh,nSeeds,dels,iz2_s, iz2_e, &
                    xfieldt,yfieldt)

!             ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: NN(:), iz2_s, iz2_e
  REAL(KIND=WP), INTENT(IN) :: sigs(:,:), cens(:), rho, frs(:), &
                               ph_sh(:), magxs(:), magys(:), dels(:)
  LOGICAL, INTENT(IN) :: qFTs(:)
  INTEGER(KIND=IP), INTENT(IN) :: nSeeds
  REAL(KIND=WP), INTENT(OUT) :: xfieldt(:), yfieldt(:)

!            LOCAL ARGS

  INTEGER(KIND=IP) :: ind, fsz
  REAL(KIND=WP), allocatable :: xfield(:), yfield(:)

  fsz = size(xfieldt)

  !  if (fsz .ne. size(yfieldt)) then

  !  Cause error

  !  end if

  allocate(xfield(fsz), yfield(fsz))


  xfieldt = 0.0_WP
  yfieldt = 0.0_WP

  DO ind = 1, nSeeds

    CALL getSeed(NN(:),sigs(ind,:),cens(ind),magxs(ind),magys(ind),qFTs(ind), &
                 qRndFj_G(ind), sSigFj_G(ind), rho,frs(ind), ph_sh(ind), &
                 dels,iz2_s, iz2_e,xfield,yfield)

    xfieldt = xfieldt + xfield
    yfieldt = yfieldt + yfield

  END DO

  deallocate(xfield, yfield)

END SUBROUTINE getSeeds

!*****************************************************

SUBROUTINE getSeed(NN,sig,cen,magx,magy,qFT,qRnd, &
                   sSigR, rho,fr,ph_sh, &
                   dels,iz2_s, iz2_e, xfield,yfield)

  IMPLICIT NONE

!             ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: NN(:), iz2_s, iz2_e
  REAL(KIND=WP), INTENT(IN) :: sig(:), cen, sSigR, rho, fr, ph_sh,&
                               magx, magy, dels(:)
  LOGICAL, INTENT(IN) :: qFT, qRnd
  REAL(KIND=WP), INTENT(OUT) :: xfield(:), yfield(:)

!             LOCAL ARGS

  REAL(KIND=WP), allocatable :: xnds(:), ynds(:), &
                   z2nds(:), &
                   xenv(:), yenv(:), &
                   z2env(:), oscx(:), &
                   oscy(:)

  REAL(KIND=WP) :: lx, ly, lz2, z2sl, z2el

  INTEGER(KIND=IP) :: ind1, ind2, ind3, gind, nz2l


!     Sample length of the field in each dimension

  lx = dels(iX_CG) * (NN(iX_CG) - 1_IP)
  ly = dels(iY_CG) * (NN(iY_CG) - 1_IP)

  z2sl = dels(iZ2_CG) * (iz2_s - 1_IP)
  z2el = dels(iZ2_CG) * (iz2_e - 1_IP)

  nz2l = iz2_e - iz2_s + 1


  allocate(xnds(NN(iX_CG)), ynds(NN(iY_CG)), &
           z2nds(nz2l), &
           xenv(NN(iX_CG)), yenv(NN(iY_CG)), &
           z2env(nz2l), oscx(nz2l), &
           oscy(nz2l))


!     Coordinates of field nodes in x, y and z2 (sample points)

  IF (NN(iX_CG) == 1_IP) THEN
    xnds = 1_WP
  ELSE
    xnds = linspace(-lx/2_WP, lx/2_WP, NN(iX_CG))
  END IF


  IF (NN(iY_CG) == 1_IP) THEN
    ynds = 1_WP
  ELSE
    ynds = linspace(-ly/2_WP, ly/2_WP, NN(iY_CG))
  END IF


!  call linspacesr(z2sl,z2el,nz2l, z2nds)
  z2nds = linspace(z2sl,z2el,nz2l)

!     Profile in each dimension

  IF (NN(iX_CG) == 1_IP) THEN
    xenv = 1_WP
  ELSE
    xenv = gaussian(xnds,0.0_WP,sig(iX_CG))
  END IF


  IF (NN(iY_CG) == 1_IP) THEN
    yenv = 1_WP
  ELSE
    yenv = gaussian(ynds,0.0_WP,sig(iY_CG))
  END IF


  IF (qFT) THEN




    if (qRnd) then

      call ftron(z2env, 2*sig(iZ2_CG), sSigR, cen, z2nds)

    else


      WHERE (z2nds < (cen - sig(iZ2_CG)))

        z2env = 0.0_WP

      ELSEWHERE (z2nds > (cen + sig(iZ2_CG)))

        z2env = 0.0_WP

      ELSEWHERE

        z2env = 1.0_WP

      END WHERE

    end if

  ELSE

    z2env = gaussian(z2nds,cen,sig(iZ2_CG))

  END IF


!     x and y polarized fields in z2

  oscx = z2env * sin(fr * z2nds / (2.0_WP * rho) - ph_sh)! + 4.0_wp*(cos(10_wp * z2nds)))
  oscy = z2env * cos(fr * z2nds / (2.0_WP * rho) - ph_sh)!+ 4.0_wp*(cos(10_wp * z2nds)))

!     Full 3D field

  do ind1 = 1, nz2l
    do ind2 = 1,NN(iY_CG)
      do ind3 = 1,NN(iX_CG)

        gind = ind3 + NN(iX_CG)*(ind2-1) + NN(iX_CG)*NN(iY_CG)*(ind1-1)
        xfield(gind) = magx * xenv(ind3) * yenv(ind2) * oscx(ind1)
        yfield(gind) = magy * xenv(ind3) * yenv(ind2) * oscy(ind1)

      end do
    end do
  end do


deallocate(xnds, ynds, &
           z2nds, &
           xenv, yenv, &
           z2env, oscx, &
           oscy)


END SUBROUTINE getSeed







subroutine ftron(env, fl_len, rn_sig, cen, z2nds)


  implicit none


  real(kind=wp), intent(inout) :: env(:)
  real(kind=wp), intent(in) :: fl_len, rn_sig, cen, z2nds(:)


  real(kind=wp) :: len_gauss, sSt, sEd, sg1cen, sg1st, &
                   sg2cen, sg2st, sftst

  integer(kind=ip) :: nnz2



  len_gauss = gExtEj_G * rn_sig /2.0_wp  ! model out how many sigma??



  sSt = cen - (fl_len / 2.0_wp) - len_gauss  !
  sEd = cen + (fl_len / 2.0_wp) + len_gauss



  sg1st = sSt              ! Start of 1st half gauss
  sftst = sSt + len_gauss  ! Start of flat-top section
  sg2st = sftst + fl_len    ! Start of second half-gaussian

  sg1cen = sftst      ! mean of 1st gauss
  sg2cen = sg2st      ! mean of 2nd gauss




  where ((z2nds >= sSt) .and. (z2nds <= sftst))

    env = gaussian(z2nds, sg1cen, rn_sig)

  elsewhere ((z2nds > sg1cen) .and. (z2nds <= sg2cen))

    env = 1.0_wp

  elsewhere ((z2nds > sg2cen) .and.  (z2nds <= sg2st + len_gauss))

    env = gaussian(z2nds, sg2cen, rn_sig)

  elsewhere

    env = 0.0_wp

  end where



end subroutine ftron



END MODULE setupcalcs
