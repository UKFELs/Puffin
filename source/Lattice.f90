! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> This module contains subroutines to read in the lattice file and setup the
!> elements in the undulator line. It also contains the subroutines/functions 
!> for all the non-undulator segements, and the initialization of the undulator
!> segments.

MODULE lattice

USE paratype
USE Globals
USE ArrayFunctions
USE ElectronInit
use gtop2
use initConds
use functions
use pdiff

implicit none

integer(kind=ip), parameter :: iUnd = 1_ip, &
                               iChic = 2_ip, &
                               iDrift = 3_ip, &
                               iQuad = 4_ip, &
                               iModulation = 5_ip

integer(kind=ip), allocatable :: iElmType(:)

integer(kind=ip) :: iUnd_cr, iChic_cr, iDrift_cr, iQuad_cr, iModulation_cr    ! Counters for each element type

!integer(kind=ip) :: inum_latt_elms

contains

!    ####################################################



!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Top-level subroutine for setting up the elements in the undulator line, 
!> either from a supplied lattice file, or as the single undulator specified
!> in the main input file. It takes as inputs the simple/single undulator 
!> parameters as specified in the main input file. If a lattice file is 
!> supplied, these values are updated. If no lattice file is supplied, then
!> the undulator line is set up as one undulator with these parameters. Note:
!> the lattice filename is not an optional parameter! If wishing to not 
!> specify a lattice file, the filename should be input as a blank string ('')
!> @param[in] lattFile String: The lattice file name
!> @param[inout] taper The taper of the initial undulator module. Input as 
!> the taper specified in the main input file. Updated if a lattice file is 
!> supplied.
!> @param[in] sRho The FEL, or Pierce, parameter
!> @param[inout] nSteps_f Number of steps in the first undulator module. Input
!> as the number of steps calculated from the main input file. Updated if a  
!> lattice file is supplied.
!> @param[inout] dz_f Integration step size in the first undulator module. Input
!> as the step size specified in the main input file. Updated if a lattice file 
!> is supplied.
!> @param[inout] ux_f ux (see manual) in the first undulator module. Input
!> as the ux specified in the main input file. Updated if a lattice file 
!> is supplied.
!> @param[inout] uy_f uy (see manual) in the first undulator module. Input
!> as the uy specified in the main input file. Updated if a lattice file 
!> is supplied.
!> @param[inout] kbnx_f Strong (scaled) in-undulator wavenumber in x (see manual) 
!> in the first undulator module. Input as the kbetax_SF specified in the main 
!> input file. Updated if a lattice file is supplied.
!> @param[inout] kbny_f Strong (scaled) in-undulator wavenumber in y (see manual) 
!> in the first undulator module. Input as the kbetay_SF specified in the main 
!> input file. Updated if a lattice file is supplied.

  subroutine setupMods(lattFile, taper, sRho, nSteps_f, dz_f, &
                       ux_f, uy_f, kbnx_f, kbny_f)

    implicit none

    character(1024_ip), intent(in) :: LattFile 
    real(kind=wp), intent(inout) :: taper
    real(kind=wp), intent(in) :: sRho
    real(kind=wp), intent(inout) :: dz_f, ux_f, uy_f, kbnx_f, kbny_f
    integer(kind=ip), intent(inout) :: nSteps_f


    if (lattFile=='') then
      qMod_G = .false.
      if(tProcInfo_G%qRoot) print*, 'There are no dispersive sections'
    else
      qMod_G = .true.
      if(tProcInfo_G%qRoot) print*, 'There are dispersive sections'
    end if


    IF (qMod_G) then

      modNum=numOfMods(lattFile)


      allocate(mf(numOfUnds),delmz(numOfUnds),tapers(numOfUnds))
      allocate(nSteps_arr(numOfUnds), zMod(numOfUnds))
      allocate(ux_arr(numOfUnds), uy_arr(numOfUnds), &
               kbnx_arr(numOfUnds), kbny_arr(numOfUnds))
      allocate(zundtype_arr(numOfUnds))


      allocate(chic_disp(numOfChics), chic_slip(numOfChics), &
               chic_zbar(numOfChics))

      allocate(drift_zbar(numOfDrifts))

      allocate(enmod_wavenum(numOfModulations), &
                 enmod_mag(numOfModulations)) 

      allocate(quad_fx(numOfQuads), quad_fy(numOfQuads))


!    Latt file name, number of wigg periods converted to z-bar,
!    slippage in chicane in z-bar, 2 dispersive constants,
!    number of modules

      allocate(iElmType(modNum))   !  For now, using old lattice file format...

      call readLatt(lattFile, sRho)

      ModCount = 1

      dz_f =  delmz(1)
      nSteps_f = nSteps_arr(1)
      taper = tapers(1)

    else

      modNum = 1
      numOfUnds = 1
      numOfChics = 0
      numOfDrifts = 0
      numOfQuads = 0
      numOfModulations = 0

      allocate(iElmType(1))

      allocate(mf(numOfUnds),delmz(numOfUnds),tapers(numOfUnds))
      allocate(nSteps_arr(numOfUnds), zMod(numOfUnds))
      allocate(ux_arr(numOfUnds), uy_arr(numOfUnds), &
               kbnx_arr(numOfUnds), kbny_arr(numOfUnds))
      allocate(zundtype_arr(numOfUnds))


      allocate(chic_disp(numOfChics), chic_slip(numOfChics), &
               chic_zbar(numOfChics))

      allocate(drift_zbar(numOfDrifts))

      allocate(enmod_wavenum(numOfModulations), &
                 enmod_mag(numOfModulations)) 

      allocate(quad_fx(numOfQuads), quad_fy(numOfQuads))

      iElmType(1) = iUnd
      mf(1) = 1_wp
      delmz(1) = dz_f
      tapers(1) = taper
      nSteps_arr(1) = nSteps_f
      iUnd_cr = 1_ip
      nSteps_arr(1) = nSteps_f
      delmz(1) = dz_f
      mf(1) = 1_wp
      tapers(1) = taper
      ux_arr(1) = ux_f
      uy_arr(1) = uy_f
      kbnx_arr(1) = kbnx_f 
      kbny_arr(1) = kbny_f 

    end if

    iUnd_cr=1_ip
    iChic_cr=1_ip
    iDrift_cr=1_ip
    iQuad_cr=1_ip
    iModulation_cr = 1_ip

    iCsteps = 1_ip

  end subroutine setupMods




!    #####################################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine for reading in the lattice file and setting up the lattice 
!> element arrays. The element arrays are globally defined in this module.
!> @param[in] lattFile String: The lattice file name
!> @param[in] rho The FEL, or Pierce, parameter

  SUBROUTINE readLatt(lattFile, rho)

  IMPLICIT NONE

  CHARACTER(1024_IP), INTENT(IN) :: lattFile

  REAL(KIND=WP), INTENT(IN) :: rho

!                LOCAL VARS

  INTEGER(KIND=IP)   :: i,ios,nw,error,ri,NL
  REAL(KIND=WP)      :: c1, slamw

  integer(kind=ip) :: nperlam

  integer(kind=ip) :: cnt, cntq, cntu, cntc, cntd, cntm, cntt
  character(40) :: ztest

!   pi = 4.0_WP*ATAN(1.0_WP)
  c1 = 2.0_WP*rho

  cnt = 0
  cntt = 0
  cntu = 0
  cntq = 0
  cntd = 0
  cntc = 0
  cntm = 0



  open(168,FILE=lattFile, IOSTAT=ios, STATUS='OLD', ACTION='READ', POSITION ='REWIND')

  if (ios /= 0) then
    print*, 'iostat = ', ios
    stop "OPEN(input file) not performed correctly, IOSTAT /= 0"
  end if


  do 

    read (168,*, IOSTAT=ios) ztest  ! probe the line

    if (ios < 0) then  ! if reached end of file:-

      if (tProcInfo_G%qroot) print*, "Reached end of file!! (for the second time)"
      !print*, "Turns out you had ", cnt, "lines in the file!!"
      !print*, "Turns out you had ", cntq, "quads in the file!! in lines ", lineq
      !print*, "Turns out you had ", cntu, "undulators in the file!!"

      exit

    else if (ios > 0) then

      print*, 'THIS LINE HAS NOTHING FOR ME', ios
      exit
      cnt = cnt + 1

    else

      if (ztest(1:2) == 'QU') then

        backspace(168)

        cntq = cntq + 1

        read (168,*, IOSTAT=ios) ztest, quad_fx(cntq), quad_fy(cntq)  ! read vars

        cntt = cntt + 1
        iElmType(cntt) = iQuad

      else if (ztest(1:2) == 'UN') then

        backspace(168)

        cntu = cntu + 1

!       reading ... element ID, undulator type, num of periods, alpha (aw / aw0), 
!       taper (d alpha / dz), integration steps per period, ux and uy (polarization 
!       control), and kbnx and kbny, betatron wavenumbers for in-undulator strong 
!       focusing (applied in the wiggler!!! NOT from quads. Remember the natural 
!       undulator focusing is also included IN ADDITION to this...)

        read (168,*, IOSTAT=ios) ztest, zundtype_arr(cntu), nw, mf(cntu), tapers(cntu), &
                                 nperlam, ux_arr(cntu), uy_arr(cntu), kbnx_arr(cntu), &
                                 kbny_arr(cntu)  ! read vars

        cntt = cntt + 1
        iElmType(cntt) = iUnd


        nSteps_arr(cntu) = nw * nperlam

        slamw = 4.0_WP * pi * rho
        delmz(cntu) = slamw / real(nperlam, kind=wp)
  
        if (zundtype_arr(cntu) == 'curved') then

          ux_arr(cntu) = 0   ! Temp fix for initialization bug
          uy_arr(cntu) = 1

        else if (zundtype_arr(cntu) == 'planepole') then

          ux_arr(cntu) = 0   ! Temp fix for initialization bug
          uy_arr(cntu) = 1

        else if (zundtype_arr(cntu) == 'helical') then

          ux_arr(cntu) = 1   ! Temp fix for initialization bug
          uy_arr(cntu) = 1

        end if





      else if (ztest(1:2) == 'CH') then

        backspace(168)
        cntc = cntc + 1
        read (168,*, IOSTAT=ios) ztest, chic_zbar(cntc), chic_slip(cntc), chic_disp(cntc)  ! read vars

        cntt = cntt + 1
        iElmType(cntt) = iChic


        chic_slip(cntc) = 2.0_WP*pi*c1*chic_slip(cntc)
        chic_zbar(cntc) = 2.0_WP*pi*c1*chic_zbar(cntc)
        ! D = Dfact*10.0/6.0*delta ! The Dispersion parameter


      else if (ztest(1:2) == 'DR') then

        backspace(168)
        cntd = cntd + 1
        read (168,*, IOSTAT=ios) ztest, drift_zbar(cntd)   ! read vars

        cntt = cntt + 1
        iElmType(cntt) = iDrift

        drift_zbar(cntd) = drift_zbar(cntd) * 4.0_wp * pi * sRho_G

      else if (ztest(1:2) == 'MO') then

        backspace(168)
        cntm = cntm + 1
        read (168,*, IOSTAT=ios) ztest, enmod_wavenum(cntm), enmod_mag(cntm) ! read vars

        cntt = cntt + 1
        iElmType(cntt) = iModulation


      end if

      cnt = cnt + 1
      !print*, 'hi'

    end if

  end do    

  close(168, STATUS='KEEP')

  end subroutine readLatt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine for modelling the chicane element in Puffin. This is achieved
!> only through a simple point tranform.
!> @param[in] iL The element number in the lattice
!> @param[out] sZ Scaled distance through the machine

  subroutine disperse(iL, sZ)

  implicit none

  integer(kind=ip), intent(in) :: iL
  real(kind=wp), intent(out) :: sZ

  real(kind=wp) :: szbar4d
  real(kind=wp), allocatable :: sp2(:)
  logical :: qDummy



  logical :: qOKL

  szbar4d = chic_zbar(iChic_cr)

!     Propagate through chicane


  sElZ2_G = sElZ2_G - 2.0_WP * chic_disp(iChic_cr) *  &
               (sElGam_G - 1_wp) &
               + chic_slip(iChic_cr)

  if (qDiffraction_G) then

!  Convert slippage in z2bar to spatial length for diffraction

    if (szbar4d > 0.0_wp) call diffractIM(szbar4d, qDummy, qOKL)

  end if

  sZ = sZ + szbar4d
  iChic_cr = iChic_cr + 1_ip

  end subroutine disperse



! ##############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to model the electron drift between other lattice elements.
!> @param[in] iL The element number in the lattice
!> @param[out] sZ Scaled distance through the machine

  subroutine driftSection(iL, sZ)

    integer(kind=ip), intent(in) :: iL
    real(kind=wp), intent(out) :: sZ

    real(kind=wp) :: del_dr_z

    real(kind=wp), allocatable :: sp2(:)
    logical :: qDummy, qOKL


    del_dr_z = drift_zbar(iDrift_cr) ! dummy until global

    allocate(sp2(iNumberElectrons_G))

    call getP2(sp2, sElGam_G, sElPX_G, sElPY_G, sEta_G, sGammaR_G, saw_G)

    sElZ2_G = sElZ2_G + del_dr_z * sp2

    if (.not. qOneD_G) then
    
      ! drift in x and y...

      sElX_G = sElX_G + (2 * sRho_G * sKappa_G / sqrt(sEta_G) * &
            (1 + sEta_G * sp2) / sElGam_G *  &
            sElPX_G) * del_dr_z

      sElY_G = sElY_G - (2 * sRho_G * sKappa_G / sqrt(sEta_G) * &
            (1 + sEta_G * sp2) / sElGam_G *  &
            sElPY_G) * del_dr_z

    end if

    if (qDiffraction_G) call diffractIM(del_dr_z, qDummy, qOKL)

    deallocate(sp2)

    sZ = sZ + del_dr_z
    iDrift_cr = iDrift_cr + 1_ip

  end subroutine driftSection

! ##############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to model the quad element in Puffin. This is implemented as a 
!> simple point transform.
!> @param[in] iL The element number in the lattice

  subroutine Quad(iL)

    integer(kind=ip), intent(in) :: iL

    real(kind=wp), allocatable :: sp2(:)

    allocate(sp2(iNumberElectrons_G))

    call getP2(sp2, sElGam_G, sElPX_G, sElPY_G, sEta_G, sGammaR_G, saw_G)

!    Apply quad transform (point transform)


    if (.not. qOneD_G) then

      sElPX_G = sElPX_G + sqrt(sEta_G) / &
                  (2 * sRho_G * sKappa_G) * sElX_G &
                   / quad_fx(iQuad_cr)

      sElPY_G = sElPY_G - sqrt(sEta_G) / &
                  (2 * sRho_G * sKappa_G) * sElY_G &
                  / quad_fy(iQuad_cr)

    end if


  deallocate(sp2)

  iQuad_cr = iQuad_cr + 1_ip

  end subroutine Quad



! ##############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Apply a simple energy modulation to the beam in Puffin.
!> @param[in] iL The element number in the lattice

  subroutine bModulation(iL)

    integer(kind=ip), intent(in) :: iL

    sElGam_G = sElGam_G + ( enmod_mag(iModulation_cr) &
               * cos(enmod_wavenum(iModulation_cr) * sElZ2_G) )


    iModulation_cr = iModulation_cr + 1

  end subroutine bModulation





! ##############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Apply a virtual 'magnet corrector' at the end of the wiggler
!> module. It simply centers the beam in the transverse dimensions (x,y,px,py)

  subroutine correctTrans()

    real(kind=wp) :: spx_m, spy_m, sx_m, sy_m


! Calculate means

    spx_m = arr_mean_para_weighted(sElPX_G, s_chi_bar_G)
    spy_m = arr_mean_para_weighted(sElPY_G, s_chi_bar_G)

    sx_m = arr_mean_para_weighted(sElX_G, s_chi_bar_G)
    sy_m = arr_mean_para_weighted(sElY_G, s_chi_bar_G)

! Correct coordinates

    sElPX_G = sElPX_G - spx_m
    sElPY_G = sElPY_G - spy_m

    sElX_G = sElX_G - sx_m
    sElY_G = sElY_G - sy_m


  end subroutine correctTrans


!  #############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> When not using undulator ends, this subroutine re-centres the beam after
!> the undulator exit. This may be redundant, as we usually also apply
!> the correctTrans subroutine after the exit to correct the transverse motion...
!> @param[in] sZ zbar position

  subroutine matchOut(sZ)

    real(kind=wp), intent(in) :: sZ

    real(kind=wp), allocatable :: spx0_offset(:),spy0_offset(:), &
                                  sx_offset(:),sy_offset(:)

    real(kind=wp) :: kx, ky


    kx = kx_und_G
    ky = ky_und_G


    allocate(spx0_offset(iNumberElectrons_G), spy0_offset(iNumberElectrons_G))
    allocate(sx_offset(iNumberElectrons_G),sy_offset(iNumberElectrons_G))

!     Get offsets for start of undulator


    if (zUndType_G == 'curved') then

! used for curved pole puffin, the 2 order expansion of cosh and sinh
! allows us to simply add a correction term to the intial position
! when calculating initial conditions, this may need change eventually


        spx0_offset = pxOffset(sZ, srho_G, fy_G) &
            - 0.5_WP * kx**2 * sElX_G**2 &
            -  0.5_WP * kY**2 * sElY_G**2

        spy0_offset = -1_wp *  &
                      ( pyOffset(sZ, srho_G, fx_G) &
                      - kx**2 *  sElX_G  * sElY_G)


    else if (zUndType_G == 'planepole') then

! plane pole initial conditions are calculated as a 2nd order expansion
! and added as a correction term.



        spx0_offset = pxOffset(sZ, srho_G, fy_G) &
            - 0.5_WP * (sEta_G / (4 * sRho_G**2)) * sElX_G**2

        spy0_offset = -1_wp * &
                      pyOffset(sZ, srho_G, fx_G)


    else

! "normal" PUFFIN case with no off-axis undulator
! field variation


        spx0_offset = pxOffset(sZ, srho_G, fy_G)

        spy0_offset = -1.0_wp * &
                     pyOffset(sZ, srho_G, fx_G)


    end if


    sx_offset =    xOffSet(sRho_G, sAw_G, sGammaR_G, sGammaR_G * sElGam_G, &
                           sEta_G, sKappa_G, sFocusfactor_G, spx0_offset, spy0_offset, &
                           fx_G, fy_G, sZ)

    sy_offset =    yOffSet(sRho_G, sAw_G, sGammaR_G, sGammaR_G * sElGam_G, &
                           sEta_G, sKappa_G, sFocusfactor_G, spx0_offset, spy0_offset, &
                           fx_G, fy_G, sZ)


!     Add on new offset to initialize beam for undulator module


    sElX_G = sElX_G - sx_offset
    sElY_G = sElY_G - sy_offset
    sElPX_G = sElPX_G - spx0_offset
    sElPY_G = sElPY_G - spy0_offset


    deallocate(spx0_offset,spy0_offset,sx_offset,sy_offset)



  end subroutine matchOut




! ###############################################


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> When not using undulator ends, this subroutine initializes the transverse beam
!> coordinates to satisfy the initial offset conditions in the undulator. 
!> the undulator exit. 
!> @param[in] sZ zbar position

  subroutine matchIn(sZ)

    real(kind=wp), intent(in) :: sZ

    real(kind=wp), allocatable :: spx0_offset(:),spy0_offset(:), &
                                  sx_offset(:),sy_offset(:)

    real(kind=wp) :: kx, ky

    integer :: error

    kx = kx_und_G
    ky = ky_und_G


    allocate(spx0_offset(iNumberElectrons_G), spy0_offset(iNumberElectrons_G))
    allocate(sx_offset(iNumberElectrons_G),sy_offset(iNumberElectrons_G))

!     Get offsets for start of undulator

    if (zUndType_G == 'curved') then

! used for curved pole puffin, the 2 order expansion of cosh and sinh
! allows us to simply add a correction term to the intial position
! when calculating initial conditions, this may need change eventually


        spx0_offset = pxOffset(sZ, srho_G, fy_G) &
            - 0.5_WP * kx**2 * sElX_G**2 &
            -  0.5_WP * kY**2 * sElY_G**2

        spy0_offset = -1_wp *  &
                      ( pyOffset(sZ, srho_G, fx_G) &
                      - kx**2 *  sElX_G  * sElY_G)


    else if (zUndType_G == 'planepole') then

! plane pole initial conditions are calculated as a 2nd order expansion
! and added as a correction term.



        spx0_offset = pxOffset(sZ, srho_G, fy_G) &
            - 0.5_WP * (sEta_G / (4 * sRho_G**2)) * sElX_G**2

        spy0_offset = -1_wp * &
                      pyOffset(sZ, srho_G, fx_G)


    else

! "normal" PUFFIN case with no off-axis undulator
! field variation


        spx0_offset = pxOffset(sZ, srho_G, fy_G)

        spy0_offset = -1.0_wp * &
                     pyOffset(sZ, srho_G, fx_G)


    end if


    sx_offset =    xOffSet(sRho_G, sAw_G, sGammaR_G, sGammaR_G * sElGam_G, &
                           sEta_G, sKappa_G, sFocusfactor_G, spx0_offset, -spy0_offset, &
                           fx_G, fy_G, sZ)


    sy_offset =    yOffSet(sRho_G, sAw_G, sGammaR_G, sGammaR_G * sElGam_G, &
                           sEta_G, sKappa_G, sFocusfactor_G, spx0_offset, -spy0_offset, &
                           fx_G, fy_G, sZ)


!     Add on new offset to initialize beam for undulator module

    sElX_G = sElX_G + sx_offset
    sElY_G = sElY_G + sy_offset
    sElPX_G = sElPX_G + spx0_offset
    sElPY_G = sElPY_G + spy0_offset


    deallocate(spx0_offset,spy0_offset,sx_offset,sy_offset)

  end subroutine matchIn




! #########################################################


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> When not using undulator ends, this subroutine initializes the transverse beam
!> coordinates to satisfy the initial offset conditions in the undulator. 
!> the undulator exit. 
!> @param[in] iM Undulator number
!> @param[in] sZ zbar position in the machine
!> @param[inout] sZ zbar position local to undulator (initialized to = 0) here

  subroutine initUndulator(iM, sZ, szl)

    integer(kind=ip), intent(in) :: iM
    real(kind=wp), intent(in) :: sZ
    real(kind=wp), intent(inout) :: szl

! Want to update using arrays describing each module...

!     Update undulator parameter:

    n2col0 = mf(iM)
    n2col = mf(iM)
    undgrad = tapers(iM)
    sz0 = sz
    szl = 0_wp


!     Update stepsize

    sStepSize = delmz(iM) ! Change step size - make sure is still integer
                           ! of 4pirho in input file!!

    nSteps = nSteps_arr(iM)

    zUndType_G = zundtype_arr(iM)

    fx_G = ux_arr(iM)
    fy_G = uy_arr(iM)

    sKBetaXSF_G = kbnx_arr(iM)
    sKBetaYSF_G = kbny_arr(iM)

!     Setup undulator ends

    if (qUndEnds_G) then

      sZFS = 4_wp * pi * sRho_G  *  2.0_wp
      sZFE = nSteps * sStepSize - &
               4_wp * pi * sRho_G  *  2.0_wp

    else

      sZFS = 0_wp
      sZFE = nSteps * sStepSize

    end if

  end subroutine initUndulator


! #########################################################


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Function to count the number of lines on a file 
!> @param[in] fname Filename

  FUNCTION lineCount(fname)

!                ARGUMENTS

  INTEGER(KIND=IP)          :: lineCount
  CHARACTER(*), INTENT(IN)  :: fname

!                LOCAL ARGS

  INTEGER :: ios

  OPEN(1,FILE=fname, IOSTAT=ios, ACTION='READ', POSITION ='REWIND')
  IF (ios /= 0) STOP "OPEN(input file) not performed correctly, IOSTAT /= 0"

  lineCount = 0_IP

  DO
    lineCount=lineCount+1_IP
    READ(1,*,END=10)
  END DO

  10 CLOSE(1, STATUS='KEEP')

  END FUNCTION lineCount

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Function to count the number of each type of element in the lattice file
!> @param[in] fname Filename

  function numOfMods(fname)

!                ARGUMENTS

  integer(kind=ip)          :: numOfMods
  character(*), intent(in)  :: fname

!                LOCAL ARGS

  integer :: ios
  integer(kind=ip) :: cnt, cntq, cntu, cntc, cntd, cntm
  character(40) :: ztest

  ztest = ''
  cnt = 0
  cntq = 0
  cntu = 0
  cntc = 0
  cntd = 0
  cntm = 0

  open(168,FILE=fname, IOSTAT=ios, STATUS='OLD', ACTION='READ', POSITION ='REWIND')
  if (ios /= 0) then
    print*, 'iostat = ', ios
    stop "OPEN(input file) not performed correctly, IOSTAT /= 0"
  end if

  do 

    read (168,*, IOSTAT=ios) ztest  ! probe the line

    if (ios < 0) then  ! if reached end of file:-

      if (tProcInfo_G%qroot)  print*, "Reached end of file!!"
      !if (tProcInfo_G%qroot)  print*, "Turns out you had ", cnt, "lines in the file!!"
      if (tProcInfo_G%qroot)  print*, "Turns out you had ", cntq, "quads in the file!!"
      if (tProcInfo_G%qroot)  print*, "Turns out you had ", cntu, "undulators in the file!!"
      if (tProcInfo_G%qroot)  print*, "Turns out you had ", cntc, "chicanes in the file!!"
      if (tProcInfo_G%qroot)  print*, "Turns out you had ", cntd, "drifts in the file!!"
      if (tProcInfo_G%qroot)  print*, "Turns out you had ", cntd, "drifts in the file!!"
      if (tProcInfo_G%qroot)  print*, "Turns out you had ", cntm, "modulation sections in the file!!"


      exit

    else if (ios > 0) then

      print*, 'THIS LINE HAS NOTHING FOR ME'
      cnt = cnt + 1
      stop

    else

      if (ztest(1:2) == 'QU') then

        cntq = cntq + 1
!        print*, 'quad number ', cntq, ' has params ', quad1, quad2

      else if (ztest(1:2) == 'UN') then


        cntu = cntu + 1


      else if (ztest(1:2) == 'CH') then


        cntc = cntc + 1


      else if (ztest(1:2) == 'DR') then


        cntd = cntd + 1


      else if (ztest(1:2) == 'MO') then


        cntm = cntm + 1


      end if

      cnt = cnt + 1
      !print*, 'hi'

    end if

  end do

  close(168, STATUS='KEEP')


  numOfMods = cntq + cntu + cntc + cntd + cntm

  numOfUnds = cntu

  numOfChics = cntc

  numOfDrifts = cntd

  numOfModulations = cntm

  numOfQuads = cntq


  end function numOfMods

end module lattice
