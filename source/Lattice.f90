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

  subroutine setupMods(tScaling, lattFile, tUndMod_arr, tChic_arr, tDrift_arr, &
                       tBMods_arr, tQuad_arr)

    implicit none

    type(fScale), intent(in) :: tScaling
    character(1024_ip), intent(in) :: LattFile
    type(fUndMod), allocatable, intent(inout) :: tUndMod_arr(:)
    type(fChicane), allocatable, intent(inout) :: tChic_arr(:)
    type(fDrift), allocatable, intent(inout) :: tDrift_arr(:)
    type(fUndMod), allocatable, intent(inout) :: tBMods_arr(:)
    type(fBModulate), allocatable, intent(inout) :: tQuad_arr(:)


    if (lattFile=='') then
      qMod_G = .false.
      if(tProcInfo_G%qRoot) print*, 'There are no dispersive sections'
    else
      qMod_G = .true.
      if(tProcInfo_G%qRoot) print*, 'There are dispersive sections'
    end if


    IF (qMod_G) then

      modNum=numOfMods(lattFile)

      allocate(tUndMod_arr(numOfUnds))
      allocate(tChic_arr(numOfChics))
      allocate(tDrift_arr(numOfDrifts))
      allocate(tBMods_arr(numOfModulations))
      allocate(tQuad_arr(numOfQuads))

!    Latt file name, number of wigg periods converted to z-bar,
!    slippage in chicane in z-bar, 2 dispersive constants,
!    number of modules

      allocate(iElmType(modNum))   !  For now, using old lattice file format...

      call readLatt(tScaling, lattFile, sRho, tUndMod_arr, tChic_arr, tDrift_arr, &
                    tBMods_arr, tQuad_arr)

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

  subroutine readLatt(tScaling, lattFile, tUndMod_arr, tChic_arr, tDrift_arr, &
                      tBMods_arr, tQuad_arr)

  implicit none

  type(fScale), intent(in) :: tScaling
  character(1024_IP), intent(in) :: lattFile
  type(fUndMod), intent(inout) :: tUndMod_arr(:)
  type(fChicane), intent(inout) :: tChic_arr(:)
  type(fDrift), intent(inout) :: tDrift_arr(:)
  type(fUndMod), intent(inout) :: tBMods_arr(:)
  type(fBModulate), intent(inout) :: tQuad_arr(:)

!                LOCAL VARS

  integer(kind=ip)   :: i,ios,error,ri,NL
  real(kind=wp)      :: c1, slamw

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

        read (168,*, IOSTAT=ios) ztest, tQuad_arr(cntq)%qfx, tQuad_arr(cntq)%qfy  ! read vars

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

        read (168,*, IOSTAT=ios) ztest, tUndMod_arr(cntu)%zundtype, &
                                 tUndMod_arr(cntu)%nw, &
                                 tUndMod_arr(cntu)%mf, tUndMod_arr(cntu)%taper, &
                                 nperlam, tUndMod_arr(cntu)%ux, &
                                 tUndMod_arr(cntu)%uy, &
                                 tUndMod_arr(cntu)%kbx, &
                                 tUndMod_arr(cntu)%kby  ! read vars

        cntt = cntt + 1
        iElmType(cntt) = iUnd

        tUndMod_arr(cntu)%nSteps = tUndMod_arr(cntu)%nw * nperlam

        slamw = 4.0_wp * pi * rho
        tUndMod_arr(cntu)%delmz = slamw / real(nperlam, kind=wp)

        if (tUndMod_arr(cntu)%zundtype == 'curved') then

          tUndMod_arr(cntu)%ux = 0.0_wp   ! Temp fix for initialization bug
          tUndMod_arr(cntu)%uy = 1.0_wp

          tUndMod_arr(cntu)%kux = SQRT(sEta_G/(8.0_wp*tScaling%rho**2)) ! Giving equal focusing for now....
          tUndMod_arr(cntu)%kuy = SQRT(sEta_G/(8.0_wp*tScaling%rho**2))

        else if (tUndMod_arr(cntu)%zundtype == 'planepole') then

          tUndMod_arr(cntu)%kux = 0.0_wp ! Giving equal focusing for now....
          tUndMod_arr(cntu)%kuy = 0.0_wp

          tUndMod_arr(cntu)%ux = 0   ! Temp fix for initialization bug
          tUndMod_arr(cntu)%uy = 1

        else if (tUndMod_arr(cntu)%zundtype == 'helical') then

          tUndMod_arr(cntu)%ux = 1   ! Temp fix for initialization bug
          tUndMod_arr(cntu)%uy = 1

        end if





      else if (ztest(1:2) == 'CH') then

        backspace(168)
        cntc = cntc + 1
        read (168,*, IOSTAT=ios) ztest, tChic_arr(cntc)%zbar, &
                         tChic_arr(cntc)%slip, tChic_arr(cntc)%disp  ! read vars

        cntt = cntt + 1
        iElmType(cntt) = iChic

        tChic_arr(cntc)%slip = 2.0_WP*pi*c1*tChic_arr(cntc)%slip
        tChic_arr(cntc)%zbar = 2.0_WP*pi*c1*tChic_arr(cntc)%zbar
        ! D = Dfact*10.0/6.0*delta ! The Dispersion parameter


      else if (ztest(1:2) == 'DR') then

        backspace(168)
        cntd = cntd + 1
        read (168,*, IOSTAT=ios) ztest, tDrift_arr(cntd)%zbar   ! read vars

        cntt = cntt + 1
        iElmType(cntt) = iDrift

        tDrift_arr(cntd)%zbar = tDrift_arr(cntd)%zbar * 4.0_wp * pi * tScaling%rho

      else if (ztest(1:2) == 'MO') then

        backspace(168)
        cntm = cntm + 1
        read (168,*, IOSTAT=ios) ztest, tBMods_arr(cntm)%wavenum, &
                           tBMods_arr(cntm)%mag ! read vars

        cntt = cntt + 1
        iElmType(cntt) = iModulation


      end if

      cnt = cnt + 1
      !print*, 'hi'

    end if

  end do

  close(168, STATUS='KEEP')

  end subroutine readLatt

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

  subroutine matchOut(tScaling, tUndMod, sZ)

    type(fScale), intent(in) :: tScaling
    type(fUndMod), intent(in) :: tUndMod
    real(kind=wp), intent(in) :: sZ

    real(kind=wp), allocatable :: spx0_offset(:),spy0_offset(:), &
                                  sx_offset(:),sy_offset(:)

    allocate(spx0_offset(iNumberElectrons_G), spy0_offset(iNumberElectrons_G))
    allocate(sx_offset(iNumberElectrons_G),sy_offset(iNumberElectrons_G))

    call getLocalOffsets(tScaling, tUndMod, sZ, &
                         spx0_offset, spy0_offset, &
                         sx_offset, sy_offset)

!     Take away undulator offset to exit module

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

  subroutine matchIn(tScaling, tUndMod, sZ)

    type(fScale), intent(in) :: tScaling
    type(fUndMod), intent(in) :: tUndMod
    real(kind=wp), intent(in) :: sZ

    real(kind=wp), allocatable :: spx0_offset(:),spy0_offset(:), &
                                  sx_offset(:),sy_offset(:)

    integer :: error

    allocate(spx0_offset(iNumberElectrons_G), spy0_offset(iNumberElectrons_G))
    allocate(sx_offset(iNumberElectrons_G),sy_offset(iNumberElectrons_G))

    call getLocalOffsets(tScaling, tUndMod, sZ, &
                           spx0_offset, spy0_offset, &
                           sx_offset, sy_offset)

!     Add on new offset to initialize beam for undulator module

    sElX_G = sElX_G + sx_offset
    sElY_G = sElY_G + sy_offset
    sElPX_G = sElPX_G + spx0_offset
    sElPY_G = sElPY_G + spy0_offset

    deallocate(spx0_offset, spy0_offset, sx_offset, sy_offset)

  end subroutine matchIn

!       #################################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Calculates the expected offset (analytical, ignoring energy losses to FEL etc).
!> It calculates this for each macroparticle.
!> @param[in] tScaling Type describing the FEL scaled reference frame
!> @param[in] tUndMod Type describing the undulator module
!> @param[in] sZ zbar position (LOCAL)
!> @param[inout] spx0_offset Particle offsets in px
!> @param[inout] spy0_offset Particle offsets in py
!> @param[inout] sx_offset Particle offsets in xbar
!> @param[inout] sy_offset Particle offsets in ybar

  subroutine getLocalOffsets(tScaling, tUndMod, sZ, &
                             spx0_offset, spy0_offset, &
                             sx_offset, sy_offset)

    type(fScale), intent(in) :: tScaling
    type(fUndMod), intent(in) :: tUndMod
    real(kind=wp), intent(in) :: sZ
    real(kind=wp), contiguous, intent(inout) :: spx0_offset(:),spy0_offset(:), &
                                                sx_offset(:),sy_offset(:)

    real(kind=wp), allocatable :: unscEn(:)
    real(kind=wp) :: kx, ky

    allocate(unscEn(iNumberElectrons_G))

    kx = tUndMod%kux
    ky = tUndMod%kuy

    if (zUndType_G == 'curved') then

! used for curved pole puffin, the 2 order expansion of cosh and sinh
! allows us to simply add a correction term to the intial position
! when calculating initial conditions, this may need change eventually


        spx0_offset = pxOffset(tScaling, tUndMod, sZ) &
            - 0.5_WP * kx**2 * sElX_G**2 &
            -  0.5_WP * ky**2 * sElY_G**2

        spy0_offset = -1_wp *  &
                      ( pyOffset(tScaling, tUndMod, sZ) &
                      - kx**2 *  sElX_G  * sElY_G)


    else if (zUndType_G == 'planepole') then

! plane pole initial conditions are calculated as a 2nd order expansion
! and added as a correction term.



        spx0_offset = pxOffset(tScaling, tUndMod, sZ) &
            - 0.5_WP * (tScaling%eta / (4 * tScaling%rho**2)) * sElX_G**2

        spy0_offset = -1_wp * &
                      pyOffset(tScaling, tUndMod, sZ)


    else

! "normal" PUFFIN case with no off-axis undulator
! field variation


        spx0_offset = pxOffset(tScaling, tUndMod, sZ)

        spy0_offset = -1.0_wp * &
                     pyOffset(tScaling, tUndMod, sZ)


    end if


    unscEn = tScaling%gamma_r * sElGam_G

    sx_offset =    xOffSet(tScaling, tUndMod, unscEn, &
                           spx0_offset, -spy0_offset, sZ)


    sy_offset =    yOffSet(tScaling, tUndMod, unscEn, &
                           spx0_offset, -spy0_offset, sZ)

    deallocate(unscEn)

  end subroutine getLocalOffsets



! #########################################################


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Initialize undulator local tracking coord for distance travelled in machine
!> and local distance in this module.
!> @param[in] iM Undulator number
!> @param[in] sZ zbar position in the machine
!> @param[inout] sZl zbar position local to undulator (initialized to = 0) here

  subroutine initUndulator(sZ, szl)

    real(kind=wp), intent(in) :: sZ
    real(kind=wp), intent(inout) :: szl

    sz0 = sz
    szl = 0_wp

  end subroutine initUndulator



! #########################################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Setup undulator ends.
!> @param[inout] tUndMod Custom Fortran type describing undulator.
!> @param[in] tScaling Custom Fortran type describing Puffin scaled frame.

  subroutine setUndEnds(tUndMod, tScaling)

    type(fUndMod), intent(inout) :: tUndMod
    type(fScale), intent(in) :: tScaling

    if (qUndEnds_G) then

      tUndMod%sZFS = 4_wp * pi * tScaling%rho  *  2.0_wp
      tUndMod%sZFE = nSteps * sStepSize - &
                     4_wp * pi * tScaling%rho  *  2.0_wp

    else

      tUndMod%sZFS = 0_wp
      tUndMod%sZFE = nSteps * sStepSize

    end if

  end subroutine setUndEnds




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
