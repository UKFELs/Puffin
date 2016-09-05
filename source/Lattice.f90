!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

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
                               iQuad = 4_ip

integer(kind=ip), allocatable :: iElmType(:)

integer(kind=ip) :: iUnd_cr, iChic_cr, iDrift_cr, iQuad_cr    ! Counters for each element type

!integer(kind=ip) :: inum_latt_elms

contains

!    ####################################################




  subroutine setupMods(lattFile, taper, sRho, nSteps_f, dz_f)

    implicit none

!     Sets up elements in wiggler lattice. The elements
!     are read in from the file specified.
!
!     Dr Lawrence Campbell
!     University of Strathclyde
!     2015

    character(32_ip), intent(in) :: LattFile
    real(kind=wp), intent(inout) :: taper
    real(kind=wp), intent(in) :: sRho, dz_f
    integer(kind=ip), intent(in) :: nSteps_f


    if (lattFile=='') then
      qMod_G = .false.
      if(tProcInfo_G%qRoot) print*, 'There are no dispersive sections'
    else
      qMod_G = .true.
      if(tProcInfo_G%qRoot) print*, 'There are dispersive sections'
    end if


    IF (qMod_G) then

      modNum=numOfMods(lattFile)

      allocate(D(ModNum),zMod(ModNum),delta(modNum))
      allocate(mf(ModNum),delmz(ModNum),tapers(modNum))
      allocate(nSteps_arr(ModNum))

!    Latt file name, number of wigg periods converted to z-bar,
!    slippage in chicane in z-bar, 2 dispersive constants,
!    number of modules

      allocate(iElmType(2*modNum))   !  For now, using old lattice file format...
      call readLatt(lattFile,zMod,delta,D,Dfact,ModNum,taper,sRho,sStepSize)
      ModCount = 1
      modNum = 2_ip * modNum

    else

      modNum = 1

      allocate(D(ModNum),zMod(ModNum),delta(modNum))
      allocate(mf(ModNum),delmz(ModNum),tapers(modNum))
      allocate(nSteps_arr(ModNum))

      allocate(iElmType(modNum))

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


    end if

    iUnd_cr=1_ip
    iChic_cr=1_ip
    iDrift_cr=1_ip
    iQuad_cr=1_ip

    iCsteps = 1_ip

  end subroutine setupMods




!    #####################################################


  SUBROUTINE readLatt(lattFile,zMod,delta,D,Dfact,ModNum,taper,rho,&
                      sStepSize)

  IMPLICIT NONE

! Subroutine to read the information from the lattice file
! and define some of the dispersion parameters.
!
! Modified from Brian McNeil's 1D lattice FEL code
! to work with this 3D unaveraged model.
!
!                ARGUMENTS
!
! lattFile      The name of the lattice file (INPUT)
! zMod          Array containing the cumulative
!               interaction lengths of each module in
!               z-bar (OUTPUT)
! delta         Array containing lengths of dispersive
!               sections in slippage lengths (OUTPUT)
! D             Dispersive factor of the chicane,
!               D=10*Dfact/6*delta (OUTPUT)
! Dfact         Dispersive strength of the chicane (INPUT)
! ModNum        Number of Modules (INPUT)
! rho           FEL parameter

  CHARACTER(32_IP), INTENT(IN) :: lattFile
  REAL(KIND=WP), DIMENSION(:), INTENT(INOUT) :: zMod,delta,D
  REAL(KIND=WP), INTENT(IN) :: Dfact
  REAL(KIND=WP), INTENT(INOUT)  ::  taper
  REAL(KIND=WP), INTENT(IN) :: rho
  INTEGER(KIND=IP), INTENT(IN) :: ModNum
  REAL(KIND=WP), INTENT(OUT)   :: sStepSize

!                LOCAL VARS

  INTEGER(KIND=IP)   :: i,ios,nw,error,ri,NL
  REAL(KIND=WP)      :: c1

  integer(kind=ip) :: nperlam(size(delmz))

  OPEN(1,FILE=lattFile, IOSTAT=ios, ACTION='READ', POSITION ='REWIND')
  IF (ios /= 0_IP) STOP "OPEN(input file) not performed correctly, IOSTAT /= 0"

!   pi = 4.0_WP*ATAN(1.0_WP)
  c1 = 2.0_WP*rho


!     Read whitespace

  NL = 31_IP      !    Number of lines in header

  do ri = 1,NL

    read (1,*)

  end do

!     Read module data from lattice file

  do i=1,ModNum

    read (1,*) nw, delta(i), mf(i), nperlam(i), tapers(i)  !, resFactor(i) ! Wiggler periods, Chicane slippage periods, aw shift, stepsize

    delmz(i) = 4.0_WP * pi * rho / real(nperlam(i),kind=wp)

!     Calculate cumulative interaction length of modules

    nSteps_arr(i) = nw * nperlam(i)

    if (i==1) then
      zMod(i) = real(nw,KIND=WP)
      zMod(i) = 2.0_WP*pi*c1*zMod(i)
    else
      zMod(i) = zMod(i-1)+2.0_WP*pi*c1*real(nw,KIND=WP)
    end if

    iElmType(2*i-1) = iUnd
    iElmType(2*i) = iChic

  end do

  close(1, STATUS='KEEP')

!     Convert from # undulator periods to z-bar

  delta = 2.0_WP*pi*c1*delta
  D = Dfact*10.0/6.0*delta ! The Dispersion parameter

  sStepSize =  delmz(1)
  taper = tapers(1)

  END SUBROUTINE readLatt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE disperse(iL)

  IMPLICIT NONE

! Subroutine to apply phase changes to electrons beam
! due to passing through a chicane...
! Modified from Brian McNeil's Phase shift routine to
! work with p2 (electron momentum in z2 frame) as opposed
! to bar{p}_z
!
!              ARGUMENTS
!
! y_e              electron array data
! D                Dispersion parameter for the chicane
!                  (D=10*Dfact/6*delta, Dfact is the
!                  dispersive strength factor of the chicane
! delta            Slippage in resonant wavelengths

  INTEGER(KIND=IP), INTENT(IN) :: iL

  real(kind=wp) :: szbar4d, smeanp2
  real(kind=wp), allocatable :: sp2(:)
  logical :: qDummy

  LOGICAL :: qOKL

!     Propagate through chicane


  sElZ2_G = sElZ2_G - 2.0_WP * D(iChic_cr) *  &
               (sElGam_G - 1_wp) &
               + delta(iChic_cr)

  if (qDiffraction_G) then

!  Convert slippage in z2bar to spatial length for diffraction

    allocate(sp2(iNumberElectrons_G))
    call getP2(sp2, sElGam_G, sElPX_G, sElPY_G, sEta_G, sGammaR_G, saw_G)
    smeanp2 = arr_mean_para_weighted(sp2, s_chi_bar_G)
    szbar4d = delta(iChic_cr) / smeanp2
    call diffractIM(szbar4d, qDummy, qOKL)

  end if

  deallocate(sp2)

  iChic_cr = iChic_cr + 1_ip

  END SUBROUTINE disperse



! ##############################################



  subroutine driftSection(iL)

    integer(kind=wp), intent(in) :: iL

    real(kind=wp) :: del_dr_z

    real(kind=wp), allocatable :: sp2(:)
    real(kind=wp) :: dumdrifts(2)  ! dummy until global
    logical :: qDummy, qOKL


    del_dr_z = dumdrifts(iDrift_cr) ! dummy until global

    allocate(sp2(iNumberElectrons_G))

    call getP2(sp2, sElGam_G, sElPX_G, sElPY_G, sEta_G, sGammaR_G, saw_G)

    sElZ2_G = sElZ2_G + del_dr_z * sp2

    if (qDiffraction_G) call diffractIM(del_dr_z, qDummy, qOKL)

    deallocate(sp2)

    iDrift_cr = iDrift_cr + 1_ip

  end subroutine driftSection





! ##############################################



  subroutine correctTrans()

! Apply a virtual 'magnet corrector' at the end of the wiggler
! module. It simply centers the beam in the transverse dimensions

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

  FUNCTION lineCount(fname)

!     Function to count the number of lines on a file
!
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

  FUNCTION numOfMods(fname)

!     Function to count the number of lines on a file
!
!                ARGUMENTS

  INTEGER(KIND=IP)          :: numOfMods
  CHARACTER(*), INTENT(IN)  :: fname

!                LOCAL ARGS

  INTEGER :: ios

  OPEN(1,FILE=fname, IOSTAT=ios, ACTION='READ', POSITION ='REWIND')
  IF (ios /= 0) STOP "OPEN(input file) not performed correctly, IOSTAT /= 0"

  READ (1,*)
  READ (1,*)
  READ (1,*) numOfMods

  10 CLOSE(1, STATUS='KEEP')

  END FUNCTION numOfMods

END MODULE lattice
