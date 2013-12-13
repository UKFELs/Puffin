
!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module gMPsFromDists

use paratype
use readDists
use MacrosGenNew
use parallelInfoType
use DerivsGlobals
use typesAndConstants
use parBeam
use grids
use gtop2
use initConds
use remLow

implicit none

contains


subroutine getMPs(fname, nbeams, sZ, qNoise, sEThresh)


! This subroutine  loops around the beams, reading in each
! particle file, and calling genMacros to generate the 
! macroparticles in each. Then the MP's with the lowest 
! chi weights are removed, and placed into the global
! arrays.


!           ARGUMENTS

  character(*), intent(in) :: fname(:)
  integer(kind=ip), intent(in) :: nbeams
  real(kind=wp), intent(in) :: sZ, sEThresh
  logical, intent(in) :: qNoise

!           local args


  real(kind=wp), allocatable :: x(:), y(:), &
                                z2(:), px(:), &
                                py(:), pz2(:), gamma(:), &
                                z2m(:), gm(:), gsig(:), &
                                xm(:), ym(:), pxm(:), pym(:), &
                                Ne(:), pxsig(:), pysig(:), &
                                chi(:), chi_b(:)


  real(kind=wp), allocatable :: dz2(:)
  integer(kind=ip), allocatable :: nZ2(:), nZ2G(:)
  integer(kind=ip) :: nGam, ib
  real(kind=wp) :: ls, le, npk

  integer(kind=ipl), allocatable :: totMPs_b(:), b_sts(:), b_ends(:)
  integer(kind=ipl) :: tnms

  integer :: error


! nZ2 is local, nZ2G is full

  allocate(dz2(nbeams), nZ2G(nbeams), nZ2(nbeams))

  call getHeaders(fname, npk, dz2, nZ2G)

  nGam = 149_IP  !!!  TEMP, SHOULD BE READ IN

  do ib = 1, nbeams

    call splitBeam(nZ2G(ib), (nZ2G(ib)-1)*dz2(ib), tProcInfo_G%size, &
                   tProcInfo_G%rank, nZ2(ib), ls, le)

  end do

  tnms   = sum(int(nZ2(:),kind=ipl) * int((nGam),kind=ipl))

!!!!    temp

  allocate(totMPs_b(nbeams), b_sts(nbeams), b_ends(nbeams))

!  getTotalMPS(for each beam)

  totMPs_b(:) = int(nZ2(:),kind=ipl) * int(nGam,kind=ipl)  ! no of mps in z2 times num in gamma

  tnms   = sum(totMPs_b)    ! Sum of totMPs is the total number of 
                            ! MPs in the entire system

  call getStEnd(nbeams, totMPs_b, b_sts, b_ends)

  allocate(x(tnms), y(tnms), px(tnms), py(tnms), z2(tnms), gamma(tnms), &
           chi_b(tnms), chi(tnms))


  print*, tProcInfo_G%rank, '  has nZ2 array  = ', nZ2G, nZ2

!     Loop around beams, reading in dist files for each beam.

  do ib = 1, nbeams    !  Loop over beams



!     Allocate dist arrays for this beam

    allocate(z2m(nZ2(ib)), gm(nZ2(ib)), gsig(nZ2(ib)), &
             xm(nZ2(ib)), ym(nZ2(ib)), pxm(nZ2(ib)), &
             pym(nZ2(ib)), Ne(nZ2(ib)), pxsig(nZ2(ib)), &
             pysig(nZ2(ib)))

!     Read in dist file for this beam

    call getLocalDists(fname(ib), z2m, gm, &
                       xm, ym, pxm, pym, gsig, & 
                       pxsig, pysig, nz2(ib), nz2G(ib), &
                       Ne)


!     get Macroparticles in this beam

    call getMPsFDists(z2m, gm, gsig, xm, ym, pxm, pym, dz2(ib), Ne, npk, &
                      qnoise, x(b_sts(ib):b_ends(ib)), y(b_sts(ib):b_ends(ib)), &
                      px(b_sts(ib):b_ends(ib)), py(b_sts(ib):b_ends(ib)), &
                      z2(b_sts(ib):b_ends(ib)), gamma(b_sts(ib):b_ends(ib)), &   ! ....BOUNDS.... !
                      chi_b(b_sts(ib):b_ends(ib)), chi(b_sts(ib):b_ends(ib)),sZ,nGam)

    deallocate(z2m, gm, gsig, xm, ym, pxm, pym, Ne, pxsig, pysig)

  end do 

!     Remove MP's with chi weights below the threshold value.

  call removeLowNC(chi_b, chi, b_sts, b_ends, sEThresh, npk, &
                   nbeams, x, y, z2, px,&
                   py, gamma, totMPs_b)

  deallocate(totMPs_b, b_sts, b_ends)

  deallocate(x, y, px, py, z2, gamma, chi_b, chi)

  deallocate(dz2, nZ2G, nZ2)

end subroutine getMPs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine getLocalDists(fname, z2ml, gam_ml, xml, yml, pxml, &
                         pyml, gam_dl, pxdl, pydl, nz2, nz2g, Nel)


  character(*), intent(in) :: fname

  real(kind=wp), intent(inout) :: z2ml(:), & 
                               pxml(:), pyml(:), xml(:), yml(:), &
                               gam_ml(:), gam_dl(:), pxdl(:), pydl(:), &
                               Nel(:)

  integer(kind=ip), intent(inout) :: nz2, nz2g

!                 Local args

  real(kind=wp), allocatable :: z2m(:), & 
                   pxm(:), pym(:), xm(:), ym(:), &
                   gam_m(:), gam_d(:), pxd(:), pyd(:), Ne(:)

!     Allocate arrays

  allocate(z2m(nz2g), pxm(nz2g), pym(nz2g), xm(nz2g), ym(nz2g), &
           gam_m(nz2g), gam_d(nz2g), pxd(nz2g), pyd(nz2g), Ne(nz2g))

!     Read in file

  if (tProcInfo_G%qRoot) then

    call readPartDists(fname, z2m, gam_m, xm, ym, pxm, pym, &
                       gam_d, pxd, pyd, Ne, nz2G)

  end if

!     Send to local arrays

  call scdists(xml, yml, z2ml, pxml, pyml, gam_ml, Nel, &
               xm, ym, z2m, pxm, pym, gam_m, &
               pxdl, pydl, gam_dl, pxd, pyd, gam_d, Ne, &
               nz2, nz2g)

!     deallocate arrays

  deallocate(z2m, pxm, pym, xm, ym, &
             gam_m, gam_d, pxd, pyd, Ne)

end subroutine getLocalDists


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine scdists(xml, yml, z2ml, pxml, pyml, gam_ml, Nel, &
                   xm, ym, z2m, pxm, pym, gam_m, &
                   pxdl, pydl, gam_dl, pxd, pyd, gam_d, Ne, &
                   nz2, nz2g)


  real(kind=wp), intent(inout), dimension(:) :: xml, yml, z2ml, pxml, &
                                                pyml, gam_ml, pxdl, pydl, &
                                                gam_dl, Nel

  real(kind=wp), intent(inout), dimension(:) :: xm, ym, z2m, pxm, pym, &
                                             gam_m, pxd, pyd, gam_d, Ne 

  integer(kind=ip), intent(in) :: nz2, nz2g

  integer(kind=ip), allocatable :: recvs(:), displs(:)


  allocate(recvs(tProcInfo_G%size), displs(tProcInfo_G%size))

  call getGathArrs(nz2,recvs,displs)


!     Scatter mean positions to local processes

  call scatterE2Loc(z2ml,z2m,nz2,nz2g,recvs,displs,0)

  call scatterE2Loc(xml,xm,nz2,nz2g,recvs,displs,0)

  call scatterE2Loc(yml,ym,nz2,nz2g,recvs,displs,0)

  call scatterE2Loc(pxml,pxm,nz2,nz2g,recvs,displs,0)

  call scatterE2Loc(pyml,pym,nz2,nz2g,recvs,displs,0)

  call scatterE2Loc(gam_ml,gam_m,nz2,nz2g,recvs,displs,0)


!     Scatter dists to local processes

  call scatterE2Loc(pxdl,pxd,nz2,nz2g,recvs,displs,0)

  call scatterE2Loc(pydl,pyd,nz2,nz2g,recvs,displs,0)

  call scatterE2Loc(gam_dl,gam_d,nz2,nz2g,recvs,displs,0)

!     Scatter the num of electrons for each Z2 slice to 
!     local processes

  call scatterE2Loc(Nel,Ne,nz2,nz2g,recvs,displs,0)

  deallocate(recvs,displs)


end subroutine scdists

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine getMPsFDists(z2m,gm,gsig,xm,ym,pxm,pym,dz2,Ne,npk,qnoise, &
                        x, y, px, py, z2, gamma, chi_b, chi, sZ, iNMPG)   !! For 1D ONLY!!!! for now.....


! This routine creates the macroparticles according to the beam dists 
! read in from the dist file.

!     Distribute Ne over gamma
!  call getGrid(aye,inputs) ! get gamma grid - equispaced in gamma
!  call getIntegrals(aye, mair inputs)
!     Just genGrid in one dimension for 1D (technically 2D) case


!     Inputting gammagrid, mean z2, Nk, and qnoise, getting out Vk,
!     and z2 and gamma positions of macroparticles, with noise added

!  call newGenMacros(grid,z2m,Nk,Vk,qNoise)   ! We will ignore vk for now

!  dV = dZ2

!     Should now have Ne for each macroparticle (WITH noise added)
!     IGNORE Vk output by genMacros



! so.......

  real(kind=wp), intent(in) :: z2m(:), gm(:), gsig(:), xm(:), ym(:), &
                               pxm(:), pym(:), dz2, Ne(:), npk, sZ

  integer(kind=ip), intent(in) :: iNMPG

  logical, intent(in) :: qnoise

  real(kind=wp), intent(inout) :: x(:), y(:), px(:), py(:), &
                                  z2(:), gamma(:), chi_b(:), chi(:)

  integer(kind=ip) :: i, intTypeG, nMPs, NMZ2  ! Num MPs in gamma

  integer(kind=ipl) :: istart, iend, k, xin

  real(kind=wp) :: z2grid(2_IP), &
                   z2int(1_IP), px0, py0, x0, y0

  integer(kind=ip), allocatable :: arrbs(:)

  real(kind=wp), allocatable :: Nk(:), Vk(:), ggrid(:), gint(:)

  logical :: qOKL

!     Using 11 mp's and a gaussian distribution in p2 (gamma) 

  allocate(ggrid(iNMPG+1), gint(iNMPG))

  intTypeG = iGaussianDistribution_CG  ! iTopHatDistribution_CG

  NMZ2 = size(z2m)

  z2int(:) = 1_WP

  nMPs = int(NMZ2,kind=ipl) * int(iNMPG,kind=ipl)

  allocate(arrbs(iNMPG))
  allocate(Nk(nMPs))
  allocate(Vk(nMPs))

  do k = 1, NMZ2

    !    arrbs = linspace( (k-1) * iNMPG + 1,  k * (iNMPG-1) + 1, iNMPG )    !  calarrayboundsfrom k, nx, ny, npx, npy, ngamma 

    arrbs = (/ ( (k-1) * iNMPG + 1 + i,    i=0, (iNMPG-1) ) /)

    istart = (k-1) * iNMPG + 1
    iend = k * iNMPG


    z2grid = (/ z2m(k) - ( dz2 / 2.0_WP) , z2m(k) + ( dz2 / 2.0_WP) /)

! what should the length of the grid in gamma be?
! since we have a different sigGam for each?.....

    call genGrid(intTypeG, iLinear_CG, gm(k), &         
                 gsig(k), 6.0_WP*gsig(k), iNMPG, iNMPG, &
                 ggrid, gint, .FALSE., &
                 qOKL)


    call genMacrosNew(i_total_electrons  =   Ne(k), &
                      q_noise            =   qnoise,  & 
                      x_1_grid           =   z2grid,  &
                      x_1_integral       =   z2int, & 
                      p_3_grid           =   ggrid,  &
                      p_3_integral       =   gint,  &
                      s_number_macro     =   Nk(istart:iend),  &
                      x_1_coord          =   z2(istart:iend),  &
                      p_3_vector         =   gamma(istart:iend) )

!---- for NOT adding mean px, py to each slice

    px0 = pxOffset(sZ, sRho_G, fx_G)
    py0 = pyOffset(sZ, sRho_G, fy_G)

    px(istart:iend) = px0    !  In 1D giving no deviation in px
    py(istart:iend) = py0    !  or py

    x0 = xOffSet(sRho_G, sAw_G,  sGammaR_G, gm(k), &
                 sEta_G, sKBeta_G, sFocusfactor_G, &
                 px0, py0, &
                 fx_G, fy_G, sZ) 

    y0 = yOffSet(sRho_G, sAw_G,  sGammaR_G, gm(k), &
                 sEta_G, sKBeta_G, sFocusfactor_G, &
                 px0, py0, &
                 fx_G, fy_G, sZ) 

    x(istart:iend)  = x0    ! ??    x = getXR(xm,xr)
    y(istart:iend)  = y0  

!---- for adding mean px, py to each slice


    !px0 = pxOffset(sZ, sRho_G, fx_G)
    !py0 = pyOffset(sZ, sRho_G, fy_G)

    !px(istart:iend) = px0  + pxm(k)  !  Adding deviation in px
    !py(istart:iend) = py0  + pym(k)  !  and py

    !do xin = istart, iend

      !x(xin) = xOffSet(sRho_G, sAw_G,  sGammaR_G, gamma(xin), &
      !                 sEta_G, sKBeta_G, sFocusfactor_G, &
      !                 px(xin), py(xin), &
      !                 fx_G, fy_G, sZ)  +  xm(k)

      !y(xin) = yOffSet(sRho_G, sAw_G,  sGammaR_G, gamma(xin), &
      !                 sEta_G, sKBeta_G, sFocusfactor_G, &
      !                 px(xin), py(xin), &
      !                 fx_G, fy_G, sZ)  +  ym(k)

    !end do

!!------- scrap

    !px(istart:iend) = pxm(k)   +   px0
    !py(istart:iend) = pym(k)   +   py0

   ! x0 = xOffSet(sRho_G, sAw_G,  sGammaR_G, gm(k), &
   !              sEta_G, sKBeta_G, sFocusfactor_G, &
   !              px0, py0, &
   !              fx_G, fy_G, sZ)

   ! y0 = yOffSet(sRho_G, sAw_G,  sGammaR_G, gm(k), &
   !              sEta_G, sKBeta_G, sFocusfactor_G, &
   !              px0, py0, &
   !              fx_G, fy_G, sZ)    


    !do xin = istart, iend

    !  x(xin) = xOffSet(sRho_G, sAw_G,  sGammaR_G, gm(k), &
    !                   sEta_G, sKBeta_G, sFocusfactor_G, &
    !                   px(xin), py(xin), &
    !                   fx_G, fy_G, sZ)  +  xm(k)

    !  y(xin) = yOffSet(sRho_G, sAw_G,  sGammaR_G, gm(k), &
    !                   sEta_G, sKBeta_G, sFocusfactor_G, &
    !                   px(xin), py(xin), &
    !                   fx_G, fy_G, sZ)  +  ym(k)

    !end do


!    x(istart:iend) = xOffSet(sRho_G, sAw_G,  sGammaR_G, gm(k), &
!                             sEta_G, sKBeta_G, sFocusfactor_G, &
!                             px(istart:iend), py(istart:iend), &
!                             fx_G, fy_G, sZ)

!    x(istart:iend)  = xm(k)    +   x0    ! ??    x = getXR(xm,xr)
!    y(istart:iend)  = ym(k)    +   y0
!    px(istart:iend) = pxm(k)   +   px0



    !if (i < 10) print*, 'bounds are ', istart, iend

!    if (qNoise) call applyNoise(x,dx,gam,dgam,Nks)

!!------ end scrap

  end do 




  Vk(:) = dz2

  call getChi(Nk, Vk, npk, chi_b, chi)


  deallocate(Nk, Vk)


end subroutine getMPsFDists

END MODULE gMPsFromDists