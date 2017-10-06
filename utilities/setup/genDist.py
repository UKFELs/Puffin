# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This generates a 'dist' file for Puffin, a multi-frequency FEL code absent of 
the averaging / SVEA approximations. Each line of the dist file describes a 
longitudinal 'slice' of the beam. Each slice is assumed gaussian in
each of the other 5 dimensions, and each line describes the longitudinal position
of the slice, the Gaussian distribution properties in x, y, px, py and energy 
dimensions, and the charge in the slice.
"""

import sys, glob, os
import numpy as np
from numpy import pi
from numpy import arange
import tables
from puffDataClass import puffData


# Puffin Python data class, holds scaling parameters and physical constants

puffVars = puffData()

# Initialize CLARA base parameters

puffVars.aw = 1.0121809   # The PEAK!!!
puffVars.gamma0 = 456.4
puffVars.lw = 0.0275
puffVars.rho = 0.005
puffVars.undtype = 'planepole'
puffVars.ux = 0.
puffVars.uy = 1.

# Generate the rest of the Puffin scaling from the above

puffVars.genParams()  # generate rest of scaled params


# rms sigma of beam in x and y in SI units (m)
#    (for now, this is used as the beam radius for the 1D mode)

sigxSI = 70.12e-6
sigySI = 70.12e-6

eblenz2 = 50.0   # which is length of beam in metres / puffVars.lc
sicesperperiod = 16  # number of slices per nominal radiation period
lrscaled = 4. * np.pi * puffVars.rho  # scaled wavelength is 4 pi rho
dz2 = lrscaled / np.real(sicesperperiod)

nslices = np.int(np.round(eblenz2 / dz2))


# scale transverse radii to Puffin variables
sigx1D = sigxSI / np.sqrt(puffVars.lg*puffVars.lc) 
sigy1D = sigySI / np.sqrt(puffVars.lg*puffVars.lc)


# homegeneous beam properties b4 adding extra stuff like modulations etc

gamma_central = 1.0 * puffVars.gamma0  # mean beam energy, expressed as the Lorentz factor
gammasigh = 0.001 * puffVars.gamma0 # rms energy spread  (in units of gamma, NOT gamma / gamma0!!!)

x_central = 0.    # mean xbar
xsigh = sigx1D    # rms sigma in xbar

y_central = 0.    # mean ybar
ysigh = sigy1D    # rms sigma in ybar

px_central = 0.   # mean pxbar
pxsigh = 0.01     # rms sigma in pxbar

py_central = 0.   # mean pybar
pysigh = 0.01     # rms sigma in pybar

emitbarh = 1.0    # emittance, scaled to lambda_r / 4pi

#     Define properties of each slice:

z2pos = arange(0, nslices) * dz2  + dz2/2. # mean z2 coordinate
gammam = np.ones(nslices) * gamma_central + \
          1.*np.cos(2. * np.pi * (z2pos**0.2) * z2pos) # mean gamma (Lorentz factor), added modulation
xm = np.ones(nslices) * x_central # mean in xbar
ym = np.ones(nslices) * y_central # mean in ybar
pxm = np.ones(nslices) * px_central # mean in pxbar
pym = np.ones(nslices) * py_central # mean in pybar

gammasig = np.ones(nslices) * gammasigh # rms sigma in gamma (in units of gamma, NOT gamma / gamma0!!!)
xsig = np.ones(nslices) * xsigh # rms sigma in xbar
ysig = np.ones(nslices) * ysigh # rms sigma in ybar
pxsig = np.ones(nslices) * pxsigh # rms sigma in pxbar
pysig = np.ones(nslices) * pysigh # rms sigma in pybar

emitbar = np.ones(nslices) * emitbarh # emittance (scaled to lambda_r / 4pi)


#######     Charge distribution:

#    Get normalised function for charge
# For this particular implementation, define a small Gaussian tail-off for the current
# profile at each end of the beam

chdist = np.ones(nslices) # Initialize charge distribution function with flat top - all ones
nwaves4sigtail = 10.
tailsig = nwaves4sigtail * lrscaled
taillen = tailsig * 4.5

# front end - meaning, end at z2 = 0
inds = np.where(z2pos < taillen)
chdist[inds] = np.exp( -(z2pos[inds] - taillen)**2. /  2.  /  tailsig**2.)

# back tail, at max z2
inds = np.where(z2pos > z2pos[-1] - taillen)
chdist[inds] = np.exp( -(z2pos[inds] - (z2pos[-1] - taillen))**2. /  2.  /  tailsig**2.)

nfact = np.trapz(chdist, x=z2pos)
chdist = chdist / nfact  # normalize the function

charge = 100E-12 # bunch charge  (this gives around 400A for this system (CLARA))

qe = puffVars.qe
Ne = chdist * charge * dz2
Ne = Ne / qe


#######          Write dist input file:



inputfile = 'mydist.pufin'
f = open(inputfile, 'w')
f.write('Puffin dist file\n')
f.write('\n')
f.write('ipts = '  + '{:<10d}'.format(nslices)  + 'Dz2 = '  + \
      '{:<24.15E}'.format(dz2) + 'aw = '  + '{:<24.15E}'.format(puffVars.aw) + \
      'lambdaw = '  + '{:<24.15E}'.format(puffVars.lw) + \
      'lambdar = '  + '{:<24.15E}'.format(puffVars.lr) +  '\n')
f.write('rho = '  + '{:<24.15E}'.format(puffVars.rho) +  \
        'sigx1D = '  + '{:<24.15E}'.format(sigx1D) +  \
        'sigy1D = '  + '{:<24.15E}'.format(sigy1D) + '\n') #rho =     8.332324e-003  sigx1D =     0.0677  sigy1D =     0.0677
f.write('\n')
f.write('       z2           gamma          x_bar          y_bar        sig_x_bar      sig_y_bar        px_bar         py_bar    \n')
f.write(' sig_gamma_tot    sig_px_bar     sig_py_bar         Ne         emit_barL   \n')
f.write('------------------------------------------------------------------------------------------------------------------------\n')


for ix in arange(0,nslices):
    f.write( '{:<24.15E}'.format(z2pos[ix]) + '{:<24.15E}'.format(gammam[ix]) \
    + '{:<24.15E}'.format(xm[ix]) + '{:<24.15E}'.format(ym[ix]) \
    + '{:<24.15E}'.format(xsig[ix]) + '{:<24.15E}'.format(ysig[ix]) \
    + '{:<24.15E}'.format(pxm[ix]) + '{:<24.15E}'.format(pym[ix]) + '\n')
    
#      '       z2           gamma          x_bar          y_bar        sig_x_bar      sig_y_bar        px_bar         py_bar    \n')
    f.write('{:<24.15E}'.format(gammasig[ix]) + '{:<24.15E}'.format(pxsig[ix]) \
    + '{:<24.15E}'.format(pysig[ix]) + '{:<24.15E}'.format(Ne[ix]) \
    + '{:<24.15E}'.format(emitbar[ix]) + '\n') 
    
#       sig_gamma_tot    sig_px_bar     sig_py_bar         Ne         emit_barL   \n')
#write data




f.close()


