# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is an examplar script to produce a plot of the cycle-averaged magnitude 
and phase of the fields
"""

import sys
import numpy as np
from numpy import pi
from numpy import arange
import matplotlib.pyplot as plt
from retrieve import getMagPhase
from retrieve import readField
from puffdata import fdata
from puffdata import puffData


#t = np.linspace(-1, 1, 200, endpoint=False)

#sig  = np.cos(2 * np.pi * 7 * t) + \
#     np.real(np.exp(-7*(t-0.4)**2)*np.exp(1j*2*np.pi*2*(t-0.4)))



# you want the NFFT to be smaller than the total signal size, 
# it is the length of the local FFT.

#specP, freqs, time, image = specgram(sig, NFFT=25, Fs=200, noverlap=10)

#plt.imshow(specP)
#plt.show()



##################################################################
#
##






def plotMagPhase(h5fname):

    mdata = fdata(h5fname)

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0,mdata.vars.nz2)) * mdata.vars.dz2
    saxis = z2axis * mdata.vars.lc
    
    xaxis = (np.arange(0,mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0,mdata.vars.ny)) * mdata.vars.dybar

    sampleFreq = 1.0 / mdata.vars.dz2

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0,mdata.vars.nz2)) * mdata.vars.dz2

    xf, yf = readField(h5fname, f1D = 1)

    intens = np.square(xf) + np.square(yf)

    mgx, phx = getMagPhase(xf, mdata.vars.nz2, mdata.vars.rho, lenz2)
    mgy, phy = getMagPhase(yf, mdata.vars.nz2, mdata.vars.rho, lenz2)

    ax1 = plt.subplot(311)
    plt.plot(z2axis, xf, label='x-field')
    plt.plot(z2axis, yf, label='y-field')
    plt.xlabel('z2')
    plt.ylabel('Fields')
    plt.legend()

# example for adding subplot
    plt.subplot(312, sharex=ax1)
    plt.plot(z2axis, mgx, label='x-field')
    plt.plot(z2axis, mgy, label='y-field')
    plt.xlabel('z2')
    plt.ylabel('Mag')


    axes = plt.subplot(313, sharex=ax1)
    plt.plot(z2axis, phx, label='x-field')
    plt.plot(z2axis, phy, label='y-field')
    plt.xlabel('z2')
    plt.ylabel('Phase')
    axes.set_ylim([0, 2. * pi])

    nameparts = h5fname.split('_')
    basename = nameparts[0]

    z = mdata.vars.z

    plt.savefig(basename + "_magPhase_z_" + str(z) + ".png")


if __name__ == '__main__':
    h5fname=sys.argv[1]
    plotMagPhase(h5fname)
    
    


# plot(xaxis,magxrms);
# hold on;
# plot(xaxis,phasex,'r');
# hold off;