# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is an examplar script to produce a plot of the instantaneous power
spectrum in 1D (or the intensity spectrum)
"""

import sys
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.pyplot import specgram
import tables


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

def plotSpecPow(h5fname):

    h5f = tables.open_file(h5fname, mode='r')

    dz2 = h5f.root.runInfo._v_attrs.sLengthOfElmZ2
    nz2 = h5f.root.runInfo._v_attrs.nZ2
    rho = h5f.root.runInfo._v_attrs.rho

    sampleFreq = 1.0 / dz2

#   To select temporal slice of field....
    
    #z2s = 50
    #z2e = 80

    #z2si = int(np.floor(z2s / dz2))
    #z2ei = int(np.floor(z2e / dz2))

    #z2axis = (np.arange(z2si,z2ei) - z2si) * dz2


#    ...otherwise take full field

    lenz2 = (nz2-1) * dz2
    z2axis = (np.arange(0,nz2)) * dz2


    xf = h5f.root.aperp[:,:,:,0]
    # xfs = xf[z2si:z2ei]   # for selecting slice...

    yf = h5f.root.aperp[:,:,:,1]

    intens = np.square(xf) + np.square(yf)

    xff = np.fft.fft(xf)
    yff = np.fft.fft(yf)

    h = 6.626e-34 # Planck constant
    q_e = 1.60217646e-19 # Charge on electron
    c_0 = 2.99792458e8 # Speed of light in vacuum

    NumUniquePts = np.int_(np.ceil((nz2+1)/2))
    fs = (nz2)/lenz2 #sampling frequency

    ftfieldtemp = xff[:,:,0:NumUniquePts]

    ftxpower = np.square(np.absolute(ftfieldtemp))

    if np.remainder(nz2,2) == 1:
        ftxpower[:,:,1:] = ftxpower[:,:,1:]*2
    else:
        ftxpower[:,:,1:-1] = ftxpower[:,:,1:-1]*2



    ftfieldtemp = yff[:,:,0:NumUniquePts]

    ftypower = np.square(np.absolute(ftfieldtemp))

    if np.remainder(nz2,2) == 1:
        ftypower[:,:,1:] = ftypower[:,:,1:]*2
    else:
        ftypower[:,:,1:-1] = ftypower[:,:,1:-1]*2



    nx = h5f.root.runInfo._v_attrs.nX
    ny = h5f.root.runInfo._v_attrs.nY
    dx = h5f.root.runInfo._v_attrs.sLengthOfElmX
    dy = h5f.root.runInfo._v_attrs.sLengthOfElmY


    xaxis = (np.arange(0,nx)) * dx
    yaxis = (np.arange(0,ny)) * dy

    #print "first", np.shape(xaxis), np.shape(tintens)
    tintens = ftxpower
    tintensx = np.trapz(tintens, x=xaxis, axis=0)
    #print "second", np.shape(xaxis), np.shape(tintensx)
    tintensxy = np.trapz(tintensx, x=yaxis, axis=0)
    ftxpower2 = tintensxy

    tintens = ftypower
    tintensx = np.trapz(tintens, x=xaxis, axis=0)
    #print "second", np.shape(xaxis), np.shape(tintensx)
    tintensxy = np.trapz(tintensx, x=yaxis, axis=0)
    ftypower2 = tintensxy


    ftxaxis = (np.arange(0,NumUniquePts)*(fs/nz2))*(4*np.pi*rho)
    sp_x_axis='$$\omega / \omega_r$$'
    sp_title='Intensity Spectrum'

    ax1 = plt.subplot(211)
    plt.plot(z2axis, xf[np.rint(nx/2.), np.rint(ny/2.), :], label='x field')
    plt.plot(z2axis, yf[np.rint(nx/2.), np.rint(ny/2.), :], label='y field')
    plt.legend()

    print "ftx = " + str(ftxpower2.shape)

# example for adding subplot
    plt.subplot(212)
    plt.semilogy(ftxaxis, ftxpower2, label='x power')
    plt.plot(ftxaxis, ftypower2, label='y power')
    plt.plot(ftxaxis, ftxpower2 + ftypower2, label='combined')

    plt.legend()
    plt.savefig("ExEy-SpecPower3.png")

    plt.show()


#    plt.show(block=False)
    h5f.close()


if __name__ == '__main__':
    h5fname=sys.argv[1]
    plotSpecPow(h5fname)
    


