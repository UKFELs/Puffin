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
from puffdata import fdata
from puffdata import puffData
import readField


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

def plotSpecPow(h5fname, ftplottype=None):

    mdata = fdata(h5fname)

#   To select temporal slice of field....
    
    #z2s = 50
    #z2e = 80

    #z2si = int(np.floor(z2s / dz2))
    #z2ei = int(np.floor(z2e / dz2))

    #z2axis = (np.arange(z2si,z2ei) - z2si) * dz2


#    ...otherwise take full field

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0, mdata.vars.nz2)) * mdata.vars.dz2

    xaxis = (np.arange(0, mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0, mdata.vars.ny)) * mdata.vars.dybar

    xf, yf = readField.readField(h5fname, f1D=1)

    #xf = np.concatenate((xf, xf))
    #xf = np.concatenate((xf, xf))
    #xf = np.concatenate((xf, xf))
    #xf = np.concatenate((xf, xf))
    #xf[mdata.vars.nz2:] = 0.
    #yf = np.concatenate((yf, yf)) # [yf, yf, yf, yf]
    #yf = np.concatenate((yf, yf))
    #yf = np.concatenate((yf, yf))
    #yf = np.concatenate((yf, yf))
    #yf[mdata.vars.nz2:] = 0.

    intens = np.square(xf) + np.square(yf)

    #npads = np.int(np.round(mdata.vars.nz2 / 2))
    npads = mdata.vars.nz2


    xff = np.fft.fft(xf, n=npads)
    yff = np.fft.fft(yf, n=npads)

    NumUniquePts = np.int_(np.ceil((npads+1)/2))
    fs = (mdata.vars.nz2)/lenz2 #sampling frequency

    ftfieldtemp = np.zeros(NumUniquePts+1)
    ftfieldtemp = xff[0:NumUniquePts]

    ftxpower = np.square(np.absolute(ftfieldtemp))

    if np.remainder(npads,2) == 1:
        ftxpower[1:] = ftxpower[1:]*2
    else:
        ftxpower[1:-1] = ftxpower[1:-1]*2



    ftfieldtemp = yff[0:NumUniquePts]

    ftypower = np.square(np.absolute(ftfieldtemp))

    if np.remainder(npads, 2) == 1:
        ftypower[1:] = ftypower[1:]*2
    else:
        ftypower[1:-1] = ftypower[1:-1]*2
       
       
    #print str(mdata.vars.lr)
    #ftxaxis = (np.arange(0,NumUniquePts)*(fs/(npads)))*(4.*np.pi*mdata.vars.rho)
    #ftxaxis[1:] = 1 / ftxaxis[1:]
    #ftxaxis[0] = ftxaxis[-1] + 1

    if (ftplottype == 1):
        ftxaxis = mdata.vars.lr / ((np.arange(0,NumUniquePts)*(fs/(npads)))*(4.*np.pi*mdata.vars.rho))
        sp_x_axis=r'$\lambda (m)$' 

    else:
        ftxaxis = (np.arange(0,NumUniquePts)*(fs/(npads)))*(4.*np.pi*mdata.vars.rho)
        sp_x_axis=r'$\omega / \omega_r$'

    sp_title='Intensity Spectrum'

#    z2axis = [z2axis,z2axis,z2axis,z2axis]

    ax1 = plt.subplot(211)
    plt.plot(z2axis, xf, label='x field')
    plt.plot(z2axis, yf, label='y field')
    plt.xlabel(r'$\bar{z}_2$')
    plt.ylabel('Fields')
    plt.legend()

# example for adding subplot
    axes = plt.subplot(212)
    plt.xlabel(sp_x_axis, fontsize=16)
    plt.ylabel('Power')
    #print np.len(ftxaxis), np.len(ftxpower)

    if ftplottype==1:
        plt.loglog(ftxaxis, ftxpower, label='x power')
        plt.plot(ftxaxis, ftypower, label='y power')
        plt.plot(ftxaxis, ftxpower + ftypower, label='combined')
#        axes.set_xlim([5.8e-10, 7.2e-10])

    else:
        plt.semilogy(ftxaxis, ftxpower, label='x power')
        plt.plot(ftxaxis, ftypower, label='y power')
        plt.plot(ftxaxis, ftxpower + ftypower, label='combined')
#        axes.set_xlim([5.8e-10, 7.2e-10])

    plt.legend()

    nameparts = h5fname.split('_')
    basename = nameparts[0]
    z = mdata.vars.z
    
    plt.savefig(basename + "-spec-power-z-" + str(z) + ".png")

    plt.show()


#    plt.show(block=False)
#    h5f.close()


if __name__ == '__main__':
    h5fname=sys.argv[1]
    if len(sys.argv) == 3:
        ftplottype = float(sys.argv[2])
    else:
        ftplottype=None

    plotSpecPow(h5fname, ftplottype)
    


