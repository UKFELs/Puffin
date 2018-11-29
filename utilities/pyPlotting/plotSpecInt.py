# Copyright (c) 2012-2018, University of Strathclyde
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
from retrieve import readField


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

def plotSpecInt(h5fname, ftplottype=None):

    mdata = fdata(h5fname)

#   To select temporal slice of field....
    
    #z2s = 50  # start in units of z2
    #z2e = 80  # End in units of z2 
    # (everything outside this domain will be set to zero)

    #z2si = int(np.floor(z2s / dz2))
    #z2ei = int(np.floor(z2e / dz2))

    #z2axis = (np.arange(z2si,z2ei) - z2si) * dz2


#    ...otherwise take full field

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0, mdata.vars.nz2)) * mdata.vars.dz2

    xaxis = (np.arange(0, mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0, mdata.vars.ny)) * mdata.vars.dybar

    npads = mdata.vars.nz2

    NumUniquePts = np.int_(np.ceil((npads+1)/2))
    fs = (mdata.vars.nz2)/lenz2 #sampling frequency

#    xpows = np.zeros(NumUniquePts)
#    ypows = np.zeros(NumUniquePts)
    ftfieldtemp = np.zeros(NumUniquePts)

#    for ix in np.arange(0, mdata.vars.nx):
#        for iy in np.arange(0, mdata.vars.ny):
#            print ix, iy

    xf, yf = readField(h5fname, f1D=1)
    xff = np.fft.fft(xf)
    yff = np.fft.fft(yf)
            
    ftfieldtemp = xff[0:NumUniquePts]

    ftxpower = np.square(np.absolute(ftfieldtemp))

    if np.remainder(npads,2) == 1:
        ftxpower[1:] = ftxpower[1:]*2
    else:
        ftxpower[1:-1] = ftxpower[1:-1]*2

#    xpows = xpows + ftxpower

    ftfieldtemp = yff[0:NumUniquePts]

    ftypower = np.square(np.absolute(ftfieldtemp))

    if np.remainder(npads, 2) == 1:
        ftypower[1:] = ftypower[1:]*2
    else:
        ftypower[1:-1] = ftypower[1:-1]*2

#    ypows = ypows + ftypower
#   For padding with zeros!!!

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

    #intens = np.square(xf) + np.square(yf)

    #npads = np.int(np.round(mdata.vars.nz2 / 2))
    


    


    
       
       
    #print str(mdata.vars.lr)
    #ftxaxis = (np.arange(0,NumUniquePts)*(fs/(npads)))*(4.*np.pi*mdata.vars.rho)
    #ftxaxis[1:] = 1 / ftxaxis[1:]
    #ftxaxis[0] = ftxaxis[-1] + 1

    if (ftplottype == 1):
        ftxaxis = mdata.vars.lr / ((np.arange(0,NumUniquePts)*(fs/(npads)))*(4.*np.pi*mdata.vars.rho))
        sp_x_axis=r'$\lambda (nm)$' 

    else:
        ftxaxis = (np.arange(0,NumUniquePts)*(fs/(npads)))*(4.*np.pi*mdata.vars.rho)
        sp_x_axis=r'$\omega / \omega_r$'

    sp_title='Intensity Spectrum'

#    z2axis = [z2axis,z2axis,z2axis,z2axis]

#    ax1 = plt.subplot(211)
#    plt.plot(z2axis, xf, label='x field')
#    plt.plot(z2axis, yf, label='y field')
#    plt.xlabel(r'$\bar{z}_2$', fontsize=16)
#    plt.ylabel('Fields', fontsize=16)
#    plt.legend()

# Adding subplot
    axes = plt.subplot(111)
    plt.xlabel(sp_x_axis, fontsize=16)
    plt.ylabel('Intensity (a.u.)', fontsize=16)

#####  SAVE DATA


#    outfilename = 'specpow.h5'
#
#    h5=tables.open_file(outfilename,'w')
#
#    h5.create_array('/','wavelens', ftxaxis)
#    h5.root.wavelens._v_attrs.vsKind='structured'
#    h5.root.wavelens._v_attrs.vsType='mesh'
#    h5.root.wavelens._v_attrs.vsStartCell=0
#    #h5.root.zSeries._v_attrs.vsNumCells=numTimes-1 # -1 as zonal
#    h5.root.wavelens._v_attrs.vsLowerBounds=ftxaxis[0]
#    h5.root.wavelens._v_attrs.vsUpperBounds=ftxaxis[-1]
#    h5.root.wavelens._v_attrs.vsAxisLabels="lambda (m)"
#    
    fttpower = ftxpower + ftypower
#
#
#
#    h5.create_array('/','specpow',fttpower)
#    h5.root.specpow._v_attrs.vsMesh='wavelens'
#    h5.root.specpow._v_attrs.vsType='variable'
#    h5.root.specpow._v_attrs.vsAxisLabels='Power (a.u.)'
#
#
#    h5.close()


    if ftplottype==1:
        #plt.loglog(ftxaxis, ftxpower, label='x intensity')
        #plt.plot(ftxaxis, ftxpower, label='x power')
        #plt.plot(ftxaxis, ftypower, label='y power')
        plt.plot(ftxaxis*1e9, fttpower / np.max(fttpower), label='Intensity')
        axes.set_xlim([90, 110])

    else:
#        plt.semilogy(ftxaxis, ftxpower, label='x intensity')
#        plt.plot(ftxaxis, ftypower, label='y intensity')
        plt.plot(ftxaxis, fttpower / np.max(fttpower), label='Intensity')
        axes.set_xlim([0.8, 1.2])

#    plt.legend()

    nameparts = h5fname.split('_')
    basename = nameparts[0]
    z = mdata.vars.z
    
    plt.tight_layout()
    
    plt.savefig(basename + "-spec-intensity-z-" + str(z) + ".png")

#    plt.show()


#    plt.show(block=False)
#    h5f.close()


if __name__ == '__main__':
    h5fname=sys.argv[1]
    if len(sys.argv) == 3:
        ftplottype = float(sys.argv[2])
    else:
        ftplottype=None

    plotSpecInt(h5fname, ftplottype)