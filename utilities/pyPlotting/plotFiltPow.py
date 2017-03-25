# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is an examplar script to produce a plot of the cycle-averaged magnitude 
and phase of the fields
"""

import sys
import tables
import numpy as np
from numpy import pi
from numpy import arange
import matplotlib.pyplot as plt
from matplotlib.pyplot import specgram



#t = np.linspace(-1, 1, 200, endpoint=False)

#sig  = np.cos(2 * np.pi * 7 * t) + \
#     np.real(np.exp(-7*(t-0.4)**2)*np.exp(1j*2*np.pi*2*(t-0.4)))



# you want the NFFT to be smaller than the total signal size, 
# it is the length of the local FFT.

#specP, freqs, time, image = specgram(sig, NFFT=25, Fs=200, noverlap=10)

#plt.imshow(specP)
#plt.show()




def FilterField(field,crfr,distfr,nZ2,sLengthOfElmZ2, rho, q1d):

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#% filter - HARD FILTER

#%crfr=1.4167;
#%distfr=0.2;

    nn = np.round(sLengthOfElmZ2 * nZ2 * crfr / (4*pi*rho))
    nns = np.round(sLengthOfElmZ2 * nZ2 * distfr / (4*pi*rho))

    if (q1d == 1):

#%%%%%    1D    %%%%%%%

        ftfield = np.fft.fft(field)

        ftfield[0:(nn-nns)] = 0
        ftfield[(nn+nns-1):np.ceil(nZ2/2)] = 0
      
        ftfield[np.ceil(nZ2/2) + 1 - 1:nZ2-(nn+nns)+2] = 0
        ftfield[(nZ2 - (nn-nns) + 2 - 1 ) : nZ2] = 0
      
        field = np.fft.ifft(ftfield)
      
        xfield = np.real(field)

    else:
  
#%%%%%    3D    %%%%%%%

      ftfield = np.fft.fft(field)
    
      ftfield[:,:,0:(nn-nns)] = 0
      ftfield[:,:,(nn+nns-1):ceil(nZ2/2)] = 0
    
      ftfield[:,:,ceil(nZ2/2) + 1 - 1:nZ2-(nn+nns)+2] = 0
      ftfield[:,:,(nZ2 - (nn-nns) + 2 -1 ) : nZ2] = 0
    
      field = np.fft.ifft(ftfield)
    
      xfield = np.real(field)

    return xfield

#%%%%%%%%%%%%%%%%%%%%%%




##################################################################
#
##



def getMagPhase(Ex,nZ2,rho,syslen):

    wavel=4*pi*rho

    ax=1
    thix=-1

    xaxis = np.linspace(0,syslen,nZ2)  # xaxis for real space
# %Ex = ax*cos(xaxis/(2*rho)+thix); %Define x field

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# MAGNITUDE:
# 2*df is the number of nodes spanning a resonant wavelength
# de is the number of nodes to the left to average from
# dg is the number of nodes to the right
# so average around node_index-de:node_index+dg
# first of all, find the number of nodes corresponding to one lamda_r,nnl
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    lr=wavel
    nsp=syslen/(nZ2-1)
    nel=np.round(lr/nsp)
    nnl=nel+1

    df=np.floor((nnl)/2)
    dg=df

    if (np.mod(nnl,2)==1):
        de=df
    else:
        de=df-1
        
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Specify a segment around the current node
# and average around it over a wavelength
# intxrms is rms square root of the mean intensity, or
# the rms absolute field.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    intxrms=np.zeros(nZ2)
    intyrms=np.zeros(nZ2)

    for m in np.arange(nZ2):
        lo=m-de
        hi=lo+(nnl-2) # hi=m+dg
        if lo<1:
            lo=0
        if hi>nZ2:
            hi=nZ2-1
        tar = np.square(Ex[np.int_(lo):np.int_(hi+1)])
        tvar = np.mean(tar)
        intxrms[m]=np.sqrt(tvar)

    magxrms=np.sqrt(2)*intxrms # magxrms is the rms magnitude of the wave

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PHASE:
# First create the "reference" wave, cos(\omega t)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    z2cyclic=xaxis-(4*pi*rho*np.floor(xaxis/(4*pi*rho)))
    z2cyclic=z2cyclic/(2*rho)

    slopy=-np.sin(2*pi*xaxis/wavel)

    invcosx = np.zeros(nZ2)

    for a in np.arange(nZ2):
        if (Ex[a]/magxrms[a] < -1 ) or ((Ex[a]/magxrms[a] > 1 )):
            invcosx[a] = 0
        else:
            invcosx[a]=np.arccos(Ex[a]/magxrms[a])

    for a in np.arange(nZ2):
      if (abs(Ex[a])>=magxrms[a]):
          if (Ex[a]>0):
              invcosx[a]=0
          elif (Ex[a]<0):
              invcosx[a]=pi


    for a in arange(nZ2-1):
        if Ex[a]<Ex[a+1]: # best way to do this may be taking 
             #### gradient of Ex. If negative, then shift
             #### to 3rd and 4th quadrant....
             # invcosx(a)=invcosx(a)*(-1)
            invcosx[a]=(2*np.pi)-invcosx[a]

    phasex=invcosx-z2cyclic

    for a in np.arange(nZ2):
        if phasex[a]<0:
            phasex[a]=2*pi-np.absolute(phasex[a])

    return magxrms, phasex



def plotFiltPow(h5fname):

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

    xf = h5f.root.aperp[0,:]
    # xfs = xf[z2si:z2ei]   # for selecting slice...

    yf = h5f.root.aperp[1,:]


    cfr = 1.0
    dfr = 0.4

    xf = FilterField(xf, cfr, dfr, nz2, dz2, rho, 1)
    yf = FilterField(yf, cfr, dfr, nz2, dz2, rho, 1)

    intens = np.square(xf) + np.square(yf)



    mgx, phx = getMagPhase(xf,nz2,rho,lenz2)
    mgy, phy = getMagPhase(yf,nz2,rho,lenz2)

#    xff = np.fft.fft(xf)
#    yff = np.fft.fft(yf)

    h = 6.626e-34 # Planck constant
    q_e = 1.60217646e-19 # Charge on electron
    c_0 = 2.99792458e8 # Speed of light in vacuum


    ax1 = plt.subplot(311)
    plt.plot(z2axis, xf)
    plt.plot(z2axis, yf)

# example for adding subplot
    plt.subplot(312, sharex=ax1)
    plt.plot(z2axis, mgx)
    plt.plot(z2axis, mgy)


    plt.subplot(313, sharex=ax1)
    plt.plot(z2axis, phx)

#    plt.savefig("ExEy-SpecPower3.png")
    plt.show()


#    plt.show(block=False)
    h5f.close()


if __name__ == '__main__':
    h5fname=sys.argv[1]
    plotFiltPow(h5fname)
    
    


# plot(xaxis,magxrms);
# hold on;
# plot(xaxis,phasex,'r');
# hold off;


