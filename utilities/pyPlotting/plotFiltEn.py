# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is an examplar script to produce a plot of the filtered energy.
"""

import sys, glob, os
import numpy as np
from numpy import pi
from numpy import arange
import matplotlib.pyplot as plt
from matplotlib.pyplot import specgram
import tables

iTemporal = 0
iPeriodic = 1
#iMesh = iPeriodic




class puffData:
    
    c0 = 2.99792458e8
    qe = 1.60217653e-19
    eps0 = 8.854187817e-12
    me = 9.1093826e-31
    h = 6.626e-34

    def __init__(self):
        self.rho = 0.001
        self.gamma0 = 800.
        self.au = 1.
        self.lw = 0.04
        self.lr = 1.0e-7
        self.eta = 1.0e-5
        self.kappa = 1.0
        self.lg = 1.
        self.lc = 1.e-6
        self.npkbar = 1.0e9
        
        self.qscale = 1
        self.iMesh = 1
        self.q1d = 1
        
        self.dxbar = 1.0
        self.dybar = 1.0
        self.dz2 = 1.0
        self.nx = 1
        self.ny = 1
        self.nz2 = 1
        
        self.dx = 1.
        self.dy = 1.

        self.dzbar = 1.0e-3
        self.zbar = 0.
        self.zbarloc = 0.
        self.z = 0.
        self.zloc = 0.
        
        self.powScale = self.lg * self.lc * self.c0 * self.eps0 * \
                          np.square((self.gamma0 * self.me * np.square(self.c0) ) \
                          / (self.qe * self.kappa * self.lg ))


    def unscale(self):
        self.dx = self.dxbar * np.sqrt(self.lg * self.lc) # ...etc
        




class fdata:
    def __init__(self, fname):
        self.h5fname = fname
        
        h5f = tables.open_file(fname, mode='r')
        self.vars = puffData()        
        
        self.vars.rho = h5f.root.runInfo._v_attrs.rho
        self.vars.gamma0 = h5f.root.runInfo._v_attrs.gamma_r
        self.vars.au = h5f.root.runInfo._v_attrs.aw
        self.vars.lw = h5f.root.runInfo._v_attrs.lambda_w
        self.vars.lr = h5f.root.runInfo._v_attrs.lambda_r
        self.vars.eta = h5f.root.runInfo._v_attrs.eta
        self.vars.kappa = h5f.root.runInfo._v_attrs.kappa
        self.vars.lg = h5f.root.runInfo._v_attrs.Lg
        self.vars.lc = h5f.root.runInfo._v_attrs.Lc
        self.vars.npkbar = h5f.root.runInfo._v_attrs.npk_bar

        self.vars.qscale = h5f.root.runInfo._v_attrs.fieldMesh
        self.vars.iMesh = h5f.root.runInfo._v_attrs.iScale


        self.vars.dxbar = h5f.root.runInfo._v_attrs.sLengthOfElmX
        self.vars.dybar = h5f.root.runInfo._v_attrs.sLengthOfElmY
        self.vars.dz2 = h5f.root.runInfo._v_attrs.sLengthOfElmZ2
        self.vars.nx = h5f.root.runInfo._v_attrs.nX
        self.vars.ny = h5f.root.runInfo._v_attrs.nY
        self.vars.nz2 = h5f.root.runInfo._v_attrs.nZ2

        self.vars.dx = self.vars.dxbar * np.sqrt(self.vars.lg * self.vars.lc)
        self.vars.dy = self.vars.dybar * np.sqrt(self.vars.lg * self.vars.lc)

        self.vars.dzbar = h5f.root.runInfo._v_attrs.sStepSize
        self.vars.zbar = h5f.root.runInfo._v_attrs.zbarTotal
        self.vars.zbarloc = h5f.root.runInfo._v_attrs.zbarLocal
        self.vars.z = h5f.root.runInfo._v_attrs.zTotal
        self.vars.zloc = h5f.root.runInfo._v_attrs.zLocal
        
        self.vars.q1d = 0

        if (self.vars.nx==1):
            if (self.vars.ny==1):
              self.vars.q1d = 1
        
        self.vars.powScale = self.vars.lg * self.vars.lc * self.vars.c0 * self.vars.eps0 \
                              * np.square((self.vars.gamma0 * self.vars.me * \
                              np.square(self.vars.c0) ) / (self.vars.qe * \
                              self.vars.kappa * self.vars.lg ))
        
        h5f.close()




#t = np.linspace(-1, 1, 200, endpoint=False)

#sig  = np.cos(2 * np.pi * 7 * t) + \
#     np.real(np.exp(-7*(t-0.4)**2)*np.exp(1j*2*np.pi*2*(t-0.4)))



# you want the NFFT to be smaller than the total signal size, 
# it is the length of the local FFT.

#specP, freqs, time, image = specgram(sig, NFFT=25, Fs=200, noverlap=10)

#plt.imshow(specP)
#plt.show()




def FilterField(field,crfr,distfr, pvars): #nZ2,sLengthOfElmZ2, rho, q1d, iMesh):

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#% filter - HARD FILTER

#%crfr=1.4167;
#%distfr=0.2;

    nn = np.round(pvars.dz2 * pvars.nz2 * crfr / (4*pi*pvars.rho))
    nns = np.round(pvars.dz2 * pvars.nz2 * distfr / (4*pi*pvars.rho))

    if (pvars.q1d == 1):

#%%%%%    1D    %%%%%%%

        if (pvars.iMesh == iPeriodic):
            
            sn = 1
            ftfield[0:sn] = 0
            ftfield[sn+1:np.ceil(pvars.nz2/2)] = 0

            ftfield[np.ceil(pvars.nz2/2) + 1 - 1:-sn] = 0


        else:

            ftfield = np.fft.fft(field)

            ftfield[0:np.int(nn-nns)] = 0
            ftfield[np.int(nn+nns-1):np.int(np.ceil(pvars.nz2/2))] = 0
      
            ftfield[np.int(np.ceil(pvars.nz2/2) + 1 - 1):np.int(pvars.nz2-(nn+nns)+2)] = 0
            ftfield[np.int(pvars.nz2 - (nn-nns) + 2 - 1 ) : np.int(pvars.nz2)] = 0
      
        field = np.fft.ifft(ftfield)
      
        nfield = np.real(field)

    else:
  
#%%%%%    3D    %%%%%%%

      ftfield = np.fft.fft(field)
    
      if (pvars.iMesh == iPeriodic):

        sn = 1
        ftfield[:,:,0:sn] = 0
        ftfield[:,:,sn+1:np.ceil(pvars.nz2/2)] = 0

        ftfield[:,:,np.ceil(pvars.nz2/2) + 1 - 1:-sn] = 0

      else:
        ftfield[:,:,0:(nn-nns)] = 0
        ftfield[:,:,(nn+nns-1):np.ceil(pvars.nz2/2)] = 0

        ftfield[:,:,np.ceil(pvars.nz2/2) + 1 - 1:pvars.nz2-(nn+nns)+2] = 0
        ftfield[:,:,(pvars.nz2 - (nn-nns) + 2 -1 ) : pvars.nz2] = 0
    
      field = np.fft.ifft(ftfield)
    
      nfield = np.real(field)

    return nfield

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



def getFileSlices(baseName):
  """ getTimeSlices(baseName) gets a list of files

  That will be used down the line...
  """
  filelist=glob.glob(os.getcwd()+os.sep+baseName+'_aperp_C_*.h5')
  
  dumpStepNos=[]
  for thisFile in filelist:
    thisDump=int(thisFile.split(os.sep)[-1].split('.')[0].split('_')[-1])
    dumpStepNos.append(thisDump)

  for i in range(len(dumpStepNos)):
    filelist[i]=baseName+'_aperp_C_'+str(sorted(dumpStepNos)[i])+'.h5'
  return filelist



def getFiltPow(h5fname):

#    h5f = tables.open_file(h5fname, mode='r')
#
#    dz2 = h5f.root.runInfo._v_attrs.sLengthOfElmZ2
#    nz2 = h5f.root.runInfo._v_attrs.nZ2
#    rho = h5f.root.runInfo._v_attrs.rho
#    iMesh = h5f.root.runInfo._v_attrs.fieldMesh
#
#    sampleFreq = 1.0 / dz2

#   To select temporal slice of field....
    
    #z2s = 50
    #z2e = 80

    #z2si = int(np.floor(z2s / dz2))
    #z2ei = int(np.floor(z2e / dz2))

    #z2axis = (np.arange(z2si,z2ei) - z2si) * dz2


#    ...otherwise take full field

#    nx = h5f.root.runInfo._v_attrs.nX
#    ny = h5f.root.runInfo._v_attrs.nY
#    dx = h5f.root.runInfo._v_attrs.sLengthOfElmX
#    dy = h5f.root.runInfo._v_attrs.sLengthOfElmY

    mdata = fdata(h5fname)

#    lenz2 = (nz2-1) * dz2
#    z2axis = (np.arange(0,nz2)) * dz2
#
#    xaxis = (np.arange(0,nx)) * dx
#    yaxis = (np.arange(0,ny)) * dy

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0,mdata.vars.nz2)) * mdata.vars.dz2
    
    xaxis = (np.arange(0,mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0,mdata.vars.ny)) * mdata.vars.dybar

    h5f = tables.open_file(h5fname, mode='r')

    if (mdata.vars.q1d == 1):
      xf = h5f.root.aperp[:,0]
    else:
      xf = h5f.root.aperp[:,:,:,0]
    # xfs = xf[z2si:z2ei]   # for selecting slice...

    if (mdata.vars.q1d == 1):
        yf = h5f.root.aperp[:,1]
    else:
        yf = h5f.root.aperp[:,:,:,1]

    h5f.close()

    cfr = 1.0
    dfr = 0.4

    xf = FilterField(xf, cfr, dfr, mdata.vars)
    yf = FilterField(yf, cfr, dfr, mdata.vars)

    intens = np.square(xf) + np.square(yf)

    return intens


def getZData(fname):
    h5f = tables.open_file(fname, mode='r')
    zD = h5f.root.aperp._v_attrs.zTotal
    h5f.close()
    return zD





#def getRunAttrs(fname):
#    
#    self.dz2 = h5f.root.runInfo._v_attrs.sLengthOfElmZ2
#    self.nz2 = h5f.root.runInfo._v_attrs.nZ2
#    self.rho = h5f.root.runInfo._v_attrs.rho
#    self.lg = h5f.root.runInfo._v_attrs.Lg
#    self.lc = h5f.root.runInfo._v_attrs.Lc
#    self.gamma0 = h5f.root.runInfo._v_attrs.gamma_r
#    self.kappa = h5f.root.runInfo._v_attrs.kappa
#    self.iMesh = h5f.root.runInfo._v_attrs.fieldMesh
#    c0 = 2.99792458e8
#    qe = 1.60217653e-19
#    eps0 = 8.854187817e-12
#    me = 9.1093826e-31
#
#    nx = h5f.root.runInfo._v_attrs.nX
#    ny = h5f.root.runInfo._v_attrs.nY
#    dx = h5f.root.runInfo._v_attrs.sLengthOfElmX
#    dy = h5f.root.runInfo._v_attrs.sLengthOfElmY
    


def plotFiltEn(basename):


    filelist = getFileSlices(basename)
    print filelist
#    h5f = tables.open_file(filelist[0], mode='r')
#
#    dz2 = h5f.root.runInfo._v_attrs.sLengthOfElmZ2
#    nz2 = h5f.root.runInfo._v_attrs.nZ2
#    rho = h5f.root.runInfo._v_attrs.rho
#    lg = h5f.root.runInfo._v_attrs.Lg
#    lc = h5f.root.runInfo._v_attrs.Lc
#    gamma0 = h5f.root.runInfo._v_attrs.gamma_r
#    kappa = h5f.root.runInfo._v_attrs.kappa
#    iMesh = h5f.root.runInfo._v_attrs.fieldMesh
#    c0 = 2.99792458e8
#    qe = 1.60217653e-19
#    eps0 = 8.854187817e-12
#    me = 9.1093826e-31


#    powScale = lg * lc * c0 * eps0 * np.square((gamma0 * me * np.square(c0) ) / (qe * kappa * lg ))
#
#    nx = h5f.root.runInfo._v_attrs.nX
#    ny = h5f.root.runInfo._v_attrs.nY
#    dx = h5f.root.runInfo._v_attrs.sLengthOfElmX
#    dy = h5f.root.runInfo._v_attrs.sLengthOfElmY


    mdata = fdata(filelist[0])

    sampleFreq = 1.0 / mdata.vars.dz2

#   To select temporal slice of field....
    
    #z2s = 50
    #z2e = 80

    #z2si = int(np.floor(z2s / dz2))
    #z2ei = int(np.floor(z2e / dz2))

    #z2axis = (np.arange(z2si,z2ei) - z2si) * dz2


#    ...otherwise take full field

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0,mdata.vars.nz2)) * mdata.vars.dz2
    
    xaxis = (np.arange(0,mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0,mdata.vars.ny)) * mdata.vars.dybar

#    q1d = 0

#    if (nx==1):
#        if (ny==1):
#          q1d = 1
#    h5f.close()

    fcount = 0
    
    ens = np.zeros(len(filelist))
    zData = np.zeros(len(filelist))
#    zData[fieldCount] = h5in.root._f_get_child("power")._v_attrs.zTotal 
    
    if (mdata.vars.q1d==1):
    
        for ij in filelist:
            tintens = getFiltPow(ij)
            ens[fcount] = np.trapz(tintens, x=z2axis)
            zData[fcount] = getZData(ij)
            fcount += 1

    else:

        for ij in filelist:
            tintens = getFiltPow(ij)
            #ens[fcount] = np.trapz(tintens, x=z2axis)
            #print "first", np.shape(xaxis), np.shape(tintens)
            tintensx = np.trapz(tintens, x=xaxis, axis=0)
            #print "second", np.shape(xaxis), np.shape(tintensx)
            tintensxy = np.trapz(tintensx, x=yaxis, axis=0)
            ens[fcount] = np.trapz(tintensxy, x=z2axis)
            if (mdata.vars.iMesh==iPeriodic):
                ens[fcount] = ens[fcount] / lenz2
            zData[fcount] = getZData(ij)
            fcount += 1


    h = 6.626e-34 # Planck constant
    q_e = 1.60217646e-19 # Charge on electron
    c_0 = 2.99792458e8 # Speed of light in vacuum

    if (mdata.vars.iMesh==iPeriodic):
        plotLab = 'SI Power'
        axLab = 'Power (W)'
        ens = ens * mdata.vars.powScale # * mdata.vars.lc / mdata.vars.c0
    else:
        plotLab = 'Scaled Energy'
        axLab = 'Filtered Energy'
        

    ax1 = plt.subplot(111)
    plt.semilogy(zData, ens, label=plotLab)
    #ax1.set_title(axLab)
    plt.xlabel('z (m)')
    plt.ylabel(axLab)

    #plt.legend()

    plt.savefig(basename + "-power.png")
#    plt.show()


#    plt.show(block=False)
#    h5f.close()


if __name__ == '__main__':
    basename=sys.argv[1]
    plotFiltEn(basename)
    