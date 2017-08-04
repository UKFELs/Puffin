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
import tables
from fdataClass import fdata
from puffDataClass import puffData

iTemporal = 0
iPeriodic = 1



def FilterField(field,crfr,distfr, pvars):

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#% filter - HARD FILTER

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



def getFiltPow(h5fname, cfr, dfr):

    mdata = fdata(h5fname)

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0, mdata.vars.nz2)) * mdata.vars.dz2
    
    xaxis = (np.arange(0, mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0, mdata.vars.ny)) * mdata.vars.dybar

    h5f = tables.open_file(h5fname, mode='r')

#   To select temporal slice of field....
    
    #z2s = 50
    #z2e = 80

    #z2si = int(np.floor(z2s / dz2))
    #z2ei = int(np.floor(z2e / dz2))

    #z2axis = (np.arange(z2si,z2ei) - z2si) * dz2

#    ...otherwise take full field

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

    xf = FilterField(xf, cfr, dfr, mdata.vars)
    yf = FilterField(yf, cfr, dfr, mdata.vars)

    intens = np.square(xf) + np.square(yf)

    return intens


def getZData(fname):
    h5f = tables.open_file(fname, mode='r')
    zD = h5f.root.aperp._v_attrs.zTotal
    h5f.close()
    return zD


def plotFiltEn(basename, cfr, dfr):


    filelist = getFileSlices(basename)
    print filelist

    mdata = fdata(filelist[0])

    sampleFreq = 1.0 / mdata.vars.dz2

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0,mdata.vars.nz2)) * mdata.vars.dz2
    
    xaxis = (np.arange(0,mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0,mdata.vars.ny)) * mdata.vars.dybar

    fcount = 0
    
    ens = np.zeros(len(filelist))
    zData = np.zeros(len(filelist))
    
    if (mdata.vars.q1d==1):
    
        for ij in filelist:
            tintens = getFiltPow(ij, cfr, dfr)
            ens[fcount] = np.trapz(tintens, x=z2axis)
            zData[fcount] = getZData(ij)
            fcount += 1

    else:

        for ij in filelist:
            tintens = getFiltPow(ij, dfr, cfr)
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

    basename = sys.argv[1]

    if len(sys.argv) == 3:
        cfr = sys.argv[2]
        dfr = sys.argv[3]
    else:
        cfr = 1.
        dfr = 0.4

#    cfr = sys.argv[2]
#    dfr = sys.argv[3]

    plotFiltEn(basename, cfr, dfr)
    