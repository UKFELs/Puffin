# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is an examplar script to produce a plot of the filtered energy.
"""

import sys, glob, os
import numpy as np
from numpy import arange
import matplotlib.pyplot as plt
import tables
from fdataClass import fdata
from puffDataClass import puffData
import getFiltPow

iTemporal = 0
iPeriodic = 1


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
            power = getFiltPow.getFiltPow(ij, cfr, dfr, qAv = 1, qScale = 0)
            ens[fcount] = np.trapz(power, x=z2axis)
            zData[fcount] = getZData(ij)
            fcount += 1

    else:

        for ij in filelist:
            if (mdata.vars.iMesh==iPeriodic):
                ens[fcount] = getFiltPow.getFiltPow(ij, dfr, cfr, qAv = 1, qScale = 0)
            zData[fcount] = getZData(ij)
            fcount += 1

    plotLab = 'SI Power'
    axLab = 'Power (W)'

#    if (mdata.vars.iMesh == iPeriodic):
#        plotLab = 'SI Power'
#        axLab = 'Power (W)'
#    else:
#        plotLab = 'Scaled Energy'
#        axLab = 'Filtered Energy'
        

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
    