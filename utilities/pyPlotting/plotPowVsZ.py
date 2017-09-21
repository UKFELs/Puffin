# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This produces a plot of the power, averaged over the temporal mesh, from Puffin,
against distance through the undulator z.
"""

import sys, glob, os
import numpy as np
from numpy import arange
import matplotlib.pyplot as plt
import tables
from fdataClass import fdata
from puffDataClass import puffData
import getPow

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


def plotPowVsZ(basename, cfr=None, dfr=None):


    filelist = getFileSlices(basename)
    print filelist

    mdata = fdata(filelist[0])

    sampleFreq = 1.0 / mdata.vars.dz2

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0,mdata.vars.nz2)) * mdata.vars.dz2
    
    xaxis = (np.arange(0,mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0,mdata.vars.ny)) * mdata.vars.dybar

    fcount = 0
    
    pows = np.zeros(len(filelist))
    zData = np.zeros(len(filelist))
    
    if (mdata.vars.q1d==1):
    
        for ij in filelist:
            pows[fcount] = getPow.getPow(ij, cfr, dfr, qAv = 1, qScale = 0)
            zData[fcount] = getZData(ij)
            fcount += 1

    else:

        for ij in filelist:
            pows[fcount] = getPow.getPow(ij, cfr, dfr, qAv = 1, qScale = 0)
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
    plt.semilogy(zData, pows, label=plotLab)
    #ax1.set_title(axLab)
    plt.xlabel('z (m)')
    plt.ylabel(axLab)

    #plt.legend()

    if ((cfr == None) or (dfr == None)):
        opname = basename + "-unfiltered-power.png"
    else:
        opname = basename + "-filt-" + str(cfr) + "-" + str(dfr) + "-power.png"

    plt.savefig(opname)
#    plt.show()


#    plt.show(block=False)
#    h5f.close()


if __name__ == '__main__':

    basename = sys.argv[1]

    if len(sys.argv) == 4:
        cfr = float(sys.argv[2])
        dfr = float(sys.argv[3])
    else:
        cfr=None
        dfr=None

    plotPowVsZ(basename, cfr, dfr)
    