# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This produces a plot of the power from Puffin field datafiles, as a function of
distance through the undulator z. 

If the Puffin mesh type was periodic, then the power will be averaged over the 
temporal mesh.

If the mesh type was temporal, then the power plotted will be the PEAK power in
the mesh.
"""

import sys, glob, os
import numpy as np
from numpy import arange
import matplotlib.pyplot as plt
import tables
from puffdata import fdata
from puffdata import puffData
from retrieve import getPow
from retrieve import getFileSlices
from retrieve import getZData

iTemporal = 0
iPeriodic = 1


##################################################################
#
##





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

    if (mdata.vars.iMesh == iPeriodic):
        gAv = 1  #  for average...
    else:
        gAv = 2  #  for peak...
    
    for ij in filelist:
        pows[fcount] = getPow(ij, cfr, dfr, irtype = gAv, qScale = 0)
        zData[fcount] = getZData(ij)
        fcount += 1


#    plotLab = 'SI Power'
#    axLab = 'Power (W)'

    if (mdata.vars.iMesh == iPeriodic):
        plotLab = 'Power'
        axLab = 'Power (W)'
    else:
        plotLab = 'Peak Power'
        axLab = 'Power (W)'
        

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
    