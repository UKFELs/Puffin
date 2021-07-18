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

import sys
import numpy as np
from numpy import arange
import matplotlib.pyplot as plt
import tables
from puffdata import fdata
from retrieve import getPowFromInt
from retrieve import getIntFileSlices
from retrieve import getZData

iTemporal = 0
iPeriodic = 1

itemp = 0
iav = 1
ipeak = 2
icycav = 3


def plotPowVsZ(basename):
    filelist = getIntFileSlices(basename)
    print(filelist)

    mdata = fdata(filelist[0])

    fcount = 0
    
    pows = np.zeros(len(filelist))
    zData = np.zeros(len(filelist))

    if (mdata.vars.iMesh == iPeriodic):
        gAv = iav  #  for average...
        plotLab = 'Power'
        axLab = 'Power (W)'
    else:
        gAv = ipeak  #  for peak...
        plotLab = 'Peak Power'
        axLab = 'Power (W)'
    
    for ij in filelist:
        pows[fcount] = getPowFromInt(ij, irtype = gAv, qScale = 0)
        zData[fcount] = getZData(ij)
        fcount += 1

    plt.semilogy(zData, pows, label=plotLab)
    plt.xlabel('z (m)')
    plt.ylabel(axLab)

    opname = basename + "-unfiltered-power.png"

    plt.savefig(opname)

    outfilename = 'powers.h5'
    h5o = tables.open_file(outfilename,'w')
    h5o.create_array('/','power_SI',pows)
    h5o.create_array('/','z_SI',zData)
    h5o.close()


if __name__ == '__main__':
    basename = sys.argv[1]
    plotPowVsZ(basename)
    