# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is an examplar script to produce a surf plot of the temporal cycle-averaged
power evolving as a function of distance through the undulator z. That is to say,
th enormalised power is plaotted as a function of both temporal coordinate and
propagation distance.
"""

import sys
import numpy as np
from numpy import pi
from numpy import arange
import matplotlib.pyplot as plt
import tables
from puffdata import fdata
from puffdata import puffData
from retrieve import getPowFromInt
from retrieve import getIntFileSlices
from retrieve import getZData

def plotPowZZ2(basename):


    filelist = getIntFileSlices(basename)
    print(filelist)

    mdata = fdata(filelist[0])

    sampleFreq = 1.0 / mdata.vars.dz2

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0,mdata.vars.nz2)) * mdata.vars.dz2 * mdata.vars.lc * 1.e6
    
    xaxis = (np.arange(0,mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0,mdata.vars.ny)) * mdata.vars.dybar

    fcount = 0
    
    pows = np.zeros(len(filelist))
    zData = np.zeros(len(filelist))

#    if (mdata.vars.iMesh == iPeriodic):
#        gAv = 1  #  for average...
#    else:
#        gAv = 2  #  for peak...

    gAv = 0 # for temporal (no cycle averaging)

    pows = np.ones([len(filelist), mdata.vars.nz2]);
    powsN = np.ones([len(filelist), mdata.vars.nz2]);

    for ij in filelist:
        pows[-1-fcount,:] = getPowFromInt(ij, irtype = gAv, qScale = 0)
        mv = np.max(pows[-1-fcount,:])
        if (mv != 0.):
            powsN[-1-fcount,:] = pows[-1-fcount,:] / np.max(pows[-1-fcount,:])
        else:
            powsN[-1-fcount,:] = 0.
        zData[fcount] = getZData(ij)
        fcount += 1
#        print fcount

#    plotLab = 'SI Power'
#    axLab = 'Power (W)'

#    if (mdata.vars.iMesh == iPeriodic):
#        plotLab = 'SI Power'
#        axLab = 'Power (W)'
#    else:
#        plotLab = 'SI Peak Power'
    axLab = 'Power (W)'


    ax1 = plt.subplot(111)
    im = plt.imshow(powsN, aspect='auto', interpolation='bilinear', \
        extent=[z2axis[0], z2axis[-1], zData[0], zData[-1]])
    ax1.set_title('Power')
    plt.xlabel(r'$ct-z (\mu m)$')
    plt.ylabel('z (m)')

    cb = plt.colorbar(im)

    plt.tight_layout()
    #plt.legend()

    opname = basename + "-powerALL.png"

    plt.savefig(opname)
#    plt.show()


#    plt.show(block=False)
#    h5f.close()


if __name__ == '__main__':

    basename = sys.argv[1]

    plotPowZZ2(basename)
