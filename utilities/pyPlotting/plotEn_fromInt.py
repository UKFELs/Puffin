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
from retrieve import getEnFromInt
from retrieve import getIntFileSlices
from retrieve import getZData

iTemporal = 0
iPeriodic = 1

itemp = 0
iav = 1
ipeak = 2
icycav = 3


def plotEn(basename):

    filelist = getIntFileSlices(basename)
    print(filelist)
    axLab = 'Energy (J)'

    ens = np.zeros(len(filelist))
    zData = np.zeros(len(filelist))
    fcount = 0
    
    for ij in filelist:
        ens[fcount] = getEnFromInt(ij, qScale = 0)
        zData[fcount] = getZData(ij)
        fcount += 1


    ax1 = plt.subplot(111)
    plt.semilogy(zData, ens)
    plt.xlabel(r'$z (m)$')
    plt.ylabel(axLab)
    ax1.set_title('Energy')

    opname = basename + "-Energy.png"

    plt.savefig(opname)


if __name__ == '__main__':
    basename = sys.argv[1]
    plotEn(basename)
