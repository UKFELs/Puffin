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
from retrieve import getPowFromInt
from retrieve import getIntFileSlices
from retrieve import getZData

iTemporal = 0
iPeriodic = 1

itemp = 0
iav = 1
ipeak = 2
icycav = 3

##################################################################
#
##





def plotPowVsZ2(fname):


    mdata = fdata(fname)

    sampleFreq = 1.0 / mdata.vars.dz2

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0,mdata.vars.nz2)) * mdata.vars.dz2
    saxis = z2axis * mdata.vars.lc * 1e6
        
    xaxis = (np.arange(0,mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0,mdata.vars.ny)) * mdata.vars.dybar
    
    z = mdata.vars.z


    gAv = iav  #  for average...
    plotLab = 'Power'
    axLab = 'Power (W)'
    
    pows = getPowFromInt(fname, irtype = itemp, qScale = 0)


    ax1 = plt.subplot(111)
    plt.plot(saxis, pows)
    plt.xlabel(r'$ct-z (\mu m)$')
    plt.ylabel(axLab)
    ax1.set_title('z = ' + str(z) + 'm')

    #plt.legend()









    nameparts = fname.split('_')
    basename = nameparts[0]


    #plt.show()

    plt.savefig(basename + "-powvsz2-step-" + str(mdata.vars.step) + "-z-" + \
        str(z) + "m" + ".png")




#    plt.show()


#    plt.show(block=False)
#    h5f.close()


if __name__ == '__main__':

    fname = sys.argv[1]

#    if len(sys.argv) == 4:
#        cfr = float(sys.argv[2])
#        dfr = float(sys.argv[3])
#    else:
#        cfr=None
#        dfr=None

    plotPowVsZ2(fname)