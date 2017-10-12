# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is an examplar script to produce a plot of the cycle-averaged magnitude 
and phase of the fields
"""

import sys
import numpy as np
from numpy import pi
from numpy import arange
import matplotlib.pyplot as plt
import tables
from puffdata import fdata
from puffdata import puffData
from retrieve import getPow

# can maybe use argparse for more complex plotting options...



def plotPowVsZ2(h5fname, cfr=None, dfr=None, gav = 3):

    mdata = fdata(h5fname)

    sampleFreq = 1.0 / mdata.vars.dz2

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0,mdata.vars.nz2)) * mdata.vars.dz2
    saxis = z2axis * mdata.vars.lc * 1e6

    xaxis = (np.arange(0,mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0,mdata.vars.ny)) * mdata.vars.dybar

    fcount = 0

    pows = getPow(h5fname, cfr, dfr, irtype = gav, qScale = 0)


    plotLab = 'SI Power'
    axLab = 'Power (W)'


    ax1 = plt.subplot(111)
    
    plt.plot(saxis, pows)
    plt.xlabel(r'$ct-z (\mu m)$')
    plt.ylabel(axLab)
    plt.xlim(200,  210);

    nameparts = h5fname.split('_')
    basename = nameparts[0]

    z = mdata.vars.z

    plt.savefig(basename + "-powvsz2-z-" + str(z) + ".png")



if __name__ == '__main__':
    h5fname=sys.argv[1]

    if len(sys.argv) == 4:
        cfr = float(sys.argv[2])
        dfr = float(sys.argv[3])
    else:
        cfr=None
        dfr=None
    
    plotPowVsZ2(h5fname, cfr=cfr, dfr=dfr)
    
    


# plot(xaxis,magxrms);
# hold on;
# plot(xaxis,phasex,'r');
# hold off;


