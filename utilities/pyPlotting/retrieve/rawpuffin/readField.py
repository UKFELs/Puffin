# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

import numpy as np
from numpy.core.fromnumeric import shape
import tables
from puffdata import fdata


def readField(h5fname, f1D = 0, xn = None, yn = None):

    mdata = fdata(h5fname)
    h5f = tables.open_file(h5fname, mode='r')

#   To select temporal slice of field....
    
    #z2s = 50
    #z2e = 80

    #z2si = int(np.floor(z2s / dz2))
    #z2ei = int(np.floor(z2e / dz2))

    #z2axis = (np.arange(z2si,z2ei) - z2si) * dz2

#    ...otherwise take full field

    if (xn == None):
        xn = int(np.rint(mdata.vars.nx / 2))
    if (yn == None):
        yn = int(np.rint(mdata.vars.ny / 2))


    if (mdata.vars.q1d == 1):
        xf = h5f.root.aperp[:, 0]
    else:
        if (f1D == 0):
            xf = h5f.root.aperp[:, :, :, 0]
        else:
            print(xn, yn)
            print(shape(h5f.root.aperp))
            xf = h5f.root.aperp[xn, yn, :, 0]
    # xfs = xf[z2si:z2ei]   # for selecting slice...

    if (mdata.vars.q1d == 1):
        yf = h5f.root.aperp[:,1]
    else:
        if (f1D == 0):
            yf = h5f.root.aperp[:, :, :, 1]
        else:
            yf = h5f.root.aperp[xn, yn, :, 1]

    h5f.close()

    return xf, yf

