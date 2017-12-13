# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

import sys
import numpy as np
from numpy import pi
from numpy import arange
import matplotlib.pyplot as plt
from matplotlib.pyplot import specgram
import tables
from puffdata import fdata
from puffdata import puffData



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
        xn = np.round(mdata.vars.nx / 2)
    if (yn == None):
        yn = np.round(mdata.vars.ny / 2)


    if (mdata.vars.q1d == 1):
        if (h5f.root.aperp._v_attrs.vsIndexOrder == "compMinorC"):
            xf = h5f.root.aperp[:, 0]
            xf = xf.T
        else:
            xf = h5f.root.aperp[0, :]
    else:
        if (f1D == 0):
            if (h5f.root.aperp._v_attrs.vsIndexOrder == "compMinorC"):
                xf = h5f.root.aperp[:, :, :, 0]
            else:
                xf = h5f.root.aperp[0, :, :, :]
        else:
            if (h5f.root.aperp._v_attrs.vsIndexOrder == "compMinorC"):
                xf = h5f.root.aperp[xn, yn, :, 0]
            else:
                xf = h5f.root.aperp[0, :, yn, xn]
    # xfs = xf[z2si:z2ei]   # for selecting slice...

    if (mdata.vars.q1d == 1):
        if (h5f.root.aperp._v_attrs.vsIndexOrder == "compMinorC"):
            yf = h5f.root.aperp[:,1]
            yf = hf.T
        else:
            yf = h5f.root.aperp[0, :]
    else:
        if (f1D == 0):
            if (h5f.root.aperp._v_attrs.vsIndexOrder == "compMinorC"):
                yf = h5f.root.aperp[:, :, :, 1]
            else:
                yf = h5f.root.aperp[1, :, :, :]
        else:
            if (h5f.root.aperp._v_attrs.vsIndexOrder == "compMinorC"):
                yf = h5f.root.aperp[xn, yn, :, 1]
            else:
                yf = h5f.root.aperp[1, :, yn, xn]

    h5f.close()

    return xf, yf

