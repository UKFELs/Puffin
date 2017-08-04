# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is an examplar script to produce a plot of the filtered energy.
"""

import sys, glob, os
import numpy as np
from numpy import arange
import readField
import filterField
from fdataClass import fdata
from puffDataClass import puffData


def getFiltPow(h5fname, cfr, dfr, qAv = 0, qScale = None):

    mdata = fdata(h5fname)

    if (qScale==None):
        qScale = mdata.vars.qscale

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0, mdata.vars.nz2)) * mdata.vars.dz2
    
    xaxis = (np.arange(0, mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0, mdata.vars.ny)) * mdata.vars.dybar

    xf, yf = readField.readField(h5fname)

    xf = filterField.filterField(xf, cfr, dfr, mdata.vars)
    yf = filterField.filterField(yf, cfr, dfr, mdata.vars)

    intens = np.square(xf) + np.square(yf)
    
    #tintens = getFiltPow(ij, dfr, cfr)
    #ens[fcount] = np.trapz(tintens, x=z2axis)

    if (mdata.vars.q1d==1):

        if (qAv == 1):
            power = np.trapz(intens, x=z2axis) / lenz2  # * transverse area???
        else:
            power = intens  # * transverse area???

    else:
        
        tintensx = np.trapz(intens, x=xaxis, axis=0)
        tintensxy = np.trapz(tintensx, x=yaxis, axis=0)

        if (qAv == 1):
            power = np.trapz(tintensxy, x=z2axis) / lenz2
        else:
            power = tintensxy


    if (qScale == 0):
        power = power * mdata.vars.powScale # * mdata.vars.lc / mdata.vars.c0

    return power