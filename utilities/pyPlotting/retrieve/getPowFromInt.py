# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is returns the power from the field mesh files. It can return the 
'instantaneous' temporal power (which is just the square of the fields) with 
irtype = 0, the cycle averaged power (which is the usual definition of power!)
with irtype = 3, the power averaged over the whole temporal domain of the field 
mesh with irtype = 1, and the peak power with irtype = 2.

You can also specify a band pass filter with cfr and dfr, where cfr is the
center frequency and dfr is the half width of the filter. Units are scaled to
the reference frequency - so the fundamental is usually at cfr = 1.
"""

import sys, glob, os
import numpy as np
from numpy import arange
from .rawpuffin import getIntData
from .process import filterField
from . import getMagPhase
from puffdata import fdata
from puffdata import puffData

itemp = 0
iav = 1
ipeak = 2
icycav = 3

def getPowFromInt(h5fname, cfr=None, dfr=None, irtype = 0, qScale = None):

    mdata = fdata(h5fname)

    if (qScale==None):
        qScale = mdata.vars.qscale

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0, mdata.vars.nz2)) * mdata.vars.dz2
    
    xaxis = (np.arange(0, mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0, mdata.vars.ny)) * mdata.vars.dybar

    power = getIntData(h5fname, 'powerSI', irtype = irtype)

    return power