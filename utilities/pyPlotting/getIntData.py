# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This returns an integrated data set from the Puffin integrated files. It can return the 
data averaged over the whole temporal domain with irtype = 1, the peak of the data
with irtype = 2, and as a function of the temporal coordinate z2 (or ct-z) with irtype = 0.
"""

import sys, glob, os
import numpy as np
from numpy import arange
import readField
import filterField
import getMagPhase
import tables
from fdataClass import fdata
from puffDataClass import puffData

itemp = 0
iav = 1
ipeak = 2
icycav = 3

def getIntData(h5fname, dataName, irtype = 0):

    mdata = fdata(h5fname)

    h5in=tables.open_file(h5fname,'r')
    npts=h5in.root._f_get_child(dataName).shape[0]
    meshName = h5in.root._f_get_child(dataName)._v_attrs.vsMesh

    mmin=h5in.root._f_get_child(meshName)._v_attrs.vsLowerBounds
    mmax=h5in.root._f_get_child(meshName)._v_attrs.vsUpperBounds

    lenz2 = mmax - mmin
    dz2 = lenz2 / (npts-1)
    z2axis = (np.arange(0, npts)) * dz2

    dataT = h5in.root._f_get_child(dataName).read()

    if (irtype == iav):
        if (npts > 1):
            dataR = np.trapz(dataT, x=z2axis) / lenz2 # average
        else:
            dataR = dataT
    elif (irtype == ipeak):
        dataR = np.max(dataT) # peak
    else:
        dataR = dataT # temporal

    h5in.close()

    return dataR
