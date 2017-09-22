# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

import sys, glob, os
import numpy as np
from numpy import pi
from numpy import arange
import tables
from puffDataClass import puffData


class fdata:
    def __init__(self, fname):
        self.h5fname = fname
        
        h5f = tables.open_file(fname, mode='r')
        self.vars = puffData()        
        
        self.vars.rho = h5f.root.runInfo._v_attrs.rho
        self.vars.gamma0 = h5f.root.runInfo._v_attrs.gamma_r
        self.vars.au = h5f.root.runInfo._v_attrs.aw
        self.vars.lw = h5f.root.runInfo._v_attrs.lambda_w
        self.vars.lr = h5f.root.runInfo._v_attrs.lambda_r
        self.vars.eta = h5f.root.runInfo._v_attrs.eta
        self.vars.kappa = h5f.root.runInfo._v_attrs.kappa
        self.vars.lg = h5f.root.runInfo._v_attrs.Lg
        self.vars.lc = h5f.root.runInfo._v_attrs.Lc
        self.vars.npkbar = h5f.root.runInfo._v_attrs.npk_bar

        self.vars.qscale = h5f.root.runInfo._v_attrs.iScale
        self.vars.iMesh = h5f.root.runInfo._v_attrs.fieldMesh


        self.vars.dxbar = h5f.root.runInfo._v_attrs.sLengthOfElmX
        self.vars.dybar = h5f.root.runInfo._v_attrs.sLengthOfElmY
        self.vars.dz2 = h5f.root.runInfo._v_attrs.sLengthOfElmZ2
        self.vars.nx = h5f.root.runInfo._v_attrs.nX
        self.vars.ny = h5f.root.runInfo._v_attrs.nY
        self.vars.nz2 = h5f.root.runInfo._v_attrs.nZ2

        self.vars.dx = self.vars.dxbar * np.sqrt(self.vars.lg * self.vars.lc)
        self.vars.dy = self.vars.dybar * np.sqrt(self.vars.lg * self.vars.lc)

        self.vars.dzbar = h5f.root.runInfo._v_attrs.sStepSize
        self.vars.zbar = h5f.root.runInfo._v_attrs.zbarTotal
        self.vars.zbarloc = h5f.root.runInfo._v_attrs.zbarLocal
        self.vars.z = h5f.root.runInfo._v_attrs.zTotal
        self.vars.zloc = h5f.root.runInfo._v_attrs.zLocal
        self.vars.transArea = h5f.root.runInfo._v_attrs.transArea
        self.vars.transAreaSI = h5f.root.runInfo._v_attrs.transAreaSI
        
        self.vars.q1d = 0

        if (self.vars.nx==1):
            if (self.vars.ny==1):
              self.vars.q1d = 1
        
        self.vars.powScale = self.vars.lg * self.vars.lc * self.vars.c0 * self.vars.eps0 \
                              * np.square((self.vars.gamma0 * self.vars.me * \
                              np.square(self.vars.c0) ) / (self.vars.qe * \
                              self.vars.kappa * self.vars.lg ))
        
        h5f.close()