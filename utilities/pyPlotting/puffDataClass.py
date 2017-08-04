# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

import sys, glob, os
import numpy as np
from numpy import pi
from numpy import arange
import tables

class puffData:
    
    c0 = 2.99792458e8
    qe = 1.60217653e-19
    eps0 = 8.854187817e-12
    me = 9.1093826e-31
    h = 6.626e-34

    def __init__(self):
        self.rho = 0.001
        self.gamma0 = 800.
        self.au = 1.
        self.lw = 0.04
        self.lr = 1.0e-7
        self.eta = 1.0e-5
        self.kappa = 1.0
        self.lg = 1.
        self.lc = 1.e-6
        self.npkbar = 1.0e9
        
        self.qscale = 1
        self.iMesh = 1
        self.q1d = 1
        
        self.dxbar = 1.0
        self.dybar = 1.0
        self.dz2 = 1.0
        self.nx = 1
        self.ny = 1
        self.nz2 = 1
        
        self.dx = 1.
        self.dy = 1.

        self.dzbar = 1.0e-3
        self.zbar = 0.
        self.zbarloc = 0.
        self.z = 0.
        self.zloc = 0.
        
        self.powScale = self.lg * self.lc * self.c0 * self.eps0 * \
                          np.square((self.gamma0 * self.me * np.square(self.c0) ) \
                          / (self.qe * self.kappa * self.lg ))


    def unscale(self):
        self.dx = self.dxbar * np.sqrt(self.lg * self.lc) # ...etc