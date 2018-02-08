# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This file is part of the example post-processing tools for Puffin, a 
multi-frequency FEL code absent of the averaging / SVEA approximations. It
contains the object definition of the puffData class, used for holding the
scaling and mesh data about the simulation.
"""

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
        self.undtype = 'helical'
        self.ux = 1.
        self.uy = 1.
        
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

        self.intensScale = self.c0 * self.eps0 * \
                          np.square((self.gamma0 * self.me * np.square(self.c0) ) \
                          / (self.qe * self.kappa * self.lg ))


        self.fieldScale = np.square((self.gamma0 * self.me * np.square(self.c0) ) \
                          / (self.qe * self.kappa * self.lg ))



    def unscale(self):
        self.dx = self.dxbar * np.sqrt(self.lg * self.lc)
        self.dy = self.dybar * np.sqrt(self.lg * self.lc) 
        self.dct = self.dz2 * self.lc # ...etc
        
    def genParams(self):
        
        if ((self.undtype == 'curved') or (self.undtype == 'planepole')):
            awrms = self.aw / np.sqrt(2.)
            self.ux = 0
            self.uy = 1

        elif (self.undtype == 'helical'):
            awrms = self.aw
            self.ux = 1.
            self.uy = 1.
            
        else:  # elliptical polarization...

            awrms = self.aw * np.sqrt(self.ux**2 + self.uy**2) / np.sqrt(2)
        
        sbetazT = np.sqrt( self.gamma0**2. - 1. - awrms**2. ) / self.gamma0
        self.eta = (1. - sbetazT) / sbetazT
        self.lr = self.eta * self.lw
        self.kappa = self.aw / 2. / self.rho / self.gamma0
        self.lg = self.lw / 4. / np.pi / self.rho
        self.lc = self.lr / 4. / np.pi / self.rho
        self.npkbar = self.lg * self.lc**2. * self.eps0 * self.me / self.qe**2. * \
                self.gamma0**3. * self.rho**3. * (4. * \
                self.c0 * 2. * np.pi / self.lw / self.aw  )**2.

        self.powScale = self.lg * self.lc * self.c0 * self.eps0 * \
                          np.square((self.gamma0 * self.me * np.square(self.c0) ) \
                          / (self.qe * self.kappa * self.lg ))

        self.intensScale = self.c0 * self.eps0 * \
                          np.square((self.gamma0 * self.me * np.square(self.c0) ) \
                          / (self.qe * self.kappa * self.lg ))

        self.fieldScale = np.square((self.gamma0 * self.me * np.square(self.c0) ) \
                          / (self.qe * self.kappa * self.lg ))

        self.lrbar = 4. * np.pi * self.rho