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
from puffDataClass import puffData

class undulator:
    def __init__(self, scale, undtype = None, Nw = None, ux = None, uy = None, \
                 tuning = None, kbxSF = None, kbySF = None):
        
        if (undtype == None):
            print 'please specify an undulator type'
        elif (undtype == 'helical'):
            self.undtype = 'helical'
            self.ux = 1.
            self.uy = 1.
            self.kbxn = scale.aw / 2. / np.sqrt(2) / scale.rho / scale.gamma0 # 'natural' focusing wavenumber
            self.kbyn = scale.aw / 2. / np.sqrt(2) / scale.rho / scale.gamma0 # 'natural' focusing wavenumber
        elif (undtype == 'planepole'):
            self.undtype = 'planepole'
            self.ux = 0.
            self.uy = 1.
            self.kbxn = 0.
            self.kbyn = scale.aw / 2. / np.sqrt(2) / scale.rho / scale.gamma0 # 'natural' focusing wavenumber
            #self.kbyn = scale.aw / 4. / scale.rho / scale.gamma0 # 'natural' focusing wavenumber
        elif (undtype == 'curved'):
            self.undtype = 'curved'
            self.ux = 0.
            self.uy = 1.
            kxu = np.sqrt(scale.eta/(8.0 * scale.rho**2))
            kyu = np.sqrt(scale.eta/(8.0 * scale.rho**2))
            self.kbxn = scale.aw / np.sqrt(2.*scale.eta) / scale.gamma0 *kxu
            self.kbyn = scale.aw / np.sqrt(2.*scale.eta) / scale.gamma0 *kyu
        elif (undtype == ''):
            if (ux == None):
                print 'You must enter a valid ux'
            else:
                self.ux = ux

            if (uy == None):
                print 'You must enter a valid uy'
            else:
                self.uy = uy



        if (tuning == None):
            self.alpha = 1.
        else:
            self.alpha = tuning
            self.kbxn = self.kbxn * self.alpha
            self.kbyn = self.kbyn * self.alpha

        if (kbxSF == None):
            self.kbxSF = 0.
        else:
            self.kbxSF = kbxSF

        if (kbySF == None):
            self.kbySF = 0.
        else:
            self.kbySF = kbySF

        if (Nw == None):
            print 'You must specify Nw for this module!!'
        else:
            self.nw = Nw

        self.lw = scale.lw
        self.undlen = self.lw * self.nw
        self.undlensc = self.undlen / scale.lg

    def getMatrix(self, scale):
        """
        Get S.I. transport matrix for undulator
        """
        
        kbx = np.sqrt(self.kbxn**2. + self.kbxSF**2.)   # rms kbeta
        kby = np.sqrt(self.kbyn**2. + self.kbySF**2.)   # rms kbeta

        kbx = kbx / scale.lg
        kby = kby / scale.lg
        
        matx = np.zeros([2,2])

        if (kbx > 0.):
            matx[0][0] = np.cos(kbx*self.undlen)
            matx[0][1] = 1. /kbx * np.sin(kbx*self.undlen)
            matx[1][0] = -kbx * np.sin(kbx*self.undlen)
            matx[1][1] = np.cos(kbx*self.undlen)
        else:
            matx[0][0] = 1.
            matx[0][1] = self.undlen
            matx[1][0] = 0.
            matx[1][1] = 1.

        maty = np.zeros([2,2])

        if (kby > 0.):
            maty[0][0] = np.cos(kby*self.undlen)
            maty[0][1] = 1. /kby * np.sin(kby*self.undlen)
            maty[1][0] = -kby * np.sin(kby*self.undlen)
            maty[1][1] = np.cos(kby*self.undlen)
        else:
            maty[0][0] = 1.
            maty[0][1] = self.undlen
            maty[1][0] = 0.
            maty[1][1] = 1.
        
        return matx, maty


#    def getUndkb(scale, undtype = 'helical', ux = None, uy = None, \
#                 tuning = None, kbxSF = None, kbySF = None):
#
#        if (self.undtype == 'planepole'):
#            self.kbxn = 0.
#            kbyn = scale.aw / 2. / np.sqrt(2) / scale.rho / scale.gamma # 'natural' focusing wavenumber
#
#        elif (self.undtype == 'curved'):
#            kxu = np.sqrt(scale.eta / 8. / scale.rho**2)
#            kyu = np.sqrt(scale.eta / 8. / scale.rho**2)
#            kbxn = scale.aw / np.sqrt(2.*scale.eta) / scale.gamma *kxu
#            kbyn = scale.aw / np.sqrt(2.*scale.eta) / scale.gamma *kyu
#
#        elif (self.undtype == 'helical'):
#            kbxn = scale.aw / 2. / np.sqrt(2) / scale.rho / scale.gamma # 'natural' focusing wavenumber
#            kbyn = scale.aw / 2. / np.sqrt(2) / scale.rho / scale.gamma # 'natural' focusing wavenumber
#
#        else:
#            kbxn = scale.aw / 2. / np.sqrt(2) / scale.rho / scale.gamma # 'natural' focusing wavenumber
#            kbyn = scale.aw / 2. / np.sqrt(2) / scale.rho / scale.gamma # 'natural' focusing wavenumber
#            awrms = scale.aw / np.sqrt(2.) * np.sqrt(ux**2 + uy**2)