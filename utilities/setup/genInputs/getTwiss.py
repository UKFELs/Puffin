# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This gives the matched Twiss parameters for a periodic FODO lattice with
drifts and undulators. This currently assumes a very simple lattice, of
focussing quad (in x, defocusing in y), drift, undulator, drift, defocusing
quad (in x), drift, undulator, drift, focusing quad (in x). The first and
last quads are half strength.

Hopefully the below can be easily extended to any general periodic lattice.
"""

import sys, glob, os
import numpy as np
from numpy import pi
from numpy import arange
import tables
from puffDataClass import puffData
from undulator import undulator



def getMatchTwiss(fullMat):

    C = fullMat[0][0]
    S = fullMat[0][1]
    CP = fullMat[1][0];
    SP = fullMat[1][1];

    if (C-SP>0):
        opt = 1.
        beta = 4. * S**2. / (4. - (C + SP)**2.)
        beta = np.sqrt(beta)
        alpha = (C-SP) * beta / 2 / S
    else:
        opt = 2.
        beta = -S*SP / C / CP
        beta = np.sqrt(beta)
        alpha = 0.

    return beta, alpha



# Puffin Python data class, holds scaling parameters and physical constants

puffVars = puffData()

# Initialize CLARA base parameters

puffVars.aw = 0.8745*np.sqrt(2.)   # The PEAK!!!
puffVars.gamma0 = 489.237
puffVars.lw = 0.025
puffVars.rho = 0.005
puffVars.undtype = 'planepole'
puffVars.ux = 0.
puffVars.uy = 1.

# Generate the rest of the Puffin scaling from the above

puffVars.genParams()  # generate rest of scaled params


undmod = undulator(puffVars, undtype = 'planepole', Nw = 26)




# Matrix representation of undulators in x and y

mx, my = undmod.getMatrix(puffVars)


# Drift length between undulators

DL = 24. * puffVars.lw # Drift lengths

# Matrix representation of drift section between quad and undulator
# (so of length 0.5 of DL)

dr = [[1.,DL/2.],[0,1.]]


# Focusing factor of quads, given by F = (rho B) / (g L), where (rho B)
# is the magnetic rigidity, g is the gradient of the quad field (i.e. dBy/dx)
# and L is the length of the quad. (thin quad approximation)

f = 3.22 * puffVars.lg



#######  Get full matrix representation of periodic FODO element for x

# Matrices for quads of strength f in x direction

Q1 = [[1., 0.], [-1./(2.*f), 1.]]  # focusing, half strength
Q2 = [[1., 0.], [1./f, 1.]]        # defocusing, full strength
Q3 = Q1                            # focusing, half strength

fullMatx = np.matmul(Q1, dr)
fullMatx = np.matmul(fullMatx, mx)
fullMatx = np.matmul(fullMatx, dr)
fullMatx = np.matmul(fullMatx, Q2)
fullMatx = np.matmul(fullMatx, dr)
fullMatx = np.matmul(fullMatx, mx)
fullMatx = np.matmul(fullMatx, dr)
fullMatx = np.matmul(fullMatx, Q3)





#######  Get full matrix representation of periodic FODO element for y

# Matrices for quads of strength f in y direction

Q1 = [[1., 0.], [1./(2.*f), 1.]]  # defocusing, half strength
Q2 = [[1., 0.], [-1./f, 1.]]      # focusing, full strength
Q3 = Q1                           # defocusing, half strength

fullMaty = np.matmul(Q1, dr)
fullMaty = np.matmul(fullMaty, my)
fullMaty = np.matmul(fullMaty, dr)
fullMaty = np.matmul(fullMaty, Q2)
fullMaty = np.matmul(fullMaty, dr)
fullMaty = np.matmul(fullMaty, my)
fullMaty = np.matmul(fullMaty, dr)
fullMaty = np.matmul(fullMaty, Q3)



betax, alphax = getMatchTwiss(fullMatx)
betay, alphay = getMatchTwiss(fullMaty)




emitxn = 1.022e-9
sigx = np.sqrt(betax * emitxn)
emityn = 1.022e-9
sigy = np.sqrt(betay * emityn)


print 'sigx = ', sigx
print 'betax = ', betax
print 'alphax = ', alphax

print 'sigy = ', sigy
print 'betay = ', betay
print 'alphay = ', alphay

    

#print fullMaty
# C = fullMatx[0][0]
# S = fullMatx[0][1]
# CP = fullMatx[1][0];
# SP = fullMatx[1][1];
# 
# 
# if (C-SP>0):
#     opt = 1.
#     betax = 4. * S**2. / (4. - (C + SP)**2.)
#     betax = np.sqrt(betax)
#     alphax = (C-SP) * betax / 2 / S
# else:
#     opt = 2.
#     betax = -S*SP / C / CP
#     betax = np.sqrt(betax)
#     alphax = 0.
# 
# emitxn = 1.022e-9
# sigx = np.sqrt(betax * emitxn)
# print sigx, alphax




