# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
Module to transform a beam distribution to a set of Twiss parameters.
See the matchTwiss routine at the bottom of the file to do this.
"""

import numpy as np





def getMat(b1, a1, b2, a2):
    """
    Calculate matrix necessary to transform a beam of given Twiss parameters
    to the new Twiss parameters
    """

    f1 = np.sqrt(b1*b2)

    M11 = b2 / f1
    M12 = 0.
    M21 = (a1-a2) / f1
    M22 = b1 / f1

    mat = np.array([[M11,M12],[M21,M22]])

    return mat


def MM(x, px, M):
    """
    Transform beam distribution x-px with matrix MM.
    """

    ap = np.array([x, px])
    print 'SHAPE = ', ap.shape
    apn = np.matmul(M, ap)
    xn = apn[0,:]
    pxn = apn[1,:]

    return xn, pxn


    


def matchT1(x, px, b1, a1, b2, a2):
    """
    Transform beam distribution from the given Twiss parameters (b1, a1)
    to the new Twiss parameters (b2, a2).
    """

    M = getMat(b1, a1, b2, a2)
    print 'MAT = ', M
#    print 'MAT = ', M
    x2, px2 = MM(x, px, M)

    return x2, px2




def matchTwiss(x, px, y, py, TX1, TX2, TY1, TY2):
    """
    Transform x and y phase spaces from Twiss parameters in T0 to T1.
    """

    bx1 = TX1[0]
    ax1 = TX1[1]

    bx2 = TX2[0]
    ax2 = TX2[1]

    by1 = TY1[0]
    ay1 = TY1[1]

    by2 = TY2[0]
    ay2 = TY2[1]

    x2, px2 = matchT1(x, px, bx1, ax1, bx2, ax2)
    y2, py2 = matchT1(y, py, by1, ay1, by2, ay2)

    return x2, px2, y2, py2
