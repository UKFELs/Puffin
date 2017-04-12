# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is an examplar script to produce a plot of the instantaneous power
in 1D (or the intensity - just the field squared)
"""

import sys
import tables
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.pyplot import specgram



#t = np.linspace(-1, 1, 200, endpoint=False)

#sig  = np.cos(2 * np.pi * 7 * t) + \
#     np.real(np.exp(-7*(t-0.4)**2)*np.exp(1j*2*np.pi*2*(t-0.4)))



# you want the NFFT to be smaller than the total signal size, 
# it is the length of the local FFT.

#specP, freqs, time, image = specgram(sig, NFFT=25, Fs=200, noverlap=10)

#plt.imshow(specP)
#plt.show()



##################################################################
#
##

def plotPow(h5fname):

    h5f = tables.open_file(h5fname, mode='r')

    dz2 = h5f.root.runInfo._v_attrs.sLengthOfElmZ2
    nz2 = h5f.root.runInfo._v_attrs.nZ2

    sampleFreq = 1.0 / dz2

#   To select temporal slice of field....
    
    #z2s = 50
    #z2e = 80

    #z2si = int(np.floor(z2s / dz2))
    #z2ei = int(np.floor(z2e / dz2))

    #z2axis = (np.arange(z2si,z2ei) - z2si) * dz2


#    ...otherwise take full field

    z2axis = (np.arange(0,nz2)) * dz2


    xf = h5f.root.aperp[:,0]
    # xfs = xf[z2si:z2ei]   # for selecting slice...

    yf = h5f.root.aperp[:,1]

    intens = np.square(xf) + np.square(yf)


    ax1 = plt.subplot(211)
    plt.plot(z2axis, xf)
    plt.plot(z2axis, yf)

# example for adding subplot
    plt.subplot(212, sharex=ax1)
    plt.plot(z2axis, intens)

    plt.savefig("ExEy-Power.png")
    plt.show()


#    plt.show(block=False)
    h5f.close()


if __name__ == '__main__':
    h5fname=sys.argv[1]
    plotPow(h5fname)


