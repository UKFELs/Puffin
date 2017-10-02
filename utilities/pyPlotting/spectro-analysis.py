# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is an examplar script to produce a spectro-temporal plot from the Puffin 
field.
"""

import sys
import tables
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.pyplot import specgram
from puffdata import fdata
from puffdata import puffData
from retrieve import readField

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

def spectroT(h5fname, z2s=None, z2e=None):

    mdata = fdata(h5fname)


    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2

    xf, yf = readField(h5fname, f1D=1)



#    dz2 = h5f.root.runInfo._v_attrs.sLengthOfElmZ2
#    nz2 = h5f.root.runInfo._v_attrs.nZ2

    sampleFreq = 1.0 / mdata.vars.dz2

#    z2s = 0.00
#    z2e = 0.06

    
    if ((z2s==None) or (z2e==None)):
        z2si = 0
        z2ei = mdata.vars.nz2
    else:
        z2si = int(np.floor(z2s / mdata.vars.dz2))
        z2ei = int(np.floor(z2e / mdata.vars.dz2))

    xfs = xf[z2si:z2ei]
    yfs = yf[z2si:z2ei]

    xaxis = (np.arange(0, mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0, mdata.vars.ny)) * mdata.vars.dybar
    z2axis = (np.arange(z2si,z2ei) - z2si) * mdata.vars.dz2

    ax1 = plt.subplot(211)
    plt.plot(z2axis, xfs)
    plt.xlabel(r'$\bar{z}_2$')
    plt.ylabel('x-field (scaled)')

    plt.subplot(212, sharex=ax1)
    specP, freqs, time, image = specgram(xfs, \
    	NFFT=50, Fs=sampleFreq, noverlap=0)#, cmap=plt.cm.gist_heat)

    plt.xlabel(r'$\bar{z}_2$')
    plt.ylabel(r'$\bar{f}$')
    
    # then either:
    #plt.imshow(specP,cmap='PRGn')
    #plt.show()

    # -or- just

    nameparts = h5fname.split('_')
    basename = nameparts[0]
    z = mdata.vars.z
    
    plt.savefig(basename + "-spectrogram-z-" + str(z) + ".png")
    plt.show()

# see here for above - http://matplotlib.org/examples/pylab_examples/specgram_demo.html

# see here for var explanation: http://matplotlib.org/api/pyplot_api.html#matplotlib.pyplot.specgram

# see also: https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.spectrogram.html

if __name__ == '__main__':
    h5fname=sys.argv[1]
    spectroT(h5fname)
    



