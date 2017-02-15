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

def spectroT(h5fname):

    h5f = tables.open_file(h5fname, mode='r')

    dz2 = h5f.root.runInfo._v_attrs.sLengthOfElmZ2
    nz2 = h5f.root.runInfo._v_attrs.nZ2

    sampleFreq = 1.0 / dz2
    
    z2si = 74000
    z2ei = 76000

    z2axis = (np.arange(z2si,z2ei) - z2si) * dz2

    xf = h5f.root.aperp[0,:]
    xfs = xf[z2si:z2ei]

    ax1 = plt.subplot(211)
    plt.plot(z2axis, xfs)


    plt.subplot(212, sharex=ax1)
    specP, freqs, time, image = specgram(xfs, \
    	NFFT=70, Fs=sampleFreq, noverlap=0)#, cmap=plt.cm.gist_heat)

    # then either:
    # plt.imshow(specP,cmap='PRGn')
    # plt.show()

    # -or- just

    plt.show()

    h5f.close()

# see here for above - http://matplotlib.org/examples/pylab_examples/specgram_demo.html

# see here for var explanation: http://matplotlib.org/api/pyplot_api.html#matplotlib.pyplot.specgram

# see also: https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.spectrogram.html

if __name__ == '__main__':
    h5fname=sys.argv[1]
    spectroT(h5fname)
    



