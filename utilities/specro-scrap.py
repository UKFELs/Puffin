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

h5f = tables.openFile("test1_aperp_small_14790.h5", mode='r')

xf = h5f.root.aperp[0,:]
xfs = xf[65000:85000]

sampleFreq = 1.0 / (4.0*np.pi*0.005 / 19.0)


specP, freqs, time, image = specgram(xfs, NFFT=200, Fs=sampleFreq, noverlap=100, cmap=plt.cm.gist_heat)

# then either:
# plt.imshow(specP,cmap='PRGn')
# plt.show()

# -or- just

plt.show()




h5f.close()

# see here for above - http://matplotlib.org/examples/pylab_examples/specgram_demo.html

# see here for var explanation: http://matplotlib.org/api/pyplot_api.html#matplotlib.pyplot.specgram

# see also: https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.spectrogram.html


