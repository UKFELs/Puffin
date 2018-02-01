# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
Given the values of the field on the Puffin mesh, this function applies a simple
bandpass filter to the field as specified by the cfr and dfr inputs (which are the
centre and half-width of the filter, respectively). 
"""

import sys, glob, os
import numpy as np
from numpy import pi
from puffdata import puffData

iTemporal = 0
iPeriodic = 1

def filterField(field,crfr,distfr, pvars):

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#% filter - HARD FILTER

    nn = np.int(np.round(pvars.dz2 * pvars.nz2 * crfr / (4*pi*pvars.rho)))
    nns = np.int(np.floor(pvars.dz2 * pvars.nz2 * distfr / (4*pi*pvars.rho)))
    
    f0 = np.int(0)
    f1 = np.int(nn-nns)
    f2 = np.int(nn+nns+1)
    f3 = np.int(np.ceil(pvars.nz2/2) + 1)
    f4 = np.int(np.ceil(pvars.nz2/2) + 1)
    f5 = np.int(pvars.nz2-(nn+nns))
    f6 = np.int(pvars.nz2 - (nn-nns) + 2 -1)
    f7 = np.int(pvars.nz2)
    
    
#    print str(nn), str(nns)
#    print str(0), str(np.int(nn-nns))
#    print str(nn+nns+1), str(np.ceil(pvars.nz2/2) + 1)
#    print str(np.ceil(pvars.nz2/2) + 1), str(pvars.nz2-(nn+nns))
#    print str(pvars.nz2 - (nn-nns) + 2 -1 ), str(pvars.nz2)

#    print str(f0), str(f1)
#    print str(f2), str(f3)
#    print str(f4), str(f5)
#    print str(f6), str(f7)


#    if ((nn-nns) <= 0):
#        nns = 

    if (pvars.q1d == 1):

#%%%%%    1D    %%%%%%%

        if (pvars.iMesh == iPeriodic):

            ftfield = np.fft.fft(field)
            
            ftfield[f0:f1] = 0
            ftfield[f2:f3] = 0

            ftfield[f4:f5] = 0
            ftfield[f6:f7] = 0


        else:

            ftfield = np.fft.fft(field)

            ftfield[f0:f1] = 0
            ftfield[f2:f3] = 0

            ftfield[f4:f5] = 0
            ftfield[f6:f7] = 0
                  
        field = np.fft.ifft(ftfield)
      
        nfield = np.real(field)

    else:
  
#%%%%%    3D    %%%%%%%

      ftfield = np.fft.fft(field)
    
      if (pvars.iMesh == iPeriodic):

        #sn = 1
        #ftfield[:,:,0:sn] = 0
        #ftfield[:,:,sn+1:np.ceil(pvars.nz2/2)] = 0

        #ftfield[:,:,np.ceil(pvars.nz2/2) + 1 - 1:-sn] = 0

        ftfield[:,:,f0:f1] = 0
        ftfield[:,:,f2:f3] = 0

        ftfield[:,:,f4:f5] = 0
        ftfield[:,:,f6:f7] = 0

      else:
        ftfield[:,:,f0:f1] = 0
        ftfield[:,:,f2:f3] = 0

        ftfield[:,:,f4:f5] = 0
        ftfield[:,:,f6:f7] = 0
    
      field = np.fft.ifft(ftfield)
    
      nfield = np.real(field)

    return nfield
