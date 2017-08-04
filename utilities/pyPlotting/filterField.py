# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is an examplar script to produce a plot of the filtered energy.
"""

import sys, glob, os
import numpy as np
from numpy import pi
from puffDataClass import puffData

iTemporal = 0
iPeriodic = 1

def filterField(field,crfr,distfr, pvars):

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#% filter - HARD FILTER

    nn = np.int(np.round(pvars.dz2 * pvars.nz2 * crfr / (4*pi*pvars.rho)))
    nns = np.int(np.floor(pvars.dz2 * pvars.nz2 * distfr / (4*pi*pvars.rho)))
    #print str(nn), str(nns)
    #print str(0), str(np.int(nn-nns))
    #print str(nn+nns+1), str(np.ceil(pvars.nz2/2))
    #print str(np.ceil(pvars.nz2/2) + 1 - 1), str(pvars.nz2-(nn+nns)+2)
    #print str(pvars.nz2 - (nn-nns) + 2 -1 ), str(pvars.nz2)


#    if ((nn-nns) <= 0):
#        nns = 

    if (pvars.q1d == 1):

#%%%%%    1D    %%%%%%%

        if (pvars.iMesh == iPeriodic):
            
            sn = 1
            ftfield[0:sn] = 0
            ftfield[sn+1:np.ceil(pvars.nz2/2)] = 0

            ftfield[np.ceil(pvars.nz2/2) + 1 - 1:-sn] = 0


        else:

            ftfield = np.fft.fft(field)

            ftfield[0:np.int(nn-nns)] = 0
            ftfield[np.int(nn+nns-1):np.int(np.ceil(pvars.nz2/2))] = 0
      
            ftfield[np.int(np.ceil(pvars.nz2/2) + 1 - 1):np.int(pvars.nz2-(nn+nns)+2)] = 0
            ftfield[np.int(pvars.nz2 - (nn-nns) + 2 - 1 ) : np.int(pvars.nz2)] = 0
      
        field = np.fft.ifft(ftfield)
      
        nfield = np.real(field)

    else:
  
#%%%%%    3D    %%%%%%%

      ftfield = np.fft.fft(field)
    
      if (pvars.iMesh == iPeriodic):

        sn = 1
        ftfield[:,:,0:sn] = 0
        ftfield[:,:,sn+1:np.ceil(pvars.nz2/2)] = 0

        ftfield[:,:,np.ceil(pvars.nz2/2) + 1 - 1:-sn] = 0

      else:
        ftfield[:,:,0:(nn-nns)] = 0
        ftfield[:,:,(nn+nns-1):np.ceil(pvars.nz2/2)] = 0

        ftfield[:,:,np.ceil(pvars.nz2/2) + 1 - 1:pvars.nz2-(nn+nns)+2] = 0
        ftfield[:,:,(pvars.nz2 - (nn-nns) + 2 -1 ) : pvars.nz2] = 0
    
      field = np.fft.ifft(ftfield)
    
      nfield = np.real(field)

    return nfield
