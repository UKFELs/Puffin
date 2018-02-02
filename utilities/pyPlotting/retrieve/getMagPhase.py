# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

import sys
import numpy as np
from numpy import pi
from numpy import arange
import matplotlib.pyplot as plt
from matplotlib.pyplot import specgram
import tables


def getMagPhase(Ex,nZ2,rho,syslen):

    wavel=4*pi*rho

    ax=1
    thix=-1

    xaxis = np.linspace(0,syslen,nZ2)  # xaxis for real space
# %Ex = ax*cos(xaxis/(2*rho)+thix); %Define x field

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# MAGNITUDE:
# 2*df is the number of nodes spanning a resonant wavelength
# de is the number of nodes to the left to average from
# dg is the number of nodes to the right
# so average around node_index-de:node_index+dg
# first of all, find the number of nodes corresponding to one lamda_r,nnl
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    lr=wavel
    nsp=syslen/(nZ2-1)
    nel=np.round(lr/nsp)
    nnl=nel+1

    df=np.floor((nnl)/2)
    dg=df

    if (np.mod(nnl,2)==1):
        de=df
    else:
        de=df-1
        
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Specify a segment around the current node
# and average around it over a wavelength
# intxrms is rms square root of the mean intensity, or
# the rms absolute field.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    intxrms=np.zeros(nZ2)
    intyrms=np.zeros(nZ2)

    for m in np.arange(nZ2):
        lo=m-de
        hi=lo+(nnl-2) # hi=m+dg
        if lo<1:
            lo=0
        if hi>nZ2:
            hi=nZ2-1
        tar = np.square(Ex[np.int_(lo):np.int_(hi+1)])
        tvar = np.mean(tar)
        intxrms[m]=np.sqrt(tvar)

    magxrms=np.sqrt(2)*intxrms # magxrms is the rms magnitude of the wave

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PHASE:
# First create the "reference" wave, cos(\omega t)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    z2cyclic=xaxis-(4*pi*rho*np.floor(xaxis/(4*pi*rho)))
    z2cyclic=z2cyclic/(2*rho)

    slopy=-np.sin(2*pi*xaxis/wavel)

    invcosx = np.zeros(nZ2)

    for a in np.arange(nZ2):
        if (Ex[a]/magxrms[a] < -1 ) or ((Ex[a]/magxrms[a] > 1 )):
            invcosx[a] = 0
        else:
            invcosx[a]=np.arccos(Ex[a]/magxrms[a])

    for a in np.arange(nZ2):
      if (abs(Ex[a])>=magxrms[a]):
          if (Ex[a]>0):
              invcosx[a]=0
          elif (Ex[a]<0):
              invcosx[a]=pi


    for a in arange(nZ2-1):
        if Ex[a]<Ex[a+1]: # best way to do this may be taking 
             #### gradient of Ex. If negative, then shift
             #### to 3rd and 4th quadrant....
             # invcosx(a)=invcosx(a)*(-1)
            invcosx[a]=(2*np.pi)-invcosx[a]

    phasex=invcosx-z2cyclic

    for a in np.arange(nZ2):
        if phasex[a]<0:
            phasex[a]=2*pi-np.absolute(phasex[a])

    return magxrms, phasex
