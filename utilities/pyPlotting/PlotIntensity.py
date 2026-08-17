###############################

import sys, glob, os
import numpy as np
from numpy import arange
import matplotlib.pyplot as plt
import tables
import math
from math import pi


import scipy
import matplotlib
from scipy import special
from matplotlib import pyplot as plt
from matplotlib import animation
from numpy.lib.scimath import *
from cmath import sin, cos
from scipy import *

from puffdata import fdata
import h5py

iTemporal = 0
iPeriodic = 1


##################################################################
#
##


filename = sys.argv[1]
mdata=fdata(filename)
hf=h5py.File(filename, 'r')
n1=hf.get('aperp')
n1=np.array(n1)
n2=np.zeros((n1.shape[3],n1.shape[2],n1.shape[1],n1.shape[0]))
for i in range(n1.shape[0]):
    for j in range(n1.shape[2]):
        n2[:,j,:,i]=n1[i,:,j,:].T
        
xpol=n1[0,:,:,:]  
ypol=n1[1,:,:,:]

#xpol=n2[:,:,:,0]  
#ypol=n2[:,:,:,1]

z2axis = (np.arange(0,mdata.vars.nz2)) * mdata.vars.dz2


intensity2=np.trapz(xpol**2+ypol**2, x=z2axis, axis=0)/(mdata.vars.nz2*mdata.vars.dz2)
intensity=intensity2.T
#intensity=np.trapz(xpol**2+ypol**2, x=z2axis, axis=2)/(mdata.vars.nz2*mdata.vars.dz2)


#in2=np.zeros((intensity.shape[1],intensity.shape[0]))

#for j in range(intensity.shape[0]):
#   for k in range(intensity.shape[1]):
#       in2[k,j]=intensity[j,k]

dx=mdata.vars.dx
dy=mdata.vars.dy

nx=mdata.vars.nx
ny=mdata.vars.ny

x=(np.arange(-nx/2,nx/2)*dx*1e3)
y=( np.arange(-ny/2,ny/2)*dy *1e3)

X,Y=meshgrid(x,y)


font = matplotlib.font_manager.FontProperties(family='serif')
plt.clf()
plt.figure(1)
ax2=plt.subplot(111)
import matplotlib.ticker as mtick
ax2.set_ylabel(r'y [$mm$]',fontsize=12)
ax2.yaxis.set_major_formatter(mtick.ScalarFormatter(useMathText=True))
ax2.ticklabel_format(style='sci', axis='y', scilimits=(0,0))
ax2.set_xlabel(r'x [$mm$]',fontsize=12)
ax2.xaxis.set_major_formatter(mtick.ScalarFormatter(useMathText=True))
ax2.ticklabel_format(style='sci', axis='x', scilimits=(0,0))

plt.imshow(intensity*mdata.vars.intensScale, cmap='jet', interpolation='bilinear', extent=[-nx*dx*1e3/2.,nx*dx*1e3/2.,-ny*dy*1e3/2.,ny*dy*1e3/2.])
#plt.xlabel('x ($mm$)', fontproperties=font)
#plt.ylabel('y ($mm$)', fontproperties=font)
#==============================================================================
# ax= plt.axes()
# for label in ax.get_xticklabels():
#     label.set_fontproperties(font)
# for label in ax.get_yticklabels():
#     label.set_fontproperties(font)
#==============================================================================
#cbar=plt.colorbar()
import matplotlib.ticker as ticker

def fmt(x, pos):
    a, b = '{:.1e}'.format(x).split('e')
    b = int(b)
    #return r'${} {}$'.format(a, b)
    return r'${} \times 10^{{{}}}$'.format(a, b)
cbar=plt.colorbar(format=ticker.FuncFormatter(fmt),label=r'Intensity [$W/m^2$]')    
#cbar=plt.colorbar(format=ticker.ScalarFormatter(useMathText=True),label='Intensity $W/m^2$')
#cbar.set_label(family=font)
for label in cbar.ax.yaxis.get_ticklabels():
    label.set_family('serif')
plt.savefig("Intensity"+filename+".png")
plt.show()
