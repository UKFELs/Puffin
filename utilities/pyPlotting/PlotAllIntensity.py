# -*- coding: utf-8 -*-
"""
Created on Fri Apr 27 10:46:35 2018

@author: ptracz
"""

import tables
import numpy as np
import matplotlib as mpl
mpl.use('Agg')
import sys
import glob
import os
from puffdata import fdata
#import h5py
import matplotlib.ticker as mtick
import matplotlib.ticker as ticker
import matplotlib.pyplot as plt
import warnings
warnings.filterwarnings('ignore')

def getFiles(baseName):
  """ getTimeSlices(baseName) gets a list of files

  That will be used down the line...
  """
  filelist=glob.glob(os.getcwd()+os.sep+baseName+'_aperp_*.h5')
  
  dumpStepNos=[]
  for thisFile in filelist:
    thisDump=int(thisFile.split(os.sep)[-1].split('.')[0].split('_')[-1])
    dumpStepNos.append(thisDump)

  for i in range(len(dumpStepNos)):
    filelist[i]=baseName+'_aperp_'+str(sorted(dumpStepNos)[i])+'.h5'
  return filelist


def isvsh5suffix(filename):
  return filename[-5:]==".vsh5"
def ish5suffix(filename):
  return filename[-3:]==".h5"


if len(sys.argv)!=2:
  print("Usage ReorderColMajorFtoColMinorC.py filename")
  print("Please provide full filename, or a basename without extension to match multiple files")
  exit()

baseName = sys.argv[1]
filelist = getFiles(baseName)





def plot_intensity(file_name_in):
#    iTemporal = 0
#    iPeriodic = 1
    
    
    ##################################################################
    #
    ##
    
    
    filename = file_name_in
    filename_base = (file_name_in.split('.')[0]).strip()
    mdata=fdata(filename)
    # H5PY option below    
    #hf=h5py.File(filename, 'r')
    #n1=hf.get('aperp')
    #n1=np.array(n1)
    hf=tables.open_file(file_name_in,'r')

    n1=hf.root.aperp.read()    
            
    xpol=n1[0,:,:,:]  
    ypol=n1[1,:,:,:]

    
    z2axis = np.multiply((np.arange(0,mdata.vars.nz2)),mdata.vars.dz2)
    
    
    intensity2=np.divide(np.trapz(np.add(np.square(xpol),np.square(ypol)), x=z2axis, axis=0),(np.multiply(mdata.vars.nz2,mdata.vars.dz2)))
   
    intensity=np.transpose(intensity2)    


    
    dx=mdata.vars.dx
    dy=mdata.vars.dy
    
    nx=mdata.vars.nx
    ny=mdata.vars.ny
    
    #x=(np.arange(-nx/2,nx/2)*dx*1e3)
    #y=(np.arange(-ny/2,ny/2)*dy *1e3)
    
    #X,Y=np.meshgrid(x,y)
    
    
#    font = matplotlib.font_manager.FontProperties(family='serif')
    plt.clf()
    plt.figure(1)
    ax2=plt.subplot(111)

    ax2.set_ylabel(r'y [$mm$]',fontsize=12)
    ax2.yaxis.set_major_formatter(mtick.ScalarFormatter(useMathText=True))
    ax2.ticklabel_format(style='sci', axis='y', scilimits=(0,0))
    ax2.set_xlabel(r'x [$mm$]',fontsize=12)
    ax2.xaxis.set_major_formatter(mtick.ScalarFormatter(useMathText=True))
    ax2.ticklabel_format(style='sci', axis='x', scilimits=(0,0))
    
    plt.imshow(np.multiply(intensity,mdata.vars.intensScale), cmap='jet', interpolation='bilinear', extent=[-nx*dx*1e3/2.,nx*dx*1e3/2.,-ny*dy*1e3/2.,ny*dy*1e3/2.])
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
    plt.savefig("Intensity"+filename_base+".png")
    plt.show()


for iname in filelist:
    print(iname)
    plot_intensity(iname)