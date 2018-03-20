# -*- coding: utf-8 -*-
"""
Created on Tue Oct 13 17:03:14 2015

@author: piotrt
"""
# Import necessary libraries
import tables
import numpy as np
import gc
import sys
import SUF  # SU Format
import getTwiss
import matchTwiss
#import tables
from puffDataClass import puffData
from undulator import undulator

def SU2Matched(fnamein):

#    Store basename of file

    file_name_base  = (fnamein.split('.')[0]).strip()

    puffVars = puffData()

# Initialize CLARA base parameters

    puffVars.aw = 0.8745*np.sqrt(2.)   # The PEAK!!!
    puffVars.gamma0 = 489.237
    puffVars.lw = 0.025
    puffVars.rho = 0.005
    puffVars.undtype = 'planepole'
    puffVars.ux = 0.
    puffVars.uy = 1.

    emitx = 1.022e-9
    emity = 1.022e-9

# Generate the rest of the Puffin scaling from the above

    puffVars.genParams()  # generate rest of scaled params

    undmod = undulator(puffVars, undtype = 'planepole', Nw = 26)

    qf = 3.22 * puffVars.lg

    DL = 24. * puffVars.lw # Drift lengths

    twx, twy = getTwiss.getFODOTwiss(puffVars, undmod, qf, DL, emitx, emity)

    TX1 = [131.98, 69.22] # [300., 157.]  (betax, alphax) of beam to be transformed
    TY1 = [6.06, -5.93] # [9., -10.]  (betay, alphay) of beam to be transformed

    twxr = twx[1:]
    twyr = twy[1:]

    print 'desired tw x = ', twxr
    print 'desired tw y = ', twyr

#    Get beam distribution and match to given parameters



    x, px, y, py, z, pz, wghts = SUF.readSUF(fnamein)


    x2, px2, y2, py2 = matchTwiss.matchTwiss(x, px/pz, y, py/pz, TX1, twxr, TY1, twyr)

    px2 = px2 * pz
    py2 = py2 * pz

    MPs=np.vstack((x2, px2, y2, py2, z, pz, wghts)).T

    output_file=tables.open_file(file_name_base+'_matched.h5','w')


    ParticleGroup=output_file.create_array('/','Particles', MPs)
    boundsGroup=output_file.create_group('/','globalGridGlobalLimits','')
    boundsGroup._v_attrs.vsType='limits'
    boundsGroup._v_attrs.vsKind='Cartesian'
    timeGroup=output_file.create_group('/','time','time')
    timeGroup._v_attrs.vsType='time'
    ParticleGroup._v_attrs.vsType='variableWithMesh'
    ParticleGroup._v_attrs.vsTimeGroup='time'
    ParticleGroup._v_attrs.vsNumSpatialDims = 3
    ParticleGroup._v_attrs.vsLimits='globalGridGlobalLimits'
    ParticleGroup._v_attrs.vsLabels='x,px,y,py,z,pz,NE'
    output_file.close()



# Close the file
    output_file.close()


if __name__ == '__main__':

    if len(sys.argv)==2:
        fname = sys.argv[1]
        print 'Processing file:', fname
        SU2Matched(fname)
    else:
        print 'Usage: SU2Puffin <FileName> \n'
        sys.exit(1)
        
        

