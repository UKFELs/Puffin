# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This is an examplar script to produce a plot of the filtered energy.
"""

import sys, glob, os
import numpy as np
import tables
from numpy import arange
import readField
import filterField
from puffdata import fdata
from puffdata import puffData


def getFilteredFields(h5fname, cfr=None, dfr=None, qAv = 0, qScale = None):

    mdata = fdata(h5fname)

    if (qScale==None):
        qScale = mdata.vars.qscale

    lenz2 = (mdata.vars.nz2-1) * mdata.vars.dz2
    z2axis = (np.arange(0, mdata.vars.nz2)) * mdata.vars.dz2
    
    xaxis = (np.arange(0, mdata.vars.nx)) * mdata.vars.dxbar
    yaxis = (np.arange(0, mdata.vars.ny)) * mdata.vars.dybar

    xf, yf = readField.readField(h5fname)

    if ((cfr != None) and (dfr != None)):

        xf = filterField.filterField(xf, cfr, dfr, mdata.vars)
        yf = filterField.filterField(yf, cfr, dfr, mdata.vars)


    return xf, yf


def writeFiltFields(fname, oname):

    h5f = tables.open_file(fname,mode='r') # open full field dump file

    dx = h5f.root.runInfo._v_attrs.sLengthOfElmX
    dy = h5f.root.runInfo._v_attrs.sLengthOfElmY
    dz2 = h5f.root.runInfo._v_attrs.sLengthOfElmZ2

    nx = h5f.root.runInfo._v_attrs.nX
    ny = h5f.root.runInfo._v_attrs.nY
    nz2 = h5f.root.runInfo._v_attrs.nZ2

    xf, yf = getFilteredFields(fname, 2, 0.4, qAv = 0, qScale = None)



    tfield = np.zeros([nx,ny,nz2,2])
    tfield[:,:,:,0] = xf
    tfield[:,:,:,1] = yf

#    xnr = xno+1:nx-xno+1
#    ynr = yno+1:ny-yno+1


    print np.shape(tfield)
# numpy.shape(redField)

    h5o = tables.open_file(oname,'w') # open output file

# Write reduced field to file
    h5o.create_array('/','aperp', tfield)


#    if (ninner != 1):
# Copy data attributes
    h5f.root.aperp._v_attrs._f_copy(h5o.root.aperp)

# Copy runInfo from one of the files to the new reduced file
    h5f.root.globalLimits._f_copy(h5o.root)
    h5f.root.meshScaled._f_copy(h5o.root)
    h5f.root.intensityScaled._f_copy(h5o.root)

    
    h5f.root.runInfo._f_copy(h5o.root)
    h5f.root.time._f_copy(h5o.root)
# Modify vs attributes to new array shape (for VisIt)



#    h5o.root.meshScaled._v_attrs.vsLowerBounds = np.array((lowLimX, lowLimY, 0))
#    h5o.root.meshScaled._v_attrs.vsUpperBounds = np.array((uppLimX, uppLimY, dz2 * (nz2 - 1.)))
#
#    h5o.root.meshScaled._v_attrs.vsNumCells = np.array((newnx, newny, nz2))
#    
#    h5o.root.aperp._v_attrs.vsIndexOrder = 'compMinorC'
#    h5o.root.meshScaled._v_attrs.vsIndexOrder = 'compMinorC'
#    h5o.root.aperp._v_attrs.vsCentering = 'zonal'

# close files...

    h5f.close()
    h5o.close()    
#    h5o.root.meshScaled._v_attrs.vsUpperBounds = numpy.array((uppLimX, uppLimY, 0))





#h5o.create_group('/','runInfo','')
#h5o.root.runInfo = h5f.root.runInfo

# h5o.create_group('/','gridZ_SI','')
# numCells=numpy.array((numpy.int(numSpatialPoints)-1,numpy.int(numTimes)-1))
# h5o.root.gridZ_SI._v_attrs.vsLowerBounds=numpy.array((numpy.double(minS),numpy.double(minZT)))
# h5o.root.gridZ_SI._v_attrs.vsStartCell=numpy.array((numpy.int(0),numpy.int(0)))
# h5o.root.gridZ_SI._v_attrs.vsUpperBounds=numpy.array((numpy.double(maxS),numpy.double(maxZT)))
# h5o.root.gridZ_SI._v_attrs.vsNumCells=numpy.array(numCells)
# h5o.root.gridZ_SI._v_attrs.vsKind="uniform"
# h5o.root.gridZ_SI._v_attrs.vsType="mesh"
# h5o.root.gridZ_SI._v_attrs.vsCentering="nodal"
# h5o.root.gridZ_SI._v_attrs.vsAxisLabels="ct-z,z"





# print(h5o.root.runInfo._v_attrs)












if __name__ == '__main__':
    
    h5finame=sys.argv[1]
    if len(sys.argv) > 2:
      h5foname=sys.argv[2]
      print "Output file specified as: " + sys.argv[2]
    else:
      filenamepieces=h5finame.split('_')
      dumpnoAndExt = filenamepieces[-1]
      bname = filenamepieces[0:-1]
      bname = '_'.join(bname)
      h5foname = bname + '_2nd_' + dumpnoAndExt
      print "No output file specified - will be written to: " + h5foname
      
    writeFiltFields(h5finame, h5foname)





