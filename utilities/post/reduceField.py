# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
@reduceField.py  Exemplar script to create a reduced field mesh file
from the full dump. This example just forms a 1D field from the
central transverse node in an example dump. It should be easy to
extend this to something more sophisticated.
Supply field file to reduce and desired output filename when calling.
"""

import numpy,tables,glob,os,sys


def reduceField(fname, oname):

    h5f = tables.openFile(fname,mode='r') # open full field dump file

    dx = h5f.root.runInfo._v_attrs.sLengthOfElmX
    dy = h5f.root.runInfo._v_attrs.sLengthOfElmY
    dz2 = h5f.root.runInfo._v_attrs.sLengthOfElmZ2

    nx = h5f.root.runInfo._v_attrs.nX
    ny = h5f.root.runInfo._v_attrs.nY
    nz2 = h5f.root.runInfo._v_attrs.nZ2
    
    
    ninner = 1   #  getting inner 40 x 40 nodes

    if (ninner != 1):
        if ((ninner % 2) == (nx % 2)):
            newnx = ninner   # Symmetric for now
            newny = ninner   # Symmetric for now
        else:
            newnx = ninner+1   # Symmetric for now
            newny = ninner+1   # Symmetric for now
        
        xno = ((nx - newnx) / 2)
        yno = ((ny - newny) / 2)
        lowLimX = - dx * (newnx - 1.) / 2.
        lowLimY = - dy * (newny - 1.) / 2.

        uppLimX = dx * (newnx - 1.) / 2.
        uppLimY = dy * (newny - 1.) / 2.
        



#    xnr = xno+1:nx-xno+1
#    ynr = yno+1:ny-yno+1


# Field layout should be [2,nz2,ny,nx]
    if (ninner==1):
        cin = numpy.round(nx / 2)
        redField = h5f.root.aperp[0:2, :, cin, cin]  # slice out our reduced field 
        #redField = numpy.sin(numpy.arange(nz2))
    else:
        redField = h5f.root.aperp[0:2, :, xno+1:nx-xno+1, yno+1:ny-yno+1]  # slice out our reduced field
                                               # from the centre of the transverse
                                               # plane

    print numpy.shape(redField)
# numpy.shape(redField)

    h5o = tables.open_file(oname,'w') # open output file

# Write reduced field to file
    h5o.create_array('/','aperp',redField)


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



    if ninner == 1:
        # h5o.create_group('/','meshScaled','')
        h5o.root.meshScaled._v_attrs.vsLowerBounds = 0
        h5o.root.meshScaled._v_attrs.vsUpperBounds = dz2 * (nz2 - 1.)
        h5o.root.meshScaled._v_attrs.vsNumCells = nz2-1
        h5o.root.meshScaled._v_attrs.vsStartCell = 0
        h5o.root.meshScaled._v_attrs.vsKind="uniform"
        h5o.root.meshScaled._v_attrs.vsType="mesh"
#        h5o.root.meshScaled._v_attrs.vsAxisLabels = 'z2, A'
#        h5o.root.meshScaled._v_attrs.vsCentering = 'nodal'
#        h5o.root.meshScaled._v_attrs.vsStartCell = numpy.array((0))
        
        h5o.root.aperp._v_attrs.vsIndexOrder = 'compMinorC'      
        h5o.root.aperp._v_attrs.vsType = "variable"
        h5o.root.aperp._v_attrs.vsCentering = "nodal"
#        h5o.root.field1._v_attrs.vsStartCell = numpy.array((0))
        #h5o.root.field1._v_attrs.vsNumSpatialDims = 1
        #h5o.root.field1._v_attrs.numSpatialDims = 1
        #h5o.root.field1._v_attrs.vsAxisLabels='z2bar'
        h5o.root.aperp._v_attrs.vsMesh = "meshScaled"

#        h5o.root.globalLimits._v_attrs.vsLowerBounds = numpy.array((0))
#        h5o.root.globalLimits._v_attrs.vsUpperBounds = numpy.array((dz2 * (nz2 - 1.)))
        

    else:
        h5o.root.meshScaled._v_attrs.vsLowerBounds = numpy.array((lowLimX, lowLimY, 0))
        h5o.root.meshScaled._v_attrs.vsUpperBounds = numpy.array((uppLimX, uppLimY, dz2 * (nz2 - 1.)))

        h5o.root.meshScaled._v_attrs.vsNumCells = numpy.array((newnx, newny, nz2))
    
        h5o.root.aperp._v_attrs.vsIndexOrder = 'compMinorC'
        h5o.root.meshScaled._v_attrs.vsIndexOrder = 'compMinorC'
        h5o.root.aperp._v_attrs.vsCentering = 'zonal'

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
      h5foname = bname + '_small_' + dumpnoAndExt
      print "No output file specified - will be written to: " + h5foname
      
    reduceField(h5finame, h5foname)






