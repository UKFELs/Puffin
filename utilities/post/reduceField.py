"""
@reduceField.py  Exemplar script to create a reduced field mesh file
from the full dump. This example just forms a 1D field from the
central transverse node in an example dump. It should be easy to
extend this to something more sophisticated.

"""

import tables, numpy


fname = "test1_aperp_14790.h5" # filename
oname = "test1_aperp_small_14790.h5" # output filename

h5f = tables.openFile(fname,mode='r') # open full field dump file


# Field layout should be [2,nz2,ny,nx]
redField = h5f.root.aperp[0:2, :, 60, 60]  # slice out our reduced field 
                                           # from the centre of the transverse
                                           # plane


# numpy.shape(redField)



h5o = tables.open_file(oname,'w') # open output file



# Write reduced field to file
h5o.create_array('/','aperp',redField)


# Copy runInfo from one of the files to the new aggregate file

h5f.root.runInfo._f_copy(h5o.root)
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


# close files...

h5f.close()
h5o.close()
