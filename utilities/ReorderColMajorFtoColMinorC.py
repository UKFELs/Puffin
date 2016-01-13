#!/bin/python
#************* THIS HEADER MUST NOT BE REMOVED *******************
#* Copyright (c) 2013-2016, Lawrence Campbell and Brian McNeil. **
#** This program must not be copied, distributed or altered in  **
#** any way without the prior permission of the above authors.  **
#*****************************************************************

"""
@ReorderColMajorFtoColMinorC.py Switch between fortran and C ordering of columns

A script for conversion of VizSchema in CompMajorF (Column Major) to CompMajorC (Column Minor)
"""
import numpy,tables,sys

def isvsh5suffix(filename):
  return filename[-5:]==".vsh5"
def ish5suffix(filename):
  return filename[-3:]==".h5"


if len(sys.argv)!=2:
  print "Usage ReorderColMajorFtoColMinorC.py filename"
  print "Please provide full filename, or a basename without extension to match multiple files"
  exit()
filename = sys.argv[1]


# Need to handle a both cases where filename includes a _dumpstep.h5 and where it is a plain filename
# if there are multiple matches,
print "Reorder rows and columns attempting to match against: "+filename

if isvsh5suffix(filename):
  filenamepieces=filename.split('_')
  print len(filenamepieces)
  if len(filenamepieces)==4:
    (simbase,dataset,rank,dumpnoAndExt)=filenamepieces
  elif len(filenamepieces)==3:
    (simbase,dataset,dumpnoAndExt)=filenamepieces
#  outfilename = filename[:-5]+"C.vsh5"
  else:
    print "unknown file format - don't know how to tell dumps, rank, dataset from filename"
    exit()
  tdsetname=dataset+"N"
  outfilename = simbase+"_"+dataset+"_C_"+dumpnoAndExt

elif ish5suffix(filename):
  filenamepieces=filename.split('_')
  print len(filenamepieces)
  if len(filenamepieces)==4:
    (simbase,dataset,rank,dumpnoAndExt)=filenamepieces
  elif len(filenamepieces)==3:
    (simbase,dataset,dumpnoAndExt)=filenamepieces
#  outfilename = filename[:-5]+"C.vsh5"
  else:
    print "unknown file format - don't know how to tell dumps, rank, dataset from filename"
    exit()
  tdsetname=dataset+"N"
  outfilename = simbase+"_"+dataset+"_C_"+dumpnoAndExt
else:
  print"need to figure out if we're here, as there will be multiple output files"
print outfilename

tables.copy_file(filename,outfilename)
h5=tables.open_file(outfilename,'r+')
print h5.root.APerp.shape

# While this works it may be better to just read using numpy.asfortranarray() 
# ie fieldin=numpy.asfortranarray(h5.root.APerp.read()) or whatever actually works
fieldin=h5.root.APerp.read()
print "fieldin shape "+str(fieldin.shape)
testfield=numpy.asarray(fieldin, order='C')
print "testfield shape"+str(testfield.shape)
testfield=numpy.asfortranarray(fieldin)
print "testfield shape"+str(testfield.shape)
fieldout=numpy.zeros((fieldin.shape[3],fieldin.shape[2],fieldin.shape[1],fieldin.shape[0]))
print "fieldout shape"+str(fieldout.shape)
for i in range(fieldin.shape[0]):
  for j in range(fieldin.shape[2]):
    fieldout[:,j,:,i]=fieldin[i,:,j,:].T
#.reshape(numpy.array((h5.root.APerp.shape[3],h5.root.APerp.shape[2],h5.root.APerp.shape[1],2)),order='F')
print fieldout.shape
h5.create_array('/','APerpN',fieldout)
h5.copy_node_attrs('/APerp','/APerpN')
h5.remove_node('/APerp')
h5.rename_node('/APerpN','APerp')
h5.root._v_children['APerp']._v_attrs.vsIndexOrder="compMinorC"
h5.root._v_children['meshScaled']._v_attrs.vsIndexOrder="compMinorC"
h5.close()
