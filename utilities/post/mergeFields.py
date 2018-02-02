# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Jonathan Smith (Tech-X UK Ltd) & Lawrence T. Campbell
# License: BSD-3-Clause

"""
@ReorderColMajorFtoColMinorC.py Switch between fortran and C ordering of columns

A script for conversion of VizSchema in CompMajorF (Column Major) to CompMajorC (Column Minor)
"""
import numpy,tables,sys,glob,re,os

def isvsh5suffix(filename):
  return filename[-5:]==".vsh5"
def ish5suffix(filename):
  return filename[-3:]==".h5"


if len(sys.argv)!=2:
  print "Usage ReorderColMajorFtoColMinorC.py filename."
  print "The script will determine the number of ranks, dumps,  etc"
  print "Please provide full filename, or a basename without extension to match multiple files"
  exit()
filename = sys.argv[1]


# Need to handle a both cases where filename includes a _dumpstep.h5 and where it is a plain filename
# if there are multiple matches,
print "Reorder rows and columns attempting to match against: "+filename

if isvsh5suffix(filename):
  filenamepieces=filename.split('_')
  print len(filenamepieces)
  if len(filenamepieces)==6:
    print "We are here"
    (simbase,dataset,fieldpart,dsetcomponent,dsetrank,dumpnoAndExt)=filenamepieces
    print "dataset:"+dataset
  elif len(filenamepieces)==5:
    (simbase,dataset,location,dsetrank,dumpnoAndExt)=filenamepieces
  elif len(filenamepieces)==4:
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
  if len(filenamepieces)==6:
    print "We are here"
    (simbase,dataset,fieldpart,dsetcomponent,dsetrank,dumpnoAndExt)=filenamepieces
    print "dataset:"+dataset
  elif len(filenamepieces)==4:
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

print "Quickly checking size of all files."
fileList=glob.glob(simbase+"_"+dataset+"_*_real_*_"+dumpnoAndExt)
NX=0
NY=0
NZ2=0
lbx=0.
lby=0.
lbz=0.
ubx=0.
uby=0.
ubz=0.
maxRank=0
thisNZ2={}
for fname in fileList:
  t1=tables.open_file(fname,'r')
#  g=re.match(simbase+"_"+"("+dataset+".*)_(\d+)"+dumpnoAndExt,fname)
  g=re.match(simbase+"_"+"("+dataset+".*)_(\d+)_"+dumpnoAndExt,fname)

  if g:
    dsetname=g.groups()[0]
    thisRank=g.groups()[1]
    print "dsetname:"+dsetname+"   rank:"+thisRank    
    maxRank=max(maxRank,int(thisRank))
    thisNZ2[dsetname+"_"+thisRank]=t1.root._v_children[dsetname].read().shape[0]
    meshName=t1.root._v_children[dsetname]._v_attrs.vsMesh
    NZ2+=thisNZ2[dsetname+"_"+thisRank]
    NY=t1.root._v_children[dsetname].read().shape[1]
    NX=t1.root._v_children[dsetname].read().shape[2]
    print NZ2
    print NY
    print NX
    lbx=min(t1.root._v_children[meshName]._v_attrs.vsLowerBounds[0],lbx)
    lby=min(t1.root._v_children[meshName]._v_attrs.vsLowerBounds[1],lby)
    lbz=min(t1.root._v_children[meshName]._v_attrs.vsLowerBounds[2],lbz)
    ubx=max(t1.root._v_children[meshName]._v_attrs.vsUpperBounds[0],ubx)
    uby=max(t1.root._v_children[meshName]._v_attrs.vsUpperBounds[1],uby)
    ubz=max(t1.root._v_children[meshName]._v_attrs.vsUpperBounds[2],ubz)
  else:
    print "dsetname couldn't match"
  t1.close()
fieldout=numpy.zeros((NX,NY,NZ2,2))
for component in ['real','imag']:
  NZ2start=0
  for fieldPos in ['front','active','back']:
    for rank in range(0,maxRank+1):
#      try:
        loopfname=simbase+"_"+dataset+"_"+fieldPos+"_"+component+"_"+str(rank)+"_"+dumpnoAndExt
        if os.path.isfile(loopfname):
          f1=tables.open_file(loopfname)
          fieldName=dataset+"_"+fieldPos+"_"+component+"_"+str(rank)
          fieldNameR=dataset+"_"+fieldPos+"_real_"+str(rank)
          if component=='real':
#            fieldout[:,:,NZ2start:(NZ2start+thisNZ2[fieldName]+1),0]=f1.root._v_children[dataset+"_"+fieldPos+"_"+component].read().T
            fieldout[:,:,NZ2start:(NZ2start+thisNZ2[fieldNameR]),0]=f1.root._v_children[dataset+"_"+fieldPos+"_"+component].read().T
#            fieldout[:,:,NZ2start:(NZ2start+thisNZ2[fieldNameR]),0]=f1.root._v_children[dataset+"_"+fieldPos+"_"+component].read().reshape(NX,NY,thisNZ2[fieldNameR])
            NZ2start+=thisNZ2[fieldNameR]
          if component=='imag':
            fieldout[:,:,NZ2start:(NZ2start+thisNZ2[fieldNameR]),1]=f1.root._v_children[dataset+"_"+fieldPos+"_"+component].read().T
#            fieldout[:,:,NZ2start:(NZ2start+thisNZ2[fieldNameR]),1]=f1.root._v_children[dataset+"_"+fieldPos+"_"+component].read().reshape(NX,NY,thisNZ2[fieldNameR])
            NZ2start+=thisNZ2[fieldNameR]
          f1.close()
#      except:
#        print "could not use "+simbase+"_"+dataset+"_"+fieldPos+"_"+component+"_"+str(rank)+"_"+dumpnoAndExt+", does it exist?"
tables.copy_file(filename,outfilename,overwrite=1)
h5=tables.open_file(outfilename,'r+')
copiedDsetName=dataset+"_"+fieldpart+"_"+dsetcomponent
print h5.root._v_children[copiedDsetName].shape
print copiedDsetName
# While this works it may be better to just read using numpy.asfortranarray() 
# ie fieldin=numpy.asfortranarray(h5.root.aperp.read()) or whatever actually works
#a1=f1.root.aperp_front_real.read()
#a2=f2.root.aperp_active_real.read()
#a3=f3.root.aperp_back_real.read()
#fieldout=numpy.zeros((nx,ny,nz,nc))
#fieldin=h5.root.aperp.read()
#print "fieldin shape "+str(fieldin.shape)
#testfield=numpy.asarray(fieldin, order='C')
#print "testfield shape"+str(testfield.shape)
#testfield=numpy.asfortranarray(fieldin)
#print "testfield shape"+str(testfield.shape)
#fieldout=numpy.zeros((fieldin.shape[3],fieldin.shape[2],fieldin.shape[1],fieldin.shape[0]))
#print "fieldout shape"+str(fieldout.shape)
#for i in range(fieldin.shape[0]):
#  for j in range(fieldin.shape[2]):
#    fieldout[:,j,:,i]=fieldin[i,:,j,:].T
#.reshape(numpy.array((h5.root.aperp.shape[3],h5.root.aperp.shape[2],h5.root.aperp.shape[1],2)),order='F')
print fieldout.shape
h5.create_array('/',dataset,fieldout)
h5.copy_node_attrs('/'+copiedDsetName,'/'+dataset)
h5.remove_node('/'+copiedDsetName)
#h5.rename_node('/aperpN','aperp')
h5.root._v_children['aperp']._v_attrs.vsIndexOrder="compMinorC"
h5.root._v_children['aperp']._v_attrs.vsLabels="aperp_real,aperp_imag"
h5.root._v_children['meshScaled']._v_attrs.vsIndexOrder="compMinorC"
h5.root._v_children['meshScaled']._v_attrs.vsNumCells=numpy.array((NX-1, NY-1, NZ2-1))
h5.root._v_children['meshScaled']._v_attrs.vsLowerBounds=numpy.array((lbx, lby, lbz))
h5.root._v_children['meshScaled']._v_attrs.vsUpperBounds=numpy.array((ubx, uby, ubz))
print h5.root._v_children['meshScaled']._v_attrs
#h5.root._v_children['meshScaled']._v_attrs.vsNumCells=numpy.double((NX, NY, NZ2))
h5.close()
