# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Jonathan Smith (Tech-X UK Ltd)
# License: BSD-3-Clause

import time

start=time.time()


"""
@ReorderColMajorFtoColMinorC.py Switch between fortran and C ordering of columns

A script for conversion of VizSchema in CompMajorF (Column Major) to CompMajorC (Column Minor)
"""

import numpy, tables, sys, os, glob



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
  print "Usage ReorderColMajorFtoColMinorC.py filename"
  print "Please provide full filename, or a basename without extension to match multiple files"
  exit()

baseName = sys.argv[1]
filelist = getFiles(baseName)


def convertFile(filename):
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

  #tables.copy_file(filename,outfilename,overwrite=1)
  h5old=tables.open_file(filename, 'r')
  h5=tables.open_file(outfilename,'w')

  
 
  #print h5.root.aperp.shape
  # While this works it may be better to just read using numpy.asfortranarray() 
  # ie fieldin=numpy.asfortranarray(h5.root.aperp.read()) or whatever actually works
  fieldin=h5old.root.aperp.read()
  print "The input has "+str(len(fieldin.shape))+" dimensions"
  print "fieldin shape "+str(fieldin.shape)
  testfield=numpy.asarray(fieldin, order='C')
  print "testfield shape"+str(testfield.shape)
  testfield=numpy.asfortranarray(fieldin)
  print "testfield shape"+str(testfield.shape)
  
  if len(fieldin.shape)==4:
    fieldout=numpy.zeros((fieldin.shape[3],fieldin.shape[2],fieldin.shape[1],fieldin.shape[0]))
    print "fieldout shape"+str(fieldout.shape)
    print "3d plus component"

    for i in range(fieldin.shape[0]):
      for j in range(fieldin.shape[2]):
        fieldout[:,j,:,i]=fieldin[i,:,j,:].T

  elif len(fieldin.shape)==2:
        fieldout=fieldin.T
  else:
    print "Do not recognise structure of file "+filename+" setting the _C file as just a copy"

  print fieldout.shape

  
  
  
  h5.create_array('/','aperp',fieldout)
  aperp=h5old.get_node('/aperp')
  h5.copy_node_attrs(aperp,'/aperp')

 
  globalLimits=h5old.get_node('/globalLimits')
  intensityScaled=h5old.get_node('/intensityScaled')
  meshScaled=h5old.get_node('/meshScaled')
  runInfo=h5old.get_node('/runInfo')
  time=h5old.get_node('/time')


  h5.create_group('/','globalLimits')
  h5.create_group('/', 'intensityScaled')
  h5.create_group('/','meshScaled')
  h5.create_group('/', 'runInfo')
  h5.create_group('/', 'time')

  h5.copy_node_attrs(globalLimits, '/globalLimits')
  h5.copy_node_attrs(intensityScaled, '/intensityScaled')
  h5.copy_node_attrs(meshScaled, '/meshScaled')
  h5.copy_node_attrs(runInfo, '/runInfo')
  h5.copy_node_attrs(time, '/time')

  h5.root._v_children['aperp']._v_attrs.vsIndexOrder="compMinorC"
  h5.root._v_children['meshScaled']._v_attrs.vsIndexOrder="compMinorC"
  h5.root._v_children['meshScaled']._v_attrs.vsAxisLabels="x,y,z2"

  def swapXYZ(tb):
    
    bx = tb[2]
    by = tb[1]
    bz2 = tb[0]
    tbn = [bx,by,bz2]
    return tbn

  if len(fieldin.shape)==4:

    tlb = h5.root._v_children['meshScaled']._v_attrs.vsLowerBounds

    nlb = swapXYZ(tlb)
    h5.root._v_children['meshScaled']._v_attrs.vsLowerBounds=numpy.array(nlb)
  
    tub = h5.root._v_children['meshScaled']._v_attrs.vsUpperBounds
    nub = swapXYZ(tub)
    h5.root._v_children['meshScaled']._v_attrs.vsUpperBounds = numpy.array(nub)
  
    tnc = h5.root._v_children['meshScaled']._v_attrs.vsNumCells
    nnc = swapXYZ(tnc)
    h5.root._v_children['meshScaled']._v_attrs.vsNumCells = numpy.array(nnc)

    h5.root._v_children['globalLimits']._v_attrs.vsLowerBounds = numpy.array(nlb)
    h5.root._v_children['globalLimits']._v_attrs.vsUpperBounds = numpy.array(nub)

  h5.close()
  h5old.close()

for iname in filelist:
  convertFile(iname)

end=time.time()

print 'time', end-start

