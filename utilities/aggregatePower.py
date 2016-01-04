#************* THIS HEADER MUST NOT BE REMOVED *******************
#* Copyright (c) 2013-2016, Lawrence Campbell and Brian McNeil. **
#** This program must not be copied, distributed or altered in  **
#** any way without the prior permission of the above authors.  **
#*****************************************************************

"""
@aggregatePower.py  Aggregate Power Output

This should write VizSchema file with aggregated power output
"""
import numpy,tables,glob,os

def getTimeSlices(baseName):
  """ getTimeSlices(baseName) gets a list of files

  That will be used down the line...
  """
  filelist=glob.glob(os.getcwd()+os.sep+baseName+'_*.h5')
  dumpStepNos=[]
  for thisFile in filelist:
    thisDump=int(thisFile.split(os.sep)[-1].split('.')[0].split('_')[-1])
    dumpStepNos.append(thisDump)

  for i in range(len(dumpStepNos)):
    filelist[i]=baseName+'_'+str(sorted(dumpStepNos)[i])+'.h5'
  return filelist


def getTimeSliceInfo(filelist):
  """ getTimeSliceInfo() Glob for Power output and return number of files, min, max

  This should provide the number of time slices, which is used as an array dim
  It should also query the first and last to get the time (SI) and calculate z.
  z Should be present as a derived variable ultimately, but for now is not.
  """
  h5in=tables.open_file(filelist[0],'r')
  print "Checking "+filelist[0]
  min=h5in.root.time._v_attrs.vsTime
  h5in.close()
  h5in=tables.open_file(filelist[-1],'r')
  print "Checking "+filelist[-1]
  max=h5in.root.time._v_attrs.vsTime
  h5in.close()
   
  return len(filelist), min, max
   

def getNumSpatialPoints(filelist):
  """ getNumSpatialPoints(filelist) get num spatial points

  What it says on the tin. Same as extent of data.
  """
  h5in=tables.open_file(filelist[0],'r')
  length=h5in.root.Power.shape[0]
  min=h5in.root.globalLimits._v_attrs.vsLowerBounds
  max=h5in.root.globalLimits._v_attrs.vsUpperBounds
  h5in.close()
  print "length: "+str(length)
  return length,min,max

  
#baseName="Power_0"
baseName="fig7_Power_0"
outfilename=baseName+'_all.vsh5'
h5=tables.open_file(outfilename,'w')
filelist=getTimeSlices(baseName)
numTimes,minZT,maxZT=getTimeSliceInfo(filelist)
numSpatialPoints, minS,maxS=getNumSpatialPoints(filelist)
print "files in order:"
print filelist
h5.create_group('/','grid','')
numCells=numpy.array((numpy.int(numSpatialPoints)-1,numpy.int(numTimes)-1))
h5.root.grid._v_attrs.vsLowerBounds=numpy.array((numpy.double(minS),numpy.double(minZT)))
h5.root.grid._v_attrs.vsStartCell=numpy.array((numpy.int(0),numpy.int(0)))
h5.root.grid._v_attrs.vsUpperBounds=numpy.array((numpy.double(maxS),numpy.double(maxZT)))
h5.root.grid._v_attrs.vsNumCells=numpy.array(numCells)
h5.root.grid._v_attrs.vsKind="uniform"
h5.root.grid._v_attrs.vsType="mesh"
h5.root.grid._v_attrs.vsCentering="nodal"
fieldData=numpy.zeros((numSpatialPoints, numTimes))
fieldNormData=numpy.zeros((numSpatialPoints, numTimes))
fieldCount=0
for slice in filelist:
  h5in=tables.open_file(slice,'r')
  fieldData[:,fieldCount]=h5in.root.Power.read()
  fieldNormData[:,fieldCount]=h5in.root.Power.read()/numpy.max(h5in.root.Power.read())
  h5in.close()
  fieldCount+=1
h5.create_array('/','Power_ST',fieldData)
h5.create_array('/','Power_ST_Norm',fieldNormData)
for fieldname in ['Power_ST','Power_ST_Norm']:
  h5.root._v_children[fieldname]._v_attrs.vsMesh="grid"
  h5.root._v_children[fieldname]._v_attrs.vsTimeGroup="time"
  h5.root._v_children[fieldname]._v_attrs.time=0.
  h5.root._v_children[fieldname]._v_attrs.vsType="variable"
h5.create_group('/','time','')
h5.root.time._v_attrs.vsType="time"
h5.root.time._v_attrs.vsTime=0.
h5.root.time._v_attrs.vsStep=0
h5.close()