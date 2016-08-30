#************* THIS HEADER MUST NOT BE REMOVED *******************
#* Copyright (c) 2013-2016, Lawrence Campbell and Brian McNeil. **
#** This program must not be copied, distributed or altered in  **
#** any way without the prior permission of the above authors.  **
#*****************************************************************

"""
@aggregatePower.py  Aggregate Power Output

This should write VizSchema file with aggregated power output
"""
import numpy,tables,glob,os,sys

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


def getTimeSliceInfo(filelist,datasetname):
  """ getTimeSliceInfo() Glob for Power output and return number of files, min, max

  This should provide the number of time slices, which is used as an array dim
  It should also query the first and last to get the time (SI) and calculate z.
  z Should be present as a derived variable ultimately, but for now is not.
  """
  h5in=tables.open_file(filelist[0],'r')
  print "Checking "+filelist[0]
  mint=h5in.root.time._v_attrs.vsTime
  try:
    minz=h5in.root._f_getChild(datasetname)._v_attrs.z
  except:
    print "no min z data present"
    minz=None
  h5in.close()
  h5in=tables.open_file(filelist[-1],'r')
  print "Checking "+filelist[-1]
  maxt=h5in.root.time._v_attrs.vsTime
  try:
    maxz=h5in.root._f_getChild(datasetname)._v_attrs.z
  except:
    print "no max z data present"
    maxz=None
  h5in.close()
   
  return len(filelist), mint, maxt, minz, maxz
   

def getNumSpatialPoints(filelist,datasetname):
  """ getNumSpatialPoints(filelist) get num spatial points

  What it says on the tin. Same as extent of data.
  """
  h5in=tables.open_file(filelist[0],'r')
  length=h5in.root._f_getChild(datasetname).shape[0]
  min=h5in.root.globalLimits._v_attrs.vsLowerBounds
  max=h5in.root.globalLimits._v_attrs.vsUpperBounds
  h5in.close()
  print "length: "+str(length)
  return length,min,max

print "passed "+str(len(sys.argv))+" arguments"
print "1: " +sys.argv[1]
#baseName="Power_0"
#baseName="fig7_Power_0"
baseName=sys.argv[1]
inputFilename,datasetname,mpirank=baseName.split('_')
outfilename=baseName+'_all.vsh5'
h5=tables.open_file(outfilename,'w')
filelist=getTimeSlices(baseName)
numTimes,minZT,maxZT,minZZ,maxZZ=getTimeSliceInfo(filelist,datasetname)
numSpatialPoints, minS,maxS=getNumSpatialPoints(filelist,datasetname)
deltaz2 = (maxS - minS) / numSpatialPoints
sumData=numpy.zeros(numTimes)
peakData=numpy.zeros(numTimes)

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
h5.root.grid._v_attrs.vsAxisLabels="z2bar,t"
if minZZ is not None:
  h5.create_group('/','gridZ','')
  h5.root.gridZ._v_attrs.vsLowerBounds=numpy.array((numpy.double(minS),numpy.double(minZZ)))
  h5.root.gridZ._v_attrs.vsStartCell=numpy.array((numpy.int(0),numpy.int(0)))
  h5.root.gridZ._v_attrs.vsUpperBounds=numpy.array((numpy.double(maxS),numpy.double(maxZZ)))
  h5.root.gridZ._v_attrs.vsNumCells=numpy.array(numCells)
  h5.root.gridZ._v_attrs.vsKind="uniform"
  h5.root.gridZ._v_attrs.vsType="mesh"
  h5.root.gridZ._v_attrs.vsAxisLabels="Z2bar,Zbar"
  h5.root.gridZ._v_attrs.vsCentering="nodal"
fieldData=numpy.zeros((numSpatialPoints, numTimes))
fieldNormData=numpy.zeros((numSpatialPoints, numTimes))
fieldCount=0
for slice in filelist:
  h5in=tables.open_file(slice,'r')
  fieldData[:,fieldCount]=h5in.root._f_getChild(datasetname).read()
  sumData[fieldCount]=numpy.trapz(h5in.root._f_getChild(datasetname).read(), None, deltaz2)
  peakData[fieldCount]=numpy.max(h5in.root._f_getChild(datasetname).read())
  fieldNormData[:,fieldCount]=h5in.root._f_getChild(datasetname).read()/peakData[fieldCount]
  h5in.close()
  fieldCount+=1
h5.create_array('/',datasetname+'_ST',fieldData)
h5.create_array('/',datasetname+'_ST_Norm',fieldNormData)
for fieldname in [datasetname+'_ST',datasetname+'_ST_Norm']:
  h5.root._v_children[fieldname]._v_attrs.vsMesh="gridZ"
  h5.root._v_children[fieldname]._v_attrs.vsTimeGroup="time"
  h5.root._v_children[fieldname]._v_attrs.time=0.
  h5.root._v_children[fieldname]._v_attrs.vsType="variable"
  h5.root._v_children[fieldname]._v_attrs.vsAxisLabels="z,z2 or tt"
h5.create_group('/','time','')
h5.root.time._v_attrs.vsType="time"
h5.root.time._v_attrs.vsTime=0.
h5.root.time._v_attrs.vsStep=0
h5.create_array('/',datasetname+'_integral',sumData)
h5.create_array('/',datasetname+'_peak',peakData)
h5.create_group('/',datasetname+'_integralz','sumData as function of z')
h5.create_group('/',datasetname+'_peakz','peakData as a function of z')
h5.create_group('/','timeSeries','Time information')
h5.root.timeSeries._v_attrs.vsKind='uniform'
h5.root.timeSeries._v_attrs.vsType='mesh'
h5.root.timeSeries._v_attrs.vsStartCell=0
h5.root.timeSeries._v_attrs.vsNumCells=numTimes-1 # -1 as zonal
h5.root.timeSeries._v_attrs.vsLowerBounds=minZT
h5.root.timeSeries._v_attrs.vsUpperBounds=maxZT
h5.create_group('/','zSeries','Mesh of positions through machine')
h5.root.zSeries._v_attrs.vsKind='uniform'
h5.root.zSeries._v_attrs.vsType='mesh'
h5.root.zSeries._v_attrs.vsStartCell=0
h5.root.zSeries._v_attrs.vsNumCells=numTimes-1 # -1 as zonal
h5.root.zSeries._v_attrs.vsLowerBounds=minZZ
h5.root.zSeries._v_attrs.vsUpperBounds=maxZZ
h5.root.zSeries._v_attrs.vsAxisLabels="z2"
h5in=tables.open_file(filelist[-1])
h5in.root.runInfo._f_copy(h5.root)
h5in.close()
h5.create_group('/','powerZZ2','')
h5.root.powerZZ2._v_attrs.vsMesh='gridZ'
h5.root.powerZZ2._v_attrs.vsType='vsVars'
h5.root.powerZZ2._v_attrs.powerZZ2='power_ST'
for fieldname in [datasetname+'_integral',datasetname+'_peak']:
  h5.root._v_children[fieldname]._v_attrs.vsMesh='timeSeries'
  h5.root._v_children[fieldname]._v_attrs.vsType='variable'
  h5.root._v_children[fieldname+"z"]._v_attrs.vsMesh='zSeries'
  h5.root._v_children[fieldname+"z"]._v_attrs.vsType='vsVars'
  h5.root._v_children[fieldname+"z"]._v_attrs.vsAxisLabels='Z2,Z'
  h5.root._v_children[fieldname+"z"]._f_setattr(fieldname+"z",fieldname)
h5.close()
