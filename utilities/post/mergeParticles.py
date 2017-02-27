#************* THIS HEADER MUST NOT BE REMOVED *******************
#* Copyright (c) 2013-2016, Lawrence Campbell and Brian McNeil. **
#** This program must not be copied, distributed or altered in  **
#** any way without the prior permission of the above authors.  **
#*****************************************************************

"""
@mergeParticles.py  Merges multiple particle files to one file

Expects files in order basename_speciesname_procno_dumpstep.h5
"""

import numpy,tables,glob,os,sys


def getTimeSlices(baseName):
  """ getTimeSlices(baseName) gets a list of files

  That will be used down the line...
  """
  filelist=glob.glob(os.getcwd()+os.sep+baseName+'_0_*.h5')
  dumpStepNos=[]
  for thisFile in filelist:
    thisDump=int(thisFile.split(os.sep)[-1].split('.')[0].split('_')[-1])
    dumpStepNos.append(thisDump)

  for i in range(len(dumpStepNos)):
    filelist[i]=baseName+'_0_'+str(sorted(dumpStepNos)[i])+'.h5'
  return filelist, dumpStepNos

def getNumProcs(filelist,baseName,n):
  """ getNumProcs() Glob for electron files and determine num of procs

  This should provide the number of time slices, which is used as an array dim
  It should also query the first and last to get the time (SI) and calculate z.
  z Should be present as a derived variable ultimately, but for now is not.
  """
  thisFile=filelist[n]
  # determine first dump
  thisDump=int(thisFile.split(os.sep)[-1].split('.')[0].split('_')[-1])
  procPtclFiles=glob.glob(os.getcwd()+os.sep+baseName+'_*_'+str(thisDump)+'.h5')
  print procPtclFiles
  numProcs=len(procPtclFiles)
  procNos=[]
  print "numProcs: "+str(numProcs)
  for procFile in procPtclFiles:
    thisProc=int(procFile.split(os.sep)[-1].split('.')[0].split('_')[-2])
    procNos.append(thisProc)
  print "maxProcNo: "+str(max(procNos))
  if max(procNos) != numProcs:
    print "This isn't very good, is it!"   
  return numProcs, sorted(procNos)

print "Got "+str(len(sys.argv))+" arguments"
baseName=sys.argv[1]
print "using baseName = "+baseName
#baseName="fig7_electrons"
#baseName="electrons"
filelist, dumpStepNos=getTimeSlices(baseName)
numProcs0, procNos0=getNumProcs(filelist,baseName,0)
for i in range(len(filelist)):
  particleCount=0
  outfilename=baseName+'_'+str(dumpStepNos[i])+'.vsh5'
  for j in range(numProcs0):
    if j==0:
      tables.copy_file(filelist[0], outfilename,overwrite=True)
      h5out=tables.open_file(outfilename,'r+')
      particleCount+=h5out.root.electrons.shape[0]
    else:
      print "particlesSoFar: "+str(particleCount)
      print "Processor: "+str(j)
      h5in=tables.open_file(baseName+'_'+str(j)+'_'+str(dumpStepNos[i])+'.h5','r')
      particleCount+=h5in.root.electrons.shape[0]
      h5in.close()
      print "cumulativeSumSoFar: "+str(particleCount)
  oldElecs=h5out.root.electrons.read()
  elecs=numpy.zeros((particleCount+1,oldElecs.shape[1]))
  elecs[0:oldElecs.shape[0],:]=oldElecs
  particleCount=oldElecs.shape[0]
  for j in range(1,numProcs0):
    print "particlesSoFar: "+str(particleCount)
    print "Processor: "+str(j)
    h5in=tables.open_file(baseName+'_'+str(j)+'_'+str(dumpStepNos[i])+'.h5','r')
    newElecs=h5in.root.electrons.read()
    elecs[particleCount:particleCount+h5in.root.electrons.shape[0],:]=newElecs
    particleCount+=h5in.root.electrons.shape[0]
    h5in.close()
    print "cumulativeSumSoFar: "+str(particleCount)
  h5out.create_array('/','newElecs',elecs)
  h5out.copy_node_attrs('/electrons','/newElecs')
  h5out.remove_node('/electrons')
  h5out.rename_node('/newElecs','electrons')
  h5out.close()      
  
