"""@mergeParticles.py Merges multiple particle files to one file

Expects files in order basename_speciesname_procno_dumpstep.h5
"""

import numpy,tables,glob,os


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

baseName="fig7_electrons"
#baseName="electrons"
filelist, dumpStepNos=getTimeSlices(baseName)
numProcs0, procNos0=getNumProcs(filelist,baseName,0)
for i in range(len(filelist)):
  for j in range(numProcs0):
    outfilename=baseName+'_'+str(dumpStepNos[i])+'.vsh5'
    if j==0:
      tables.copy_file(filelist[0], outfilename,overwrite=True)
    else:
      print "Processor: "+str(j)
      h5out=tables.open_file(outfilename,'r+')
      h5in=tables.open_file(baseName+'_'+str(j)+'_'+str(dumpStepNos[i])+'.h5','r')
      oldElecs=h5out.root.electrons.read()
      newElecs=h5in.root.electrons.read()
      elecs=numpy.zeros((oldElecs.shape[0]+newElecs.shape[0],newElecs.shape[1]))
      elecs[0:oldElecs.shape[0],:]=oldElecs
      elecs[oldElecs.shape[0]:,:]=newElecs
      h5out.create_array('/','newElecs',elecs)
      h5out.copy_node_attrs('/electrons','/newElecs')
      h5out.remove_node('/electrons')
      h5out.rename_node('/newElecs','electrons')
      h5in.close()
      h5out.close()      
  