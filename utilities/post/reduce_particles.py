# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Jonathan Smith (Tech-X UK Ltd) & Lawrence T. Campbell
# License: BSD-3-Clause

""" @ reduce_particles.py Gives a reduced particle dataset

A reduced dataset is beneficial for elegant, and for plotting. 
"""
import numpy,tables,os,sys

def writeChargeOutput3D(histdata,edgedata,ptcldata,dataname):
  h5out=tables.open_file('_'.join(outfilebase)+dataname+'_'+str(dumpno)+'.'+fileextname,'w')
  datasetname='electron'+dataname.title()
  h5out.create_array('/',datasetname,histdata)
  h5out.root._f_getChild(datasetname)._v_attrs['time']=numpy.double(0.0)
  h5out.root._f_getChild(datasetname)._v_attrs['numSpatialDims']=numpy.int(3)
  h5out.root._f_getChild(datasetname)._v_attrs['vsNumSpatialDims']=numpy.int(3)
  h5out.root._f_getChild(datasetname)._v_attrs['vsType']="variable"
  h5out.root._f_getChild(datasetname)._v_attrs['vsMesh']="mesh"
  h5out.root._f_getChild(datasetname)._v_attrs['vsTimeGroup']="time"
  h5out.root._f_getChild(datasetname)._v_attrs['vsLimits']="globalLimits"
  h5out.root._f_getChild(datasetname)._v_attrs['vsCentering']="zonal"
  h5out.create_group('/','globalLimits')
  h5out.root.globalLimits._v_attrs['vsType']="limits"
  h5out.root.globalLimits._v_attrs['vsKind']="Cartesian"
  h5out.root.globalLimits._v_attrs['vsLowerBounds']=numpy.array((numpy.min(ptcldata[:,0]),numpy.min(ptcldata[:,1]),numpy.min(ptcldata[:,2])))
  h5out.root.globalLimits._v_attrs['vsUpperBounds']=numpy.array((numpy.max(ptcldata[:,0]),numpy.max(ptcldata[:,1]),numpy.max(ptcldata[:,2])))
  h5out.create_group('/','time')
  h5out.root.time._v_attrs['vsStep']=numpy.int(dumpno)
  h5out.root.time._v_attrs['vsTime']=numpy.double(simtime)
  h5out.root.time._v_attrs['vsType']="time"
  h5out.create_group('/','mesh')
  h5out.root.mesh._v_attrs['vsType']="mesh"
  h5out.root.mesh._v_attrs['vsKind']="uniform"
  h5out.root.mesh._v_attrs['vsCentering']="zonal"
  h5out.root.mesh._v_attrs['vsLowerBounds']=numpy.array((numpy.min(ptcldata[:,0]),numpy.min(ptcldata[:,1]),numpy.min(ptcldata[:,2])))
  h5out.root.mesh._v_attrs['vsUpperBounds']=numpy.array((numpy.max(ptcldata[:,0]),numpy.max(ptcldata[:,1]),numpy.max(ptcldata[:,2])))
  h5out.root.mesh._v_attrs['vsNumCells']=numpy.array((len(edgedata[0])-1,len(edgedata[1])-1,len(edgedata[2])-1))
  h5out.root.mesh._v_attrs['vsStartCell']=numpy.array((0,0,0))
  h5out.flush()
  h5out.close()


def writeChargeOutput1D(histdata,edgedata,ptcldata,dataname):
  h5out=tables.open_file('_'.join(outfilebase)+dataname+'_'+str(dumpno)+'.'+fileextname,'w')
  datasetname='electron'+dataname.title()
  h5out.create_array('/',datasetname,histdata)
  h5out.root._f_getChild(datasetname)._v_attrs['time']=numpy.double(0.0)
  h5out.root._f_getChild(datasetname)._v_attrs['numSpatialDims']=numpy.int(1)
  h5out.root._f_getChild(datasetname)._v_attrs['vsNumSpatialDims']=numpy.int(1)
  h5out.root._f_getChild(datasetname)._v_attrs['vsType']="variable"
  h5out.root._f_getChild(datasetname)._v_attrs['vsMesh']="mesh"
  h5out.root._f_getChild(datasetname)._v_attrs['vsTimeGroup']="time"
  h5out.root._f_getChild(datasetname)._v_attrs['vsLimits']="globalLimits"
  h5out.root._f_getChild(datasetname)._v_attrs['vsCentering']="zonal"
  h5out.create_group('/','globalLimits')
  h5out.root.globalLimits._v_attrs['vsType']="limits"
  h5out.root.globalLimits._v_attrs['vsKind']="Cartesian"
  h5out.root.globalLimits._v_attrs['vsLowerBounds']=numpy.array((lb[2]))
  h5out.root.globalLimits._v_attrs['vsUpperBounds']=numpy.array((ub[2]))
  h5out.create_group('/','time')
  h5out.root.time._v_attrs['vsStep']=numpy.int(dumpno)
  h5out.root.time._v_attrs['vsTime']=numpy.double(simtime)
  h5out.root.time._v_attrs['vsType']="time"
  h5out.create_group('/','mesh')
  h5out.root.mesh._v_attrs['vsType']="mesh"
  h5out.root.mesh._v_attrs['vsKind']="uniform"
  h5out.root.mesh._v_attrs['vsCentering']="zonal"
#  h5out.root.mesh._v_attrs['vsLowerBounds']=numpy.array((numpy.min(ptcldata[:,2])))
#  h5out.root.mesh._v_attrs['vsUpperBounds']=numpy.array((numpy.max(ptcldata[:,2])))
  h5out.root.mesh._v_attrs['vsLowerBounds']=numpy.array((lb[2]))
  h5out.root.mesh._v_attrs['vsUpperBounds']=numpy.array((ub[2]))
  h5out.root.mesh._v_attrs['vsNumCells']=numpy.array((numpy.int(len(edgedata)-1)))
  h5out.root.mesh._v_attrs['vsStartCell']=numpy.array((numpy.int(0)))
  h5out.flush()
  h5out.close()

print sys.argv
if len(sys.argv)<2:
  print "usage reduce_particles.py input_filename [num_required_particles]" 
input_filename=sys.argv[1]
if len(sys.argv)==3:
  num_required_particles=numpy.int(sys.argv[2])
else:
  num_required_particles=10000
h5=tables.open_file(input_filename)
elecs=h5.root.electrons.read()
total_charge=numpy.sum(elecs[:,6])
print "Total charge: "+str(total_charge)
print "Total required particles: "+str(num_required_particles)
sum=numpy.double(0)
charge_per_new_macro=total_charge/float(num_required_particles)
print "Charge pre new macro: "+str(charge_per_new_macro)
num_old_elec_macroparticles=int(elecs.shape[0])
print "Number of original particles: "+str(elecs.shape[0])
lb=h5.root.globalLimits._v_attrs.vsLowerBounds
print lb
ub=h5.root.globalLimits._v_attrs.vsUpperBounds
print ub
simtime=h5.root.electrons._v_attrs.time
newelecs_count=0
newparticles=numpy.zeros((num_required_particles,6))
for i in range(num_old_elec_macroparticles):
  sum+=elecs[i,6]
  if sum>charge_per_new_macro:
    newparticles[newelecs_count,:]=elecs[i,0:6]
    charge_per_new_macro+=total_charge/float(num_required_particles)
    newelecs_count+=1
outfilename,fileextname=input_filename.split('.')
outfilebase=outfilename.split('_')[:-1]
dumpno=outfilename.split('_')[-1]
h5out=tables.open_file('_'.join(outfilebase)+'_reduced_'+dumpno+'.'+fileextname,'w')
h5out.create_array('/','electrons',newparticles)
h5out.root.electrons._v_attrs['time']=numpy.double(0.0)
h5out.root.electrons._v_attrs['numSpatialDims']=numpy.int(3)
h5out.root.electrons._v_attrs['vsNumSpatialDims']=numpy.int(3)
h5out.root.electrons._v_attrs['vsType']="variableWithMesh"
h5out.root.electrons._v_attrs['vsTimeGroup']="time"
h5out.root.electrons._v_attrs['vsLimits']="globalLimits"
h5out.create_group('/','globalLimits')
h5out.root.globalLimits._v_attrs['vsType']="limits"
h5out.root.globalLimits._v_attrs['vsKind']="Cartesian"
h5out.root.globalLimits._v_attrs['vsLowerBounds']=numpy.array((numpy.min(newparticles[:,0]),numpy.min(newparticles[:,1]),numpy.min(newparticles[:,2])))
h5out.root.globalLimits._v_attrs['vsUpperBounds']=numpy.array((numpy.max(newparticles[:,0]),numpy.max(newparticles[:,1]),numpy.max(newparticles[:,2])))
h5out.create_group('/','time')
h5out.root.time._v_attrs['vsStep']=numpy.int(0)
h5out.root.time._v_attrs['vsTime']=numpy.double(0.0)
h5out.root.time._v_attrs['vsType']="time"
h5out.flush()
h5out.close()
###
# Can we write a charge density field?
##
print "histogramming charge data"
H,edges=numpy.histogramdd(elecs[:,0:3],bins=(numpy.linspace(lb[0],ub[0],32),numpy.linspace(lb[1],ub[1],32),numpy.linspace(lb[2],ub[2],228)),weights=elecs[:,6])
print "histogramming reduced data"
H_reduced,edges_reduced=numpy.histogramdd(newparticles[:,0:3],bins=(32,32,228))
#print edges[0]
print numpy.max(H)
print "writing charge data"
writeChargeOutput3D(H,edges,elecs,'charge3D')
print "and writing reduced charge data"
writeChargeOutput3D(H_reduced,edges_reduced,newparticles,'charge3D_reduced')
print "1D histogram"
H1D,edges1D=numpy.histogram(elecs[:,2],bins=(numpy.linspace(lb[2],ub[2],570)),weights=elecs[:,6])
print edges1D
writeChargeOutput1D(H1D,edges1D,elecs,'charge1D')
Hreduced1D,edgesReduced1D=numpy.histogram(newparticles[:,2],bins=(numpy.linspace(lb[2],ub[2],570)))
print edgesReduced1D
print str(len(edgesReduced1D))
print str(len(edgesReduced1D)-1)
writeChargeOutput1D(Hreduced1D,edgesReduced1D,newparticles,'charge1D_reduced')
exit()
