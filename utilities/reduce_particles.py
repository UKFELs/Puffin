""" @ reduce_particles.py Gives a reduced particle dataset

A reduced dataset is beneficial for elegant, and for plotting. 
"""
import numpy,tables,os,sys
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
H,edges=numpy.histogramdd(elecs[:,0:3],bins=(32,32,114),weights=elecs[:,6])
print edges[0]
print numpy.max(H)
h5out=tables.open_file('_'.join(outfilebase)+'Charge_'+dumpno+'.'+fileextname,'w')
h5out.create_array('/','electronCharge',H)
h5out.root.electronCharge._v_attrs['time']=numpy.double(0.0)
h5out.root.electronCharge._v_attrs['numSpatialDims']=numpy.int(3)
h5out.root.electronCharge._v_attrs['vsNumSpatialDims']=numpy.int(3)
h5out.root.electronCharge._v_attrs['vsType']="variable"
h5out.root.electronCharge._v_attrs['vsMesh']="mesh"
h5out.root.electronCharge._v_attrs['vsTimeGroup']="time"
h5out.root.electronCharge._v_attrs['vsLimits']="globalLimits"
h5out.root.electronCharge._v_attrs['vsCentering']="zonal"
h5out.create_group('/','globalLimits')
h5out.root.globalLimits._v_attrs['vsType']="limits"
h5out.root.globalLimits._v_attrs['vsKind']="Cartesian"
h5out.root.globalLimits._v_attrs['vsLowerBounds']=numpy.array((numpy.min(newparticles[:,0]),numpy.min(newparticles[:,1]),numpy.min(newparticles[:,2])))
h5out.root.globalLimits._v_attrs['vsUpperBounds']=numpy.array((numpy.max(newparticles[:,0]),numpy.max(newparticles[:,1]),numpy.max(newparticles[:,2])))
h5out.create_group('/','time')
h5out.root.time._v_attrs['vsStep']=numpy.int(0)
h5out.root.time._v_attrs['vsTime']=numpy.double(0.0)
h5out.root.time._v_attrs['vsType']="time"
h5out.create_group('/','mesh')
h5out.root.mesh._v_attrs['vsType']="mesh"
h5out.root.mesh._v_attrs['vsKind']="uniform"
h5out.root.mesh._v_attrs['vsCentering']="zonal"
h5out.root.mesh._v_attrs['vsLowerBounds']=numpy.array((numpy.min(newparticles[:,0]),numpy.min(newparticles[:,1]),numpy.min(newparticles[:,2])))
h5out.root.mesh._v_attrs['vsUpperBounds']=numpy.array((numpy.max(newparticles[:,0]),numpy.max(newparticles[:,1]),numpy.max(newparticles[:,2])))
h5out.root.mesh._v_attrs['vsNumCells']=numpy.array((len(edges[0])-1,len(edges[1])-1,len(edges[2])-1))
h5out.root.mesh._v_attrs['vsStartCell']=numpy.array((0,0,0))
h5out.flush()
h5out.close()
exit()
