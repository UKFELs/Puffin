import numpy,tables,os,sys
from scipy.signal install hilbert
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
#Check input is correct

def getMagPhase(h5,xi,yi,component):
    zLoBounds=h5.root.globalLimits._v_attrs.vsLowerBounds[2]
    zUpBounds=h5.root.globalLimits._v_attrs.vsUpperBounds[2]
    zNumCells=h5.root.meshScaled._v_attrs.vsNumCells[2]
    zNumNodes=zNumCells+1
    zLen=(zUpBounds-zLoBounds)
  # nodes is num cells+1
    zMesh=numpy.linspace(zLoBounds,zUpBounds,zNumNodes)
    dz=zLen/zNumCells
    analytic_signal=hibert(h5.root.aperp[xi,yi,:,component]
    amplitude_envelope = numpy.abs(analytic_signal)    
    instantaneous_phase = numpy.unwrap(numpy.angle(analytic_signal))    
    instantaneous_freq = numpy.diff(instantaneous_phase) / (2.0*numpy.pi*dz)
    return amplitude_envelope,instantaneous_phase,instantaneous_freq
    
if len(sys.argv) == 2:
  inputFilename=sys.argv[1]
  h5=tables.open_file(inputFilename)
  (nx,ny,nz,nComponents)=h5.root.aperp.shape
  print "nx: " + str(nx)
  print "ny: " + str(ny)
  print "nz: " + str(nz)
#  print numpy.int((ny-1)/8.)
#  print numpy.int(7*(ny-1)/8.)+1
#  print numpy.int(numpy.ceil((ny-1)/8.))
  print "nComponents: " + str(nComponents)
  
#  plt.figure(figsize=(35,35))
#  gs = gridspec.GridSpec(7,7)
#  count=0
#  for yi in range(numpy.int((ny-1)/8.),numpy.int(7*(ny-1)/8.)+1,numpy.int(numpy.ceil((ny-1)/8.))):
#    for xi in range(numpy.int((nx-1)/8.),numpy.int(7*(nx-1)/8.)+1,numpy.int(numpy.ceil((nx-1)/8.))):
#      plt.subplot(gs[count])
#      mymax=max(numpy.max(numpy.abs((h5.root.aperp[xi,yi,:numpy.int(nz/2),0]))),numpy.max(numpy.abs(h5.root.aperp[xi,yi,:numpy.int(nz/2),1])))
#      plt.scatter(h5.root.aperp[xi,yi,:(numpy.int(nz/2)),0]/mymax,h5.root.aperp[xi,yi,:(numpy.int(nz/2)),1]/mymax)
#      plt.axis([-1.2,1.2,-1.2,1.2])
#      plt.title("xi="+str(xi)+" yi="+str(yi)+ " norm={:1.1}".format(mymax))
    #plt.savefig("ExEy-"+str(xi)+"-"+str(yi)+".png")
#      count+=1
#  plt.savefig("ExEy.png")
  # Now the converted stuff from Lawrence's matlab
  # Now with Lawrence's stuff ported to python
  StokesParams=numpy.zeros((nx,ny,nz,6))
  #output file goes in current directory whether or not input was there.
  outFilename=inputFilename.split(os.sep)[-1].replace('aperp','stokes')
  print outFilename
#  for xi in range(0,nx):
#    for yi in range(0,ny):
# Just look in the middle while testing
  for xi in range(int(14*nx/32),int(18*nx/32)):
    for yi in range(int(14*ny/32),int(18*ny/32)):
      print "xi: "+str(xi)+"  yi: "+str(yi)
      magx,phasex,freqx=getMagPhase(h5,xi,yi,0)
      magy,phasey,freqy=getMagPhase(h5,xi,yi,1)
#      s0=numpy.max(numpy.add(numpy.square(magx),numpy.square(magy)),1.e-99)
#      s1=numpy.subtract(numpy.square(magx),numpy.square(magy))
#      s2=2*numpy.multiply(numpy.multiply(magx,magy),numpy.cos(numpy.subtract(phasex,phasey)))
#      s3=2*numpy.multiply(numpy.multiply(magx,magy),numpy.cos(numpy.subtract(phasex,phasey)))
  # various averages are calculated using stokesLength, but they don't appear to be used.
      StokesParams[xi,yi,:,0]=magx
      StokesParams[xi,yi,:,1]=phasex
      StokesParams[xi,yi,:,2]=freqx
      StokesParams[xi,yi,:,3]=magy
      StokesParams[xi,yi,:,4]=phasey
      StokesParams[xi,yi,:,5]=freqy
  #writeFieldOutput3D(StokesParams,filename)
  tables.copy_file(inputFilename,outFilename,overwrite=1)
  h5out=tables.open_file(outFilename,'r+')
  dn='stokes' #dataname - shortening
  h5out.create_array('/',dn,StokesParams)
  h5out.copy_node_attrs('/aperp','/stokes')
  h5out.remove_node('/aperp')
  h5out.root.stokes._v_attrs.vsLabels="magx,phasex,freqx,magy,phasey,freqy"
  h5out.root.create_group('/','s0')
  h5out.root.s0._v_attrs.vsType="vsVars"
  h5out.root.s0._v_attrs.s0="max((sqr(magx)+sqr(magy)),1.e-99)"
  h5out.root.create_group('/','P1')
  h5out.root.P1._v_attrs.vsType="vsVars"
  h5out.root.P1._v_attrs.P1="(sqr(magx)-sqr(magy))/s0"
  h5out.root.create_group('/','P2')
  h5out.root.P2._v_attrs.vsType="vsVars"
  h5out.root.P2._v_attrs.P2="2*magx*magy*cos(phasex-phasey)/s0"
  h5out.root.create_group('/','P3')
  h5out.root.P3._v_attrs.vsType="vsVars"
  h5out.root.P3._v_attrs.P3="2*magx*magy*sin(phasex-phasey)/s0"
  h5out.close()
  h5.close()
else:
  print "Usage: plotPolarization.py filename"
  print "We don't appear to have the correct arguments to proceed"
  
