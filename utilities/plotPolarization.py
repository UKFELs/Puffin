import numpy,tables,matplotlib,os,sys
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
#Check input is correct

def getMagPhase(h5,rho,xi,yi,component):
    wavelength=4*numpy.pi*rho
    zLoBounds=h5.root.globalLimits._v_attrs.vsLowerBounds[2]
    zUpBounds=h5.root.globalLimits._v_attrs.vsUpperBounds[2]
    zNumCells=h5.root.meshScaled._v_attrs.vsNumCells[2]
    zNumNodes=zNumCells+1
    zLen=(zUpBounds-zLoBounds)
  # nodes is num cells+1
    zMesh=numpy.linspace(zLoBounds,zUpBounds,zNumNodes)
    dz=zLen/zNumCells
    nElements=numpy.int(numpy.round(wavelength/dz)) # in our 4 rho section for making an average, nel
    nNodes=nElements+1 # nnl
    nElAverage=numpy.int(numpy.floor((nNodes)/2.)) # df,  2*df is the number of nodes spanning a resonant wavelength
    nElRight=nElAverage # dg is the number of nodes to the right
    if (nNodes%2)==1:
      nElLeft=nElRight
    else:
      nElLeft=nElRight-1
  
    intxrms=numpy.zeros(nz)
    #intxrms=numpy.zeros(nz) # doesn't appear to be used. Should this be magxrms
    magxrms=numpy.zeros(nz) # doesn't appear to be used. Should this be magxrms
    # note zero based index for python, but was probably 1 based for matlab
    for m in range(0,nz):
      lo = m-nElLeft
      #print "lo "+str(lo)
      hi = m+nElRight #
      #print "hi "+str(hi)
      hi = lo+nNodes-2 # weird way of calculating a range if you ask me
      #print "hi "+str(hi) # is one less than the number above it's supposed to look like...
      hi = lo+nElements # seems much more sensible.
      if lo<0:
        lo=0
      if hi>(nz-1):
        hi=nz-1
      intxrms[m]=numpy.sqrt(numpy.mean(numpy.square(h5.root.aperp[xi,yi,lo:hi,component])))
    magxrms=numpy.sqrt(2)*intxrms

    # some reference "wave", which looks more like a line to me
    z2cyclic=(numpy.subtract(zMesh,(4.*numpy.pi*rho*numpy.floor(zMesh/(4.*numpy.pi*rho)))))/(2.*rho)
    # slopy does not appear to be used
    #figure out some sort of instantaneous phase
    invcosx=numpy.arccos(numpy.divide(h5.root.aperp[xi,yi,:,component],numpy.max(magxrms,1.e-99)))
    for a in range(0,nz):
      if numpy.abs(h5.root.aperp[xi,yi,a,component])>magxrms[a]:
        if h5.root.aperp[xi,yi,a,component]>0:
          invcosx[a]=0.
        elif h5.root.aperp[xi,yi,a,component]<0:
          invcosx[a]=numpy.pi
    # new loop determines whether to shift quadrant dependent on slope
    for a in range(0,nz-1):
      if h5.root.aperp[xi,yi,a,component]<h5.root.aperp[xi,yi,(a+1),component]:
        invcosx[a]=(2*numpy.pi)-invcosx[a]
    phasex=numpy.subtract(invcosx,z2cyclic)
    for a in range(0,nz):
      if phasex[a]<0:
        phasex[a]=numpy.pi*2-phasex[a]
    return magxrms, phasex
    
if len(sys.argv) == 4 or len(sys.argv) == 3:
  inputFilename=sys.argv[1]
  rho=numpy.double(sys.argv[2])
  if len(sys.argv) == 4:
    stokesLength=int(sys.argv[3])
  else:
    stokesLength=200
  h5=tables.open_file(inputFilename)
  (nx,ny,nz,nComponents)=h5.root.aperp.shape
  print "rho " + str(rho)
  print "stokes averaging length " + str(stokesLength)
  print "nx: " + str(nx)
  print "ny: " + str(ny)
  print "nz: " + str(nz)
  print numpy.int((ny-1)/8.)
  print numpy.int(7*(ny-1)/8.)+1
  print numpy.int(numpy.ceil((ny-1)/8.))
  print "nComponents: " + str(nComponents)
  
  plt.figure(figsize=(35,35))
  gs = gridspec.GridSpec(7,7)
  count=0
  for yi in range(numpy.int((ny-1)/8.),numpy.int(7*(ny-1)/8.)+1,numpy.int(numpy.ceil((ny-1)/8.))):
    for xi in range(numpy.int((nx-1)/8.),numpy.int(7*(nx-1)/8.)+1,numpy.int(numpy.ceil((nx-1)/8.))):
      plt.subplot(gs[count])
      mymax=max(numpy.max(numpy.abs((h5.root.aperp[xi,yi,:numpy.int(nz/2),0]))),numpy.max(numpy.abs(h5.root.aperp[xi,yi,:numpy.int(nz/2),1])))
      plt.scatter(h5.root.aperp[xi,yi,:(numpy.int(nz/2)),0]/mymax,h5.root.aperp[xi,yi,:(numpy.int(nz/2)),1]/mymax)
      plt.axis([-1.2,1.2,-1.2,1.2])
      plt.title("xi="+str(xi)+" yi="+str(yi)+ " norm={:1.1}".format(mymax))
    #plt.savefig("ExEy-"+str(xi)+"-"+str(yi)+".png")
      count+=1
  plt.savefig("ExEy.png")
  # Now the converted stuff from Lawrence's matlab
  # Now with Lawrence's stuff ported to python
  StokesParams=numpy.zeros((nx,ny,nz,3))
  #output file goes in current directory whether or not input was there.
  outFilename=inputFilename.split(os.sep)[-1].replace('aperp','stokes')
  print outFilename
#  for xi in range(0,nx):
#    for yi in range(0,ny):
# Just look in the middle while testing
  for xi in range(int(14*nx/32),int(18*nx/32)):
    for yi in range(int(14*ny/32),int(18*ny/32)):
      print "xi: "+str(xi)+"  yi: "+str(yi)
      magx,phasex=getMagPhase(h5,rho,xi,yi,0)
      magy,phasey=getMagPhase(h5,rho,xi,yi,1)
      s0=numpy.max(numpy.add(numpy.square(magx),numpy.square(magy)),1.e-99)
      s1=numpy.subtract(numpy.square(magx),numpy.square(magy))
      s2=2*numpy.multiply(numpy.multiply(magx,magy),numpy.cos(numpy.subtract(phasex,phasey)))
      s3=2*numpy.multiply(numpy.multiply(magx,magy),numpy.sin(numpy.subtract(phasex,phasey)))
  # various averages are calculated using stokesLength, but they don't appear to be used.
      StokesParams[xi,yi,:,0]=numpy.divide(s1,s0)
      StokesParams[xi,yi,:,1]=numpy.divide(s2,s0)
      StokesParams[xi,yi,:,2]=numpy.divide(s3,s0)
  #writeFieldOutput3D(StokesParams,filename)
  tables.copy_file(inputFilename,outFilename,overwrite=1)
  h5out=tables.open_file(outFilename,'r+')
  dn='stokes' #dataname - shortening
  h5out.create_array('/',dn,StokesParams)
  h5out.copy_node_attrs('/aperp','/stokes')
  h5out.remove_node('/aperp')
  h5out.root.stokes._v_attrs.vsLabels="P1,P2,P3"
  h5out.close()
  h5.close()
else:
  print "Usage: plotPolarization.py filename rho"
  print "We don't appear to have the correct arguments to proceed"
  
