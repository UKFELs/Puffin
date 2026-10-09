# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Jonathan Smith (Tech-X UK Ltd)
# License: BSD-3-Clause

import numpy,tables,matplotlib,os,sys
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
#Check input is correct

def readFieldLayout(h5):
    """How the aperp dataset's component axis is laid out.

    Returns (nFieldComp, qAveraged).

    Unaveraged, one complex pair holds both polarisations: the real and
    imaginary parts are (A_x, -A_y), so nFieldComp is 1 and the two
    polarisation components are that pair's two halves.

    Averaged, a complex pair is one envelope of one polarisation state, so
    the x and y polarisations are *separate* pairs: components (0,1) are
    Re/Im of Atilde_x and (2,3) are Re/Im of Atilde_y. Reading (0,1) as a
    polarisation pair there compares an envelope's real part with its own
    imaginary part, which is not a polarisation at all.

    Dumps written before these attributes existed are unaveraged by
    construction, so the fallback is the single-pair layout.
    """
    try:
        attrs = h5.root.runInfo._v_attrs
    except Exception:
        return 1, False
    try:
        nFieldComp = int(attrs.nFieldComp)
    except AttributeError:
        nFieldComp = 1
    try:
        # kz2Carrier is the carrier the stored field is an envelope about,
        # and is written as exactly zero in unaveraged mode.
        qAveraged = numpy.double(attrs.kz2Carrier) != 0.
    except AttributeError:
        qAveraged = False
    return nFieldComp, qAveraged

def getMagPhaseEnvelope(h5,xi,yi,iEnv,qNegate=False):
    """Amplitude and carrier phase of envelope iEnv, read straight off the dump.

    In averaged mode the dump already holds what getMagPhase works to
    recover: a complex envelope whose modulus is the amplitude and whose
    argument is the phase relative to the carrier. There is no resolved
    carrier in the stored field to average over, so demodulating it would be
    both unnecessary and wrong.

    Sign convention: the unaveraged dump's complex pair is (A_x, -A_y), so
    the phase getMagPhase returns for its second component is pi away from
    A_y's own phase, which flips the sign of Stokes s2 and s3. qNegate
    reproduces that here, so the averaged and unaveraged paths report Stokes
    parameters in one convention rather than two. It is the stored
    convention that is odd, not this.
    """
    reA=numpy.asarray(h5.root.aperp[xi,yi,:,2*iEnv])
    imA=numpy.asarray(h5.root.aperp[xi,yi,:,2*iEnv+1])
    if qNegate:
      reA,imA=-reA,-imA
    mag=numpy.hypot(reA,imA)
    # A = Re[Atilde exp(-i phi)] = mag cos(phi - arg Atilde), against
    # getMagPhase's A = mag cos(phi + phase), so phase = -arg Atilde.
    phase=numpy.mod(-numpy.arctan2(imA,reA),2.*numpy.pi)
    return mag, phase

def polarisationSamples(h5,xi,yi,izHi,nFieldComp,nPhase=24):
    """(A_x, -A_y) pairs tracing out the polarisation ellipse.

    Unaveraged, the stored field is the instantaneous one, so the pair is
    read off directly and the carrier's own advance through z2 traces the
    ellipse as it goes.

    Averaged, the carrier has been divided out and a single z2 node can span
    a whole resonant wavelength, so advancing through z2 no longer sweeps the
    carrier phase and would trace nothing. The ellipse is recovered instead
    by putting the carrier back analytically, A_x = Re[Atilde_x exp(-i phi)]
    and likewise for y, sampled at nPhase phases per node.

    Both branches return the stored (A_x, -A_y) convention, so the averaged
    plot is laid out the same way round as the unaveraged one.
    """
    if nFieldComp < 2:
      ax=numpy.asarray(h5.root.aperp[xi,yi,:izHi,0])
      ay=numpy.asarray(h5.root.aperp[xi,yi,:izHi,1])
      return ax, ay

    reX=numpy.asarray(h5.root.aperp[xi,yi,:izHi,0])
    imX=numpy.asarray(h5.root.aperp[xi,yi,:izHi,1])
    # negated to give -A_y, matching the unaveraged dump's convention
    reY=-numpy.asarray(h5.root.aperp[xi,yi,:izHi,2])
    imY=-numpy.asarray(h5.root.aperp[xi,yi,:izHi,3])
    phi=numpy.linspace(0.,2.*numpy.pi,nPhase,endpoint=False)
    cosPhi,sinPhi=numpy.cos(phi),numpy.sin(phi)
    ax=numpy.outer(reX,cosPhi)+numpy.outer(imX,sinPhi)
    ay=numpy.outer(reY,cosPhi)+numpy.outer(imY,sinPhi)
    return ax.ravel(), ay.ravel()

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
    nElements=int(numpy.round(wavelength/dz)) # in our 4 rho section for making an average, nel
    nNodes=nElements+1 # nnl
    nElAverage=int(numpy.floor((nNodes)/2.)) # df,  2*df is the number of nodes spanning a resonant wavelength
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
    invcosx=numpy.arccos(numpy.divide(h5.root.aperp[xi,yi,:,component],numpy.maximum(magxrms,1.e-99)))
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
  nFieldComp,qAveraged=readFieldLayout(h5)
  print("rho " + str(rho))
  print("stokes averaging length " + str(stokesLength))
  print("nx: " + str(nx))
  print("ny: " + str(ny))
  print("nz: " + str(nz))
  print(int((ny-1)/8.))
  print(int(7*(ny-1)/8.)+1)
  print(int(numpy.ceil((ny-1)/8.)))
  print("nComponents: " + str(nComponents))
  print("nFieldComp: " + str(nFieldComp) + ("  (period-averaged)" if qAveraged else ""))

  if qAveraged and nFieldComp < 2:
    # A single averaged envelope carries no polarisation of its own: which
    # state it represents is fixed by the undulator and is not stored, so
    # there is nothing here to resolve. Say so rather than plotting an
    # envelope's real part against its imaginary part.
    print("")
    print("This dump is period-averaged with a single field envelope. Its")
    print("polarisation state is implied by the undulator rather than stored,")
    print("so Stokes parameters cannot be recovered from it. Re-run with the")
    print("two-envelope field (nFieldComp = 2) and try again.")
    h5.close()
    sys.exit(1)

  # Note on normalisation in averaged mode: for a planar undulator the stored
  # envelope is the single-envelope Atilde rather than Atilde_x = sqrt(2)
  # Atilde, so |Atilde_x| read here is low by sqrt(2). Atilde_y is zero in
  # that case, so neither the ellipse (normalised below) nor the normalised
  # Stokes parameters are affected by it.

  plt.figure(figsize=(35,35))
  gs = gridspec.GridSpec(7,7)
  count=0
  for yi in range(int((ny-1)/8.),int(7*(ny-1)/8.)+1,int(numpy.ceil((ny-1)/8.))):
    for xi in range(int((nx-1)/8.),int(7*(nx-1)/8.)+1,int(numpy.ceil((nx-1)/8.))):
      plt.subplot(gs[count])
      ax,ay=polarisationSamples(h5,xi,yi,int(nz/2),nFieldComp)
      mymax=max(numpy.max(numpy.abs(ax)),numpy.max(numpy.abs(ay)))
      plt.scatter(ax/mymax,ay/mymax)
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
  print(outFilename)
#  for xi in range(0,nx):
#    for yi in range(0,ny):
# Just look in the middle while testing
  for xi in range(int(14*nx/32),int(18*nx/32)):
    for yi in range(int(14*ny/32),int(18*ny/32)):
      print("xi: "+str(xi)+"  yi: "+str(yi))
      if nFieldComp > 1:
        magx,phasex=getMagPhaseEnvelope(h5,xi,yi,0)
        magy,phasey=getMagPhaseEnvelope(h5,xi,yi,1,qNegate=True)
      else:
        magx,phasex=getMagPhase(h5,rho,xi,yi,0)
        magy,phasey=getMagPhase(h5,rho,xi,yi,1)
      s0=numpy.maximum(numpy.add(numpy.square(magx),numpy.square(magy)),1.e-99)
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
  print("Usage: plotPolarization.py filename rho")
  print("We don't appear to have the correct arguments to proceed")

