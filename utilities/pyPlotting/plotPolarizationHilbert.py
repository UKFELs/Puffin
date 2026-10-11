# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Jonathan Smith (Tech-X UK Ltd)
# License: BSD-3-Clause

import numpy,tables,os,sys
from scipy.signal import hilbert
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
#Check input is correct

def readFieldLayout(h5):
    """How the aperp dataset's component axis is laid out.

    Returns (nFieldComp, qAveraged, kz2Carrier).

    Unaveraged, one complex pair holds both polarisations: the real and
    imaginary parts are (A_x, -A_y), so nFieldComp is 1 and the two
    polarisation components are that pair's two halves.

    Averaged, a complex pair is one envelope of one polarisation state, so
    the x and y polarisations are *separate* pairs: components (0,1) are
    Re/Im of Atilde_x and (2,3) are Re/Im of Atilde_y. Reading (0,1) as a
    polarisation pair there takes an envelope's real part and its own
    imaginary part for two polarisations, which they are not.

    Dumps written before these attributes existed are unaveraged by
    construction, so the fallback is the single-pair layout.

    Kept identical to the copy in plotPolarization.py rather than shared:
    importing that module would run its command-line block as a side effect.
    """
    try:
        attrs = h5.root.runInfo._v_attrs
    except Exception:
        return 1, False, 0.
    try:
        nFieldComp = int(attrs.nFieldComp)
    except AttributeError:
        nFieldComp = 1
    try:
        # kz2Carrier is the carrier the stored field is an envelope about,
        # and is written as exactly zero in unaveraged mode.
        kz2Carrier = numpy.double(attrs.kz2Carrier)
    except AttributeError:
        kz2Carrier = 0.
    return nFieldComp, kz2Carrier != 0., kz2Carrier

def getMagPhaseEnvelope(h5,xi,yi,iEnv,kz2Carrier,qNegate=False):
    """Amplitude, phase and instantaneous frequency of envelope iEnv.

    The averaged-mode counterpart of getMagPhase below. There is no resolved
    carrier in the stored field, so there is no analytic signal to form: the
    dump already holds the complex envelope that the Hilbert transform exists
    to recover. Transforming it would treat an envelope as a carrier.

    All three returned quantities keep their unaveraged meanings, by putting
    the carrier back analytically. For A = Re[Atilde exp(i kz2Carrier z2)],
    which is the relation the dump documents, the analytic signal is
    |Atilde| exp(i(phi - arg Atilde)) with carrier phase phi = -kz2Carrier z2.
    So the amplitude is |Atilde| and the instantaneous phase is
    phi - arg Atilde. That is exact here, where the transform is only
    narrowband-accurate and has edge artefacts.

    Sign convention: as in plotPolarization.py. The unaveraged pair is
    (A_x, -A_y), so the phase getMagPhase recovers for its second component
    is pi from A_y's own, which flips P2 and P3. qNegate reproduces that, so
    the two modes report in one convention rather than two.
    """
    zLoBounds=h5.root.globalLimits._v_attrs.vsLowerBounds[2]
    zUpBounds=h5.root.globalLimits._v_attrs.vsUpperBounds[2]
    zNumCells=h5.root.meshScaled._v_attrs.vsNumCells[2]
    zNumNodes=zNumCells+1
    zLen=(zUpBounds-zLoBounds)
    zMesh=numpy.linspace(zLoBounds,zUpBounds,zNumNodes)
    dz=zLen/zNumCells
    reA=numpy.asarray(h5.root.aperp[xi,yi,:,2*iEnv])
    imA=numpy.asarray(h5.root.aperp[xi,yi,:,2*iEnv+1])
    if qNegate:
      reA,imA=-reA,-imA
    if zMesh.size!=reA.size:
      raise ValueError("z2 mesh has %d nodes but the field has %d"
                       % (zMesh.size,reA.size))
    amplitude_envelope=numpy.hypot(reA,imA)
  # Only the envelope's own argument is unwrapped: the carrier phase is
  # continuous by construction, and unwrapping their sum would fail outright,
  # since at one resonant wavelength per cell the carrier advances 2 pi per
  # node and every step is already ambiguous.
    instantaneous_phase=(-kz2Carrier*zMesh)-numpy.unwrap(numpy.arctan2(imA,reA))
    instantaneous_freq = numpy.diff(instantaneous_phase) / (2.0*numpy.pi*dz)
    instantaneous_freq = numpy.hstack((instantaneous_freq,numpy.array([0])))
    return amplitude_envelope,instantaneous_phase,instantaneous_freq

def getMagPhase(h5,xi,yi,component):
    zLoBounds=h5.root.globalLimits._v_attrs.vsLowerBounds[2]
    zUpBounds=h5.root.globalLimits._v_attrs.vsUpperBounds[2]
    zNumCells=h5.root.meshScaled._v_attrs.vsNumCells[2]
    zNumNodes=zNumCells+1
    zLen=(zUpBounds-zLoBounds)
  # nodes is num cells+1
    zMesh=numpy.linspace(zLoBounds,zUpBounds,zNumNodes)
    dz=zLen/zNumCells
    analytic_signal=hilbert(h5.root.aperp[xi,yi,:,component])
    amplitude_envelope = numpy.abs(analytic_signal)    
    instantaneous_phase = numpy.unwrap(numpy.angle(analytic_signal))
  # Get an instantaneous frequency from rate of change of phase, and append 0 to make length correspond
  # strictly should plot zonally instead of nodally, then all would work.    
    instantaneous_freq = numpy.diff(instantaneous_phase) / (2.0*numpy.pi*dz)
    instantaneous_freq = numpy.hstack((instantaneous_freq,numpy.array([0])))
    return amplitude_envelope,instantaneous_phase,instantaneous_freq
    
if len(sys.argv) == 2:
  inputFilename=sys.argv[1]
  h5=tables.open_file(inputFilename)
  (nx,ny,nz,nComponents)=h5.root.aperp.shape
  nFieldComp,qAveraged,kz2Carrier=readFieldLayout(h5)
  print("nx: " + str(nx))
  print("ny: " + str(ny))
  print("nz: " + str(nz))
#  print(numpy.int((ny-1)/8.))
#  print(numpy.int(7*(ny-1)/8.)+1)
#  print(numpy.int(numpy.ceil((ny-1)/8.)))
  print("nComponents: " + str(nComponents))
  print("nFieldComp: " + str(nFieldComp) + ("  (period-averaged)" if qAveraged else ""))

  if qAveraged and nFieldComp < 2:
    # A single averaged envelope carries no polarisation of its own: which
    # state it represents is fixed by the undulator and is not stored, so
    # there is nothing here to resolve. Say so rather than Hilbert
    # transforming an envelope and reporting the result as a polarisation.
    print("")
    print("This dump is period-averaged with a single field envelope. Its")
    print("polarisation state is implied by the undulator rather than stored,")
    print("so it cannot be resolved into components here. Re-run with the")
    print("two-envelope field (nFieldComp = 2) and try again.")
    h5.close()
    sys.exit(1)

  # Note on normalisation in averaged mode: for a planar undulator the stored
  # envelope is the single-envelope Atilde rather than Atilde_x = sqrt(2)
  # Atilde, so magx is low by sqrt(2). Atilde_y is zero in that case, so the
  # normalised P1, P2 and P3 are unaffected by it; s0 is not.

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
  print(outFilename)
  for xi in range(0,nx):
    for yi in range(0,ny):
# Just look in the middle while testing
#  for xi in range(int(14*nx/32),int(18*nx/32)):
#    for yi in range(int(14*ny/32),int(18*ny/32)):
      if xi%16==0:
        if yi%16==0:
          print("xi: "+str(xi)+"  yi: "+str(yi))
      if nFieldComp > 1:
        magx,phasex,freqx=getMagPhaseEnvelope(h5,xi,yi,0,kz2Carrier)
        magy,phasey,freqy=getMagPhaseEnvelope(h5,xi,yi,1,kz2Carrier,qNegate=True)
      else:
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
  h5out.create_group('/','s0','normalization of stokes parameters')
  h5out.root.s0._v_attrs.vsType="vsVars"
  h5out.root.s0._v_attrs.s0="max((sqr(magx)+sqr(magy)),1.e-99)"
  h5out.create_group('/','P1','Stokes parameter P1')
  h5out.root.P1._v_attrs.vsType="vsVars"
  h5out.root.P1._v_attrs.P1="(sqr(magx)-sqr(magy))/s0"
  h5out.create_group('/','P2','Stokes parameter P2')
  h5out.root.P2._v_attrs.vsType="vsVars"
  h5out.root.P2._v_attrs.P2="2*magx*magy*cos(phasex-phasey)/s0"
  h5out.create_group('/','P3','Stokes parameter P3')
  h5out.root.P3._v_attrs.vsType="vsVars"
  h5out.root.P3._v_attrs.P3="2*magx*magy*sin(phasex-phasey)/s0"
  h5out.close()
  h5.close()
else:
  print("Usage: plotPolarization.py filename")
  print("We don't appear to have the correct arguments to proceed")
  
