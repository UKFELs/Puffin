#@ifn input file name, from command line
#@ifh input file handle
import numpy,sys,tables,re
c=2.998e8
ifn=sys.argv[1]
print "reading in "+str(ifn)
ifh=open(ifn,'r')
inDataAsText=ifh.read()
#numPtcls=inDataAsText.count('\n')
ptclStrings=inDataAsText.split('\n')
pdata=numpy.zeros((len(ptclStrings),6))
failIndices=[]
for ptclIndex in range(0,len(ptclStrings)):
  try:
    pdata[ptclIndex,:]=numpy.array((re.split("\s+",ptclStrings[ptclIndex])[0:6])).astype(numpy.float)    
  except:
    print "Problem allocating data to array, expect due to lack of data in row"
    failIndices.append(ptclIndex)
  if ptclIndex%1000==0:
    print ptclIndex
# HACK! - get rid of just last index
print "failing at indices: "+str(failIndices)
pdata=pdata[0:-1]
#for fixed weight particles oriented in x
sx=numpy.sum(pdata[:,0])
sy=numpy.sum(pdata[:,1])
sz=numpy.sum(pdata[:,2])
spx=numpy.sum(pdata[:,3])
spy=numpy.sum(pdata[:,4])
spz=numpy.sum(pdata[:,5])
sx2=numpy.sum(numpy.square(pdata[:,0]))
sy2=numpy.sum(numpy.square(pdata[:,1]))
sz2=numpy.sum(numpy.square(pdata[:,2]))
spx2=numpy.sum(numpy.square(pdata[:,3]))
spy2=numpy.sum(numpy.square(pdata[:,4]))
spz2=numpy.sum(numpy.square(pdata[:,5]))
n=numpy.shape(pdata[:,0])[0]
sxpx=numpy.sum(numpy.multiply(pdata[:,0],pdata[:,3]))
sypy=numpy.sum(numpy.multiply(pdata[:,1],pdata[:,4]))
szpz=numpy.sum(numpy.multiply(pdata[:,2],pdata[:,5]))
mx=(sx2/n)-numpy.square(sx/n)
my=(sy2/n)-numpy.square(sy/n)
mz=(sz2/n)-numpy.square(sz/n)
mpx=(spx2/n)-numpy.square(spx/n)
mpy=(spy2/n)-numpy.square(spy/n)
mpz=(spz2/n)-numpy.square(spz/n)
mxpx=(sxpx/n)-(sx*spx/n/n)
mypy=(sypy/n)-(sy*spy/n/n)
mzpz=(szpz/n)-(sz*spz/n/n)
eps_n_x=numpy.sqrt(mx*mpx-mxpx*mxpx)/c
eps_n_y=numpy.sqrt(my*mpy-mypy*mypy)/c
eps_n_z=numpy.sqrt(mz*mpz-mzpz*mzpz)/c
#now assume beam is in x, so just calc betay, betaz
print "mpx:"+str(mpx) + "  sqrt(mpx): "+str(numpy.sqrt(mpx))
#beta=<x^2>/epsilon(non-norm)=<x^2>*lorentzgamma*lorentzbeta/epsilon(norm)
beta_y=my*spx/(n*eps_n_y)
beta_z=mz*spx/(n*eps_n_z)
#alpha=-<xx'>/eps(non-norm)=-<xx'>*lorentzgamma*lorentzbeta/epsilon(norm)
alpha_y=mypy/(c*eps_n_y)
alpha_z=mzpz/(c*eps_n_z)
print "eps_n_x"+str(eps_n_x)
print "eps_n_y"+str(eps_n_y)
print "eps_n_z"+str(eps_n_z)
print "beta_y"+str(beta_y)
print "beta_z"+str(beta_z)
print "alpha_y"+str(alpha_y)
print "alpha_z"+str(alpha_z)
#Now try doing just with the primes
yp=numpy.divide(pdata[:,4],pdata[:,3])
zp=numpy.divide(pdata[:,5],pdata[:,3])

def oldEmittance2D(x,px,w):
  dx=numpy.subtract(x,numpy.average(x,weights=w))
  dxp=numpy.subtract(px,numpy.average(px,weights=w))
  c11 = numpy.average(numpy.multiply(dx,dx), weights=w)
  c12 = numpy.average(numpy.multiply(dx,dxp), weights=w)
  c22 = numpy.average(numpy.multiply(dxp,dxp),weights=w)
  if verbose:
    print "c11:"+str(c11)
    print "c12:"+str(c12)
    print "c22:"+str(c22)
  covMatrix = numpy.matrix([ [c11,c12], [c12,c22] ])
  sqEmittance = la.det(covMatrix)
  return numpy.sqrt(sqEmittance)

#numpy.sum(pdata[numpy.where(pdata[:,0]
