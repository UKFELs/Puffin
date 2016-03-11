import numpy,tables,matplotlib
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
h5=tables.open_file('/gpfs/stfc/local/HCP084/bwm06/nxt14-bwm06/fig7a/test_aperp_C_1200.h5')
plt.figure(figsize=(35,35))
gs = gridspec.GridSpec(7,7)
count=0
for yi in range(8,57,8):
  for xi in range(8,57,8):
    plt.subplot(gs[count])
    mymax=max(numpy.max(numpy.abs((h5.root.aperp[xi,yi,:(numpy.floor(h5.root.aperp.shape[2]/2)),0]))),numpy.max(numpy.abs(h5.root.aperp[xi,yi,:(numpy.floor(h5.root.aperp.shape[2]/2)),1])))
    plt.scatter(h5.root.aperp[xi,yi,:(numpy.floor(h5.root.aperp.shape[2]/2)),0]/mymax,h5.root.aperp[xi,yi,:(numpy.floor(h5.root.aperp.shape[2]/2)),1]/mymax)
    plt.axis([-1.2,1.2,-1.2,1.2])
    plt.title("xi="+str(xi)+" yi="+str(yi)+ " norm={:1.1}".format(mymax))
    #plt.savefig("ExEy-"+str(xi)+"-"+str(yi)+".png")
    count+=1
plt.savefig("ExEy.png")