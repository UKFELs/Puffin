import tables
# import physConst

def readSUF(fname):
    f=tables.open_file(fname,'r')
    Electrons=f.root.Particles.read()
    
# Assign the data to arrays

    x = Electrons[:,0]
    px = Electrons[:,1]
    y = Electrons[:,2]
    py = Electrons[:,3]
    z = Electrons[:,4]
    pz = Electrons[:,5]
    wt = Electrons[:,6]
    
    return x, px, y, py, z, pz, wt
