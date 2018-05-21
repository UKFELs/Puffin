import tables

def readSUF(fname):
    f=tables.open_file(fname,'r')
    MPs=f.root.Particles.read()
    f.close()
    return MPs

def writeSUF(fname, MPs):

    output_file=tables.open_file(fname ,'w')

# Particle data:

    ParticleGroup=output_file.create_array('/','Particles', MPs)

# VizSchema metadata    
    
    boundsGroup=output_file.create_group('/','globalGridGlobalLimits','')
    boundsGroup._v_attrs.vsType='limits'
    boundsGroup._v_attrs.vsKind='Cartesian'
    timeGroup=output_file.create_group('/','time','time')
    timeGroup._v_attrs.vsType='time'
    ParticleGroup._v_attrs.vsType='variableWithMesh'
    ParticleGroup._v_attrs.vsTimeGroup='time'
    ParticleGroup._v_attrs.vsNumSpatialDims = 3
    ParticleGroup._v_attrs.vsLimits='globalGridGlobalLimits'
    ParticleGroup._v_attrs.vsLabels='x,px,y,py,z,pz,NE'
    output_file.close()
    