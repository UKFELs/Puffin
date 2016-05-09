#!/usr/bin/python
import sys,os,sdds
if len(sys.argv)==2:
   fname=sys.argv[1]
   print 'Processing file:', fname
else:
   print 'Usage: plotDist <file name> \n'
   sys.exit(1)   

#os.system("sddsplot -columnName=z,q %s -graphic=dot" %fname)
os.system("sddsplot -columnName=z,pz %s -graphic=dot" %fname)
os.system("sddsplot -columnName=z,gamma %s -graphic=dot" %fname)
os.system("sddsplot -columnName=z,x %s -graphic=dot" %fname)
os.system("sddsplot -columnName=z,px %s -graphic=dot" %fname)
os.system("sddsplot -columnName=z,y %s -graphic=dot" %fname)
os.system("sddsplot -columnName=z,py %s -graphic=dot" %fname)