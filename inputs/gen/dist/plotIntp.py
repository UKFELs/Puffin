#!/usr/bin/python
import sys,os,sdds
if len(sys.argv)==2:
   fname=sys.argv[1]
   print 'Processing file:', fname
else:
   print 'Usage: plotIntp <file name> \n'
   sys.exit(1)   

sig_gammaSym   = "'$gs$bg'"

os.system("sddsplot -columnName=tMean,I %s" %fname)
os.system("sddsplot -columnName=zMean,IoA %s" %fname)
os.system("sddsplot -columnName=tMean,gammaMean %s" %fname)
os.system("sddsplot -columnName=tMean,gammaStDev -yLabel=%s %s" %(sig_gammaSym,fname))
os.system("sddsplot -columnName=tMean,rStDev %s" %fname)
os.system("sddsplot -columnName=tMean,emitnrms %s" %fname)
#os.system("sddsplot -columnName=zMean,emitntrrms %s" %fname)
