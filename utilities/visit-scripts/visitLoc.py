import os
import sys

def visitLoc():
    localVisItDir = "/home/tml/bin/visit/visit2_10_3.linux-x86_64"
    localPythonPackageDir = "/home/tml/bin/visit/visit2_10_3.linux-x86_64/2.10.3/linux-x86_64/lib/site-packages" 
    sys.path.insert(0,localPythonPackageDir)
    return localVisItDir, localPythonPackageDir


#if __name__ == '__main__':
#    pBaseName=sys.argv[1]
#    eDB, iDB, localPowerAllDB = getDBNames.getDBNames(pBaseName)
#    visit.Launch(vdir=localVisItDir)
#    plotPowNorm(localPowerAllDB)
#    visit.OpenGUI()
        