import os
import sys
import visitLoc

localVisItDir, localPythonPackageDir = visitLoc.visitLoc()
sys.path.insert(0,localPythonPackageDir)

import visit


def getDBNames(pBaseName):

  currDir = os.getcwd()

# Database suffixes for power, electron macroparticle and integrated data respectively

  pFileSffx = "_integrated_all.vsh5"
  eFileSffx = "_electrons_* database"
  iFileSffx = "_integrated_* database"


# Full database paths

  if os.name == 'nt':  # If OS is windows...
    eDB1 = "localhost:" + currDir + "\\" + pBaseName + eFileSffx
    iDB1 = "localhost:" + currDir + "\\" + pBaseName + iFileSffx
    localPowerAllDB1 = currDir + "\\" + pBaseName + pFileSffx
  else:   # else assuming linux!!
    eDB1 = "localhost:" + currDir + "/" + pBaseName + eFileSffx
    iDB1 = "localhost:" + currDir + "/" + pBaseName + iFileSffx
    localPowerAllDB1 = currDir + "/" + pBaseName + pFileSffx

  return eDB1, iDB1, localPowerAllDB1