# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell & Jonathan Smith (Tech-X UK Ltd)
# License: BSD-3-Clause

"""
Defines the names of the datasets from Puffin from the Puffin input basename.
"""

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
  fFileSffx = "_aperp_C_* database"


# Full database paths

  if os.name == 'nt':  # If OS is windows...
    eDB1 = "localhost:" + currDir + "\\" + pBaseName + eFileSffx
    iDB1 = "localhost:" + currDir + "\\" + pBaseName + iFileSffx
    fDB1 = "localhost:" + currDir + "\\" + pBaseName + fFileSffx
    localPowerAllDB1 = currDir + "\\" + pBaseName + pFileSffx
  else:   # else assuming linux!!
    eDB1 = "localhost:" + currDir + "/" + pBaseName + eFileSffx
    iDB1 = "localhost:" + currDir + "/" + pBaseName + iFileSffx
    fDB1 = "localhost:" + currDir + "/" + pBaseName + fFileSffx
    localPowerAllDB1 = currDir + "/" + pBaseName + pFileSffx

  return eDB1, fDB1, iDB1, localPowerAllDB1
