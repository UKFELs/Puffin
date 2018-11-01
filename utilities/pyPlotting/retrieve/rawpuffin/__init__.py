# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This file is part of Puffin, a multi-frequency FEL code absent of the 
averaging / SVEA approximations. This file allows the base reading of data
from the Puffin input files to be used as a single package.
"""

from .readField import readField
from .getIntData import getIntData
from .pfilelists import getFileSlices
from .pfilelists import getIntFileSlices
from .pfilelists import getZData

#import readField
#import filterField
#import getMagPhase

