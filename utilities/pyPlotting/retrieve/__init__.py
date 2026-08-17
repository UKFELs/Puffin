# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This file is part of Puffin, a multi-frequency FEL code absent of the 
averaging / SVEA approximations. This file defines the package for retrieving
data from the Puffin output files.
"""

from .process import filterField
from .getPow import getPow
from .getPowFromInt import getPowFromInt
from .getMagPhase import getMagPhase
from .getEnFromInt import getEnFromInt
from .rawpuffin import getIntData
from .rawpuffin import readField
from .rawpuffin import getFileSlices
from .rawpuffin import getIntFileSlices
from .rawpuffin import getZData

#import readField
#import filterField
#import getMagPhase

