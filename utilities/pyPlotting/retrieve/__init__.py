# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""
This file is part of Puffin, a multi-frequency FEL code absent of the 
averaging / SVEA approximations. This file allows the Puffin data and data
file classes to be used as a package.
"""

from .process import filterField
from .getPow import getPow
from .getMagPhase import getMagPhase
from .rawpuffin import getIntData
from .rawpuffin import readField

#import readField
#import filterField
#import getMagPhase

