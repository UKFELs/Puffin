
import numpy as np
import SUMatch as TMT
from puffDataClass import puffData
from undulator import undulator

##### FOR NEW OUTER DRIVER:

puffVars = puffData()

# Initialize CLARA base parameters

puffVars.aw = 0.8745*np.sqrt(2.)   # The PEAK!!!
puffVars.gamma0 = 489.237
puffVars.lw = 0.025
puffVars.rho = 0.005
puffVars.undtype = 'planepole'
puffVars.ux = 0.
puffVars.uy = 1.

#    emitx = 1.022e-9
#    emity = 1.022e-9

# Generate the rest of the Puffin scaling from the above

puffVars.genParams()  # generate rest of scaled params

undmod = undulator(puffVars, undtype = 'planepole', Nw = 26)

qf = 3.22 * puffVars.lg
DL = 24. * puffVars.lw # Drift lengths

fnamein = 'short_CLARA_001_A2SU.h5'

TMT.SU2Matched(fnamein, puffVars, undmod, qf, DL)
