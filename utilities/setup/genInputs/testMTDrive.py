import sys
import numpy as np
import SUMatch as TMT
from puffDataClass import puffData
from undulator import undulator

##### FOR NEW OUTER DRIVER:
def MTdriver(fnamein):

    puffVars = puffData()

# Initialize CLARA base parameters

    puffVars.aw = 0.8745*np.sqrt(2.)   # The PEAK!!!
    puffVars.gamma0 = 469.0
    puffVars.lw = 0.025
    puffVars.rho = 0.005
    puffVars.undtype = ''
    puffVars.ux = 1.
    puffVars.uy = 0.

#    emitx = 1.022e-9
#    emity = 1.022e-9

# Generate the rest of the Puffin scaling from the above

    puffVars.genParams()  # generate rest of scaled params

    undmod = undulator(puffVars, undtype = '', Nw = 26, ux = 1., uy = 0.)

    qf = 3.14 * puffVars.lg
    DL = 24. * puffVars.lw # Drift lengths

    #fnamein = 'short_CLARA_001_A2SU.h5'

    TMT.SU2Matched(fnamein, puffVars, undmod, qf, DL)

####    For matching a segment of the beam 'by eye', specify these
####    as the Twis parameters in the region you want matched

#    twx = [7.5e-10, 13., 3.]
#    twy = [7.5e-10, 4., -5.]

#    TMT.SU2Matched(fnamein, puffVars, undmod, qf, DL, twx1 = twx, twy1 = twy)


if __name__ == '__main__':

    if len(sys.argv)==2:
        fname = sys.argv[1]
        print('Processing file:', fname)
        MTdriver(fname)
    else:
        print('Usage: SU2Puffin <FileName> \n')
        sys.exit(1)
