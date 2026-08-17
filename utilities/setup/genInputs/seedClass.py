# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

import fBoolean
import numpy as np

class sd:
    """This class defines the seed radiation input file creation for input
    into Puffin."""

    nseeds = 1
    sftype = 'simple'
    FreqFrac = np.array([1])       # Frequency as a fraction of resonant frequency
    phshift = np.array([0.])
    pkx = np.array([0.])            # Peak of seed in A_x
    pky = np.array([0.])            # Peak of seed in A_y
    ssig = np.array([1., 1., 1.])
    qFlat = [True]
    meanz2 = np.array([0.])
    qFRoundEj = [True]
    sigFEj = np.array([0.5])
    qmatchs2b = [True]

    def wrseed(self,f,no):
        torf = fBoolean.torf

        f.write('! PUFFIN RADIATION SEED FILE\n')
        f.write('!\n')
        f.write('! Describes externally injected radiation seeds for the FEL in Puffin.\n')
        f.write('! Please refer to POP-REF for an explanation of the scaled variables\n')
        f.write('! used, such as xbar, z2 etc.\n')
        f.write('!\n')
        f.write('! namelist NSLIST - Number and type of seed fields\n')
        f.write('!\n')
        f.write('! nbeams - number of injected radiation seeds\n')
        f.write('! dtype  - Input type - choose from \'simple\', for analtyically described')
        f.write('! homogeneous seed or full, direct mesh description \n')
        f.write('!\n')
        f.write('! namelist SLIST - Simple seed parameters\n')
        f.write('!\n')
        f.write('! freqf - Fractional frequency of each seed\n')

        f.write('! ph_sh - Array containing phase shifts (between 0:2pi) for each seed\n')

        f.write('! sA0_X - Array containing scaled magnitudes for each seed in the x-polarization.\n')
        f.write('! sA0_Y - Array containing scaled magnitudes for each seed in the y-polarization.\n')

        f.write('! sSigmaF - gaussian std dev in each dimension - x, y, then z2. If multiple seeds\n')
        f.write('!           are used, then order inputs as *all* std devs in x, then y, then z2.\n')

        f.write('! qFlatTop - Array containing logicals, describing whether to use a flat top profile\n')
        f.write('!           for each seed in the z2 (longitudinal) dimension.\n')

        f.write('! meanZ2 - Array containing mean (center) of each seed in the longitudinal dimension.\n')

        f.write('! qRndFj_G - Array of logicals, one for each seed. If .true., and using \n')
        f.write('!            a flat-top seed, then seed will be rounded off with a Gaussian \n')
        f.write('!            \'tail\' at each end. \n')

        f.write('! sSigFj_G - Array of rms standard deviations for the tails, if used (see qRndFj_G). \n')
        f.write('!            One standard deviation for each seed - the same s.d. is used for each \n')
        f.write('!            end. of the seed \n')

        f.write('! qMatchS_G - Array of logicals, one for each seed. If .true., then the seed rms \n')
        f.write('!            s.d. in x and y will be altered to that of the input beams. \n')

        f.write('! ========================================================================\n')
        f.write('\n')
        f.write('\n')
        f.write('\n')

        f.write('&NSLIST\n')
        f.write('nseeds = ' + '{:d}'.format(self.nseeds) + '\n')
        f.write('dtype = ' + '\'' + self.sftype + '\'' + '\n')
        f.write('/\n')

        f.write('\n')
        f.write('\n')
        f.write('\n')

        f.write('&SLIST\n')
        f.write('freqf = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.FreqFrac])  +  '\n')
        f.write('ph_sh = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.phshift])  +  '\n')
        f.write('sA0_X = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.pkx])  +  '\n')
        f.write('sA0_Y = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.pky])  +  '\n')
        f.write('sSigmaF = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.ssig])  +  '\n')
        f.write('qFlatTop = ' + ', '.join([torf(ij) for ij in self.qFlat]) + '\n')
        f.write('meanZ2 = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.meanz2]) + '\n')
        f.write('qRndFj_G = ' + ', '.join([torf(ij) for ij in self.qFRoundEj]) + '\n')
        f.write('sSigFj_G = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.sigFEj]) + '\n')
        f.write('qMatchS_G = ' + ', '.join([torf(ij) for ij in self.qmatchs2b]) + '\n')

        f.write('/\n')
