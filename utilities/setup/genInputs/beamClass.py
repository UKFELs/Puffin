# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

import numpy as np
import fBoolean

class bm:
    """This class defines the electron beam data input file
    creation for Puffin."""
    nbeams = 1
    bftype = 'simple'
    sig = np.array([1., 1., 1., 1., 1., 1.])
    le = np.array([6., 6., 6., 6., 6., 6.])
    nmps  = np.ones(1) * 1
    qmatch = np.ones(1) * 1
    eratio = np.ones(1) * 1
    emitx = np.ones(1) * 1
    emity = np.ones(1) * 1
    alphax = np.ones(1) * 0
    alphay = np.ones(1) * 0
    chirp = np.ones(1) * 0
    bcenz2 = np.ones(1) * 0
    Q = np.ones(1) * 1e-12
    qRoundEj = np.ones(1) * 1
    sigEj = np.ones(1) * 1
    bosc_Mag = np.ones(1) * 0
    bosc_Fr = np.ones(1) * 1
    qmatch = [False]
    trload = 2
    qequixy = [False]
    
    
    def wrbeam(self,f,no):
        
        torf = fBoolean.torf

        f.write('! PUFFIN BEAM FILE\n')
        f.write('!\n')
        f.write('! Describes electron beams for input into puffin. Multiple beams with\n')
        f.write('! different parameters can be used. Please refer to POP-REF for an\n')
        f.write('! explanation of the scaled variables used, such as z2 and p2.\n')
        f.write('!\n')
        f.write('! namelist NBLIST - Number and type of electron beams\n')
        f.write('!\n')
        f.write('! nbeams - number of electron beams\n')
        f.write('! dtype  - Input type - simple, distribution, or macroparticle \n')
        f.write('!\n')
        f.write('! namelist BLIST - SIMPLE BEAM PARAMETERS\n')
        f.write('!========================================================================\n')
        f.write('! sSigmaE - gaussian std dev in each dimension - x, y, z2, px, py, gamma.\n')
        f.write('! sLenE   - Total length of beam modelled in each dimension - x, y, z2, px, py, gamma...\n')
        f.write('! bcenter - Center of beam in z2\n')
        f.write('! iNumElectrons  -  Number of macroparticles in each dimension used to model the beam \n')
        f.write('! sEmit_n - Scaled transverse beam emittance\n')
        f.write('! sQe - Beam charge \n')
        f.write('! gammaf - Ratio of average beam energy to reference beam energy gamma / gamma_r \n')
        f.write('! chirp  - Energy chirp in z2 i.e. dgamma/dz2\n')
        f.write('! mag    - magnitude of energy oscillation on beam\n')
        f.write('! fr     - frequency in z2 of beam energy oscillation\n')
        f.write('! qRndEj_G - Round edge of flat top?\n')
        f.write('! sSigEj_G - Gaussian sigma of tail-off if used\n')
        f.write('! qMatched_A - Automatically match beam to focusing channel??\n')
        f.write('! TrLdMeth - Beam loading method in 5D transverse and energy planes:\n')
        f.write('!                TrLdMeth = 1 for random sequences (default)\n')
        f.write('!                TrLdMeth = 2 for Halton sequences\n')
        f.write('!========================================================================\n')

        f.write('\n')
        f.write('\n')
        f.write('\n')

        f.write('&NBLIST\n')
        f.write('nbeams = ' + '{:d}'.format(self.nbeams) + '\n')
        f.write('dtype = ' + '\'' + self.bftype + '\'' + '\n')
        f.write('/\n')

        f.write('\n')
        f.write('\n')
        f.write('\n')

        f.write('&BLIST\n')
        f.write('sSigmaE = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.sig])  +  '\n')
        f.write('sLenE = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.le])  +  '\n')
        f.write('iNumElectrons = ' + ', '.join(['{:d}'.format(ij) for ij in self.nmps])  +  '\n')
        f.write('sQe = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.Q])  + '\n')
        f.write('bcenter = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.bcenz2]) + '\n')

        f.write('gammaf = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.eratio]) + '\n')
        f.write('chirp = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.chirp]) + '\n')
        f.write('mag = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.bosc_Mag]) + '\n')
        f.write('fr = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.bosc_Fr]) + '\n')
        f.write('emitx = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.emitx]) + '\n')
        f.write('emity = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.emity]) + '\n')
        f.write('alphax = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.alphax]) + '\n')
        f.write('alphay = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.alphay]) + '\n')
        f.write('qRndEj_G = ' + ', '.join([torf(ij) for ij in self.qRoundEj]) + '\n')
        f.write('sSigEj_G = ' + ', '.join(['{:.15E}'.format(ij) for ij in self.sigEj]) + '\n')
        f.write('qMatched_A = ' + ', '.join([torf(ij) for ij in self.qmatch]) + '\n')
        f.write('TrLdMeth = ' + ', '.join(['{:d}'.format(ij) for ij in self.trload]) + '\n')
        f.write('/\n')




