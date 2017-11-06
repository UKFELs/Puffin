# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

import math
import fBoolean

class sd:
    """This class defines the seed data."""
    FreqFrac = 1       # Frequency as a fraction of resonant frequency
    pkx = 0            # Peak of seed in A_x
    pky = 0            # Peak of seed in A_y
    sigx = 1
    sigy = 1
    sigz2 = 1
    qFlat = 1
    meanz2 = 1        
    qFRoundEj = 1
    sigFEj = 0.5

    def wrseed(self,f,no):
        torf = fBoolean.torf

        f.write('! PUFFIN RADIATION SEED FILE\n')
        f.write('!\n')
        f.write('!Describes externally injected radiation seeds for the FEL in Puffin.\n')
        f.write('!Please refer to POP-REF for an explanation of the scaled variables\n')
        f.write('!used, such as xbar, z2 etc.\n')
        f.write('!\n')
        f.write('! namelist NSLIST - Number and type of seed field\n')
        f.write('!\n')
        f.write('! nbeams - number of electron beams\n')
        f.write('! dtype  - Input type - simple or full, direct mesh description \n')
        f.write('!\n')
        f.write('! namelist SLIST - Simple seed parameters\n')
        f.write('!\n')
        f.write('! sSigmaE - gaussian std dev in each dimension - x, y, z2, px, py, gamma, then for additional beams\n')
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

        f.write('========================================================================\n')
        f.write('SEED ' + '{:d}'.format(int(no)) + ':-\n')
        f.write('\n')
        f.write('{:<24.15E}'.format(self.FreqFrac)  + 'FreqFrac            Ratio of seed to resonant frequency i.e. omega_s / omega_r\n')
        f.write('{:<24.15E}'.format(self.pkx)       + 'A0_X                Initial peak field value (real, also the x-polarized field)\n')
        f.write('{:<24.15E}'.format(self.pky)       + 'A0_Y                Initial peak field value (imaginary, also the y-polarized field)\n')
        f.write('{:<24.15E}'.format(self.sigx)      + 'SigmaX              Seed field sigma in x direction (Calculated in code for matched beam) =1E8 for flat top\n')
        f.write('{:<24.15E}'.format(self.sigy)      + 'SigmaY              Seed field sigma in y direction (Calculated in code for matched beam) =1E8 for flat top\n')
        f.write('{:<24.15E}'.format(self.sigz2)     + 'SigmaZ2             Seed field sigma in z2 direction =1E8 for flat top\n')
        f.write('{:<24}'.format(torf(self.qFlat))   + 'qFlatTop            =.TRUE. if flat top seed, else gaussian is assumed\n')
        f.write('{:<24.15E}'.format(self.meanz2)    + 'MeanZ2              Mean or center position of seed in Z2\n')
        f.write('{:<24}'.format(torf(self.qFRoundEj))    + 'qRoundEj            Round the edge of the seed envelope if flat top?\n')
        f.write('{:<24.15E}'.format(self.sigFEj)    + 'sigma_ej            Sigma of the rounded edge, if above is true\n')
        f.write('========================================================================\n')
        f.write('\n')
