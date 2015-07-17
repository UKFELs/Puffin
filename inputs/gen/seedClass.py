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
