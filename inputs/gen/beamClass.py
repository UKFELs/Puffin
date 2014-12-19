import math
import fBoolean

class bm:
    """This class defines the electron beam beam data."""
    sigx  = 0.1
    sigy  = 0.1
    sigz2 = 1E8
    sigpx = 0.5
    sigpy = 0.5
    sigp2 = 1E8
    lex = 0.6
    ley = 0.6
    lez2 = 1
    lepx = 3
    lepy = 3
    lep2 = 0.001
    nmpx  = 1
    nmpy  = 1
    nmpz2 = 5
    nmppx = 3
    nmppy = 3
    nmpp2 = 1
    qmatch = 1
    eratio = 1
    emit_bar = 1
    chirp = 0
    bcenz2 = 0
    Q = 1e-12
    qRoundEj = 1
    sigEj = 1
    
    
    def wrbeam(self,f,no):
        torf = fBoolean.torf
        f.write('========================================================================\n')
        f.write('BEAM ' + '{:d}'.format(int(no)) + ':-\n')
        f.write('\n')
        f.write('STANDARD DEVIATION OF BEAM IN EACH DIMENSION (=1E8 FOR FLAT TOP)   - REAL\n')
        f.write('\n')
        f.write('{:<24.15E}'.format(self.sigx) + 'SIGX       (overwritten for matched beam)\n')
        f.write('{:<24.15E}'.format(self.sigy) + 'SIGY       (overwritten for matched beam)\n')
        f.write('{:<24.15E}'.format(self.sigz2) + 'SIGZ2\n')
        f.write('{:<24.15E}'.format(self.sigpx) + 'SIGPX      (overwritten for matched beam)\n')
        f.write('{:<24.15E}'.format(self.sigpy) + 'SIGPY      (overwritten for matched beam)\n')
        f.write('{:<24.15E}'.format(self.sigp2) + 'SIGGAMMA\n')
        f.write('\n')
        f.write('TOTAL MODELLED LENGTH OF BEAM IN EACH DIMENSION    - REAL\n')
        f.write('\n')
        f.write('{:<24.15E}'.format(self.lex)  + 'LENX       (overwritten for matched beam)\n')
        f.write('{:<24.15E}'.format(self.ley)  + 'LENY       (overwritten for matched beam)\n')
        f.write('{:<24.15E}'.format(self.lez2)  + 'LENZ2\n')
        f.write('{:<24.15E}'.format(self.lepx)  + 'LENPX      (overwritten for matched beam)\n')
        f.write('{:<24.15E}'.format(self.lepy)  + 'LENPY      (overwritten for matched beam)\n')
        f.write('{:<24.15E}'.format(self.lep2)  + 'LENGAMMA\n')
        f.write('\n')
        f.write('NUMBER OF MACROPARTICLES TO MODEL BEAM IN EACH DIMENSION (EQUISPACED BEFORE NOISE IS ADDED, IF USED)   - INTEGER\n')
        f.write('\n')
        f.write('{:<24d}'.format(int(math.ceil(self.nmpx)))    + 'NUMX\n')
        f.write('{:<24d}'.format(int(math.ceil(self.nmpy)))    + 'NUMY\n')
        f.write('{:<24d}'.format(int(math.ceil(self.nmpz2)))   + 'NUMZ2\n')
        f.write('{:<24d}'.format(int(math.ceil(self.nmppx)))   + 'NUMPX\n')
        f.write('{:<24d}'.format(int(math.ceil(self.nmppy)))   + 'NUMPY\n')
        f.write('{:<24d}'.format(int(math.ceil(self.nmpp2)))   + 'NUMGAMMA\n')
        f.write('\n')
        f.write('BEAM PARAMETERS\n')
        f.write('\n')
        f.write('{:<24}'.format(torf(self.qmatch)) + 'Match beam to undulator?\n')
        f.write('{:<24.15E}'.format(self.eratio)  + 'Ratio of average beam energy to resonant beam energy gamma / gamma_r\n')
        f.write('{:<24.15E}'.format(self.emit_bar)  + 'Scaled emittance (bar{epsilon})\n')
        f.write('{:<24.15E}'.format(self.chirp) + 'Energy chirp in z2 i.e. dgamma/dz2\n')
        f.write('{:<24.15E}'.format(self.bcenz2) + 'Center of beam in z2\n')
        f.write('{:<24}'.format(torf(self.qRoundEj)) + 'Round edge if flat top?\n')
        f.write('{:<24.15E}'.format(self.sigEj) + 'Sigma or rounded edge in z2 (if round edged flat top)\n')
        f.write('{:<24.15E}'.format(self.Q) + 'Charge in coulombs\n')
        f.write('========================================================================\n')
        f.write('\n')
