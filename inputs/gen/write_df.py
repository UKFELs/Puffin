# Copyright (c) 2012-2017, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause
#
# Python script containing the code to write the 3 input files for 
# Puffin.
#
# The three files are:
#
#   1) The main input file, describing free parameters and the 
#      integration sampling
#
#   2) The beam input file, which can describe multiple electron 
#      beams if necessary
#
#   3) The seed input file, which can describe multiple seeds
#
# This approach is (hopefully) quite flexible, allowing Puffin to 
# generate a simple, more conventional FEL, or a more esoteric 
# configuration with multiple seeds and electron beams, with different 
# powers, frequencies, energies, and distributions.
#
# In addition, for more fun, a lattice file may be constructed to 
# describe a series of undulator modules seperated by chicane/slippage 
# sections. The name of this lattice file should be entered in the 
# 'lattfile' variable in the input file. Each line of the lattice file 
# contains 4 numbers describing a complete undulator-chicane pair, 
# in the following format:
#
# Nw   Nc  aw_fact   stepsize
#
# where 
#
# Nw            is the number of undulator periods
#
# Nc            is the length of the chicane slippage section
#               expressed as the number of resonant wavelengths
#
# aw_f          is the undulator parameter expressed as a fraction
#               of the aw in the input file. So if aw_0 is the aw
#               given in the input file, then the undulator 
#               parameter for this module is aw = aw_f * aw_0.
#
# stepsize      is the stepsize used for the numerical integration
#               of this undulator section. The mesh describing the
#               radiation field does not alter as the undulator
#               tuning changes, but a larger aw wil cause the beam
#               to propagate more quickly in the radiation frame
#               i.e. the beam slips more quickly behind the radiation
#               field. This option will be necessary to stop the beam
#               skipping over nodes, see e.g. the Courant number.
#
# Also note that if aw is decreased in an undulator module you should
# ensure the radiation mesh is fine enough to model the higher frequency
# content which will arise.
#
# The strength of the dispersive chicanes can be given in the 'Dfact' 
# variable in the main input file. Currently, only this one strength 
# factor may be specified which will be shared by all chicanes.
# 
# An undulator taper can also be added in the main input file.
# Presently, if a lattice file is used, this taper will be applied
# to ALL undulator modules, and the taper will be applied to the
# undulator parameter of each module. Only a linear taper may be
# supplied.
# 
# - Lawrence Campbell
#   University of Strathclyde
#   July 2013

import math
import beamClass
import seedClass
import fBoolean

torf = fBoolean.torf

pow = math.pow
pi = math.pi
sqrt = math.sqrt
log = math.log
bm = beamClass.bm
sd = seedClass.sd

# Physical constants

m_e = 9.109e-31
q_e = 1.602e-19
eps_0 = 8.85e-12
c = 2.997924e8

# File names for input files

inputfile = 'example.in'
beamfile = 'beam_file.in'
seedfile = 'seed_file.in'

nbeams = 1
nseeds = 1

### Sampling

elms_per_wave = 20         # Field nodes per resonant wavelength 
emps_per_wave = 24         # Electron macroparticles per resonant wavelength
steps_per_per = 100        # should be roughly 4-5* elms_per_wave

# Undulator and beam parameters
# Below similar to CLARA parameters

E = 250e6            # Beam energy
gamma = E / ( (m_e * pow(c,2) / q_e) ) # Rel. factor
aw = 1.0               # rms wiggler parameter
emit = 1e-6 / gamma       # Unnormalised Emittance
lambda_w = 0.027        # Undulator period
N_w = 200              # Number of undulator periods
# gamma = 100.0          # Relativistic factor
ff = math.sqrt(2)      # Focus factor
Q = 0.25e-9               # Charge
qFlatTopZ2 = 1         # =1 if flat top current profile, else gaussian.
qHardEdgeX = 0         # =1 if disk (circle) in transverse plane, else gaussian.
qRoundZ2 = 1           # If rounding off edges of flat top in z2
sigRound_lam = 6       # Sigma of gaussian used to round off the flat top edges, in resonant wavelengths
bEnOscMag = 0          # Magnitude of oscillation on beam energy | Units of gamma
bEnOscFr = 7E3         # Frequency (2pi/lambda, where lambda in units of ct) of oscillation on beam energy 
#E = 300e6            # Beam energy
#gamma = E / (m_e * pow(c,2)) # Rel. factor
sig_gamma = 0.0004      # Energy spread


k_w = 2 * pi / lambda_w             # Get wiggler wavenumber
sigt = 250e-15 # 0.005570423008216 / c        # Get sigma in t dimension
sigz = c * sigt                     # Convert sigma in t to z
k_beta = aw * k_w / ( ff * gamma )  # Betatron wavelength
N = Q / q_e                         # Number of real electrons in pulse
lambda_r = lambda_w / (2 * pow(gamma,2)) * (1 + pow(aw,2))
                                    # ^ Resonant wavelength

sigRoundz = sigRound_lam * lambda_r

sigx = sqrt(emit / k_beta)     # Beam standard deviation in x
sigy = sigx                           # and y

sig_av = sqrt((pow(sigx,2) + pow(sigy,2))/2.0)


########################################
# Beam area and FEL parameter

if qHardEdgeX == 1:
    r_av = sig_av                 # Hard edged circle in x and y
else:
    r_av = math.sqrt(2) * sig_av  # Gaussian dist in x and y

tArea = pi * pow(r_av,2)          # Tranverse beam area

if qFlatTopZ2 == 1:
    if qRoundZ2 == 1:
        lArea = sqrt(2*pi) * sigRoundz  + sigz     # flat top + gaussian
    else:
        lArea = sigz                  # longitudinal integral over charge dist (flat top)
else:
    lArea = sqrt(2*pi) * sigz     # longitudinal integral over charge dist(gaussian)

n_p = N / (tArea * lArea)              # Electron number density
wp = sqrt(pow(q_e,2) * n_p / (eps_0 * m_e) )         # plasma frequency

rho = 1.0 / gamma * pow((aw * wp / ( 4.0 * c * k_w )),(2.0/3.0))  # FEL parameter

#######################################
# Scaled parameters for Puffin


lambda_z2 = 4*pi*rho              # Resonant wavelength in z2
Lg = lambda_w / lambda_z2         # Gain length
Lc = lambda_r / lambda_z2         # Cooperation length
zbarprop = N_w * lambda_z2        # Length of undulator in zbar (gain lengths)
sigz2 = sigz / Lc                 # Length of pulse in z2 (cooperation lengths)

sigRoundZ2 = sigRoundz / Lc       # Sigma of tail off in z2 (cooperation lengths)

bEnOscFr = bEnOscFr * Lc

beta = sqrt(pow(gamma,2) - 1.0 - pow(aw,2))/gamma    # Average velocity over c
eta = (1-beta)/beta                                # Scaled average velocity

k_beta_bar = k_beta * Lg                           # Scaled betatron wavenumber
emit_bar = emit / (rho * Lc)                       # Scaled emittance
Z_R = pi * pow(r_av,2) / lambda_r                  # Rayleigh range
Z_bar_R = pow(r_av,2) / (Lg * Lc) / (4.0 * rho)      # Scaled Rayleigh Range
B = pow((2 * Z_bar_R),(3.0/2.0))                       # Saldin diffraction parameter


NL = N/sigz2 * lambda_z2                           # electrons per radiation period

Anoise = 6 * sqrt(pi) * rho / (NL * sqrt(log(NL/rho)))  # Spontaneous noise estimate (in scaled units)
Acse = 16 * pow(rho,2)                             # CSE estimate for flat-top current



#################################################
# Chirp - 1% per sigma_z

dcgamma = 0.01 * gamma    # Change in gamma per sigma_z
chirp = dcgamma / sigz    # Energy chirp in z
chirpz2 = Lc * chirp     # Energy chirp in z2




##################################################
# Sampling

if qFlatTopZ2 == 1:
    if qRoundZ2 == 1:
        lez2 = (7.5 * sigRoundZ2)  + sigz2     # flat top + gaussian
    else:
        lez2     = sigz2       # flat-top
        sigz2_in = 1e8 
else:
    lez2     = 9*sigz2     # gaussian
    sigz2_in = sigz2 

lwz2 = 50.0                # Total size of sampled field in z2
lsys_z2 = lwz2 + lez2      # Total length of sampled system in z2

dz2 = lambda_z2 / elms_per_wave    # Node spacing in z2
NNodesZ2 = lsys_z2 / dz2 + 1       # Number of radiation field nodes

dz2e = lambda_z2 / emps_per_wave   # Macroparticle spacing in z2
NMElecsZ2 = lez2 / dz2e            # Number of macroparticles in z2

NMElecsP2 = 19                     # Number of macroparticles to sample energy spread

dz = lambda_z2 / steps_per_per     # Step size in zbar
Nsteps = zbarprop / dz             # Number of steps


###################################################
# Chirp - 1% per sigma_z

dcgamma = 0.0 # 0.01 * gamma;   # Change in gamma per sigma_z due to chirp
chirp = -dcgamma / sigz;   # Energy chirp in z
chirpz2 = Lc * chirp;    # Energy chirp in z2


###################################################

# Set up the beam and seed classes, which will contain
# the data to write in the beam and seed files

beam = [bm() for ib in range(nbeams)] # list of electron beams

seeds = [sd() for ic in range(nseeds)] # list of seeds

# Assign electron pulse data to beams

sigx = sigx / (sqrt(Lg) * sqrt(Lc))
sigy = sigy / (sqrt(Lg) * sqrt(Lc))

lex = 6*sigx
ley = 6*sigy

beam[0].lex = lex
beam[0].ley = ley
beam[0].lez2 = lez2
beam[0].lepx = 1E0
beam[0].lepy = 1E0
beam[0].lep2 = 6*sig_gamma

beam[0].sigx = sigx
beam[0].sigy = sigy


if qFlatTopZ2 == 1:
    beam[0].sigz2 = 1e8          # Flat top case
else:
    beam[0].sigz2 = sigz2

beam[0].sigpy = 1E0
beam[0].sigpy = 1E0
beam[0].sigp2 = sig_gamma

beam[0].nmpx = 1
beam[0].nmpy = 1
beam[0].nmppx = 1
beam[0].nmppy = 1

beam[0].nmpz2 = NMElecsZ2
beam[0].nmpp2 = NMElecsP2

beam[0].eratio = 1
beam[0].emit_bar = emit_bar
beam[0].chirp = chirpz2
beam[0].bcenz2 = 0
beam[0].Q = Q
beam[0].qRoundEj = qRoundZ2
beam[0].sigEj = sigRoundZ2

beam[0].bosc_Mag = bEnOscMag
beam[0].bosc_Fr  = bEnOscFr


##################################################
# Field info

NNodesX = 1 # Next, create a class for the field vars
NNodesY = 1 # and a routine to write the seed file

lwx = 1E0
lwy = 1E0

lsys_x = lwx
lsys_y = lwy

filtFrac = 0.3
diffFrac = 1.0

#################################################
# Flags

qoned = True
qfieldevo = True
qEevolve  = True
qEFcouple = True
qFocusing = False
qMatchedBeam = False
qDiffraction = False
qFilter = True
qNoise = True
qDump = False
qResume = False
qStepFiles = True
qFormatFiles = True
qWriteZ = True
qWriteA = True
qWritePperp = True
qWritep2 = True
qWritez2 = True
qWritex = True
qWritey = True


################################################
# Write data

# Main input file:

f = open(inputfile, 'w')

# Header

f.write('!--------------------------------------------------------------------------------------------------!\n')
f.write('!VALUE              TYPE        NAME                       DESCRIPTION\n')
f.write('!--------------------------------------------------------------------------------------------------!\n')


# Options


f.write('!                       OPTIONS\n')
f.write('\n')
f.write('\n')
f.write('{:<24}'.format(torf(qoned)) + 'LOGICAL     qOneD                      If TRUE, model 1D FEL, with only 1 node and 1 macroparticle in transverse dimensions\n')
f.write('{:<24}'.format(torf(qfieldevo)) + 'LOGICAL     qFieldEvolve               if letting the radiation field evolve\n')
f.write('{:<24}'.format(torf(qEevolve)) + 'LOGICAL     qElectronsEvolve           if integrating electron equations\n')
f.write('{:<24}'.format(torf(qEFcouple)) + 'LOGICAL     qElectronFieldCoupling     if allowing field to feedback onto the electron equations\n')
f.write('{:<24}'.format(torf(qFocusing)) + 'LOGICAL     qFocussing                 if focussing is included in the transverse plane\n')
f.write('{:<24}'.format(torf(qMatchedBeam)) + 'LOGICAL     qMatchedBeam               if matching beam to undulator. If TRUE, electron pulse sigma and length in x,y,px,py are automatically calculated\n')
f.write('{:<24}'.format(torf(qDiffraction)) + 'LOGICAL     qDiffraction               if modelling diffraction\n')
f.write('{:<24}'.format(torf(qFilter)) + 'LOGICAL     qFilter                    TRUE to filter, if FALSE the low frequencies will just be ignored during diffraction\n')
f.write('{:<24}'.format(torf(qNoise))  + 'LOGICAL     q_noise                    Shot noise in initial electron beam distribution\n')
f.write('{:<24}'.format(torf(qDump))  + 'LOGICAL     qDump                      Do you wish to dump data so the run can be resumed if anything goes wrong? .TRUE. for yes.\n')
f.write('{:<24}'.format(torf(qResume)) + 'LOGICAL     qResume                    If resuming from dump files left from a previous run\n')
f.write('{:<24}'.format(torf(qStepFiles))  + 'LOGICAL     qSeparateFiles             Write data to separate SDDS files at each step\n')
f.write('{:<24}'.format(torf(qFormatFiles))  + 'LOGICAL     qFormattedFiles            Write data as formatted text(TRUE) or binary(FALSE)\n')
f.write('{:<24}'.format(torf(qWriteZ))  + 'LOGICAL     qWriteZ                    Write out Z data\n')
f.write('{:<24}'.format(torf(qWriteA))  + 'LOGICAL     qWriteA                    Write out A data\n')
f.write('{:<24}'.format(torf(qWritePperp))  + 'LOGICAL     qWritePperp                Write out Pperp data\n')
f.write('{:<24}'.format(torf(qWritep2))  + 'LOGICAL     qWriteP2                   Write out P2 data\n')
f.write('{:<24}'.format(torf(qWritez2))  + 'LOGICAL     qWriteZ2                   Write out Z2 data\n')
f.write('{:<24}'.format(torf(qWritex))  + 'LOGICAL     qWriteX                    Write out X data\n')
f.write('{:<24}'.format(torf(qWritey))  + 'LOGICAL     qWriteY                    Write out Y data\n')
f.write('\n')






# Macroparticle info

f.write('              ELECTRON MACROPARTICLE SAMPLING\n')
f.write('\n')
f.write('\n')
f.write('{:<24}'.format('\'' + beamfile + '\'') + 'CHARACTER   beam_file                  Name of the beam file\n')
f.write('{:<24.15E}'.format(0.5)   + 'REAL        sEThreshold          Beyond the threshold level(%) * the average of real electrons are removed(ignored)\n')


f.write('\n')
f.write('\n')
f.write('\n')

# Field Sampling

f.write('                  FIELD NODE SAMPLING\n')
f.write('\n')
f.write('\n')
f.write('{:<24d}'.format(int(math.floor(NNodesX)))        + 'INTEGER     iNumNodes(1)         Number of Elements in x direction\n')
f.write('{:<24d}'.format(int(math.floor(NNodesY)))        + 'INTEGER     iNumNodes(2)         Number of Elements in y direction\n')
f.write('{:<24d}'.format(int(math.floor(elms_per_wave)))      + 'INTEGER     nodesperlambda         Number of nodes per resonant wavelength in z2\n')
f.write('{:<24.15E}'.format(lsys_y)   + 'REAL        sFModelLengthX       Length of radiation field model in x direction\n')
f.write('{:<24.15E}'.format(lsys_x)   + 'REAL        sFModelLengthY       Length of radiation field model in y direction\n')
f.write('{:<24.15E}'.format(lsys_z2)  + 'REAL        sWigglerLengthZ2     Length of wiggler in z2-bar direction\n')
f.write('{:<24d}'.format(1)        + 'INTEGER     iRedNodesX           Length of central wiggler section in x where electrons will not leave\n')
f.write('{:<24d}'.format(1)        + 'INTEGER     iRedNodesY           Length of central wiggler section in y where electrons will not leave\n')
f.write('{:<24.15E}'.format(filtFrac)   + 'REAL        sFiltFrac            Specifies cutoff for high pass filter as fraction of resonant frequency\n')
f.write('{:<24.15E}'.format(diffFrac)   + 'REAL        sDiffFrac                  Specifies diffraction step size as fraction of the undulator period\n')
f.write('{:<24.15E}'.format(diffFrac)   + 'REAL        beta                 Absorption coefficient\n')
f.write('{:<24}'.format('\'' + seedfile + '\'') + 'CHARACTER   seed_file                  Name of the seed file\n')


f.write('\n')
f.write('\n')


# Independent Variables

f.write('                 INDEPENDANT VARIABLES\n')
f.write('\n')
f.write('\n')
f.write('Input the scaled independant variables from [1] here\n')
f.write('\n')
f.write('\n')
f.write('{:<24.15E}'.format(rho) + 'REAL        rho                  Pierce parameter, describe the strength of the field\n')
f.write('{:<24.15E}'.format(1.0)   + 'REAL        ux                   Normalised magnitude of wiggler magnetic field x-vector\n')
f.write('{:<24.15E}'.format(1.0)   + 'REAL        uy                   Normalised magnitude of wiggler magnetic field y-vector\n')
f.write('{:<24.15E}'.format(aw) + 'REAL        aw                  rms unduator parameter\n')
f.write('{:<24.15E}'.format(gamma) + 'REAL        gamma                      Resonant Energy\n')
f.write('{:<24.15E}'.format(ff) + 'REAL        sFocusfactor         Focussing factor f, from the betatron wavenumber\n')
f.write('{:<24.15E}'.format(lambda_w) + 'REAL        lambda_w                   Undulator period\n')
f.write('{:<24.15E}'.format(0.0)      + 'REAL        Dfact                Dispersive strength factor for chicane\n')
f.write('{:<24}'.format('\'\'')  + 'CHARACTER   undType                    Undulator type - \'curved\' , \'planepole\' , else 1D (no off axis variation of aw)\n')
f.write('{:<24.15E}'.format(0.0)      + 'REAL        taper                gradient of taper daw/dz\n')


f.write('\n')
f.write('\n')

# Integration through undulator


f.write('                      INTEGRATION AND OUTPUT\n')
f.write('\n')
f.write('\n')
f.write('Here,a lattice file can be input to specify an undulator-chicane lattice.\n')
f.write('If it is specified, then the user supplied value of nSteps and \n')
f.write('sStepSize is ignored, and the number of steps per undulator period\n')
f.write('is used instead. Otherwise steps per und period is ignored and use \n')
f.write('nsteps and sstepsize.\n')
f.write('\n')
f.write('What do we want to calculate in the code to output i.e. bunching, pulse current weighted average x, av sigma x etc\n')
f.write('\n')
f.write('\n')
f.write('{:<24}'.format('\'\'')       + 'CHARACTER   lattFile             Contents: N_w(periods), delta(periods) (!!! NO BLANK LINES AT END !!!)Blank if none.\n')
f.write('{:<24d}'.format(steps_per_per) + 'INTEGER     stepsPerPeriod  Number of steps per wiggler period\n')
f.write('{:<24d}'.format(int(N_w))    +   'INTEGER     nPeriods        Number of wiggler periods\n')
f.write('{:<24.15E}'.format(0.0)    + 'REAL        sZ0                  Starting z position\n')
f.write('{:<24}'.format('\'DataFile.dat\'') + 'CHARACTER   zDataFileName        Data file name\n')
f.write('{:<24d}'.format(100)    +  'INTEGER     iWriteNthSteps       Steps to write data at\n')
f.write('{:<24d}'.format(100)    +  'INTEGER     iWriteIntNthSteps  Steps to write integrated data at\n')
f.write('{:<24d}'.format(100)    +  'INTEGER     iDumpNthSteps        Steps to dump data at (0 for no dumping)\n')
f.write('{:<24.15E}'.format(100)    +  'REAL        sPEOut               Percentage of macroparticles to write out. Macroparticles are randomly selected.\n')
f.write('\n')

f.close()





#
# beam file

f = open(beamfile, 'w')


f.write('PUFFIN BEAM FILE\n')
f.write('\n')
f.write('Describes electron beams for input into puffin. Multiple beams with\n')
f.write('different parameters can be used. Please refer to POP-REF for an\n')
f.write('explanation of the scaled variables used, such as z2 and p2.\n')
f.write('\n')
f.write('{:d}'.format(int(nbeams)) + '         # of beams\n')
f.write('\n')
f.write('READ IN BEAM CHARACTERISTICS\n')


# Write beam files

for ib in range(nbeams):
    beam[ib].wrbeam(f,ib+1)

#
# seed file

f = open('seed_file.in', 'w')

f.write('PUFFIN SEED FILE\n')
f.write('\n')
f.write('Describes seed fields for input into puffin. Multiple seeds with\n')
f.write('different frequencies, profiles and positions can be used. Please \n')
f.write('refer to POP-REF for an explanation of the scaled variables used, \n')
f.write('such as z2.\n')
f.write('\n')
f.write('{:d}'.format(int(nseeds)) + '         # of beams\n')
f.write('\n')
f.write('READ IN SEED CHARACTERISTICS\n')

# Write seed data

for ic in range(nseeds):
    seeds[ic].wrseed(f,ic+1)


f.close()
















