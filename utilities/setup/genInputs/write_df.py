# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause
#
# Python script containing the code to write the input files for 
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
# sections and/or quads. The name of this lattice file should be entered
# in the 'lattfile' variable in the input file. 
#
#
#     DESCRIBE UNDULATOR LATTICE FILE
#
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


# simple setup
  # define beam and undulator - frame is assumed the same as the beam and undulator
  # scale parameters

# - OR - 

# advanced setup
  # define frame
  # define beam in frame
    # use same beam as reference beam?
  # scale parameters

import math
import beamClass
import seedClass
import fBoolean
import numpy as np

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
lattFile = ''

nbeams = 1
nseeds = 1

### Sampling

elms_per_wave = 20         # Field nodes per resonant wavelength 
emps_per_wave = 24         # Electron macroparticles per resonant wavelength
steps_per_per = 30        # should be roughly 4-5* elms_per_wave

# Undulator and beam parameters
# Below similar to CLARA parameters

Er = 240e6            # Beam energy (reference particle)
gamma = Er / ( (m_e * pow(c,2) / q_e) ) # Rel. factor
aw = 1.01               # PEAK wiggler parameter
lambda_w = 0.0275        # Undulator period
N_w = 200              # Number of undulator periods
undtype = 'planepole'
ux = 0
uy = 1


# RMS wiggler param depends on undulator type / polarization:

if (undtype == 'planepole'):
  ux = 0
  uy = 1
  awrms = aw / np.sqrt(2)

elif (undtype == 'curved'):
  ux = 0
  uy = 1
  awrms = aw / np.sqrt(2.)

elif (undtype == 'helical'):
  ux = 1
  uy = 1
  awrms = aw

else:
  awrms = aw / np.sqrt(2.) * np.sqrt(ux**2 + uy**2)



qFlatTopZ2 = np.array([1])         # =1 if flat top current profile, else gaussian.
qHardEdgeX = np.array([0])         # =1 if disk (circle) in transverse plane, else gaussian.
qRoundZ2 = np.array([1])           # If rounding off edges of flat top in z2
sigRound_lam = np.array([6])       # Sigma of gaussian used to round off the flat top edges, in resonant wavelengths
bEnOscMag = np.array([0])          # Magnitude of oscillation on beam energy | Units of gamma
bEnOscFr = np.array([7E3])         # Frequency (2pi/lambda, where lambda in units of ct) of oscillation on beam energy 
#E = 300e6            # Beam energy
#gamma = E / (m_e * pow(c,2)) # Rel. factor
sig_gamma = np.array([0.0004])      # Energy spread (relative to Er i.e. sig_gam / gamma_r)


Ej = np.array([240e6])
eratio = Ej / Er # [ij / Er for ij in Ej] # Rel. factor
gammaj = Ej / ( (m_e * pow(c,2) / q_e) ) # [ij / ( (m_e * pow(c,2) / q_e) ) for ij in Ej] # Rel. factor
emitx = np.array([1e-6 / gamma])       # Unnormalised Emittance in x
emity = np.array([1e-6 / gamma])       # Unnormalised Emittance in y
Q = np.array([0.1e-9])                # Charge


k_w = 2 * pi / lambda_w             # Get wiggler wavenumber
sigt = np.array([250e-15]) # 0.005570423008216 / c        # Get sigma in t dimension
sigz = c * sigt                     # Convert sigma in t to z




#if 
ff = np.sqrt(2)

k_beta = aw * k_w / ( ff * gamma )  # Betatron wavenumber
N = [ij / q_e for ij in Q]                         # Number of real electrons in pulse
lambda_r = lambda_w / (2 * np.power(gamma,2)) * (1 + np.power(awrms,2))
                                    # ^ Resonant wavelength

sigRoundz = sigRound_lam * lambda_r

k_betax = k_beta
k_betay = k_beta   #...else strong...

alphax = np.array([0.])
alphay = np.array([0.])

#sigx = np.sqrt(emitx / k_betax)     # Beam standard deviation in x...
#sigy = np.sqrt(emity / k_betay)     # ...and y

sigx = np.array([50e-6])
sigy = np.array([50e-6])

sig_av = np.sqrt((np.power(sigx,2) + np.power(sigy,2))/2.0)


########################################
# Beam area and FEL parameter

if qHardEdgeX == 1:
    r_av = sig_av                 # Hard edged circle in x and y
else:
    r_av = np.sqrt(2.) * sig_av  # Gaussian dist in x and y

tArea = np.pi * np.power(r_av,2)          # Tranverse beam area

if qFlatTopZ2[0] == 1:
    if qRoundZ2[0] == 1:
        lArea = np.sqrt(2*pi) * sigRoundz  + sigz     # flat top + gaussian
    else:
        lArea = sigz                  # longitudinal integral over charge dist (flat top)
else:
    lArea = sqrt(2*pi) * sigz     # longitudinal integral over charge dist(gaussian)

n_p = N / (tArea * lArea)              # Electron number density
wp = np.sqrt(np.power(q_e,2) * n_p / (eps_0 * m_e) )         # plasma frequency

rho = 1.0 / gamma * np.power((aw * wp[0] / ( 4.0 * c * k_w )),(2.0/3.0))  # FEL parameter

#######################################
# Scaled parameters for Puffin


lambda_z2 = 4*pi*rho              # Resonant wavelength in z2
Lg = lambda_w / lambda_z2         # Gain length
Lc = lambda_r / lambda_z2         # Cooperation length
zbarprop = N_w * lambda_z2        # Length of undulator in zbar (gain lengths)
sigz2 = sigz / Lc                 # Length of pulse in z2 (cooperation lengths)

sigRoundZ2 = sigRoundz / Lc       # Sigma of tail off in z2 (cooperation lengths)

bEnOscFr = bEnOscFr * Lc

beta = sqrt(np.power(gamma,2) - 1.0 - np.power(awrms,2))/gamma    # Average velocity over c
eta = (1-beta)/beta                                # Scaled average velocity (of reference electron)

k_beta_bar = k_beta * Lg                           # Scaled betatron wavenumber
emitx_bar = emitx / (rho * Lc)                       # Scaled emittance
emity_bar = emity / (rho * Lc)                       # Scaled emittance
Z_R = pi * np.power(r_av,2) / lambda_r                  # Rayleigh range
Z_bar_R = np.power(r_av,2) / (Lg * Lc) / (4.0 * rho)      # Scaled Rayleigh Range
B = np.power((2 * Z_bar_R),(3.0/2.0))                       # Saldin diffraction parameter


NL = N/sigz2 * lambda_z2                           # electrons per radiation period

Anoise = 6 * sqrt(pi) * rho / (NL * sqrt(log(NL/rho)))  # Spontaneous noise estimate (in scaled units)
Acse = 16 * np.power(rho,2)                             # CSE estimate for flat-top current


###################################################


if (undtype == 'planepole'):
  kbxn = 0.
  kbyn = aw / 2. / np.sqrt(2) / rho / gamma # 'natural' focusing wavenumber

elif (undtype == 'curved'):
  kxu = np.sqrt(eta / 8. / rho**2)
  kyu = np.sqrt(eta / 8. / rho**2)
  kbxn = aw / np.sqrt(2.*eta) / gamma *kxu
  kbyn = aw / np.sqrt(2.*eta) / gamma *kyu

elif (undtype == 'helical'):
  kbxn = aw / 2. / np.sqrt(2) / rho / gamma # 'natural' focusing wavenumber
  kbyn = aw / 2. / np.sqrt(2) / rho / gamma # 'natural' focusing wavenumber

else:
  kbxn = aw / 2. / np.sqrt(2) / rho / gamma # 'natural' focusing wavenumber
  kbyn = aw / 2. / np.sqrt(2) / rho / gamma # 'natural' focusing wavenumber
  awrms = aw / np.sqrt(2.) * np.sqrt(ux**2 + uy**2)





##################################################
# Sampling

if qFlatTopZ2[0] == 1:
    if qRoundZ2[0] == 1:
        lez2 = (7.5 * sigRoundZ2)  + sigz2     # flat top + gaussian
    else:
        lez2     = sigz2       # flat-top
        sigz2_in = 1.0e8 
else:
    lez2     = 9.*sigz2     # gaussian
    sigz2_in = sigz2 

lwz2 = 50.0                # Total size of sampled field in z2
lsys_z2 = lwz2 + lez2[0]      # Total length of sampled system in z2

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

#beam = [bm() for ib in range(nbeams)] # list of electron beams

#seeds = [sd() for ic in range(nseeds)] # list of seeds

# Assign electron pulse data to beams



sigx = sigx / (sqrt(Lg) * sqrt(Lc))
sigy = sigy / (sqrt(Lg) * sqrt(Lc))

lex = 6.*sigx
ley = 6.*sigy

beam = bm()
beam.nbeams = nbeams
beam.bftype = 'simple'

for ij in range(nbeams):
  if qFlatTopZ2[ij] == 1:
      sigz2[ij] = 1e8          # Flat top case
  else:
      sigz2[ij] = sigz2[ij]
      lez2[ij] = sigz2[ij] * 8.

sigpx = np.array([1.])
sigpy = np.array([1.])
siggam = sig_gamma

lepx = 6. * sigpx
lepy = 6. * sigpy
legam = siggam * 7.


nmpx = np.array([10])
nmpy = np.array([10])
nmpz2 = NMElecsZ2

nmppx = np.array([10])
nmppy = np.array([10])
nmpgam = np.array([10])
qmatch = [False]
trloadmeth = (np.array([np.int_(2)]))
qequixy = [False]

bcenter = np.array([0.0])


#beam.sig = np.append(sigx, np.append(sigy, np.append(sigz2, np.append(sigpx, \
#                          np.append( sigpy, siggam) ) ) ))


beam.sig = np.concatenate( (sigx, sigy, sigz2, sigpx, sigpy, siggam) )
beam.le = np.concatenate( (lex, ley, lez2, lepx, lepy, legam) )
beam.nmps = np.int_(np.round(np.concatenate( (nmpx, nmpy, nmpz2, nmppx, nmppy, nmpgam) )))
beam.eratio = eratio
beam.emitx = emitx
beam.emity = emity
beam.alphax = alphax
beam.alphay = alphay
beam.chirp = chirp
beam.bcenz2 = bcenter
beam.Q = Q
beam.qRoundEj = qRoundZ2
beam.sigEj = sigRoundZ2
beam.bosc_Mag = bEnOscMag
beam.bosc_Fr  = bEnOscFr
beam.qmatch  = qmatch
beam.trload = trloadmeth


#beam.sig = sigx + sigy + sigz2 + sigpy + sigpy + siggam
#beam.le = lex + ley + lez2 + lepy + lepy + legam
#beam.nmps = nmpx + nmpy + nmpz2 + nmppx + nmppy + nmpgam
#beam.eratio = eratio
#beam.emitx = emitx
#beam.emity = emity
#beam.emitx = alphax
#beam.emity = alphay
#beam.chirp = chirp
#beam.bcenz2 = bcenter
#beam.Q = Q
#beam.qRoundEj = qRoundZ2
#beam.sigEj = sigRoundZ2
#beam.bosc_Mag = bEnOscMag
#beam.bosc_Fr  = bEnOscFr
#beam.qmatch  = qmatch
#beam.trload = trloadmeth


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


########  OTHER DATA TEMP! #################

qScaled = True
qUndEnds = True
qFMesh = True

taper = 0.0
sZ0 = 0.0
kbetax_SF = 0.0
kbetay_SF = 0.0
wrFile = ''  # 'wr_cl.txt'
wrFreq_Full = 30
wrFreq_integrated = 30


################################################
# Write data

# Main input file:

f = open(inputfile, 'w')



# Header

f.write('! The main input parameters are described below - Puffin takes the namelist blocks at the\n')
f.write('! bottom of this file as input. This is the \'main\' input file, containing info about the\n')
f.write('! wiggler, field sampling, and general flags and other numerical instructions for the\n')
f.write('! simulation. This file also points to a beam file, describing the electron beam,\n')
f.write('! and optionally, a seed file (describing a radiation seed) and/or a lattice file.\n')
f.write('!\n')
f.write('!\n')
# Options


f.write('!                       OPTIONS\n')
f.write('!\n')
f.write('!\n')
f.write('! qOneD                      If TRUE, model 1D FEL, with only 1 node and 1 macroparticle in transverse dimensions\n')
f.write('! qFieldEvolve               if letting the radiation field evolve\n')
f.write('! qElectronsEvolve           if integrating electron equations\n')
f.write('! qElectronFieldCoupling     if allowing field to feedback onto the electron equations\n')
f.write('! qFocussing                 if strong intra-undulator focussing is included in the transverse plane\n')
f.write('! qMatchedBeam               if matching beam to undulator. If TRUE, electron pulse sigma and length in x,y,px,py are automatically calculated\n')
f.write('! qDiffraction               if modelling diffraction\n')
f.write('! qFilter                    TRUE to filter, if FALSE the low frequencies will just be ignored during diffraction\n')
f.write('! q_noise                    Shot noise in initial electron beam distribution\n')
f.write('! qResume                    If resuming from dump files left from a previous run\n')
f.write('! qMeasure                   If .TRUE., then use measured FFT plans in FFTW\n')
f.write('! qDumpEnd                   If .TRUE., the do full data dump at end of simulation\n')
f.write('! qInitWrLat\n')
f.write('!\n')
f.write('!\n')
f.write('!\n')
f.write('!\n')

# Field Sampling

f.write('!                  FIELD MESH DESCRIPTION\n')
f.write('!\n')
f.write('!\n')
f.write('! iNumNodesX               Number of nodes to sample radiation field in x direction\n')
f.write('! iNumNodesY               Number of nodes to sample radiation field in y direction\n')
f.write('! nodesPerLambdar            Number of nodes per resonant wavelength\n')
f.write('! sFModelLengthX             Length of radiation field model in x direction\n')
f.write('! sFModelLengthY             Length of radiation field model in y direction\n')
f.write('! sWigglerLengthZ2           Length of field model in z2-bar direction\n')
f.write('! iRedNodesX                 Length of central field section in x where electrons will not leave\n')
f.write('! iRedNodesY                 Length of central field section in y where electrons will not leave\n')
f.write('! sFiltFrac                  Specifies cutoff for high pass filter as fraction of resonant frequency - used in diffraction step\n')
f.write('! sDiffFrac                  Specifies diffraction step size as fraction of the undulator period\n')
f.write('! beta                       Absorption coefficient\n')

f.write('!\n')
f.write('!\n')


f.write('!                  INDEPENDANT VARIABLES\n')
f.write('!\n')
f.write('!\n')
f.write('! Scaled independent vars. These describe the reference beam and undulator \n')
f.write('! used for the scaling of the system. \n')
f.write('!\n')
f.write('!\n')
f.write('! srho       Pierce or FEL parameter, describing the strength of the interaction (or efficiency)\n')
f.write('! sux        Normalised magnitude of wiggler magnetic field x-vector: H=1 is helical, H=0 is planar\n')
f.write('! suy        Normalised magnitude of wiggler magnetic field y-vector: H=1 is helical, H=0 is planar\n')
f.write('! saw        peak undulator parameter\n')
f.write('! sgamma_r   Resonant, or reference, beam energy\n')
f.write('! lambda_w   Undulator period\n')

f.write('!')
f.write('!')
f.write('!                  SIMPLE UNDULATOR SETUP')
f.write('!')
f.write('! \'Quick\' or \'simple\' undulator. If not using a lattice file, then this \n')
f.write('! undulator will be used. If a lattice file is used then these parameters \n')
f.write('! are ignored.\n')
f.write('!')
f.write('!')
f.write('! zundType     Undulator type - \'curved\' , \'planepole\', \'helical\' or blank\n')
f.write('! taper        gradient of taper - d/dz of alpha\n')
f.write('! nPeriods           Number of wiggler periods\n')
f.write('! stepsPerPeriod     Number of integration steps per wiggler period\n')
f.write('!\n')
f.write('!\n')
f.write('!                  EXTERNAL FILES\n')
f.write('!\n')
f.write('! The beam file is required, if not specified then the beam file is\n')
f.write('! assumed = \'beam_file.in\', which must be in working directory.\n')
f.write('!\n')
f.write('! beam_file     Name of the beam file\n')
f.write('! seed_file     Name of the seed file\n')
f.write('! lattFile      Name of lattice file (optional).\n')
f.write('! wr_file       Name of the write file (optional).\n')


f.write('!\n')
f.write('!\n')
f.write('!               DATA WRITING FREQUENCY\n')
f.write('!\n')
f.write('! iWriteNthSteps     Steps to write data at\n')
f.write('! iWriteIntNthSteps  Steps to write integrated data at\n')
f.write('! sZ0                Starting zbar position\n')

f.write('!\n')
f.write('!\n')
f.write('!  Additional Beam Loading Options... (rarely used!!!) \n')
f.write('!\n')
f.write('! sPEOut        Percentage of macroparticles to write out\n')
f.write('! sEThreshold   Below the threshold level(%) * the average of real electrons are removed(ignored)\n')

f.write('\n')
f.write('\n')
f.write('\n')
f.write('\n')

f.write('&MDATA\n')
f.write(' qScaled                = ' + torf(qScaled) + '\n')
f.write(' qOneD                  = ' + torf(qoned) + '\n')
f.write(' qFieldEvolve           = ' + torf(qfieldevo) + '\n')
f.write(' qElectronsEvolve       = ' + torf(qEevolve) + '\n')
f.write(' qElectronFieldCoupling = ' + torf(qEFcouple) + '\n')
f.write(' qFocussing             = ' + torf(qFocusing) + '\n')
f.write(' qDiffraction           = ' + torf(qDiffraction) + '\n')
f.write(' qUndEnds               = ' + torf(qUndEnds) + '\n')
f.write(' qFMesh_G               = ' + torf(qFMesh) + '\n')
f.write(' beam_file              = ' +  '\'' + beamfile + '\'' + '\n')
if (seedfile != ''):
  f.write(' seed_file              = ' + '\'' + seedfile + '\'' + '\n')
#f.write(' lattFile               = ' + '\'' + lattFile + '\'' + '\n')
f.write(' wr_file                = ' + '\'' + wrFile + '\'' + '\n')


# Field Mesh:

f.write(' iNumNodesX             = ' + '{:d}'.format(int(math.floor(NNodesX))) + '\n')
f.write(' iNumNodesY             = ' + '{:d}'.format(int(math.floor(NNodesY))) + '\n')
f.write(' nodesPerLambdar        = ' + '{:d}'.format(int(math.floor(elms_per_wave)))  + '\n')
f.write(' sFModelLengthX         = ' + '{:.15E}'.format(lsys_x) + '\n')
f.write(' sFModelLengthY         = ' + '{:.15E}'.format(lsys_y) + '\n')
f.write(' sFModelLengthZ2        = ' + '{:.15E}'.format(lsys_z2) + '\n')
f.write(' iRedNodesX             = ' + '{:d}'.format(1) + '\n')
f.write(' iRedNodesY             = ' + '{:d}'.format(1) + '\n')
f.write(' sFiltFrac              = ' + '{:.15E}'.format(filtFrac) + '\n')
f.write(' sDiffFrac              = ' + '{:.15E}'.format(diffFrac) + '\n')
f.write(' sBeta                  = ' + '{:.15E}'.format(0.1) + '\n')


# Scaling parameters - sets up the frame

f.write(' srho                   = ' + '{:.15E}'.format(rho) + '\n')
f.write(' saw                    = ' + '{:.15E}'.format(aw) + '\n')
f.write(' sgamma_r               = ' + '{:.15E}'.format(gamma) + '\n')
f.write(' lambda_w               = ' + '{:.15E}'.format(lambda_w) + '\n')
f.write(' zundType               = ' + '\'' + undtype + '\'' + '\n')

# 'base' wiggler - with lambda_w above. Ignored with a lattice file

f.write(' lambda_w               = ' + '{:.15E}'.format(lambda_w) + '\n')
f.write(' taper                  = ' + '{:.15E}'.format(taper) + '\n')
f.write(' sKBetaXSF              = ' + '{:.15E}'.format(kbetax_SF) + '\n')
f.write(' sKBetaYSF              = ' + '{:.15E}'.format(kbetay_SF) + '\n')
f.write(' nPeriods               = ' + '{:d}'.format(int(N_w)) + '\n')

# integration/write options
f.write(' stepsPerPeriod         = ' + '{:d}'.format(steps_per_per) + '\n')
f.write(' sZ0                    = ' + '{:.15E}'.format(sZ0) + '\n')
f.write(' iWriteNthSteps         = ' + '{:d}'.format(wrFreq_Full) + '\n')
f.write(' iWriteIntNthSteps      = ' + '{:d}'.format(wrFreq_integrated) + '\n')
f.write(' /\n')

f.close()


print 'Main data file written to: ' + inputfile


#
# beam file

f = open(beamfile, 'w')


# Write beam files

#for ib in range(nbeams):
#    beam[ib].wrbeam(f,ib+1)
beam.wrbeam(f,1)
f.close()

print 'Beam file written to: ' + beamfile

#
# seed file

if (seedfile != ''):

  seed = sd()
  f = open(seedfile, 'w')

# Write seed data

  for ic in range(nseeds):
      seed.wrseed(f,ic+1)


  f.close()

  print 'Radiation seed file written to: ' + seedfile















