# Copyright (c) 2012-2026, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""Checks plotPolarizationHilbert.py's averaged-mode path.

The decisive test is cross-validation. One physical field is built, the
resolved version of it is handed to the existing Hilbert path and the
envelope version to the new analytic path, and amplitude, instantaneous
phase and instantaneous frequency are required to agree. That is what says
the carrier is put back with the right sign and magnitude, which no
self-consistent check of the envelope alone can see.

The test signal is deliberately periodic over the record. The reference
analytic signal is built by FFT, so a window holding a non-integer number of
carrier cycles leaks across its whole length, and the reference is then
worthless as one. Getting that wrong looks exactly like a broken
implementation: it first showed up here as an 8e-2 disagreement in both
amplitude and phase.

Run it directly; it needs numpy and scipy. See README.md.
"""

import sys

import numpy

import fakeDump

fakeDump.stubPlottingImports()
ph = fakeDump.importPlotting('plotPolarizationHilbert')

ck = fakeDump.Checker()

RHO = 0.005
KZ2 = -1.0 / (2. * RHO)      # the carrier as the dump stores it: -100
CHIRP = 4.0                  # d(arg Atilde)/d z2, a constant frequency offset
NZ = 385
NCYCLE = 12                  # carrier cycles in the record, a whole number

KEFF = -KZ2 - CHIRP          # 96: the carrier the resolved field shows
DZ = 2. * numpy.pi * NCYCLE / (KEFF * NZ)
z2 = DZ * numpy.arange(NZ)
PERIOD = DZ * NZ

# One physical field. The amplitude is modulated periodically over the record
# and the envelope phase is linearly chirped, so the instantaneous frequency
# is constant and the forward difference the script takes is exact.
amp = 1.0 + 0.3 * numpy.sin(2. * numpy.pi * z2 / PERIOD)
psi = 0.7 + CHIRP * z2
Atilde = amp * numpy.exp(1j * psi)
phi = -KZ2 * z2                                  # carrier phase, = z2/(2 rho)
Aresolved = numpy.real(Atilde * numpy.exp(-1j * phi))


def envelope(AyOverAx=None):
    Ay = None if AyOverAx is None else AyOverAx * Atilde
    return fakeDump.envelopeDump(Atilde, Ay, kz2Carrier=KZ2, z2=z2)


print('cross-validation: resolved Hilbert path against averaged analytic path')
resolved = fakeDump.resolvedDump(Aresolved, z2=z2)
magH, phaseH, freqH = ph.getMagPhase(resolved, 0, 0, 0)
magE, phaseE, freqE = ph.getMagPhaseEnvelope(envelope(1j), 0, 0, 0, KZ2)

ck.check('amplitude agrees', magE / magH, 1.0, tol=2e-3)
# The two unwraps start from independent origins, so they are comparable only
# up to a constant multiple of 2 pi.
dphase = phaseE - phaseH
dphase = dphase - 2. * numpy.pi * numpy.round(dphase[0] / (2. * numpy.pi))
ck.check('instantaneous phase agrees', dphase, 0.0, tol=5e-3)
# The phase is linear, so the forward difference is exact in exact
# arithmetic; 1e-9 on 15.28 is the floating-point round-off floor, and the
# last node is dropped because the difference has nothing to reach forward to.
fexpect = KEFF / (2. * numpy.pi)
ck.check('analytic frequency is keff/2pi', freqE[:-1], fexpect, tol=1e-9)
ck.check('Hilbert frequency agrees with it', freqH[:-1], fexpect, tol=5e-3)
ck.check('amplitude is exactly |Atilde|', magE, amp, tol=1e-14)

print('helical, Atilde_y = i Atilde_x')
magx, phasex, freqx = ph.getMagPhaseEnvelope(envelope(1j), 0, 0, 0, KZ2)
magy, phasey, freqy = ph.getMagPhaseEnvelope(envelope(1j), 0, 0, 1, KZ2,
                                             qNegate=True)
s0 = magx**2 + magy**2
ck.check('P1 = 0', (magx**2 - magy**2) / s0, 0.0, tol=1e-14)
ck.check('P2 = 0', 2 * magx * magy * numpy.cos(phasex - phasey) / s0, 0.0)
ck.check('|P3| = 1  (fully circular)',
         numpy.abs(2 * magx * magy * numpy.sin(phasex - phasey) / s0), 1.0,
         tol=1e-14)
ck.check('both components report one frequency', freqy, freqx, tol=1e-9)

print('planar, Atilde_y = 0')
magx, phasex, freqx = ph.getMagPhaseEnvelope(envelope(0.0), 0, 0, 0, KZ2)
magy, phasey, freqy = ph.getMagPhaseEnvelope(envelope(0.0), 0, 0, 1, KZ2,
                                             qNegate=True)
s0 = numpy.maximum(magx**2 + magy**2, 1e-99)
ck.check('|Atilde_y| = 0', magy, 0.0, tol=0)
ck.check('P1 = 1  (fully linear, x)', (magx**2 - magy**2) / s0, 1.0, tol=1e-14)

print('linear at 45 degrees, Atilde_y = Atilde_x')
magx, phasex, freqx = ph.getMagPhaseEnvelope(envelope(1.0), 0, 0, 0, KZ2)
magy, phasey, freqy = ph.getMagPhaseEnvelope(envelope(1.0), 0, 0, 1, KZ2,
                                             qNegate=True)
s0 = magx**2 + magy**2
ck.check('P1 = 0', (magx**2 - magy**2) / s0, 0.0, tol=1e-14)
ck.check('|P2| = 1  (fully linear, diagonal)',
         numpy.abs(2 * magx * magy * numpy.cos(phasex - phasey) / s0), 1.0)
ck.check('P3 = 0', 2 * magx * magy * numpy.sin(phasex - phasey) / s0, 0.0)

print('layout detection and guards')
ck.check('averaged two-envelope -> nFieldComp 2',
         ph.readFieldLayout(envelope(1j))[0], 2, tol=0)
ck.check('averaged two-envelope -> qAveraged',
         float(ph.readFieldLayout(envelope(1j))[1]), 1.0, tol=0)
ck.check('kz2Carrier returned', ph.readFieldLayout(envelope(1j))[2], KZ2, tol=0)
ck.check('unaveraged -> not averaged',
         float(ph.readFieldLayout(resolved)[1]), 0.0, tol=0)
ck.check('unaveraged -> carrier of zero',
         ph.readFieldLayout(resolved)[2], 0.0, tol=0)

# A mesh that disagrees with the field would otherwise broadcast into
# nonsense, or silently reduce to a scalar.
mismatched = envelope(1j)
mismatched.root.meshScaled._v_attrs.vsNumCells = [1, 1, NZ + 9]
ck.checkRaises('a z2 mesh of the wrong length is rejected', ValueError,
               ph.getMagPhaseEnvelope, mismatched, 0, 0, 0, KZ2)

sys.exit(ck.report())
