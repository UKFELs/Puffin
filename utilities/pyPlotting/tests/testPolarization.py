# Copyright (c) 2012-2026, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

"""Checks plotPolarization.py's field helpers against known states.

Each case builds a field whose polarisation is known analytically, hands it
to the helpers, and checks the Stokes parameters and the reconstructed
ellipse that come back. The three states pin the whole map down between
them: helical is circular, a planar run is linear along x, and equal
envelopes are linear at 45 degrees, which is the only one of the three that
distinguishes P2 from P3.

The unaveraged path is checked to read its two components verbatim, since
this script's fix has to leave unaveraged output alone.

Run it directly; it needs only numpy. See README.md.
"""

import sys

import numpy

import fakeDump

fakeDump.stubPlottingImports()
pp = fakeDump.importPlotting('plotPolarization')

ck = fakeDump.Checker()

NZ = 64
rng = numpy.random.default_rng(7)
amp = 1.0 + rng.random(NZ)                      # varying amplitude along z2
psi = rng.uniform(-numpy.pi, numpy.pi, NZ)      # varying envelope phase
Ax = amp * numpy.exp(1j * psi)


def stokes(h5):
    """The normalised Stokes parameters, as the script's main block forms them."""
    magx, phasex = pp.getMagPhaseEnvelope(h5, 0, 0, 0)
    magy, phasey = pp.getMagPhaseEnvelope(h5, 0, 0, 1, qNegate=True)
    s0 = numpy.maximum(magx**2 + magy**2, 1e-99)
    s1 = magx**2 - magy**2
    s2 = 2 * magx * magy * numpy.cos(phasex - phasey)
    s3 = 2 * magx * magy * numpy.sin(phasex - phasey)
    return magx, magy, s1 / s0, s2 / s0, s3 / s0


print('layout detection')
helical = fakeDump.envelopeDump(Ax, 1j * Ax)
ck.check('averaged two-envelope -> nFieldComp 2',
         pp.readFieldLayout(helical)[0], 2, tol=0)
ck.check('averaged two-envelope -> qAveraged',
         float(pp.readFieldLayout(helical)[1]), 1.0, tol=0)
resolved = fakeDump.resolvedDump(numpy.zeros(NZ))
ck.check('unaveraged -> nFieldComp 1',
         pp.readFieldLayout(resolved)[0], 1, tol=0)
ck.check('unaveraged -> not averaged',
         float(pp.readFieldLayout(resolved)[1]), 0.0, tol=0)
ck.check('dump with no runInfo -> unaveraged single pair',
         pp.readFieldLayout(fakeDump.Dump(numpy.zeros((1, 1, NZ, 2))))[0], 1,
         tol=0)
ck.check('averaged single envelope -> qAveraged',
         float(pp.readFieldLayout(fakeDump.envelopeDump(Ax))[1]), 1.0, tol=0)

print('helical, Atilde_y = i Atilde_x  (expect circular)')
magx, magy, s1, s2, s3 = stokes(helical)
ck.check('|Atilde_x| recovered', magx, amp)
ck.check('|Atilde_y| = |Atilde_x|', magy, amp)
ck.check('s1/s0 = 0  (no linear x-y imbalance)', s1, 0.0)
ck.check('s2/s0 = 0  (no diagonal component)', s2, 0.0)
ck.check('|s3/s0| = 1  (fully circular)', numpy.abs(s3), 1.0)

print('helical ellipse reconstruction')
ax, ay = pp.polarisationSamples(helical, 0, 0, NZ, 2, nPhase=64)
r2 = (ax**2 + ay**2).reshape(NZ, 64)
ck.check('|A|^2 constant over carrier phase',
         r2.std(axis=1) / r2.mean(axis=1), 0.0)
ck.check('circle radius = |Atilde_x|', numpy.sqrt(r2.mean(axis=1)), amp)

print('planar, Atilde_y = 0  (expect linear along x)')
planar = fakeDump.envelopeDump(Ax, numpy.zeros(NZ, dtype=complex))
magx, magy, s1, s2, s3 = stokes(planar)
ck.check('|Atilde_y| = 0', magy, 0.0, tol=0)
ck.check('s1/s0 = 1  (fully linear, x)', s1, 1.0)
ck.check('s2/s0 = 0', s2, 0.0)
ck.check('s3/s0 = 0', s3, 0.0)
ax, ay = pp.polarisationSamples(planar, 0, 0, NZ, 2, nPhase=16)
ck.check('ellipse collapses onto the x axis', ay, 0.0, tol=0)
# 16 samples of the carrier phase do not land exactly on the peak, so the
# sampled extent falls a little short of the envelope's amplitude.
ck.check('x extent reaches |Atilde_x|', numpy.max(numpy.abs(ax)),
         numpy.max(amp), tol=2e-2)

print('linear at 45 degrees, Atilde_y = Atilde_x')
magx, magy, s1, s2, s3 = stokes(fakeDump.envelopeDump(Ax, Ax))
ck.check('s1/s0 = 0', s1, 0.0)
ck.check('|s2/s0| = 1  (fully linear, diagonal)', numpy.abs(s2), 1.0)
ck.check('s3/s0 = 0', s3, 0.0)

print('unaveraged path unchanged')
comp0 = numpy.cos(numpy.linspace(0, 20, NZ))
comp1 = numpy.sin(numpy.linspace(0, 20, NZ))
ax, ay = pp.polarisationSamples(fakeDump.resolvedDump(comp0, comp1),
                                0, 0, NZ, 1)
ck.check('component 0 read verbatim', ax, comp0, tol=0)
ck.check('component 1 read verbatim', ay, comp1, tol=0)

sys.exit(ck.report())
