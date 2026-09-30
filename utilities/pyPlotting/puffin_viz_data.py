#!/usr/bin/env python3
"""Shared data reduction for the Puffin field viewers.

Kept apart from puffin_viz_theme (look) and puffin_viz_player (behaviour):
this is about turning what Puffin wrote into the number a plot should show,
and getting it wrong is a correctness bug rather than a cosmetic one.
"""

import numpy as np

C0 = 2.99792458e8

# puffin/lib/deriv_globals.f90
#   integer(kind=ip), parameter :: iTemporal = 0_ip
#   integer(kind=ip), parameter :: iPeriodic = 1_ip
I_TEMPORAL = 0
I_PERIODIC = 1


def mesh_is_periodic(attrs):
    """True if this run used the periodic (single radiation period) mesh.

    Falls back to periodic when the attribute is absent, which is what the
    viewers assumed before `fieldMesh` was consulted at all.
    """
    return int(attrs.get('fieldMesh', I_PERIODIC)) == I_PERIODIC


# ── solver mode ──────────────────────────────────────────────────────────────

def is_averaged(attrs):
    """True if this run used the period-averaged (SVEA) solver mode.

    Defaults to False, which is both the common case and the right guess for
    files written before `qAveraged` was recorded at all - those predate the
    mode existing.
    """
    return int(attrs.get('qAveraged', 0)) == 1


def carrier_kz2(attrs):
    """z2 wavenumber of the carrier the stored field is an envelope about.

    Zero for an unaveraged run, where the field arrays already resolve the
    carrier, so `A_perp = aperp * exp(i * carrier_kz2 * z2)` holds in both
    modes and applying it unaveraged is a no-op.
    """
    if 'kz2Carrier' in attrs:
        return float(attrs['kz2Carrier'])
    return -1.0 / (2.0 * float(attrs['rho'])) if is_averaged(attrs) else 0.0


def lambdar_per_cell(attrs):
    """z2 cell size in resonant wavelengths."""
    if 'lambdarPerCell' in attrs:
        return float(attrs['lambdarPerCell'])
    rho = float(attrs['rho'])
    return float(attrs['sLengthOfElmZ2']) / (4.0 * np.pi * rho)


def mode_label(attrs):
    """Short tag naming what the field arrays hold, for plot titles."""
    if not is_averaged(attrs):
        return 'resolved field'
    return 'envelope, %.3g λr/cell' % lambdar_per_cell(attrs)


def band_edges(attrs):
    """(lo, hi) of the representable band in w/wr, or None if unaveraged.

    The envelope mesh can carry roughly +/- 1/(2 lambdarPerCell) of the
    resonant frequency; outside that a viewer is plotting aliases.
    """
    if not is_averaged(attrs):
        return None
    half = 0.5 / lambdar_per_cell(attrs)
    return (1.0 - half, 1.0 + half)


def reconstruct_resolved(ar, ai, attrs, upsample=16):
    """Rebuild A_perp from a stored envelope: A = Atilde * exp(i kc z2).

    Returns (z2_nodes, A_real, A_imag) on a mesh `upsample` times finer than
    the stored one, because the carrier is by construction unresolved on the
    envelope's own mesh - that is what storing an envelope bought.

    This is a *view*, not data Puffin computed: the envelope is interpolated
    linearly between its nodes, so the result is exact only insofar as the
    envelope really is slow. It is meant for showing someone what the field
    looks like, not for measuring anything.
    """
    ar = np.asarray(ar, dtype=float)
    ai = np.asarray(ai, dtype=float)
    n = ar.size
    if n == 0:
        return np.zeros(0), np.zeros(0), np.zeros(0)

    dz2 = float(attrs['sLengthOfElmZ2'])
    kc = carrier_kz2(attrs)

    coarse = np.arange(n) * dz2
    fine = np.linspace(coarse[0], coarse[-1], (n - 1) * int(upsample) + 1)

    env = np.interp(fine, coarse, ar) + 1j * np.interp(fine, coarse, ai)
    full = env * np.exp(1j * kc * fine)
    return fine, full.real, full.imag


def spectrum(xf, yf, attrs):
    """Normalised spectral intensity against w/wr.

    Unaveraged, xf and yf are the two real components of a field that
    resolves the carrier, and each has its own real spectrum; bin n sits at
    w/wr = 4 pi rho n / L.

    Averaged, they are the real and imaginary parts of one complex envelope,
    so the transform to take is the complex one and its frequencies are
    offsets from the carrier rather than absolute. The sign follows from
    A = Atilde exp(-i z2 / 2rho): a component of the envelope at exp(+i k z2)
    lands at physical wavenumber k - 1/2rho, so a *positive* envelope
    frequency is a *lower* physical one, and

        w/wr = 1 - 4 pi rho * f_env

    which is the same expression as the unaveraged axis, subtracted from one.
    Returned sorted ascending so a plot does not wrap.
    """
    xf = np.asarray(xf, dtype=float)
    yf = np.asarray(yf, dtype=float)
    nz2 = int(attrs['nZ2'])
    dz2 = float(attrs['sLengthOfElmZ2'])
    rho = float(attrs['rho'])
    lenz2 = (nz2 - 1) * dz2
    if lenz2 <= 0 or xf.size == 0:
        return np.zeros(0), np.zeros(0)

    if is_averaged(attrs):
        ft = np.fft.fft(xf + 1j * yf)
        f_env = np.fft.fftfreq(xf.size, d=dz2)
        omega = 1.0 - 4.0 * np.pi * rho * f_env
        spec = np.abs(ft) ** 2
        order = np.argsort(omega)
        omega, spec = omega[order], spec[order]
    else:
        npts = int(np.ceil((nz2 + 1) / 2))
        px = np.abs(np.fft.fft(xf)[:npts]) ** 2
        py = np.abs(np.fft.fft(yf)[:npts]) ** 2
        for p in (px, py):
            if nz2 % 2 == 1:
                p[1:] *= 2
            else:
                p[1:-1] *= 2
        spec = px + py
        omega = np.arange(npts) * (1.0 / lenz2) * (4.0 * np.pi * rho)

    peak = np.max(spec) if spec.size else 0.0
    return omega, (spec / peak if peak > 0 else spec)


def reduce_power(power, attrs):
    """Collapse a powerSI array over z2 to one number for the power-vs-z curve.

    The right reduction depends on what the z2 window *is*:

    periodic mesh — the window is exactly one radiation period, so the mean
        over it is a genuine cycle average and is the quantity wanted.

    temporal mesh — the window is a chunk of ct-z holding a pulse that fills
        only part of it (24% in the PhyOfPlasmas fig7a example). The mean then
        divides by however much empty window the deck happened to allocate, so
        it is not a property of the radiation at all — halve sFModelLengthZ2
        and the "power" doubles. Peak power is the standard pulse quantity and
        is independent of the window, so use that. In fig7a the two differ by
        4.6x.

    Returns (value, label) so the caller can title the axis with the
    reduction actually applied rather than leaving the reader to guess.
    """
    p = np.asarray(power, dtype=float)
    if p.size == 0:
        return 0.0, 'power'
    if mesh_is_periodic(attrs):
        return float(np.mean(p)), 'mean power (cycle avg)'
    return float(np.max(p)), 'peak power'


def power_axis_label(attrs):
    """Y-axis label matching the reduction reduce_power() will apply."""
    return ('Mean power (W)' if mesh_is_periodic(attrs)
            else 'Peak power (W)')


def power_title(attrs):
    return ('Power vs z  (cycle-averaged)' if mesh_is_periodic(attrs)
            else 'Peak power vs z')


# ── energy ───────────────────────────────────────────────────────────────────

def dt_per_node(attrs):
    """Time step between z2 nodes, in seconds.

    z2 is a length (ct - z) in scaled units, so the physical node spacing is
    sLengthOfElmZ2 * Lc metres and the light-transit time across it is that
    over c.
    """
    return float(attrs['sLengthOfElmZ2']) * float(attrs['Lc']) / C0


def pulse_energy(power, attrs):
    """Radiated energy in J: the integral of P dt across the z2 window.

    Unlike peak power this is an extensive quantity, so it answers a
    different question — peak power says how intense the spike is, energy
    says how much light there is in total. Neither substitutes for the
    other, which is why they get their own plots rather than two y-axes.

    On a periodic mesh the window is one radiation period, so this is the
    energy per period rather than a pulse energy; energy_title() says which.
    """
    p = np.asarray(power, dtype=float)
    if p.size == 0:
        return 0.0
    return float(np.sum(p) * dt_per_node(attrs))


def energy_axis_label(attrs):
    return ('Energy per period (J)' if mesh_is_periodic(attrs)
            else 'Pulse energy (J)')


def energy_title(attrs):
    return ('Energy vs z  (per radiation period)' if mesh_is_periodic(attrs)
            else 'Pulse energy vs z')


# ── temporal profile ─────────────────────────────────────────────────────────

def mesh_bounds(h5file, group='intFieldMeshSI'):
    """(lower, upper) bounds in metres from a Vs mesh group, or None."""
    g = h5file.get(group)
    if g is None:
        return None
    lo = g.attrs.get('vsLowerBounds')
    hi = g.attrs.get('vsUpperBounds')
    return None if lo is None or hi is None else (lo, hi)


# ── unit systems ─────────────────────────────────────────────────────────────

SCALED, SI = 0, 1


class Units:
    """Conversion factors and axis labels for one unit system.

    Puffin works in scaled variables and writes most quantities twice, once
    scaled and once in SI (power/powerSI, Intensity/IntensitySI,
    beamCurrent/beamCurrentSI), so where a scaled dataset exists this picks
    it rather than converting.

    Each `*_mul` converts from the form the viewers hold internally to the
    displayed one. Those internal forms differ per quantity, so they are
    named here rather than left implicit:

        z          metres (zTotal, as written)
        z2         scaled (node index x sLengthOfElmZ2)
        x, y       scaled (node offset x sLengthOfElmX)
        intensity  |A_perp|^2
    """

    def __init__(self, attrs, mode=SI):
        self.mode = mode
        self.scaled = (mode == SCALED)
        self.averaged = is_averaged(attrs)
        Lg = float(attrs['Lg'])
        Lc = float(attrs['Lc'])
        self.Lg, self.Lc = Lg, Lc
        self.trans = np.sqrt(Lg * Lc)      # scaled transverse -> metres

        if self.scaled:
            self.z_mul,  self.z_label  = 1.0 / Lg, 'z̄'
            self.z2_mul, self.z2_label = 1.0,      'z₂'
            self.z2_unit               = ''      # scaled: dimensionless
            self.xy_mul                = 1.0
            self.x_label, self.y_label = 'x̄', 'ȳ'
            self.intens_mul            = 1.0
            #  |Atilde|^2 is by construction the period average of |A_perp|^2,
            #  so the number plotted means the same thing in both modes - only
            #  the symbol changes.
            self.intens_label          = '|Ã⊥|²' if self.averaged else '|A⊥|²'
            self.power_key,  self.power_label  = 'power',       'Power (scaled)'
            self.curr_key,   self.curr_label   = 'beamCurrent', 'Current (scaled)'
            self.energy_label                  = 'Energy (scaled)'
        else:
            self.z_mul,  self.z_label  = 1.0,          'z (m)'
            self.z2_mul, self.z2_label = Lc * 1e6,     'ct − z  (µm)'
            self.z2_unit               = 'µm'
            self.xy_mul                = self.trans * 1e3
            self.x_label, self.y_label = 'x (mm)', 'y (mm)'
            self.intens_mul            = None      # caller supplies intens_scale
            self.intens_label          = 'Intensity  (W m⁻²)'
            self.power_key,  self.power_label  = 'powerSI',       'Power (W)'
            self.curr_key,   self.curr_label   = 'beamCurrentSI', 'Current (A)'
            self.energy_label                  = 'Pulse energy (J)'

    def intensity_factor(self, iscale):
        """|A|^2 -> displayed intensity."""
        return 1.0 if self.scaled else iscale

    def energy(self, power, attrs):
        """Energy for the *displayed* power units.

        In SI this is joules; scaled it is the same integral in scaled
        variables, which is why the label just says "scaled" rather than
        naming a unit it does not have.
        """
        p = np.asarray(power, dtype=float)
        if p.size == 0:
            return 0.0
        if self.scaled:
            return float(np.sum(p) * float(attrs['sLengthOfElmZ2']))
        return float(np.sum(p) * dt_per_node(attrs))

    def z2_axis(self, n, attrs, bounds=None):
        """ct-z / z2 axis in the displayed units."""
        base = np.arange(n) * float(attrs['sLengthOfElmZ2'])
        if self.scaled or bounds is None or None in bounds:
            return base * self.z2_mul
        lo, hi = bounds
        return np.linspace(float(lo) * 1e6, float(hi) * 1e6, n)

    def power_title(self, attrs):
        which = ('Mean power' if mesh_is_periodic(attrs) else 'Peak power')
        return f'{which} vs {"z̄" if self.scaled else "z"}'

    def energy_title(self, attrs):
        which = ('Energy per period' if mesh_is_periodic(attrs)
                 else 'Pulse energy')
        return f'{which} vs {"z̄" if self.scaled else "z"}'
