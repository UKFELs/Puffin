#!/usr/bin/env python3
"""Compare a period-averaged Puffin run against an unaveraged one (1D).

Usage: python3 compare.py <plain_dir> <avg_dir> <rho>

The unaveraged field A_perp = A_x - i A_y is demodulated onto the averaged
mode's two rms Cartesian envelopes (see puffin/lib/undulator/averaging.f90):
A_c = sqrt(2) Re[B_c exp(-i z2/2rho)], so B_c = sqrt(2) <A_c exp(+i z2/2rho)>,
averaged over exactly one resonant wavelength.  Both are compared node by node
with the averaged run's stored envelopes, whatever the undulator polarisation.
Power vs z is compared through the integrated files, as the mean of /power
over the middle 30% of the mesh (the flat top); in the unaveraged planar case
|A|^2 ripples at 2k_r, which that mean removes.

If the averaged run carries harmonic bands (avgHarmonics; /runInfo
fieldCompHarmonic), each is compared at every /aperp write the two runs share
against the unaveraged field demodulated the same way at exp(+i h z2/2rho):
B_c^(h) = sqrt(2) <A_c exp(+i h z2/2rho)>.  The one-wavelength window has
exact nulls at every other harmonic, so each band is isolated.

Pure standard library: fields are pulled out with `h5dump -b LE`.
"""

import array
import cmath
import math
import os
import re
import subprocess
import sys


def read_ds(path, ds):
    binpath = path + "." + ds.strip("/").replace(" ", "_") + ".bin"
    subprocess.run(["h5dump", "-d", ds, "-b", "LE", "-o", binpath, path],
                   check=True, stdout=subprocess.DEVNULL)
    vals = array.array("d")
    with open(binpath, "rb") as fh:
        vals.frombytes(fh.read())
    os.remove(binpath)
    return list(vals)


_ATTRS = {}


def attr(path, name, default=None):
    if path not in _ATTRS:
        _ATTRS[path] = subprocess.run(["h5dump", "-A", path], check=True,
                                      stdout=subprocess.PIPE).stdout.decode()
    m = re.search(r'ATTRIBUTE "%s" \{.*?\(0\): ([^\s]+)' % name, _ATTRS[path], re.S)
    if m is None:
        if default is None:
            raise KeyError("%s has no attribute %s" % (path, name))
        return default
    return float(m.group(1))


def attr_list(path, name):
    """An array attribute, as a list of numbers, or None if it is absent."""
    attr(path, "zbarTotal")      # fills the cache
    m = re.search(r'ATTRIBUTE "%s" \{.*?DATA \{\s*\(0\): ([^}]*)\}' % name,
                  _ATTRS[path], re.S)
    if m is None:
        return None
    return [float(v) for v in re.sub(r"\(\d+\):", "", m.group(1)).replace(",", " ").split()]


def comp_harmonics(d, k):
    """The harmonic of each field component of write k: 0 for a resolved field,
    [1, 1] for an averaged run without harmonic bands, [1, 1, 3, 3, ...] with.
    Dumps written before fieldCompHarmonic existed carry the fundamental only.
    """
    p = os.path.join(d, "run_aperp_%d.h5" % k)
    h = attr_list(p, "fieldCompHarmonic")
    if h is None:
        nc = int(attr(p, "nFieldComp", 1))
        return [1] * nc if attr(p, "qAveraged", 0.0) > 0.5 else [0] * nc
    return [int(x) for x in h]


def last_index(d, stem):
    ks = [int(re.search(r"_%s_(\d+)\.h5" % stem, f).group(1))
          for f in os.listdir(d) if re.search(r"_%s_\d+\.h5$" % stem, f)]
    return sorted(ks)


def field_comps(d, k):
    """All field components of write k: ([[complex]], dz2, zbarTotal).

    The slowest dimension of /aperp is 2*nFieldComp, ordered (component 1 real,
    component 1 imaginary, component 2 real, ...) - see writeRunAtts in
    io/hdf5PuffLow.f90.  Unaveraged there is one component, whose real and
    imaginary parts are A_x and -A_y.  Averaged mode carries the rms envelope
    of each Cartesian component, B_x then B_y, both driven.  Files written
    before nFieldComp existed are read as single-component, which is what they
    were.
    """
    p = os.path.join(d, "run_aperp_%d.h5" % k)
    vals = read_ds(p, "/aperp")
    nc = int(attr(p, "nFieldComp", 1))
    n = len(vals) // (2 * nc)
    comps = [[complex(vals[2 * c * n + i], vals[(2 * c + 1) * n + i]) for i in range(n)]
             for c in range(nc)]

#   /runInfo/sLengthOfElmZ2 is the cell size the solver used.  The Vs mesh
#   group's bounds are not: vsUpperBounds is nZ2 * dz2 while vsNumCells is
#   nZ2 - 1, so their ratio is 0.06% high on this mesh.  Keep the ratio as the
#   fallback for dumps written before runInfo carried the cell size.

    dz2 = attr(p, "sLengthOfElmZ2",
               attr(p, "vsUpperBounds") / int(attr(p, "vsNumCells")))
    return comps, dz2, attr(p, "zbarTotal")


def cart_env(Ap, dzp, rho):
    """[B_x, B_y], the averaged mode's envelopes, from an unaveraged A_perp.

    The unaveraged pair is (A_x, -A_y).  A_c = sqrt(2) Re[B_c exp(-i z2/2rho)]
    gives B_c = sqrt(2) <A_c exp(+i z2/2rho)>; the exp(-i z2/2rho) image of the
    real A_c averages out over the one-wavelength window.
    """
    nper = int(round(4 * math.pi * rho / dzp))
    ax = [a.real for a in Ap]
    ay = [-a.imag for a in Ap]
    return [demod(ax, dzp, rho, nper, math.sqrt(2.0)),
            demod(ay, dzp, rho, nper, math.sqrt(2.0))]


def cart_env_h(Ap, dzp, rho, h):
    """[B_x^(h), B_y^(h)], harmonic band h of an unaveraged A_perp."""
    nper = int(round(4 * math.pi * rho / dzp))
    ax = [a.real for a in Ap]
    ay = [-a.imag for a in Ap]
    return [demod(ax, dzp, rho, nper, math.sqrt(2.0), h),
            demod(ay, dzp, rho, nper, math.sqrt(2.0), h)]


def demod(A, dz2, rho, nper, fac, h=1):
    """Envelope of the exp(-i h z2/2rho) component, trapezoid over one wavelength.

    The window must span exactly one wavelength (nper cells), or the 2k term
    of a linearly polarised field does not cancel; over one wavelength every
    other harmonic, and every image, averages out too.
    """
    env = [fac * a * cmath.exp(1j * h * i * dz2 / (2 * rho)) for i, a in enumerate(A)]
    h = nper // 2
    out = []
    for c in range(len(env)):
        lo, hi = c - h, c - h + nper
        if lo < 0 or hi >= len(env):
            out.append(None)
            continue
        s = sum(env[lo:hi + 1]) - 0.5 * (env[lo] + env[hi])
        out.append(s / (hi - lo))
    return out


def mid_power(d, k):
    p = read_ds(os.path.join(d, "run_integrated_%d.h5" % k), "/power")
    lo, hi = int(0.35 * len(p)), int(0.65 * len(p))
    return sum(p[lo:hi]) / (hi - lo)


def metrics(plain, avg, rho):
    """(envelope rel L2 difference, rows, power table) for avg against plain.

    The L2 is over both Cartesian envelopes together.  Each row is
    (z2, |B|^2 plain, |B|^2 avg, |B_y|^2/|B|^2 plain, the same avg), |B|^2
    summed over the components, so the polarisation is compared as well as the
    intensity.
    """
    Ap, dzp, _ = field_comps(plain, last_index(plain, "aperp")[-1])
    Aa, dza, _ = field_comps(avg, last_index(avg, "aperp")[-1])
    env = cart_env(Ap[0], dzp, rho)

    num = den = 0.0
    rows = []
    for j in range(len(Aa[0])):
        i = int(round(j * dza / dzp))
        if i >= len(env[0]) or env[0][i] is None:
            continue
        e = [env[c][i] for c in range(2)]
        a = [Aa[c][j] for c in range(2)]
        num += sum(abs(a[c] - e[c]) ** 2 for c in range(2))
        den += sum(abs(e[c]) ** 2 for c in range(2))
        pe = sum(abs(x) ** 2 for x in e)
        pa = sum(abs(x) ** 2 for x in a)
        rows.append((j * dza, pe, pa, abs(e[1]) ** 2 / pe if pe else 0.0,
                     abs(a[1]) ** 2 / pa if pa else 0.0))

    ks = sorted(set(last_index(plain, "integrated")) & set(last_index(avg, "integrated")))
    power = [(k, mid_power(plain, k), mid_power(avg, k))
             for k in ks[:: max(1, len(ks) // 15)] + [ks[-1]]]
    return math.sqrt(num / den), rows, power


def harmonic_power(plain, avg, rho):
    """Mid-bunch |B^(h)|^2 against zbar, per band of the averaged run.

    Returns (harmonics, rows), rows (k, zbar, {h: (plain, avg)}) for every
    /aperp write the two runs share.  |B^(h)|^2 is summed over x and y and
    averaged over the middle 30% of the averaged mesh's z2 extent; the same z2
    window is used for the unaveraged field.  Empty if the averaged run
    carries the fundamental alone.
    """
    ks = sorted(set(last_index(plain, "aperp")) & set(last_index(avg, "aperp")))
    harm = sorted(set(comp_harmonics(avg, ks[-1])))
    rows = []
    for k in ks:
        Ap, dzp, zb = field_comps(plain, k)
        Aa, dza, _ = field_comps(avg, k)
        ha = comp_harmonics(avg, k)
        lo, hi = 0.35 * len(Aa[0]) * dza, 0.65 * len(Aa[0]) * dza
        out = {}
        for h in harm:
            env = cart_env_h(Ap[0], dzp, rho, h)
            pp = [sum(abs(env[c][i]) ** 2 for c in range(2))
                  for i in range(len(env[0]))
                  if lo <= i * dzp <= hi and env[0][i] is not None]
            cs = [c for c in range(len(ha)) if ha[c] == h]
            pa = [sum(abs(Aa[c][j]) ** 2 for c in cs)
                  for j in range(len(Aa[0])) if lo <= j * dza <= hi]
            out[h] = (sum(pp) / len(pp), sum(pa) / len(pa))
        rows.append((k, zb, out))
    return harm, rows


def main():
    plain, avg, rho = sys.argv[1], sys.argv[2], float(sys.argv[3])

    l2, rows, power = metrics(plain, avg, rho)
    print("envelope rel L2 difference (avg vs demodulated plain): %.4e" % l2)

    print("   z2      |B|^2 plain   |B|^2 avg    y frac plain  y frac avg")
    for r in rows[:: max(1, len(rows) // 12)]:
        print("  %6.2f  %11.4e  %11.4e  %9.4f  %9.4f" % r)

    print()
    print("  write   P plain      P avg      avg/plain")
    for k, pp, pa in power:
        print("  %4d  %11.4e  %11.4e  %8.4f" % (k, pp, pa, pa / pp if pp else float("nan")))

    harm, rows = harmonic_power(plain, avg, rho)
    if len(harm) > 1:
        print()
        print_harmonics(harm, rows)


def print_harmonics(harm, rows):
    """Band power table, |B^(h)|^2 mid-bunch, unaveraged and averaged."""
    print("  zbar   " + "".join("   h=%d plain     h=%d avg   ratio " % (h, h) for h in harm))
    for k, zb, out in rows:
        line = "  %5.2f " % zb
        for h in harm:
            pp, pa = out[h]
            line += "  %10.4e  %10.4e  %6.4f" % (pp, pa, pa / pp if pp else float("nan"))
        print(line)


if __name__ == "__main__":
    main()
