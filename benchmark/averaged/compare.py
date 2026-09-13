#!/usr/bin/env python3
"""Compare a period-averaged Puffin run against an unaveraged one (1D).

Usage: python3 compare.py <plain_dir> <avg_dir> <rho> [<undtype>]

The unaveraged field A_perp is demodulated onto the averaged envelope's
normalisation (see puffin/lib/undulator/averaging.f90):
A+ = <A_perp exp(+i z2/2rho)>, averaged over exactly one resonant wavelength,
and Atilde = sqrt(fp)/u- * A+.  That is compared node by node with the
averaged run's stored envelope.  Power vs z is compared through the integrated
files, as the mean of /power over the middle 30% of the mesh (the flat top);
in the unaveraged planar case |A|^2 ripples at 2k_r, which that mean removes.

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


def attr(path, name):
    out = subprocess.run(["h5dump", "-A", path], check=True,
                         stdout=subprocess.PIPE).stdout.decode()
    m = re.search(r'ATTRIBUTE "%s" \{.*?\(0\): ([^\s]+)' % name, out, re.S)
    return float(m.group(1))


def last_index(d, stem):
    ks = [int(re.search(r"_%s_(\d+)\.h5" % stem, f).group(1))
          for f in os.listdir(d) if re.search(r"_%s_\d+\.h5$" % stem, f)]
    return sorted(ks)


def field(d, k):
    p = os.path.join(d, "run_aperp_%d.h5" % k)
    vals = read_ds(p, "/aperp")
    n = len(vals) // 2
    dz2 = attr(p, "vsUpperBounds") / int(attr(p, "vsNumCells"))
    return [complex(vals[i], vals[n + i]) for i in range(n)], dz2, attr(p, "zbarTotal")


def pol(undtype):
    """sqrt(fp)/u-, taking the exp(-i z2/2rho) component to the stored envelope."""
    cx, cy = {"helical": (1.0, 1.0), "planepole": (0.0, 1.0)}[undtype]
    um, fp = 0.5 * (cy + cx), 0.5 * (cx * cx + cy * cy)
    return math.sqrt(fp) / um


def demod(A, dz2, rho, nper, fac):
    """Envelope of the exp(-i z2/2rho) component, trapezoid over one wavelength.

    The window must span exactly one wavelength (nper cells), or the 2k term
    of a linearly polarised field does not cancel.
    """
    env = [fac * a * cmath.exp(1j * i * dz2 / (2 * rho)) for i, a in enumerate(A)]
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


def metrics(plain, avg, rho, undtype):
    """(envelope rel L2 difference, rows, power table) for avg against plain."""
    lamr = 4 * math.pi * rho
    Ap, dzp, _ = field(plain, last_index(plain, "aperp")[-1])
    Aa, dza, _ = field(avg, last_index(avg, "aperp")[-1])
    env = demod(Ap, dzp, rho, int(round(lamr / dzp)), pol(undtype))

    num = den = 0.0
    rows = []
    for j, a in enumerate(Aa):
        i = int(round(j * dza / dzp))
        if i >= len(env) or env[i] is None:
            continue
        e = env[i]
        num += abs(a - e) ** 2
        den += abs(e) ** 2
        rows.append((j * dza, abs(e) ** 2, abs(a) ** 2, cmath.phase(e), cmath.phase(a)))

    ks = sorted(set(last_index(plain, "integrated")) & set(last_index(avg, "integrated")))
    power = [(k, mid_power(plain, k), mid_power(avg, k))
             for k in ks[:: max(1, len(ks) // 15)] + [ks[-1]]]
    return math.sqrt(num / den), rows, power


def main():
    plain, avg, rho = sys.argv[1], sys.argv[2], float(sys.argv[3])
    undtype = sys.argv[4] if len(sys.argv) > 4 else "helical"

    l2, rows, power = metrics(plain, avg, rho, undtype)
    print("envelope rel L2 difference (avg vs demodulated plain): %.4e" % l2)

    print("   z2      |A|^2 plain   |A|^2 avg    phase plain  phase avg")
    for r in rows[:: max(1, len(rows) // 12)]:
        print("  %6.2f  %11.4e  %11.4e  %9.4f  %9.4f" % r)

    print()
    print("  write   P plain      P avg      avg/plain")
    for k, pp, pa in power:
        print("  %4d  %11.4e  %11.4e  %8.4f" % (k, pp, pa, pa / pp if pp else float("nan")))


if __name__ == "__main__":
    main()
