#!/usr/bin/env python3
"""Compare a period-averaged Puffin run against an unaveraged one (3D).

Usage: python3 compare3d.py <plain_dir> <avg_dir>

The 1D compare.py demodulates the unaveraged field onto the envelope and
differences it node by node.  That does not carry over: in 3D the useful
question is not whether two fields agree phase by phase on meshes an order of
magnitude apart, but whether the same physics comes out.  So this compares
mesh-independent observables instead.

  power      total radiated power, sum(/power) * dz2, so the two z2 meshes
             are comparable.  The headline number.
  bunching   current-weighted mean of /bunchingFundamental.  In the exponential
             regime this is the most sensitive measure of the coupling, and it
             is not diluted by the seed the way power is early on.
  field      transverse RMS width of |A|^2, summed over z2, from the last
             /aperp - the observable that diffraction moves.  Also the relative
             L2 between the two transverse intensity profiles, each normalised
             to unit total, which catches a profile that has the right width
             but the wrong shape.
  beam       current-weighted /sigmaXbar, /sigmaYbar.  On the matched deck
             these should be flat; an error in the averaged natural focusing
             makes the beam breathe.

Mesh geometry is read from each file's own Vs mesh group - /meshScaled for the
field dumps, /intFieldMeshSc for the integrated ones - rather than assumed,
since the two runs have different z2 meshes and Puffin rescales the transverse
lengths given in the deck.

Pure standard library: datasets are pulled out with `h5dump -b LE`.
"""

import array
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


def mesh_attrs(path, group):
    """vsLowerBounds, vsUpperBounds, vsNumCells of a Vs mesh group, as lists."""
    out = subprocess.run(["h5dump", "-g", group, "-A", path],
                         check=True, stdout=subprocess.PIPE).stdout.decode()

    def vals(name):
        m = re.search(r'ATTRIBUTE "%s" \{.*?\(0\): ([^\n]+)' % name, out, re.S)
        if m is None:
            raise SystemExit("%s: no %s in %s" % (path, name, group))
        return [float(v) for v in m.group(1).split(",")]

    return vals("vsLowerBounds"), vals("vsUpperBounds"), vals("vsNumCells")


def mesh3d(path):
    """(nx, ny, nz2, dx, dy, dz2) of an aperp file, from /meshScaled.

    vsLowerBounds / vsUpperBounds / vsNumCells are ordered (z2, y, x), matching
    the (component, z2, y, x) layout of /aperp.
    """
    lo, hi, nc = mesh_attrs(path, "/meshScaled")
    nz2, ny, nx = (int(c) + 1 for c in nc)
    dz2, dy, dx = ((hi[i] - lo[i]) / nc[i] for i in range(3))
    return nx, ny, nz2, dx, dy, dz2


def dz2_of(path):
    """z2 cell size of an integrated file, from its 1D /intFieldMeshSc."""
    lo, hi, nc = mesh_attrs(path, "/intFieldMeshSc")
    return (hi[0] - lo[0]) / nc[0]


def last_index(d, stem):
    ks = [int(re.search(r"_%s_(\d+)\.h5" % stem, f).group(1))
          for f in os.listdir(d) if re.search(r"_%s_\d+\.h5$" % stem, f)]
    return sorted(ks)


def transverse(d, k):
    """(sigma_x, sigma_y, x profile, y profile) of |A|^2 summed over z2."""
    p = os.path.join(d, "run_aperp_%d.h5" % k)
    v = read_ds(p, "/aperp")
    nx, ny, nz2, dx, dy, _ = mesh3d(p)
    n = nx * ny * nz2
    if len(v) != 2 * n:
        raise SystemExit("%s: /aperp is %d values, mesh says %d" % (p, len(v), 2 * n))

    px, py = [0.0] * nx, [0.0] * ny
    for iz in range(nz2):
        base = iz * ny * nx
        for iy in range(ny):
            row = base + iy * nx
            for ix in range(nx):
                j = row + ix
                w = v[j] * v[j] + v[n + j] * v[n + j]
                px[ix] += w
                py[iy] += w

    def sig(prof, h):
        tot = sum(prof)
        if tot <= 0.0:
            return float("nan")
        c = (len(prof) - 1) / 2.0
        return math.sqrt(sum(prof[i] * ((i - c) * h) ** 2 for i in range(len(prof))) / tot)

    return sig(px, dx), sig(py, dy), px, py


def prof_l2(a, b):
    """Relative L2 between two profiles, each normalised to unit total."""
    if len(a) != len(b):
        return float("nan")
    sa, sb = sum(a), sum(b)
    if sa <= 0.0 or sb <= 0.0:
        return float("nan")
    a = [x / sa for x in a]
    b = [x / sb for x in b]
    num = sum((x - y) ** 2 for x, y in zip(a, b))
    return math.sqrt(num / sum(x * x for x in a))


def total_power(d, k):
    """sum(/power) * dz2 - independent of how finely z2 is sampled."""
    p = os.path.join(d, "run_integrated_%d.h5" % k)
    return sum(read_ds(p, "/power")) * dz2_of(p)


def weighted(d, k, ds):
    """Current-weighted mean of a per-slice dataset."""
    p = os.path.join(d, "run_integrated_%d.h5" % k)
    s = read_ds(p, ds)
    w = read_ds(p, "/beamCurrent")
    tw = sum(w)
    return sum(a * b for a, b in zip(s, w)) / tw if tw else float("nan")


def metrics(plain, avg):
    """Final-write comparison of avg against plain, plus the power/bunching series.

    Refuses to compare two runs that stopped at different zbar.  A Puffin run
    that gives up rearranging its parallel field prints a message and stops -
    with exit status 0 - so a half-finished run looks exactly like a finished
    one from the outside, and its last write is simply earlier.  Comparing that
    against a complete run silently produces a ratio that means nothing.
    """
    kp, ka = last_index(plain, "integrated")[-1], last_index(avg, "integrated")[-1]

    zp, za = zbar(plain, kp), zbar(avg, ka)
    if abs(zp - za) > 1e-6 * max(abs(zp), abs(za), 1.0):
        raise SystemExit(
            "runs ended at different zbar - %s at %.6g, %s at %.6g.\n"
            "One of them stopped early (Puffin exits 0 when it gives up "
            "rearranging the parallel field); there is nothing to compare."
            % (plain, zp, avg, za))

    res = {
        "power": (total_power(plain, kp), total_power(avg, ka)),
        "bunching": (weighted(plain, kp, "/bunchingFundamental"),
                     weighted(avg, ka, "/bunchingFundamental")),
        "beam sigma_x": (weighted(plain, kp, "/sigmaXbar"), weighted(avg, ka, "/sigmaXbar")),
        "beam sigma_y": (weighted(plain, kp, "/sigmaYbar"), weighted(avg, ka, "/sigmaYbar")),
    }

    fp, fa = last_index(plain, "aperp"), last_index(avg, "aperp")
    if fp and fa:
        sxp, syp, pxp, pyp = transverse(plain, fp[-1])
        sxa, sya, pxa, pya = transverse(avg, fa[-1])
        res["field sigma_x"] = (sxp, sxa)
        res["field sigma_y"] = (syp, sya)
        res["profile L2 x"] = (float("nan"), prof_l2(pxp, pxa))
        res["profile L2 y"] = (float("nan"), prof_l2(pyp, pya))

    # Power and bunching against zbar.  The two runs write on different step
    # counts, so each averaged write is paired with the nearest plain one and
    # the pair is kept only if they really are at the same zbar.
    pk = [(zbar(plain, k), k) for k in last_index(plain, "integrated")]
    tol = 1e-6 * max(abs(z) for z, _ in pk)
    series = []
    for k in last_index(avg, "integrated"):
        za = zbar(avg, k)
        z, kp = min(pk, key=lambda t: abs(t[0] - za))
        if abs(z - za) > tol:
            continue
        series.append((za, total_power(plain, kp), total_power(avg, k),
                       weighted(plain, kp, "/bunchingFundamental"),
                       weighted(avg, k, "/bunchingFundamental")))
    return res, series


def zbar(d, k):
    p = os.path.join(d, "run_integrated_%d.h5" % k)
    out = subprocess.run(["h5dump", "-A", p], check=True,
                         stdout=subprocess.PIPE).stdout.decode()
    m = re.search(r'ATTRIBUTE "zbarTotal" \{.*?\(0\): ([^\s]+)', out, re.S)
    return float(m.group(1))


def main():
    if len(sys.argv) < 3:
        raise SystemExit(__doc__)
    plain, avg = sys.argv[1], sys.argv[2]

    res, series = metrics(plain, avg)

    print("  %-16s %-13s %-13s %s" % ("", "unaveraged", "averaged", "avg/unavg"))
    for name in ("power", "bunching", "field sigma_x", "field sigma_y",
                 "beam sigma_x", "beam sigma_y"):
        if name not in res:
            continue
        p, a = res[name]
        r = a / p if p else float("nan")
        print("  %-16s %-13.6e %-13.6e %.5f" % (name, p, a, r))
    for name in ("profile L2 x", "profile L2 y"):
        if name in res:
            print("  %-16s %-13s %-13.4e" % (name, "-", res[name][1]))

    if series:
        print()
        print("  %-9s %-12s %-12s %-9s %-12s %-12s %s"
              % ("zbar", "P unavg", "P avg", "P ratio", "b unavg", "b avg", "b ratio"))
        for z, pp, pa, bp, ba in series[:: max(1, len(series) // 12)] + [series[-1]]:
            print("  %-9.3f %-12.4e %-12.4e %-9.4f %-12.4e %-12.4e %.4f"
                  % (z, pp, pa, pa / pp if pp else float("nan"),
                     bp, ba, ba / bp if bp else float("nan")))


if __name__ == "__main__":
    main()
