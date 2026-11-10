#!/usr/bin/env python3
"""Analyse the runs written by run_corefine.py.

Usage:
  python3 analyse_corefine.py OUTDIR [--json FILE]

For every run, at the end of the undulator: pulse energy, peak power (averaged
over one resonant wavelength), mid-beam bunching and mean energy loss - the
definitions ../averaged/plot_compare_latt.py uses - and the complex field
envelope: each field component demodulated on the resonant carrier and
averaged over each resonant wavelength.  The envelope is mesh-independent, so
runs at every level and with every kernel can be compared directly.

Errors are taken against a shared reference, the finest diagonal run of a
kernel, and printed against both the cubic and the linear one: if the kernels
share a limit, the two references agree with each other far better than either
agrees with a coarse run.  Scalars also get a Richardson estimate from the last
three diagonal levels.  Needs numpy and h5py.
"""

import argparse
import glob
import json
import os
import re
import sys

import h5py
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "averaged"))
import plot_compare_latt as pcl  # noqa: E402


def envelope(d):
    """Complex field envelope of each component from the last aperp dump,
    sampled at the centre of every resonant wavelength.  A_c exp(i z2/2rho) is
    low-passed with a Gaussian of rms width lambda_r/2, which removes the
    2-carrier term to ~1e-9 whatever the node spacing (Puffin's is not an
    integer number of nodes per wavelength), then evaluated at the common sample
    points from its Fourier series, so no interpolation error enters.  The same filter for every run, so it is a fair observable."""
    f = sorted(glob.glob(os.path.join(d, "*_aperp_*.h5")), key=pcl.idx)[-1]
    with h5py.File(f, "r") as h:
        a = h["aperp"][()]
        rho = float(h["runInfo"].attrs["rho"])
        dz2 = float(h["runInfo"].attrs["sLengthOfElmZ2"])
    n = a.shape[1]
    z = np.arange(n) * dz2
    lam = 4.0 * np.pi * rho
    sig = 0.5 * lam
    k = 2.0 * np.pi * np.fft.fftfreq(n, d=dz2)
    filt = np.exp(-0.5 * (k * sig) ** 2)
    zs = (np.arange(int((z[-1] - 4 * lam) / lam)) + 2.5) * lam
    keep = filt > 1e-14                 # the filter's support; beyond it nothing survives
    phase = np.exp(1j * np.outer(zs, k[keep]))
    out = []
    for c in range(a.shape[0]):
        sdem = a[c] * np.exp(1j * z / (2.0 * rho))
        coef = 2.0 * (np.fft.fft(sdem) * filt)[keep] / n
        out.append(phase @ coef)        # the filtered signal, evaluated exactly at zs
    return np.array(out)


def scalars(d):
    a, (z, ppk, en, g, sg, bun) = pcl.history(d, 1.0)
    secs, _ = pcl.runtime(d)
    return dict(E=en[-1], P=ppk[-1], b=bun[-1], g=g[-1], secs=secs,
                Ehist=en.tolist(), zhist=z.tolist())


def rel(x, y):
    return abs(x - y) / abs(y)


def env_err(e, ref):
    m = min(e.shape[1], ref.shape[1])
    return float(np.linalg.norm(e[:, :m] - ref[:, :m]) / np.linalg.norm(ref[:, :m]))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("outdir")
    ap.add_argument("--json")
    o = ap.parse_args()
    result = {}
    for und in sorted(os.listdir(o.outdir)):
        ud = os.path.join(o.outdir, und)
        if not os.path.isdir(ud):
            continue
        runs = {}
        for d in sorted(glob.glob(os.path.join(ud, "o*_*"))):
            if not os.path.exists(os.path.join(d, "done")):
                continue
            k, nodes, mpw, steps = [int(v) for v in open(os.path.join(d, "done")).read().split()]
            tag = re.match(r"o\d_(.*)", os.path.basename(d)).group(1)
            s = scalars(d)
            s.update(order=k, tag=tag, nodes=nodes, mpw=mpw, steps=steps)
            runs[(k, tag)] = (s, envelope(d))
        levels = sorted({int(t[1:]) for _, t in runs if re.fullmatch(r"r\d+", t)})
        top = "r%d" % levels[-1]
        refs = {k: runs[(k, top)] for k in (1, 3) if (k, top) in runs}
        print("\n=== %s: shared references at %s ===" % (und, top))
        if len(refs) == 2:
            s1, e1 = refs[1]
            s3, e3 = refs[3]
            print("  linear vs cubic at %s: envelope %.2e, energy %.2e, peak power %.2e, "
                  "bunching %.2e" % (top, env_err(e1, e3), rel(s1["E"], s3["E"]),
                                     rel(s1["P"], s3["P"]), rel(s1["b"], s3["b"])))
        print("  %-6s %-10s %5s %4s %5s  %-9s %-9s  %-9s %-9s %-9s %-9s %7s" %
              ("kernel", "run", "nodes", "mpw", "steps", "env/cub", "env/lin",
               "E/cub", "P/cub", "b/cub", "dg/cub", "secs"))
        rows = []
        for (k, tag) in sorted(runs, key=lambda t: (t[0], len(t[1]) > 3, t[1])):
            s, e = runs[(k, tag)]
            row = dict(s, env_cub=env_err(e, refs[3][1]) if 3 in refs else None,
                       env_lin=env_err(e, refs[1][1]) if 1 in refs else None)
            if 3 in refs:
                r3 = refs[3][0]
                row.update(E_cub=rel(s["E"], r3["E"]), P_cub=rel(s["P"], r3["P"]),
                           b_cub=rel(s["b"], r3["b"]),
                           g_cub=abs((s["g"] - r3["g"]) / (1.0 - r3["g"])))
            rows.append(row)
            print("  %-6d %-10s %5d %4d %5d  %-9.2e %-9.2e  %-9.2e %-9.2e %-9.2e %-9.2e %7.1f" %
                  (k, tag, s["nodes"], s["mpw"], s["steps"], row["env_cub"], row["env_lin"],
                   row["E_cub"], row["P_cub"], row["b_cub"], row["g_cub"], s["secs"]))
        # Richardson on the last three diagonal levels
        rich = {}
        if len(levels) >= 3:
            l1, l2, l3 = ["r%d" % v for v in levels[-3:]]
            for k in (1, 2, 3):
                if all((k, t) in runs for t in (l1, l2, l3)):
                    q = {}
                    for key in ("E", "P", "b"):
                        a, b, c = (runs[(k, t)][0][key] for t in (l1, l2, l3))
                        if (b - a) * (c - b) > 0 and abs(c - b) > 0:
                            p = np.log2((b - a) / (c - b))
                            q[key] = dict(order=float(p), limit=float(c + (c - b) / (2 ** p - 1)))
                        else:
                            q[key] = dict(order=None, limit=None)
                    rich[k] = q
                    print("  Richardson kernel %d: " % k + "  ".join(
                        "%s order %s limit %s" % (key, "%.2f" % v["order"] if v["order"] else "-",
                                                  "%.5g" % v["limit"] if v["limit"] else "-")
                        for key, v in q.items()))
        result[und] = dict(rows=rows, richardson=rich, top=top)
    if o.json:
        json.dump(result, open(o.json, "w"), indent=1, default=float)


if __name__ == "__main__":
    main()
