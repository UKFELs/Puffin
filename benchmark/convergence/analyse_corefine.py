#!/usr/bin/env python3
"""Analyse the runs written by run_corefine.py.

Usage:
  python3 analyse_corefine.py OUTDIR [--json FILE]

What is measured, for every run:

  Scalars, at the end of the undulator, with the definitions
  ../averaged/plot_compare_latt.py uses: pulse energy E (powerSI summed over
  the window), peak power P (powerSI after a running mean over one resonant
  wavelength), bunching b (|mean bunchingFundamental| over the middle 30% of
  the slices holding beam) and energy loss L = 1 - <gamma>/gamma_r over the
  same slices.  Also the pulse energy along the undulator, one value per
  period.

  The field envelope: each field component times exp(i z2/2rho), low-passed
  with a Gaussian of rms width lambda_r/2 (which removes the twice-carrier term
  to ~1e-9 whatever the node spacing), and evaluated from its Fourier series
  at common points one resonant wavelength apart - so every run, at every
  mesh, is sampled at the same z2 and no interpolation error enters.  The
  filter is part of the observable: it treats every run the same but blurs
  features shorter than about a wavelength.

  The total-energy drift: W = 1/2 sum |A|^2 dV + sum chi Gamma / 2rho from the
  field and electron dumps, the quantity getrhs conserves.  Reported as
  (W_end - W_start) / (field energy gained), since the beam's own energy dwarfs
  what it exchanges.  The gather and scatter are transposes, so the spatially
  discretised system conserves W exactly at any mesh and kernel: the drift
  measures time-integration error only, not the mesh error.

What everything is measured against:

  A Richardson extrapolation from the three finest diagonal levels, per kernel,
  with the order fitted from those levels for the scalars, and fixed at the
  scalars' observed order (2) for the envelope and the energy history, which
  are extrapolated point by point - possible because every run is sampled at
  the same points.  The kernels' extrapolations are averaged to give one
  shared limit, and their spread is reported: if it is far below the coarse
  runs' errors, the kernels share a limit and the comparison is fair.  No
  error is taken against a finite run.

Output columns: E, P, b, L - signed relative error against the shared limit
(L relative to the limit's energy loss); env - relative L2 error of the
envelope against the shared extrapolated envelope; dW - the energy drift
above.  Needs numpy and h5py.
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

ORDER = 2.0      # the scalars' observed order, used for the pointwise extrapolations


def envelope(d):
    """Complex envelope of each field component at the common sample points."""
    f = sorted(glob.glob(os.path.join(d, "*_aperp_*.h5")), key=pcl.idx)[-1]
    with h5py.File(f, "r") as h:
        a = h["aperp"][()]
        rho = float(h["runInfo"].attrs["rho"])
        dz2 = float(h["runInfo"].attrs["sLengthOfElmZ2"])
    n = a.shape[1]
    z = np.arange(n) * dz2
    lam = 4.0 * np.pi * rho
    k = 2.0 * np.pi * np.fft.fftfreq(n, d=dz2)
    filt = np.exp(-0.5 * (k * 0.5 * lam) ** 2)
    zs = (np.arange(int((z[-1] - 4 * lam) / lam)) + 2.5) * lam
    keep = filt > 1e-14                 # the filter's support; beyond it nothing survives
    phase = np.exp(1j * np.outer(zs, k[keep]))
    out = []
    for c in range(a.shape[0]):
        coef = 2.0 * (np.fft.fft(a[c] * np.exp(1j * z / (2.0 * rho))) * filt)[keep] / n
        out.append(phase @ coef)        # the filtered signal, evaluated exactly at zs
    return np.array(out)


def energy_drift(d):
    """(W_end - W_start) / field energy gained, from the field and beam dumps."""
    W, Wf = [], []
    for fa in sorted(glob.glob(os.path.join(d, "*_aperp_*.h5")), key=pcl.idx):
        with h5py.File(fa, "r") as h:
            a = h["aperp"][()]
            at = h["runInfo"].attrs
            rho = float(at["rho"])
            dV = float(at["sLengthOfElmX"]) * float(at["sLengthOfElmY"]) * float(at["sLengthOfElmZ2"])
        with h5py.File(fa.replace("_aperp_", "_electrons_"), "r") as h:
            e = h["electrons"][()]
        if e.shape[1] != 7:
            e = e.T
        wf = 0.5 * np.sum(a * a) * dV
        Wf.append(wf)
        W.append(wf + np.sum(e[:, 6] * e[:, 5]) / (2.0 * rho))
    return (W[-1] - W[0]) / (Wf[-1] - Wf[0])


def scalars(d):
    a, (z, ppk, en, g, sg, bun) = pcl.history(d, 1.0)
    secs, _ = pcl.runtime(d)
    return dict(E=en[-1], P=ppk[-1], b=bun[-1], L=1.0 - g[-1], secs=secs,
                Ehist=en.tolist(), zhist=z.tolist())


def richardson3(a, b, c, p=None):
    """Limit from three levels at ratio 2; the order fitted unless given."""
    if p is None:
        if (b - a) * (c - b) <= 0 or c == b:
            return None, None
        p = np.log2((b - a) / (c - b))
    return c + (c - b) / (2.0 ** p - 1.0), p


def trim(arrs):
    m = min(x.shape[-1] for x in arrs)
    return [x[..., :m] for x in arrs]


def rel_l2(x, ref):
    x, ref = trim([x, ref])
    return float(np.linalg.norm(x - ref) / np.linalg.norm(ref))


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
            s.update(order=k, tag=tag, nodes=nodes, mpw=mpw, steps=steps, dW=energy_drift(d))
            runs[(k, tag)] = (s, envelope(d))
        levels = sorted({int(t[1:]) for _, t in runs if re.fullmatch(r"r\d+", t)})
        if len(levels) < 3:
            raise SystemExit("%s: need three diagonal levels to extrapolate" % und)
        l1, l2, l3 = ["r%d" % v for v in levels[-3:]]
        kernels = [k for k in (1, 2, 3) if all((k, t) in runs for t in (l1, l2, l3))]

        # per-kernel limits
        rich, env_lim, hist_lim = {}, {}, {}
        for k in kernels:
            sa, sb, sc = (runs[(k, t)][0] for t in (l1, l2, l3))
            rich[k] = {}
            for key in ("E", "P", "b", "L"):
                lim, p = richardson3(sa[key], sb[key], sc[key])
                rich[k][key] = dict(limit=lim, order=p)
            ea, eb, ec = trim([runs[(k, t)][1] for t in (l1, l2, l3)])
            env_lim[k], _ = richardson3(ea, eb, ec, ORDER)
            rich[k]["env_order"] = float(np.log2(np.linalg.norm(eb - ea) / np.linalg.norm(ec - eb)))
            ha, hb, hc = (np.array(s["Ehist"]) for s in (sa, sb, sc))
            hist_lim[k], _ = richardson3(ha, hb, hc, ORDER)

        lim = {key: float(np.mean([rich[k][key]["limit"] for k in kernels]))
               for key in ("E", "P", "b", "L")}
        spread = {key: float(np.ptp([rich[k][key]["limit"] for k in kernels]) / abs(lim[key]))
                  for key in ("E", "P", "b", "L")}
        envs = trim([env_lim[k] for k in kernels])
        env_shared = np.mean(envs, axis=0)
        spread["env"] = max(rel_l2(e, env_shared) for e in envs)
        hist_shared = np.mean([hist_lim[k] for k in kernels], axis=0)

        print("\n=== %s: errors against the shared extrapolated limit (levels %s, %s, %s) ===" %
              (und, l1, l2, l3))
        print("  limits: E %.6g  P %.6g  b %.6g  L %.6g" % (lim["E"], lim["P"], lim["b"], lim["L"]))
        print("  kernels' limits differ by at most: " +
              "  ".join("%s %.1e" % (key, v) for key, v in spread.items()))
        for k in kernels:
            print("  kernel %d observed orders: " % k + "  ".join(
                "%s %.2f" % (key, rich[k][key]["order"]) for key in ("E", "P", "b", "L")
                if rich[k][key]["order"] is not None) + "  envelope %.2f" % rich[k]["env_order"])
        print("  %-6s %-10s %5s %4s %5s  %-10s %-10s %-10s %-10s  %-9s  %-9s %7s" %
              ("kernel", "run", "nodes", "mpw", "steps", "E", "P", "b", "L",
               "env", "dW", "secs"))
        rows = []
        for (k, tag) in sorted(runs, key=lambda t: (t[0], len(t[1]) > 3, t[1])):
            s, e = runs[(k, tag)]
            row = dict(s, env=rel_l2(e, env_shared),
                       **{"%s_err" % key: (s[key] - lim[key]) / lim[key] for key in lim})
            h = np.array(s["Ehist"])
            row["Ehist_err"] = (h / hist_shared[:len(h)] - 1.0).tolist()
            rows.append(row)
            print("  %-6d %-10s %5d %4d %5d  %+-10.2e %+-10.2e %+-10.2e %+-10.2e  %-9.2e  %+-9.1e %7.1f" %
                  (k, tag, s["nodes"], s["mpw"], s["steps"], row["E_err"], row["P_err"],
                   row["b_err"], row["L_err"], row["env"], s["dW"], s["secs"]))
        result[und] = dict(rows=rows, levels=[l1, l2, l3], limits=lim, spread=spread,
                           richardson={str(k): v for k, v in rich.items()})
    if o.json:
        json.dump(result, open(o.json, "w"), indent=1, default=float)


if __name__ == "__main__":
    main()
