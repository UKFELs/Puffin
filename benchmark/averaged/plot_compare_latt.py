#!/usr/bin/env python3
"""Overlay an averaged and an unaveraged run, 1D or 3D, on the same axes.

Usage:
  python3 plot_compare_latt.py OUT.png --unavg DIR --avg DIR [--before DIR]
        [--fold F] [--modules "A:B:label;..."] [--title T] [--scenario TEXT]
        [--specx LO,HI] [--command TEXT]

  DIR         a run directory written by run_compare_latt.py (run_*_aperp_*,
              run_integrated_*, run_electrons_*).
  --before    a second averaged run to draw dotted, e.g. one made before a fix.
  --fold F    the frequency, relative to the carrier, that the radiation and
              bunching sit at [1].  The phase space is folded at that
              wavelength, and the profiles along the pulse are averaged over
              the beat period with the carrier (1/|1-F| wavelengths), or over
              one wavelength when F = 1.
  --modules   undulator modules to shade on the z axes, in periods.
  --scenario  text under the title; paragraphs separated by a blank line.

Panels: peak power, pulse energy, beam mean energy and |bunching| along the
undulator; and at the last dump, power along the pulse, the spectrum of the
middle 30% of the pulse, and the longitudinal phase space of the middle 30%
of the beam, both runs overlaid.  Bunching, <gamma> and sigma_gamma are means
over the middle 30% of the slices holding beam (the flat top).  SI units throughout, using the datasets
Puffin writes (powerSI, Lc, Lg), via utilities/pyPlotting/puffin_viz_data.
Each run's legend entry carries its solver time and MPI ranks, from the
stdout.txt run_compare_latt.py keeps, and the ratio box the speed-up.  Time
the two runs on the same ranks and an otherwise idle machine for the ratio
to mean anything.  Needs numpy, h5py and matplotlib.
"""

import argparse
import glob
import os
import re
import sys
import textwrap

import h5py
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "utilities", "pyPlotting"))
import puffin_viz_data as pvd  # noqa: E402

C_UN, C_AV, C_PRE = "#1f4e9c", "#d9480f", "#868e96"


def idx(p):
    return int(re.findall(r"_(\d+)\.h5$", p)[0])


def files(d, kind):
    out = sorted(glob.glob(os.path.join(d, "*_%s_*.h5" % kind)), key=idx)
    if not out:
        raise SystemExit("no %s files in %s" % (kind, d))
    return out


def smooth(y, k):
    k = max(1, int(k))
    return np.convolve(y, np.ones(k) / k, mode="same") if k > 1 else y


def mid(a):
    n = len(a)
    return a[int(0.35 * n):int(0.65 * n)]


def window_nodes(attrs, n, fold):
    """Nodes in one beat period (or one wavelength at fold = 1) on an n-node
    array spanning the field window."""
    rho = float(attrs["rho"])
    lam = 4 * np.pi * rho / (abs(1 - fold) if fold != 1 else 1.0)
    span = float(attrs["sLengthOfElmZ2"]) * (int(attrs["nZ2"]) - 1)
    return round(lam / (span / max(n - 1, 1)))


def runtime(d):
    """(seconds, MPI ranks): the solver time Puffin reports, summed over the
    lattice's modules, from the stdout.txt run_compare_latt.py keeps.  It
    excludes start-up and the initial writes."""
    with open(os.path.join(d, "stdout.txt")) as fh:
        out = fh.read()
    m = re.match(r"# mpirun -np (\d+)", out)
    secs = sum(float(v) for v in re.findall(r"Finished undulator module in\s+([0-9.E+-]+)", out))
    return secs, int(m.group(1)) if m else None


def fmt_time(secs, ranks):
    t = "%.0f s" % secs if secs >= 10 else "%.1f s" % secs
    if secs >= 600:
        t = "%.0f min" % (secs / 60)
    return t + ("" if ranks is None else ", %d rank%s" % (ranks, "" if ranks == 1 else "s"))


def beam_slices(h):
    """A reader of a slice dataset restricted to the slices that hold beam.  The z2 mesh can be longer than the beam, so
    the middle 30% of the whole mesh may take in empty slices, whose mean
    gamma is written as 0; the beam averages use the middle 30% of these."""
    cur = h["beamCurrent"][()]
    occ = cur > 0.5 * cur.max()

    def on(name):           # the slice datasets need not share beamCurrent's mesh
        n = len(h[name])
        return h[name][()][occ[np.round(np.linspace(0, len(occ) - 1, n)).astype(int)]]
    return on


def history(d, fold):
    z, ppk, en, g, sg, b = [], [], [], [], [], []
    for f in files(d, "integrated"):
        with h5py.File(f, "r") as h:
            a = dict(h["runInfo"].attrs)
            p = h["powerSI"][()]
            z.append(float(a["zTotal"]))
            ppk.append(np.max(smooth(p, window_nodes(a, len(p), fold))))
            en.append(pvd.pulse_energy(p, a))
            on = beam_slices(h)
            g.append(np.mean(mid(on("meanGamma"))))
            sg.append(np.mean(mid(on("meanSigmaGamma"))))
            b.append(abs(np.mean(mid(on("bunchingFundamental")))))
    return a, [np.array(v) for v in (z, ppk, en, g, sg, b)]


def profile(d, fold):
    with h5py.File(files(d, "integrated")[-1], "r") as h:
        a = dict(h["runInfo"].attrs)
        p = h["powerSI"][()]
        m = h.get("intFieldMeshSI")
        lo = float(np.ravel(m.attrs["vsLowerBounds"])[0])
        hi = float(np.ravel(m.attrs["vsUpperBounds"])[0])
    return np.linspace(lo, hi, len(p)) * 1e6, smooth(p, window_nodes(a, len(p), fold))


def spectrum(d):
    """Normalised |FT|^2 of the middle 30% of the field, Hann windowed,
    against w/wr.  Every stored component is used: the unaveraged pair
    (A_x, -A_y), or each averaged Cartesian envelope (B_x, B_y).  In 3D the
    spectrum of each transverse node is taken and summed over the mesh, so
    it is the spectrum of the transversely integrated power."""
    with h5py.File(files(d, "aperp")[-1], "r") as h:
        f = h["aperp"][()]
        a = dict(h["runInfo"].attrs)
    n = f.shape[1]
    lo, hi = int(0.35 * n), int(0.65 * n)
    w = np.hanning(hi - lo).reshape((-1,) + (1,) * (f.ndim - 2))
    rho, dz2 = float(a["rho"]), float(a["sLengthOfElmZ2"])
    trans = tuple(range(1, f.ndim - 1))

    def power(sig):
        return (np.abs(np.fft.fft(sig * w, axis=0)) ** 2).sum(axis=trans)

    if pvd.is_averaged(a):
        tot = sum(power(f[2 * c, lo:hi] + 1j * f[2 * c + 1, lo:hi])
                  for c in range(f.shape[0] // 2))
        om = 1.0 - 4.0 * np.pi * rho * np.fft.fftfreq(hi - lo, d=dz2)
        o = np.argsort(om)
        return om[o], tot[o] / tot.max(), a
    if f.ndim == 2:
        om, s = pvd.spectrum(f[0, lo:hi] * w, f[1, lo:hi] * w,
                             dict(a, nZ2=hi - lo))
        return om, s, a
    tot = sum(power(f[c, lo:hi]) for c in range(f.shape[0]))
    om = 4.0 * np.pi * rho * np.fft.fftfreq(hi - lo, d=dz2)
    keep = om > 0                     # a real field: the negative half mirrors it
    om, tot = om[keep], tot[keep]
    o = np.argsort(om)
    return om[o], tot[o] / tot.max(), a


def phase_space(d, fold, nmax=20000):
    with h5py.File(files(d, "electrons")[-1], "r") as h:
        e = h["electrons"][()]
        rho = float(h["runInfo"].attrs["rho"])
    if e.shape[1] != 7:
        e = e.T
    z2, gam = e[:, 2], e[:, 5]
    lo, hi = np.quantile(z2, [0.35, 0.65])
    sel = (z2 > lo) & (z2 < hi)
    z2, gam = z2[sel], gam[sel]
    if len(z2) > nmax:
        k = np.random.default_rng(0).choice(len(z2), nmax, replace=False)
        z2, gam = z2[k], gam[k]
    return np.mod(fold * z2 / (2 * rho), 2 * np.pi), gam


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--unavg", required=True)
    ap.add_argument("--avg", required=True)
    ap.add_argument("--before")
    ap.add_argument("--fold", type=float, default=1.0)
    ap.add_argument("--modules", default="")
    ap.add_argument("--title", default="")
    ap.add_argument("--scenario", default="")
    ap.add_argument("--specx", default="", help="lo,hi of the spectrum axis")
    ap.add_argument("--command", default="", help="the run_compare_latt.py command, printed")
    o = ap.parse_args()

    def label(d):
        """The resolution as the deck asked for it (run.in), not as read back
        from the dump, which carries the mesh after Puffin rounded it."""
        with open(os.path.join(d, "run.in")) as fh:
            deck = fh.read()
        if re.search(r"qAveraged\s*=\s*\.true\.", deck, re.I):
            v = re.search(r"(?m)^\s*lambdarPerCell\s*=\s*([^\s,]+)", deck).group(1)
            return "averaged, %s λr/cell" % v
        v = re.search(r"(?m)^\s*nodesPerLambdar\s*=\s*([^\s,]+)", deck).group(1)
        return "unaveraged, %s nodes/λr" % v

    T = {d: runtime(d) for d in (o.unavg, o.avg)}
    runs = [("%s (%s)" % (label(d), fmt_time(*T[d])), d, c, ls)
            for d, c, ls in ((o.unavg, C_UN, "-"), (o.avg, C_AV, "--"))]
    if o.before:
        runs.append((label(o.before) + ", before Bessel fix", o.before, C_PRE, ":"))

    text = "\n".join(textwrap.fill(" ".join(par.split()), 200)
                     for par in o.scenario.split("\n\n"))
    nlines = text.count("\n") + 1 if text else 0
    fig = plt.figure(figsize=(18, 11 + 0.18 * nlines))
    top_in = 0.75 + 0.18 * nlines + 0.45          # title, caption, gap (inches)
    gs = fig.add_gridspec(2, 4, hspace=0.42, wspace=0.32,
                          top=1 - top_in / fig.get_figheight())
    ax = {
        "P": fig.add_subplot(gs[0, 0]), "E": fig.add_subplot(gs[0, 1]),
        "g": fig.add_subplot(gs[0, 2]), "b": fig.add_subplot(gs[0, 3]),
        "prof": fig.add_subplot(gs[1, 0]), "spec": fig.add_subplot(gs[1, 1]),
        "ps": fig.add_subplot(gs[1, 2:]),
    }

    H = {}
    for lab, d, c, ls in runs:
        a, (z, ppk, en, g, sg, b) = H[lab] = history(d, o.fold)
        ax["P"].semilogy(z, ppk, ls, color=c, lw=1.6, label=lab)
        ax["E"].semilogy(z, en, ls, color=c, lw=1.6, label=lab)
        ax["g"].plot(z, g, ls, color=c, lw=1.6, label=lab)
        ax["g"].fill_between(z, g - sg, g + sg, color=c, alpha=0.08, lw=0)
        ax["b"].plot(z, b, ls, color=c, lw=1.6, label=lab)
        x, p = profile(d, o.fold)
        ax["prof"].plot(x, p / 1e6, ls, color=c, lw=1.4, label=lab)
        om, s, sa = spectrum(d)
        ax["spec"].semilogy(om, s, ls, color=c, lw=1.3, label=lab)

    # module shading, in metres (period length = 4 pi rho Lg)
    per = 4 * np.pi * float(a["rho"]) * float(a["Lg"])
    mods = [m.split(":") for m in o.modules.split(";") if m.strip()]
    for k in ("P", "E", "g", "b"):
        for i, (s0, s1, lab) in enumerate(mods):
            ax[k].axvspan(float(s0) * per, float(s1) * per, color="#e9ecef" if i % 2 else "#f8f9fa",
                          zorder=0)
            ax[k].text((float(s0) + float(s1)) / 2 * per, 1.0, lab, ha="center", va="bottom",
                       fontsize=7.5, color="#495057", transform=ax[k].get_xaxis_transform())
        ax[k].set_xlabel("z (m)")

    lu, la = runs[0][0], runs[1][0]
    r = lambda i: H[la][1][i][-1] / H[lu][1][i][-1]  # noqa: E731
    (tu, nu), (ta, na) = T[o.unavg], T[o.avg]
    speed = "\n\nsolver time, unaveraged / averaged\n %.0fx" % (tu / ta)
    if nu != na:
        speed += " (on %s and %s ranks)" % (nu, na)
    ax["P"].text(0.98, 0.03, "at end, averaged / unaveraged\n peak power %.3f\n pulse energy %.3f"
                 "\n bunching %.3f%s" % (r(1), r(2), r(5), speed),
                 transform=ax["P"].transAxes, ha="right", va="bottom", fontsize=8,
                 family="monospace", bbox=dict(fc="white", ec="#ced4da", alpha=0.9))

    ax["P"].set_title("Peak power", loc="left", pad=14)
    ax["P"].set_ylabel("W")
    ax["E"].set_title("Radiation pulse energy", loc="left", pad=14)
    ax["E"].set_ylabel("J")
    ax["g"].set_title("Beam mean energy ⟨γ⟩/γr  (± σγ)", loc="left", pad=14)
    ax["b"].set_title("|Bunching| (bunchingFundamental, flat-top mean)", loc="left", pad=14)
    sm = "one wavelength" if o.fold == 1 else "beat period %.0f λr" % (1 / abs(1 - o.fold))
    ax["prof"].set_title("Temporal power profile, end", loc="left")
    ax["prof"].set_xlabel("ct − z (µm)")
    ax["prof"].set_ylabel("MW (mean over %s)" % sm)
    ax["spec"].set_title("Spectrum, end (middle 30%)", loc="left")
    ax["spec"].set_xlabel("ω/ωr")
    ax["spec"].set_ylabel("normalised")
    ax["spec"].set_ylim(1e-6, 2)
    if o.specx:
        ax["spec"].set_xlim(*[float(v) for v in o.specx.split(",")])
    else:
        span = max(0.15, 1.6 * abs(1 - o.fold))
        ax["spec"].set_xlim(min(1, o.fold) - span / 2 - 0.05, max(1, o.fold) + span / 2 + 0.05)

    for lab, d, c, _ in runs[:2][::-1]:
        th, gm = phase_space(d, o.fold)
        ax["ps"].scatter(th, gm, s=0.4, color=c, alpha=0.30, lw=0, rasterized=True, label=lab)
    ax["ps"].set_xlim(0, 2 * np.pi)
    ax["ps"].set_title("Longitudinal phase space, end (middle 30% of beam)", loc="left")
    ax["ps"].set_xlabel("ponderomotive phase at ω/ωr = %g, mod 2π" % o.fold)
    ax["ps"].set_ylabel("γ/γr")
    lg = ax["ps"].legend(fontsize=8, loc="upper right", markerscale=12)
    for h in lg.legend_handles:
        h.set_alpha(1)

    for k in ax:
        ax[k].grid(alpha=0.25)
        ax[k].tick_params(labelsize=8)
    ax["E"].legend(fontsize=8, loc="upper left")
    fig.text(0.01, 1 - 0.25 / fig.get_figheight(), o.title, fontsize=14, weight="bold", va="top")
    H_in = fig.get_figheight()
    fig.text(0.01, 1 - 0.65 / H_in, text, fontsize=9.5, va="top", color="#343a40")
    if o.command:
        fig.text(0.01, 0.005, "Runs: " + o.command, fontsize=8, family="monospace",
                 va="bottom", color="#6c757d")
    fig.savefig(o.out, dpi=90, bbox_inches="tight")
    print(o.out)


if __name__ == "__main__":
    main()
