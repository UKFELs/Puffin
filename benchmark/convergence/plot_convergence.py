#!/usr/bin/env python3
"""The step-size convergence figure for iShapeOrder and qQuiverSplit.

Usage:
  python3 plot_convergence.py OUT.png --scans DIR [--title T] [--scenario TEXT]
        [--command TEXT]

  DIR holds the SCAN_JSON files that run_scan.py writes, named
    pp_n12_o{1,2,3}.json    1D planepole, nodesPerLambdar = 12, split both ways
    hel_n12_o{1,2,3}.json   1D helical, the same
    pp_n24_o{1,2,3}.json    1D planepole, nodesPerLambdar = 24
    3d_o{1,2,3}.json        the 3D e2e deck, unsplit
  (make_figures.sh runs them all).  A missing file leaves its curves out.

Panels: relative L2 error of the final field against each kernel's own
240 step/period reference, against steps per period, for 1D planar, 1D helical
and 3D; the observed order of convergence over 12 to 20 steps/period against
the kernel's degree (planepole and 3D: on helical the split error is not
monotonic between 12 and 20, so a slope there means nothing); the 1D error against cells crossed per step, which is what
the linear kernel's error collapses onto and the cubic's does not; and what
turning qQuiverSplit on is worth, by kernel and undulator.  Same layout and
look as ../averaged/plot_compare_latt.py.  Needs numpy and matplotlib.
"""

import argparse
import json
import os
import textwrap

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

KERN = {1: ("linear (C0)", "#1f4e9c"), 2: ("quadratic (C1)", "#2b8a3e"),
        3: ("cubic (C2)", "#d9480f")}
FIT = (12, 20)          # the order is the slope between these two step counts


def load(d, name):
    p = os.path.join(d, name + ".json")
    if not os.path.exists(p):
        return None
    with open(p) as fh:
        return json.load(fh)


def series(scan, split):
    """(steps, err) for one split setting, sorted by steps."""
    rows = sorted((r["steps"], r["err"]) for r in scan["runs"] if r["split"] == split)
    return np.array([s for s, _ in rows]), np.array([e for _, e in rows])


def order(steps, err):
    """Slope of log err against log(1/steps) between the two step counts in
    FIT - the README's definition.  Not a fit through the points between: with
    the split on, the error is not monotonic there (see the README), and a fit
    through 14 and 16 measures that rather than the order."""
    e = dict(zip(steps, err))
    if FIT[0] not in e or FIT[1] not in e:
        return np.nan
    return np.log(e[FIT[0]] / e[FIT[1]]) / np.log(FIT[1] / FIT[0])


def err_panel(ax, scans, split_too, title):
    for o in (1, 2, 3):
        sc = scans.get(o)
        if sc is None:
            continue
        lab, c = KERN[o]
        if split_too:
            s, e = series(sc, True)
            ax.loglog(s, e, "-o", color=c, lw=1.6, ms=4, label=lab + ", split")
        s, e = series(sc, False)
        ax.loglog(s, e, "--o" if split_too else "-o", color=c, lw=1.3 if split_too else 1.6,
                  ms=3.5, mfc="white" if split_too else c, label=lab + (", unsplit" if split_too else ""))
    ax.set_title(title, loc="left", pad=8)
    ax.set_xlabel("steps per undulator period")
    ax.set_ylabel("relative L2 error, final field")
    ax.set_xticks([10, 12, 16, 20, 30, 40, 60])
    ax.get_xaxis().set_major_formatter(matplotlib.ticker.ScalarFormatter())
    ax.get_xaxis().set_minor_formatter(matplotlib.ticker.NullFormatter())


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--scans", required=True)
    ap.add_argument("--title", default="")
    ap.add_argument("--scenario", default="")
    ap.add_argument("--command", default="")
    o = ap.parse_args()

    S = {key: {k: load(o.scans, "%s_o%d" % (key, k)) for k in (1, 2, 3)}
         for key in ("pp_n12", "hel_n12", "pp_n24", "3d")}

    text = "\n".join(textwrap.fill(" ".join(par.split()), 200)
                     for par in o.scenario.split("\n\n"))
    nlines = text.count("\n") + 1 if text else 0
    fig = plt.figure(figsize=(18, 11 + 0.18 * nlines))
    top_in = 0.75 + 0.18 * nlines + 0.45
    gs = fig.add_gridspec(2, 4, hspace=0.42, wspace=0.32,
                          top=1 - top_in / fig.get_figheight())
    ax = {"pp": fig.add_subplot(gs[0, 0]), "hel": fig.add_subplot(gs[0, 1]),
          "3d": fig.add_subplot(gs[0, 2]), "ord": fig.add_subplot(gs[0, 3]),
          "cps": fig.add_subplot(gs[1, :2]), "spl": fig.add_subplot(gs[1, 2:])}

    err_panel(ax["pp"], S["pp_n12"], True, "1D planepole, 12 nodes/λr")
    err_panel(ax["hel"], S["hel_n12"], True, "1D helical, 12 nodes/λr")
    err_panel(ax["3d"], S["3d"], False, "3D e2e deck, 21 nodes/λr, unsplit")

    # the shipped default and the headline point on the planepole panel
    lin, cub = S["pp_n12"][1], S["pp_n12"][3]
    if lin and cub:
        e0 = dict(zip(*series(lin, False)))
        e1 = dict(zip(*series(cub, True)))
        if 30 in e0 and 30 in e1:
            ax["pp"].annotate("", xy=(30, e1[30]), xytext=(30, e0[30]),
                              arrowprops=dict(arrowstyle="->", color="#495057", lw=1.2))
            ax["pp"].text(31.5, np.sqrt(e0[30] * e1[30]),
                          "%.0fx\nshipped default\n→ cubic + split" % (e0[30] / e1[30]),
                          fontsize=8, color="#343a40", va="center")
    # the 3D gain at 30 steps/period
    l3, c3 = S["3d"][1], S["3d"][3]
    if l3 and c3:
        e0, e1 = dict(zip(*series(l3, False))), dict(zip(*series(c3, False)))
        if 30 in e0 and 30 in e1:
            ax["3d"].annotate("", xy=(30, e1[30]), xytext=(30, e0[30]),
                              arrowprops=dict(arrowstyle="->", color="#495057", lw=1.2))
            ax["3d"].text(31.5, np.sqrt(e0[30] * e1[30]), "%.0fx" % (e0[30] / e1[30]),
                          fontsize=8, color="#343a40", va="center")

    # observed order against degree
    marks = (("pp_n12", True, "o", "1D planepole, split"),
             ("3d", False, "^", "3D, unsplit"))
    rows = []
    for key, sp, m, lab in marks:
        ps = [order(*series(S[key][k], sp)) if S[key][k] else np.nan for k in (1, 2, 3)]
        rows.append((lab, ps))
        ax["ord"].plot([1, 2, 3], ps, m + "-", color="#495057", mfc="white", ms=7, lw=1,
                       label=lab)
    ax["ord"].axhline(4, color="#adb5bd", lw=1, ls=":")
    ax["ord"].text(1.02, 4.1, "RK4's design order", fontsize=7.5, color="#6c757d")
    ax["ord"].set_xticks([1, 2, 3], ["linear\nC0", "quadratic\nC1", "cubic\nC2"])
    ax["ord"].set_xlim(0.7, 3.3)
    ax["ord"].set_title("Observed order, slope from %d to %d steps/period" % FIT, loc="left", pad=8)
    ax["ord"].set_ylabel("p in error ∝ (1/steps)^p")
    ax["ord"].legend(fontsize=8, loc="upper left")
    ax["ord"].set_ylim(0, 6)
    tbl = "\n".join("%-19s %s" % (lab, "  ".join("%4.1f" % p for p in ps)) for lab, ps in rows)
    ax["ord"].text(0.98, 0.03, "order, linear / quad / cubic\n" + tbl,
                   transform=ax["ord"].transAxes, ha="right", va="bottom", fontsize=7.5,
                   family="monospace", bbox=dict(fc="white", ec="#ced4da", alpha=0.9))

    # error against cells per step, split, both mesh densities
    for key, m, nl in (("pp_n12", "o", "12 nodes/λr"), ("pp_n24", "s", "24 nodes/λr")):
        for k in (1, 2, 3):
            sc = S[key][k]
            if sc is None:
                continue
            s, e = series(sc, True)
            cps = sc["nodes"] / s
            lab, c = KERN[k]
            ax["cps"].loglog(cps, e, m + ("-" if key == "pp_n12" else ":"), color=c, ms=6,
                             mfc="white" if key == "pp_n24" else c, lw=1.2,
                             label="%s, %s" % (lab, nl))
    ax["cps"].set_title("1D planepole, split: error against cells crossed per step",
                        loc="left", pad=8)
    ax["cps"].set_xlabel("cells per step (nodesPerLambdar / stepsPerPeriod)")
    ticks = [0.2, 0.3, 0.4, 0.6, 0.8, 1.0, 1.2, 1.5, 2.0]
    ax["cps"].set_xticks(ticks, [("%g" % t) for t in ticks])
    ax["cps"].get_xaxis().set_minor_formatter(matplotlib.ticker.NullFormatter())
    ax["cps"].set_ylabel("relative L2 error, final field")
    ax["cps"].legend(fontsize=8, loc="lower right", ncol=2)
    ax["cps"].text(0.02, 0.97, "Cells per step is the main variable for every kernel.  At the same\n"
                   "cells/step the 24-node mesh has 1.3-3.8x less error than the 12-node\n"
                   "one, by much the same factor for each kernel (README, section 2).",
                   transform=ax["cps"].transAxes, va="top", fontsize=8, color="#343a40")

    # what the split is worth
    for key, ls, und in (("pp_n12", "-", "planepole"), ("hel_n12", "--", "helical")):
        for k in (1, 3):
            sc = S[key][k]
            if sc is None:
                continue
            s0, e0 = series(sc, False)
            s1, e1 = series(sc, True)
            com = sorted(set(s0) & set(s1))
            r = [dict(zip(s0, e0))[s] / dict(zip(s1, e1))[s] for s in com]
            lab, c = KERN[k]
            ax["spl"].semilogy(com, r, ls + "o", color=c, ms=4, lw=1.5,
                               label="%s, %s" % (und, lab))
    ax["spl"].axhline(1, color="#adb5bd", lw=1)
    ax["spl"].set_title("1D, 12 nodes/λr: error reduced by turning qQuiverSplit on",
                        loc="left", pad=8)
    ax["spl"].set_xlabel("steps per undulator period")
    ax["spl"].set_ylabel("unsplit error / split error")
    ax["spl"].legend(fontsize=8, loc="upper right")
    ax["spl"].text(0.02, 0.03, "In 3D the split is worth 1.0x at every kernel and step "
                   "(benchmark/convergence/README.md, section 4b):\nx and y still carry the "
                   "quiver, so it is not plotted here.",
                   transform=ax["spl"].transAxes, va="bottom", fontsize=8, color="#343a40")

    for k in ("pp", "hel", "3d"):
        ax[k].legend(fontsize=7.5, loc="lower left")
    for k in ax:
        ax[k].grid(alpha=0.25, which="both" if k != "ord" else "major")
        ax[k].tick_params(labelsize=8)
    fig.text(0.01, 1 - 0.25 / fig.get_figheight(), o.title, fontsize=14, weight="bold", va="top")
    fig.text(0.01, 1 - 0.65 / fig.get_figheight(), text, fontsize=9.5, va="top", color="#343a40")
    if o.command:
        fig.text(0.01, 0.005, "Runs: " + o.command, fontsize=8, family="monospace",
                 va="bottom", color="#6c757d")
    fig.savefig(o.out, dpi=90, bbox_inches="tight")
    print(o.out)
    for lab, ps in rows:
        print("  order %-22s %s" % (lab, "  ".join("%.2f" % p for p in ps)))


if __name__ == "__main__":
    main()
