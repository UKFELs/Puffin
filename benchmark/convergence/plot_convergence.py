#!/usr/bin/env python3
"""The step-size convergence figure for the deposition kernel (iShapeOrder).

Usage:
  python3 plot_convergence.py OUT.png --scans DIR [--title T] [--scenario TEXT]
        [--command TEXT]

  DIR holds the SCAN_JSON files that run_scan.py writes, named
    pp_n12_o{1,2,3}.json    1D planepole, nodesPerLambdar = 12
    pp_n24_o{1,2,3}.json    1D planepole, nodesPerLambdar = 24
    hel_n12_o{1,2,3}.json   1D helical, nodesPerLambdar = 12
    3d_o{1,2,3}.json        the 3D e2e deck
    3d_o{3,2,1}xy{1,1,3}.json   the same, z2 and x/y degrees differing
  (make_figures.sh runs them all).  A missing file leaves its curves out.

Panels: relative L2 error of the final field against each kernel's own
240 step/period reference, against steps per period, for each of the four;
the observed order (slope from 12 to 20 steps/period) against the kernel's
degree; what the cubic and quadratic kernels are worth against the linear one,
step by step, in each case; and the error against cells crossed per step.
Same layout and look as ../averaged/plot_compare_latt.py.  Needs numpy and
matplotlib.
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
CASES = (("pp_n12", "1D planepole, 12 nodes/λr", "o", "-"),
         ("pp_n24", "1D planepole, 24 nodes/λr", "s", "--"),
         ("hel_n12", "1D helical, 12 nodes/λr", "D", ":"),
         ("3d", "3D e2e deck, 21 nodes/λr", "^", "-."))
FIT = (12, 20)          # the order is the slope between these two step counts
STEP_TICKS = [10, 12, 16, 20, 30, 40, 60]


def load(d, name):
    p = os.path.join(d, name + ".json")
    if not os.path.exists(p):
        return None
    with open(p) as fh:
        return json.load(fh)


def series(scan):
    """(steps, err), sorted by steps.  Rows from before the quiver split was
    removed carry a split flag; only the unsplit ones are the shipped scheme."""
    rows = sorted((r["steps"], r["err"]) for r in scan["runs"] if not r.get("split", False))
    return np.array([s for s, _ in rows]), np.array([e for _, e in rows])


def order(steps, err):
    """Slope of log err against log(1/steps) between the two step counts in FIT."""
    e = dict(zip(steps, err))
    if FIT[0] not in e or FIT[1] not in e:
        return np.nan
    return np.log(e[FIT[0]] / e[FIT[1]]) / np.log(FIT[1] / FIT[0])


def steps_axis(ax):
    ax.set_xticks(STEP_TICKS)
    ax.get_xaxis().set_major_formatter(matplotlib.ticker.ScalarFormatter())
    ax.get_xaxis().set_minor_formatter(matplotlib.ticker.NullFormatter())
    ax.set_xlabel("steps per undulator period")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--scans", required=True)
    ap.add_argument("--title", default="")
    ap.add_argument("--scenario", default="")
    ap.add_argument("--command", default="")
    o = ap.parse_args()

    S = {key: {k: load(o.scans, "%s_o%d" % (key, k)) for k in (1, 2, 3)}
         for key, _, _, _ in CASES}

    text = "\n".join(textwrap.fill(" ".join(par.split()), 200)
                     for par in o.scenario.split("\n\n"))
    nlines = text.count("\n") + 1 if text else 0
    fig = plt.figure(figsize=(18, 11 + 0.18 * nlines))
    top_in = 0.75 + 0.18 * nlines + 0.45
    gs = fig.add_gridspec(2, 4, hspace=0.42, wspace=0.32,
                          top=1 - top_in / fig.get_figheight())
    ax = {key: fig.add_subplot(gs[0, i]) for i, (key, _, _, _) in enumerate(CASES)}
    ax["ord"] = fig.add_subplot(gs[1, 0])
    ax["gain"] = fig.add_subplot(gs[1, 1:3])
    ax["cps"] = fig.add_subplot(gs[1, 3])

    # error against step, one panel per case
    for key, title, m, _ in CASES:
        for k in (1, 2, 3):
            sc = S[key][k]
            if sc is None:
                continue
            s, e = series(sc)
            lab, c = KERN[k]
            ax[key].loglog(s, e, "-" + m, color=c, lw=1.6, ms=4, label=lab)
        if key == "3d":
            for kz, kxy in ((3, 1), (2, 1), (1, 3)):
                sc = load(o.scans, "3d_o%dxy%d" % (kz, kxy))
                if sc is None:
                    continue
                s, e = series(sc)
                ax[key].loglog(s, e, "--" + m, color=KERN[kz][1], lw=1.3, ms=4, mfc="white",
                               label="%s z2, %s x/y" % (KERN[kz][0].split()[0],
                                                       KERN[kxy][0].split()[0]))
        ax[key].set_title(title, loc="left", pad=8)
        ax[key].set_ylabel("relative L2 error, final field")
        steps_axis(ax[key])
        ax[key].legend(fontsize=7 if key == "3d" else 7.5, loc="lower left")
        lin, cub = S[key][1], S[key][3]
        if lin and cub:
            e0, e1 = dict(zip(*series(lin))), dict(zip(*series(cub)))
            if 30 in e0 and 30 in e1:
                ax[key].annotate("", xy=(30, e1[30]), xytext=(30, e0[30]),
                                 arrowprops=dict(arrowstyle="->", color="#495057", lw=1.1))
                ax[key].text(0.97, 0.97, "at 30 steps/period:\ncubic %.1fx less error"
                             % (e0[30] / e1[30]), transform=ax[key].transAxes, ha="right",
                             va="top", fontsize=8, family="monospace",
                             bbox=dict(fc="white", ec="#ced4da", alpha=0.9))

    # observed order against degree
    rows = []
    for key, title, m, _ in CASES:
        ps = [order(*series(S[key][k])) if S[key][k] else np.nan for k in (1, 2, 3)]
        rows.append((title, ps))
        ax["ord"].plot([1, 2, 3], ps, m + "-", color="#495057", mfc="white", ms=7, lw=1,
                       label=title)
    ax["ord"].axhline(4, color="#adb5bd", lw=1, ls=":")
    ax["ord"].text(1.02, 4.08, "RK4's design order", fontsize=7.5, color="#6c757d")
    ax["ord"].set_xticks([1, 2, 3], ["linear\nC0", "quadratic\nC1", "cubic\nC2"])
    ax["ord"].set_xlim(0.7, 3.3)
    ax["ord"].set_ylim(0, 6)
    ax["ord"].set_title("Observed order, slope %d→%d steps/period" % FIT, loc="left", pad=8)
    ax["ord"].set_ylabel("p in error ∝ (1/steps)^p")
    ax["ord"].legend(fontsize=7, loc="lower right")

    # what the kernel is worth, step by step
    for key, title, m, ls in CASES:
        lin = S[key][1]
        if lin is None:
            continue
        s0, e0 = series(lin)
        base = dict(zip(s0, e0))
        for k in (2, 3):
            sc = S[key][k]
            if sc is None:
                continue
            s, e = series(sc)
            com = [v for v in s if v in base]
            g = [base[v] / dict(zip(s, e))[v] for v in com]
            lab, c = KERN[k]
            ax["gain"].semilogy(com, g, ls + m, color=c, ms=5, lw=1.5,
                                mfc=c if k == 3 else "white",
                                label="%s, %s" % (title, lab.split()[0]))
    ax["gain"].axhline(1, color="#adb5bd", lw=1)
    ax["gain"].set_title("Error reduced by the smoother kernel (linear error / its error)",
                         loc="left", pad=8)
    ax["gain"].set_ylabel("error reduced by")
    steps_axis(ax["gain"])
    ax["gain"].set_ylim(0.8, 1000)
    ax["gain"].legend(fontsize=7, loc="upper left", ncol=2)

    # error against cells crossed per step
    for key, title, m, ls in CASES:
        for k in (1, 3):
            sc = S[key][k]
            if sc is None:
                continue
            s, e = series(sc)
            lab, c = KERN[k]
            ax["cps"].loglog(sc["nodes"] / s, e, ls + m, color=c, ms=4, lw=1.1,
                             mfc=c if k == 3 else "white")
    ax["cps"].set_title("Error against cells per step", loc="left", pad=8)
    ax["cps"].set_xlabel("cells per step (nodes/λr ÷ steps/period)")
    ax["cps"].set_ylabel("relative L2 error, final field")
    ticks = [0.2, 0.3, 0.5, 1.0, 2.0]
    ax["cps"].set_xticks(ticks, [("%g" % t) for t in ticks])
    ax["cps"].get_xaxis().set_minor_formatter(matplotlib.ticker.NullFormatter())
    ax["cps"].text(0.03, 0.97, "blue open: linear\norange filled: cubic\nline style and marker:\n"
                   "case, as in the panel left", transform=ax["cps"].transAxes, va="top",
                   fontsize=7.5, color="#343a40")

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
        print("  order %-28s %s" % (lab, "  ".join("%.2f" % p for p in ps)))


if __name__ == "__main__":
    main()
