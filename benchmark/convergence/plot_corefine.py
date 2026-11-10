#!/usr/bin/env python3
"""Figure for the co-refinement study (run_corefine.py, analyse_corefine.py).

Usage:
  python3 plot_corefine.py OUT.png --json RESULTS.json [--title T]
        [--scenario TEXT] [--command TEXT]

One row per undulator type:
  - field-envelope error against refinement level, each kernel, against the
    finest cubic run (solid) and the finest linear run (dashed);
  - the same for pulse energy;
  - from level 2, which single factor carries the error: the envelope error
    at level 2, with the cells, the macroparticles or the steps doubled alone,
    and with all three doubled (level 4);
  - pulse energy along the undulator at each level, relative to the finest
    cubic run.
Same look as ../averaged/plot_compare_latt.py.  Needs numpy and matplotlib.
"""

import argparse
import json
import textwrap

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

KERN = {1: ("linear", "#1f4e9c"), 2: ("quadratic", "#2b8a3e"), 3: ("cubic", "#d9480f")}
VARIANTS = (("r2", "level 2"), ("r2_cells2", "cells ×2"), ("r2_mps2", "macroparticles ×2"),
            ("r2_steps2", "steps ×2"), ("r4", "all ×2 (level 4)"))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--json", required=True)
    ap.add_argument("--title", default="")
    ap.add_argument("--scenario", default="")
    ap.add_argument("--command", default="")
    o = ap.parse_args()
    R = json.load(open(o.json))
    unds = [u for u in ("planepole", "helical") if u in R]

    text = "\n".join(textwrap.fill(" ".join(p.split()), 200) for p in o.scenario.split("\n\n"))
    nl = text.count("\n") + 1 if text else 0
    fig = plt.figure(figsize=(18, 5.4 * len(unds) + 1.0 + 0.18 * nl))
    top_in = 0.75 + 0.18 * nl + 0.45
    gs = fig.add_gridspec(len(unds), 4, hspace=0.45, wspace=0.32,
                          top=1 - top_in / fig.get_figheight())

    for iu, und in enumerate(unds):
        rows = {(r["order"], r["tag"]): r for r in R[und]["rows"]}
        top = R[und]["top"]
        diag = sorted({t for _, t in rows if t[1:].isdigit()}, key=lambda t: int(t[1:]))
        ax = [fig.add_subplot(gs[iu, i]) for i in range(4)]
        rich = R[und]["richardson"]
        elim = np.mean([rich[str(k)]["E"]["limit"] for k in (1, 2, 3)])

        for k in (1, 2, 3):
            lab, c = KERN[k]
            lv = [t for t in diag if (k, t) in rows and t != top]
            cells = [12 * int(t[1:]) for t in lv]
            ax[0].loglog(cells, [rows[(k, t)]["env_cub"] for t in lv], "-o", color=c, lw=1.6,
                         ms=5, label="%s, against finest cubic" % lab)
            ax[0].loglog(cells, [rows[(k, t)]["env_lin"] for t in lv], "--o", color=c, lw=1.0,
                         ms=4, mfc="white", label="%s, against finest linear" % lab)
            lall = [t for t in diag if (k, t) in rows]
            ax[1].loglog([12 * int(t[1:]) for t in lall],
                         [abs(rows[(k, t)]["E"] / elim - 1.0) for t in lall], "-o", color=c,
                         lw=1.6, ms=5, label=lab)
        if (1, top) in rows:
            v = rows[(1, top)]["env_cub"]
            ax[0].axhline(v, color="#868e96", lw=1, ls=":")
            ax[0].text(0.98, v, " finest linear vs finest cubic", transform=ax[0].get_yaxis_transform(),
                       ha="right", va="bottom", fontsize=7.5, color="#495057")
        for a_, t in ((ax[0], "field envelope error, vs finest run"),
                      (ax[1], "pulse energy error, vs extrapolated limit")):
            a_.set_xticks([12, 24, 48, 96], ["12", "24", "48", "96"])
            a_.get_xaxis().set_minor_formatter(matplotlib.ticker.NullFormatter())
            a_.set_xlabel("cells per λr (all ratios fixed)")
            a_.set_title("%s: %s" % (und, t), loc="left", pad=8, fontsize=10.5)
            a_.set_ylabel("relative error")
        ax[0].legend(fontsize=7, loc="lower left")
        ax[1].legend(fontsize=7.5, loc="lower left")

        w = 0.26
        for i, k in enumerate((1, 2, 3)):
            lab, c = KERN[k]
            vals = [rows[(k, t)]["env_cub"] if (k, t) in rows else np.nan for t, _ in VARIANTS]
            ax[2].bar(np.arange(len(VARIANTS)) + (i - 1) * w, vals, w, color=c, label=lab)
        ax[2].set_yscale("log")
        ax[2].set_xticks(range(len(VARIANTS)), [n for _, n in VARIANTS], rotation=20, ha="right",
                         fontsize=8)
        ax[2].set_title("%s: from level 2, one factor ×2" % und, loc="left", pad=8, fontsize=10.5)
        ax[2].set_ylabel("envelope error, against finest cubic")
        ax[2].legend(fontsize=7.5)

        ref = rows.get((3, top))
        for k, ls in ((1, "-"), (3, "--")):
            lab, c = KERN[k]
            for j, t in enumerate(t for t in diag if t != top):
                if (k, t) not in rows or ref is None:
                    continue
                r = rows[(k, t)]
                z = np.array(r["zhist"])
                e = np.array(r["Ehist"]) / np.array(ref["Ehist"])[:len(z)] - 1.0
                ax[3].plot(z, e, ls, color=c, lw=1.2, alpha=0.4 + 0.6 * (1 - j / max(1, len(diag) - 2)),
                           label="%s, %d cells/λr" % (lab, 12 * int(t[1:])))
        ax[3].axhline(0, color="#adb5bd", lw=1)
        ax[3].set_title("%s: pulse energy along z, vs finest cubic" % und, loc="left", pad=8,
                        fontsize=10.5)
        ax[3].set_xlabel("z (m)")
        ax[3].set_ylabel("relative difference")
        ax[3].legend(fontsize=6.5, loc="lower left", ncol=2)
        for a_ in ax:
            a_.grid(alpha=0.25, which="both")
            a_.tick_params(labelsize=8)

    fig.text(0.01, 1 - 0.25 / fig.get_figheight(), o.title, fontsize=14, weight="bold", va="top")
    fig.text(0.01, 1 - 0.65 / fig.get_figheight(), text, fontsize=9.5, va="top", color="#343a40")
    if o.command:
        fig.text(0.01, 0.005, "Runs: " + o.command, fontsize=8, family="monospace",
                 va="bottom", color="#6c757d")
    fig.savefig(o.out, dpi=90, bbox_inches="tight")
    print(o.out)


if __name__ == "__main__":
    main()
