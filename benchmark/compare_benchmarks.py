#!/usr/bin/env python3
"""Compare two run_benchmarks.py result files and report the deltas.

    ./compare_benchmarks.py before.json after.json
    ./compare_benchmarks.py before.json after.json --threshold 5

The minimum over repetitions is used for the comparison: the minimum is the
least noisy estimator of a run's cost, since scheduler interference and
background load can only ever make a run slower.

With --threshold N, the script exits 1 if any case's integration time
(undulator_s) regressed by more than N percent, so it can gate CI.
"""

import argparse
import json
import sys
from pathlib import Path


def pct(before: float, after: float) -> float:
    return (after - before) / before * 100.0


def warn_host_mismatch(before: dict, after: dict) -> None:
    """Shout if the two result sets came from different machines.

    Absolute timings are meaningless across hosts, so a cross-machine
    comparison produces percentages that look authoritative and mean nothing.
    This is a warning rather than an error: eyeballing two machines is
    occasionally useful, and refusing outright would be obstructive.
    """
    bt = before.get("toolchain", {})
    at = after.get("toolchain", {})
    differing = [k for k in ("host", "platform", "machine", "cpu_count")
                 if bt.get(k) is not None and at.get(k) is not None
                 and bt[k] != at[k]]
    if not differing:
        return
    print("WARNING: these results came from different machines — the deltas "
          "below are not meaningful.", file=sys.stderr)
    for k in differing:
        print(f"  {k}: {bt[k]!r} vs {at[k]!r}", file=sys.stderr)
    print("  Generate a baseline on this machine instead; see "
          "benchmark/README.md.\n", file=sys.stderr)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("before", help="baseline results JSON")
    ap.add_argument("after", help="new results JSON")
    ap.add_argument("--threshold", type=float, default=None,
                    help="fail (exit 1) if undulator time regresses by more than this %%")
    ap.add_argument("--markdown", action="store_true",
                    help="emit a markdown table instead of plain text")
    args = ap.parse_args()

    before = json.loads(Path(args.before).read_text())
    after = json.loads(Path(args.after).read_text())

    b_label = before.get("label") or Path(args.before).stem
    a_label = after.get("label") or Path(args.after).stem

    warn_host_mismatch(before, after)

    names = [n for n in before["results"] if n in after["results"]]
    missing = sorted(set(before["results"]) ^ set(after["results"]))
    if not names:
        print("no cases in common between the two result files", file=sys.stderr)
        return 2

    rows = []
    for name in names:
        b, a = before["results"][name], after["results"][name]
        rows.append((
            name,
            b["ranks"],
            b["undulator_min"], a["undulator_min"], pct(b["undulator_min"], a["undulator_min"]),
            b["wall_min"], a["wall_min"], pct(b["wall_min"], a["wall_min"]),
        ))

    if args.markdown:
        print(f"| Case | Ranks | Integration `{b_label}` (s) | Integration `{a_label}` (s) | Δ | "
              f"Wall `{b_label}` (s) | Wall `{a_label}` (s) | Δ |")
        print("|---|---:|---:|---:|---:|---:|---:|---:|")
        for n, r, bu, au, du, bw, aw, dw in rows:
            print(f"| `{n}` | {r} | {bu:.3f} | {au:.3f} | {du:+.1f}% | "
                  f"{bw:.3f} | {aw:.3f} | {dw:+.1f}% |")
    else:
        print(f"baseline: {b_label}    new: {a_label}")
        print(f"{'case':12s} {'rk':>3s} {'integ before':>13s} {'integ after':>12s} "
              f"{'delta':>8s} {'wall before':>12s} {'wall after':>11s} {'delta':>8s}")
        for n, r, bu, au, du, bw, aw, dw in rows:
            print(f"{n:12s} {r:3d} {bu:13.3f} {au:12.3f} {du:+7.1f}% "
                  f"{bw:12.3f} {aw:11.3f} {dw:+7.1f}%")

    if missing:
        print(f"\nnote: cases present in only one file, skipped: {', '.join(missing)}",
              file=sys.stderr)

    if args.threshold is not None:
        bad = [(n, du) for n, _, _, _, du, *_ in rows if du > args.threshold]
        if bad:
            print(f"\nFAIL: integration time regressed by more than {args.threshold}%:",
                  file=sys.stderr)
            for n, du in bad:
                print(f"  {n}: {du:+.1f}%", file=sys.stderr)
            return 1
        print(f"\nOK: no case regressed by more than {args.threshold}%.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
