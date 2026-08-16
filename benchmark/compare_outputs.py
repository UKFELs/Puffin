#!/usr/bin/env python3
"""Compare Puffin HDF5 output from two run directories, dump by dump.

A companion to the timing benchmarks: when you change the code for speed, this
answers the other half of the question — did the numbers move?

    ./compare_outputs.py <dirA> <dirB> <stem>

e.g. ./compare_outputs.py runA/inputs/3D runB/inputs/3D bench_3d

Dumps are matched on their `istep` attribute rather than on the file index,
because the write-index counter is not guaranteed to agree between two builds.

Why not just h5diff: h5diff's `-p` compares |a-b|/|a| element by element. On a
field mesh whose values span 1e-17 to 1e-3, that metric is dominated by
near-zero nodes, where a relative difference of 1e-3 means an absolute
difference of 1e-20 — it will report thousands of "differences" for a run that
is identical to the last bit everywhere that matters. This script reports the
difference against the *dataset's own scale* instead:

  maxabsdiff/max|A|   largest absolute difference as a fraction of the peak value
  L2rel               ||A - B||_2 / ||A||_2, the whole-field divergence
  maxrel(|A|>1e-3max) worst relative difference restricted to nodes carrying
                      real signal — the honest "relative error" number
  bitdiff             how many values differ at all

For double precision, ~1e-16 is one unit in the last place. Values around
1e-15 to 1e-14 are accumulated round-off. Anything at 1e-9 or above in
`L2rel` or `maxrel` is a real change in behaviour and should be explained.

Run the same comparison across several dumps: a difference that stays flat as
the run proceeds is round-off, while one that grows exponentially is a genuine
divergence being amplified by FEL gain.
"""

import glob
import math
import os
import re
import struct
import subprocess
import sys
import tempfile

DATASETS = (("aperp", "/aperp"), ("electrons", "/electrons"))


def h5_attr(path, dset, name):
    out = subprocess.run(["h5dump", "-a", f"{dset}/{name}", path],
                         capture_output=True, text=True).stdout
    m = re.search(r"\(0\):\s*(\S+)", out)
    return m.group(1) if m else None


def h5_read(path, dset):
    tmp = tempfile.mktemp(suffix=".bin")
    try:
        subprocess.run(["h5dump", "-d", dset, "-b", "LE", "-o", tmp, path],
                       capture_output=True)
        raw = open(tmp, "rb").read()
    finally:
        if os.path.exists(tmp):
            os.unlink(tmp)
    n = len(raw) // 8
    return struct.unpack(f"<{n}d", raw)


def index_by_step(rundir, stem, kind, dset):
    found = {}
    for f in glob.glob(os.path.join(rundir, f"{stem}_{kind}_*.h5")):
        s = h5_attr(f, dset, "istep")
        if s is not None:
            found[int(float(s))] = f
    return found


def compare(fa, fb, dset):
    a, b = h5_read(fa, dset), h5_read(fb, dset)
    if len(a) != len(b):
        return f"SHAPE MISMATCH: {len(a)} vs {len(b)}"
    amax = max(abs(v) for v in a) or 1.0
    dmax = max(abs(x - y) for x, y in zip(a, b))
    l2a = math.sqrt(sum(v * v for v in a)) or 1.0
    l2d = math.sqrt(sum((x - y) ** 2 for x, y in zip(a, b)))
    thr = amax * 1e-3
    sig = [abs(x - y) / abs(x) for x, y in zip(a, b) if abs(x) >= thr]
    nbit = sum(1 for x, y in zip(a, b) if x != y)
    return (f"max|A|={amax:.4e}  maxabsdiff/max|A|={dmax / amax:.3e}  "
            f"L2rel={l2d / l2a:.3e}  maxrel(|A|>1e-3max)={max(sig, default=0.0):.3e}  "
            f"bitdiff={nbit}/{len(a)}")


def main() -> int:
    if len(sys.argv) != 4:
        print(__doc__)
        return 2
    dira, dirb, stem = sys.argv[1:4]
    worst = 0.0
    for kind, dset in DATASETS:
        ia = index_by_step(dira, stem, kind, dset)
        ib = index_by_step(dirb, stem, kind, dset)
        common = sorted(set(ia) & set(ib))
        print(f"--- {kind}: A steps={sorted(ia)}  B steps={sorted(ib)}")
        if not common:
            print("    no dumps at matching istep")
            continue
        for s in common:
            line = compare(ia[s], ib[s], dset)
            print(f"  istep {s:6d}: {line}")
            m = re.search(r"L2rel=(\S+)", line)
            if m:
                worst = max(worst, float(m.group(1)))
    print(f"\nworst L2rel over all matched dumps: {worst:.3e}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
