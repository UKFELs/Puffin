#!/usr/bin/env python3
"""Step-size convergence scan for the Puffin integrator.

Runs the seeded 1D deck at a range of stepsPerPeriod values and measures the
final radiation field against the finest-step run, which stands in for the
exact solution.  No h5py on this machine, so the field is pulled out of the
HDF5 dump with `h5dump -b LE`, which writes the raw little-endian doubles.

Usage:  python3 run_scan.py <undulator-type> <ref-steps> <steps> [<steps> ...]

Environment:
  SCAN_NODES  nodesPerLambdar to use instead of the deck's 12
  SCAN_ORDER  iShapeOrder: 1 linear (default), 2 quadratic, 3 cubic
  PUFFIN      path to the puffin binary, if not the in-tree build
"""

import array
import math
import os
import re
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
PUFFIN = os.environ.get(
    "PUFFIN", os.path.join(HERE, "..", "..", "build", "puffin", "puffin"))


NODES_PER_LAMBDAR = os.environ.get("SCAN_NODES")
SHAPE_ORDER = int(os.environ.get("SCAN_ORDER", "1"))

KERNEL_NAME = {1: "linear", 2: "quadratic", 3: "cubic"}


def write_deck(path, und_type, steps, split, order):
    with open(os.path.join(HERE, "scan.in")) as fh:
        text = fh.read()
    if NODES_PER_LAMBDAR:
        text = re.sub(r"nodesPerLambdar\s*=\s*\d+",
                      "nodesPerLambdar        = %s" % NODES_PER_LAMBDAR, text)
    text = re.sub(r"zundType\s*=\s*'[^']*'", "zundType               = '%s'" % und_type, text)
    text = re.sub(r"stepsPerPeriod\s*=\s*\d+", "stepsPerPeriod         = %d" % steps, text)
    text = text.replace(
        "&MDATA\n",
        "&MDATA\n qQuiverSplit           = .%s.\n iShapeOrder            = %d\n"
        % ("true" if split else "false", order))
    with open(path, "w") as fh:
        fh.write(text)


def run(tag, und_type, steps, split=False, order=1, nproc=1):
    workdir = os.path.join(HERE, "run_%s" % tag)
    shutil.rmtree(workdir, ignore_errors=True)
    os.makedirs(workdir)
    for f in ("beam_file.in", "seed_file.in"):
        shutil.copy(os.path.join(HERE, f), workdir)
    write_deck(os.path.join(workdir, "run.in"), und_type, steps, split, order)

    env = dict(os.environ, OMP_NUM_THREADS="1")
    proc = subprocess.run(["mpirun", "-np", str(nproc), PUFFIN, "run.in"],
                          cwd=workdir, env=env,
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if proc.returncode != 0:
        sys.stderr.write(proc.stdout.decode(errors="replace")[-2000:])
        raise SystemExit("puffin failed for %s steps=%d order=%d"
                         % (und_type, steps, order))

    secs = 0.0
    for line in proc.stdout.decode(errors="replace").splitlines():
        m = re.search(r"Finished undulator module in\s+([0-9.E+-]+)", line)
        if m:
            secs += float(m.group(1))
    return workdir, secs


def read_field(workdir):
    """Return the final aperp dump as a flat list of doubles (real then imag)."""
    dumps = sorted(f for f in os.listdir(workdir)
                   if f.endswith(".h5") and "_aperp_" in f)
    last = max(dumps, key=lambda f: int(re.search(r"_aperp_(\d+)", f).group(1)))
    binpath = os.path.join(workdir, "aperp.bin")
    subprocess.run(["h5dump", "-d", "/aperp", "-b", "LE", "-o", binpath,
                    os.path.join(workdir, last)],
                   check=True, stdout=subprocess.DEVNULL)
    vals = array.array("d")
    with open(binpath, "rb") as fh:
        vals.frombytes(fh.read())
    return list(vals)


def l2(xs):
    return math.sqrt(sum(x * x for x in xs))


def main():
    """<und-type> <ref-steps> <steps>...

    The reference is always run UNSPLIT at <ref-steps>, and with the SAME
    deposition kernel as the runs being measured against it, so what comes out
    is the error from the integration step alone.  Mixing kernels here would
    measure something else: a smoother kernel filters the field slightly
    differently in space, so it converges, at any step size, to a marginally
    different answer.  That spatial difference is real but it is not step-size
    error, and at these step counts it would swamp it.

    Using one reference for both the split and unsplit columns is what makes
    those two comparable, and doubles as the correctness check on the split
    scheme: it solves the same ODE, so at a fine step it has to agree.
    """
    und_type = sys.argv[1]
    ref_steps = int(sys.argv[2])
    steps = [int(s) for s in sys.argv[3:]]

    order = SHAPE_ORDER
    kern = KERNEL_NAME.get(order, str(order))
    nodes = NODES_PER_LAMBDAR or "12"

    print("  kernel: %s (iShapeOrder=%d), nodesPerLambdar=%s"
          % (kern, order, nodes), flush=True)

    wd, secs = run("%s_ref_o%d" % (und_type, order), und_type, ref_steps,
                   split=False, order=order)
    ref = read_field(wd)
    ref_norm = l2(ref)
    ref_energy = sum(x * x for x in ref)
    print("  reference: unsplit, %s kernel, %d steps/period, %.1f s"
          % (kern, ref_steps, secs), flush=True)

    rows = []
    for split in (False, True):
        for s in steps:
            tag = "%s_%d_o%d_%s" % (und_type, s, order, "split" if split else "plain")
            wd, secs = run(tag, und_type, s, split=split, order=order)
            f = read_field(wd)
            err = l2([a - b for a, b in zip(f, ref)]) / ref_norm
            den = abs(sum(x * x for x in f) - ref_energy) / ref_energy
            rows.append((split, s, err, den, secs))
            print("  ran %-10s N=%-4d %-6s err=%.4e" %
                  (und_type, s, "split" if split else "plain", err), flush=True)

    print()
    print("  kernel = %s, nodesPerLambdar = %s" % (kern, nodes))
    print("  %-10s  %-9s  %-10s  %-12s  %-12s  %-12s  %s" %
          ("und", "steps/pd", "cells/step", "plain L2", "split L2",
           "err x lower", "runtime (s)"))
    print("  " + "-" * 88)
    plain = dict((s, (e, d, t)) for sp, s, e, d, t in rows if not sp)
    split = dict((s, (e, d, t)) for sp, s, e, d, t in rows if sp)
    for s in steps:
        pe = plain[s][0]
        se = split[s][0]
        print("  %-10s  %-9d  %-10.2f  %-12.4e  %-12.4e  %-12s  %6.2f" %
              (und_type, s, float(nodes) / s, pe, se,
               ("%.1fx" % (pe / se)) if se > 0 else "-", split[s][2]))


if __name__ == "__main__":
    main()
