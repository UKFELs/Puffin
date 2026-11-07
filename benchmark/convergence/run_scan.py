#!/usr/bin/env python3
"""Step-size convergence scan for the Puffin integrator.

Runs the seeded 1D deck at a range of stepsPerPeriod values and measures the
final radiation field against the finest-step run, which stands in for the
exact solution.  No h5py on this machine, so the field is pulled out of the
HDF5 dump with `h5dump -b LE`, which writes the raw little-endian doubles.

Usage:  python3 run_scan.py <undulator-type> <ref-steps> <steps> [<steps> ...]

Environment:
  SCAN_NODES   nodesPerLambdar to use instead of the deck's
  SCAN_ORDER   iShapeOrder: 1 linear (default), 2 quadratic, 3 cubic
  SCAN_SPLIT   "both" (default) runs every step count unsplit and split;
               "off" unsplit only, "on" split only
  SCAN_DECK    the deck to scan instead of scan.in, e.g. the 3D e2e deck
               test/inputs/3D/clara_test.in
  SCAN_INPUTS  a directory whose entries are linked into every run directory,
               for the files the deck reads [this directory: beam_file.in,
               seed_file.in].  For the 3D deck, test/, which holds inputs/3D/.
  SCAN_RUNS    where the run directories go [runs/ here]
  SCAN_WRITE   write the field, beam and integrated data every this many
               undulator periods, for plotting; off by default, when only the
               start and the end are written.  Writes do not change the result.
  SCAN_JSON    also write the results to this file, for plot_convergence.py
  PUFFIN       path to the puffin binary, if not the in-tree build

The undulator type argument replaces the deck's zundType.
Each run directory keeps Puffin's stdout as stdout.txt, headed by the mpirun
line, which is what ../averaged/plot_compare_latt.py reads solver times from.
"""

import array
import json
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
SPLIT = os.environ.get("SCAN_SPLIT", "both")
DECK = os.path.abspath(os.environ.get("SCAN_DECK", os.path.join(HERE, "scan.in")))
INPUTS = os.path.abspath(os.environ.get("SCAN_INPUTS", HERE))
RUNS = os.path.abspath(os.environ.get("SCAN_RUNS", os.path.join(HERE, "runs")))
WRITE_PERIODS = os.environ.get("SCAN_WRITE")
JSON_OUT = os.environ.get("SCAN_JSON")

KERNEL_NAME = {1: "linear", 2: "quadratic", 3: "cubic"}


def write_deck(path, und_type, steps, split, order):
    with open(DECK) as fh:
        text = fh.read()
    if NODES_PER_LAMBDAR:
        text = re.sub(r"nodesPerLambdar\s*=\s*\d+",
                      "nodesPerLambdar        = %s" % NODES_PER_LAMBDAR, text)
    text = re.sub(r"zundType\s*=\s*'[^']*'", "zundType               = '%s'" % und_type, text)
    text = re.sub(r"stepsPerPeriod\s*=\s*\d+", "stepsPerPeriod         = %d" % steps, text)
    every = steps * int(WRITE_PERIODS) if WRITE_PERIODS else 1000000
    for key in ("iWriteNthSteps", "iWriteIntNthSteps"):
        text = re.sub(r"\b%s\s*=\s*\d+" % key, "%-22s = %d" % (key, every), text)
    text = text.replace(
        "&MDATA\n",
        "&MDATA\n qQuiverSplit           = .%s.\n iShapeOrder            = %d\n"
        % ("true" if split else "false", order))
    with open(path, "w") as fh:
        fh.write(text)


def run(tag, und_type, steps, split=False, order=1, nproc=1):
    # Under runs/, not HERE: a "run_*" directory alongside the script makes
    # rm -rf run_* eat run_scan.py itself. It has happened twice.
    workdir = os.path.join(RUNS, tag)
    shutil.rmtree(workdir, ignore_errors=True)
    os.makedirs(workdir)
    if INPUTS == HERE:
        for f in ("beam_file.in", "seed_file.in"):
            shutil.copy(os.path.join(HERE, f), workdir)
    else:
        for f in os.listdir(INPUTS):
            os.symlink(os.path.join(INPUTS, f), os.path.join(workdir, f))
    write_deck(os.path.join(workdir, "run.in"), und_type, steps, split, order)

    env = dict(os.environ, OMP_NUM_THREADS="1")
    cmd = ["mpirun", "-np", str(nproc), PUFFIN, "run.in"]
    proc = subprocess.run(cmd, cwd=workdir, env=env,
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    with open(os.path.join(workdir, "stdout.txt"), "w") as fh:
        fh.write("# " + " ".join(cmd) + "\n")
        fh.write(proc.stdout.decode(errors="replace"))
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
    with open(DECK) as fh:
        deck_nodes = re.search(r"nodesPerLambdar\s*=\s*(\d+)", fh.read()).group(1)
    nodes = NODES_PER_LAMBDAR or deck_nodes

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
    splits = {"both": (False, True), "off": (False,), "on": (True,)}[SPLIT]
    for split in splits:
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
    nan = (float("nan"), float("nan"), float("nan"))
    for s in steps:
        pe = plain.get(s, nan)[0]
        se = split.get(s, nan)[0]
        print("  %-10s  %-9d  %-10.2f  %-12.4e  %-12.4e  %-12s  %6.2f" %
              (und_type, s, float(nodes) / s, pe, se,
               ("%.1fx" % (pe / se)) if se > 0 else "-",
               split.get(s, plain.get(s, nan))[2]))

    if JSON_OUT:
        with open(JSON_OUT, "w") as fh:
            json.dump({"und": und_type, "order": order, "nodes": float(nodes),
                       "deck": os.path.basename(DECK), "ref_steps": ref_steps,
                       "runs": [{"split": sp, "steps": s, "err": e, "energy_err": d,
                                 "secs": t} for sp, s, e, d, t in rows]},
                      fh, indent=1)


if __name__ == "__main__":
    main()
