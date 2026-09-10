#!/usr/bin/env python3
"""Step-size convergence scan for the Puffin integrator.

Runs the seeded 1D deck at a range of stepsPerPeriod values and measures the
final radiation field against the finest-step run, which stands in for the
exact solution.  No h5py on this machine, so the field is pulled out of the
HDF5 dump with `h5dump -b LE`, which writes the raw little-endian doubles.

Usage:  python3 run_scan.py <undulator-type> <steps> [<steps> ...]
The LAST steps value is treated as the reference.
"""

import array
import math
import os
import re
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
PUFFIN = os.path.join(HERE, "..", "build", "puffin", "puffin")


NODES_PER_LAMBDAR = os.environ.get("SCAN_NODES")


def write_deck(path, und_type, steps, split):
    with open(os.path.join(HERE, "scan.in")) as fh:
        text = fh.read()
    if NODES_PER_LAMBDAR:
        text = re.sub(r"nodesPerLambdar\s*=\s*\d+",
                      "nodesPerLambdar        = %s" % NODES_PER_LAMBDAR, text)
    text = re.sub(r"zundType\s*=\s*'[^']*'", "zundType               = '%s'" % und_type, text)
    text = re.sub(r"stepsPerPeriod\s*=\s*\d+", "stepsPerPeriod         = %d" % steps, text)
    text = text.replace(
        "&MDATA\n",
        "&MDATA\n qQuiverSplit           = .%s.\n" % ("true" if split else "false"))
    with open(path, "w") as fh:
        fh.write(text)


def run(tag, und_type, steps, split=False):
    workdir = os.path.join(HERE, "run_%s" % tag)
    shutil.rmtree(workdir, ignore_errors=True)
    os.makedirs(workdir)
    for f in ("beam_file.in", "seed_file.in"):
        shutil.copy(os.path.join(HERE, f), workdir)
    write_deck(os.path.join(workdir, "run.in"), und_type, steps, split)

    env = dict(os.environ, OMP_NUM_THREADS="1")
    proc = subprocess.run(["mpirun", "-np", "1", PUFFIN, "run.in"],
                          cwd=workdir, env=env,
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if proc.returncode != 0:
        sys.stderr.write(proc.stdout.decode(errors="replace")[-2000:])
        raise SystemExit("puffin failed for %s steps=%d" % (und_type, steps))

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

    The reference is always run UNSPLIT at <ref-steps>, so the split runs are
    measured against the same exact solution as the unsplit ones - that is what
    makes the two columns comparable, and it is also the correctness check: the
    split scheme solves the same ODE, so it must converge to the same answer.
    """
    und_type = sys.argv[1]
    ref_steps = int(sys.argv[2])
    steps = [int(s) for s in sys.argv[3:]]

    wd, secs = run("%s_ref" % und_type, und_type, ref_steps, split=False)
    ref = read_field(wd)
    ref_norm = l2(ref)
    ref_energy = sum(x * x for x in ref)
    print("  reference: unsplit, %d steps/period, %.1f s" % (ref_steps, secs), flush=True)

    rows = []
    for split in (False, True):
        for s in steps:
            tag = "%s_%d_%s" % (und_type, s, "split" if split else "plain")
            wd, secs = run(tag, und_type, s, split=split)
            f = read_field(wd)
            err = l2([a - b for a, b in zip(f, ref)]) / ref_norm
            den = abs(sum(x * x for x in f) - ref_energy) / ref_energy
            rows.append((split, s, err, den, secs))
            print("  ran %-10s N=%-4d %-6s err=%.4e" %
                  (und_type, s, "split" if split else "plain", err), flush=True)

    print()
    print("  %-10s  %-9s  %-12s  %-12s  %-12s  %s" %
          ("und", "steps/pd", "plain L2", "split L2", "improvement", "runtime (s)"))
    print("  " + "-" * 76)
    plain = dict((s, (e, d, t)) for sp, s, e, d, t in rows if not sp)
    split = dict((s, (e, d, t)) for sp, s, e, d, t in rows if sp)
    for s in steps:
        pe = plain[s][0]
        se = split[s][0]
        print("  %-10s  %-9d  %-12.4e  %-12.4e  %-12s  %6.2f" %
              (und_type, s, pe, se,
               ("%.1fx" % (pe / se)) if se > 0 else "-", split[s][2]))


if __name__ == "__main__":
    main()
