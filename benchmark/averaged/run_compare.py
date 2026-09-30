#!/usr/bin/env python3
"""Run the averaged and unaveraged solvers on the same 1D deck and compare.

Usage:
  python3 run_compare.py <helical|planepole> [--aw AW]
                         [--plain NODES:STEPS ...] [--avg CELLS:STEPS ...]

  --plain NODES:STEPS   unaveraged run, nodesPerLambdar = NODES,
                        stepsPerPeriod = STEPS     [default 12:30 24:60 48:120]
  --avg CELLS:STEPS     averaged run, lambdarPerCell = CELLS,
                        stepsPerPeriod = STEPS     [default 1:2]

The first --avg run is compared against every --plain run: envelope relative L2
difference, and mid-bunch power ratio at the last write.  Keep NODES/STEPS at
0.4 cells per step or finer for the unaveraged runs, or they are measuring
their own step error (see benchmark/convergence).  Needs mpirun and h5dump;
standard library Python only.  PUFFIN overrides the executable path.
"""

import os
import re
import shutil
import subprocess
import sys

from compare import metrics

HERE = os.path.dirname(os.path.abspath(__file__))
PUFFIN = os.environ.get("PUFFIN",
                        os.path.join(HERE, "..", "..", "build", "puffin", "puffin"))
RHO = 0.005


def sub(text, name, value):
    return re.sub(r"(?m)^(\s*%s\s*=\s*)[^\n]*$" % name, r"\g<1>%s" % value, text)


def run(tag, und, aw, averaged, mesh, steps):
    workdir = os.path.join(HERE, "run_%s" % tag)
    shutil.rmtree(workdir, ignore_errors=True)
    os.makedirs(workdir)
    shutil.copy(os.path.join(HERE, "beam_file.in"), workdir)

    with open(os.path.join(HERE, "seed_file.in")) as fh:
        seed = fh.read()
    if und == "helical":            # the resonant helicity, circularly polarised
        seed = sub(seed, "sA0_Y", "0.01")
    with open(os.path.join(workdir, "seed_file.in"), "w") as fh:
        fh.write(seed)

    with open(os.path.join(HERE, "deck.in")) as fh:
        deck = fh.read()
    deck = sub(deck, "zundType", "'%s'" % und)
    deck = sub(deck, "saw", aw)
    deck = sub(deck, "stepsPerPeriod", steps)
    deck = sub(deck, "iWriteIntNthSteps", steps)     # one integrated write per period
    if averaged:
        mode = " qAveraged = .true.\n lambdarPerCell = %s\n" % mesh
    else:
        deck = sub(deck, "nodesPerLambdar", mesh)
        mode = " qAveraged = .false.\n"
    deck = deck.replace("&MDATA\n", "&MDATA\n" + mode)
    with open(os.path.join(workdir, "run.in"), "w") as fh:
        fh.write(deck)

    env = dict(os.environ, OMP_NUM_THREADS="1")
    proc = subprocess.run(["mpirun", "-np", "1", PUFFIN, "run.in"], cwd=workdir, env=env,
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    out = proc.stdout.decode(errors="replace")
    if proc.returncode != 0 or "Tried rearranging" in out:
        sys.stderr.write(out[-2000:])
        raise SystemExit("puffin failed for %s" % tag)
    m = re.search(r"Finished undulator module in\s+([0-9.E+-]+)", out)
    return workdir, float(m.group(1)) if m else float("nan")


def main():
    args = sys.argv[1:]
    und, aw = args.pop(0), "1.0121809"
    plains, avgs, which = [], [], None
    while args:
        a = args.pop(0)
        if a == "--aw":
            aw = args.pop(0)
        elif a in ("--plain", "--avg"):
            which = plains if a == "--plain" else avgs
        else:
            which.append(tuple(a.split(":")))
    plains = plains or [("12", "30"), ("24", "60"), ("48", "120")]
    avgs = avgs or [("1", "2")]

    avg_runs = []
    for cells, steps in avgs:
        wd, secs = run("%s_avg_c%s_s%s" % (und, cells, steps), und, aw, True, cells, steps)
        avg_runs.append((cells, steps, wd, secs))
        print("  averaged   lambdarPerCell=%-5s steps=%-4s %7.1f s" % (cells, steps, secs),
              flush=True)

    ref_avg = avg_runs[0][2]
    print()
    print("  %-10s %-16s %-9s %-12s %s" % ("und", "unaveraged", "runtime", "env rel L2",
                                          "P_avg/P_unavg (last write)"))
    for nodes, steps in plains:
        wd, secs = run("%s_plain_n%s_s%s" % (und, nodes, steps), und, aw, False, nodes, steps)
        l2, _, power = metrics(wd, ref_avg, RHO, und)
        k, pp, pa = power[-1]
        print("  %-10s n=%-4s s=%-8s %7.1f s %-12.4e %.4f" % (und, nodes, steps, secs, l2, pa / pp),
              flush=True)


if __name__ == "__main__":
    main()
