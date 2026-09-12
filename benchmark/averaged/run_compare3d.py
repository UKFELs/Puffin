#!/usr/bin/env python3
"""Run the averaged and unaveraged solvers on the same 3D deck and compare.

Usage:
  python3 run_compare3d.py <helical|planepole|curved> [--aw AW] [--np N]
                           [--periods N] [--writes N]
                           [--plain NODES:STEPS ...] [--avg CELLS:STEPS ...]

  --plain NODES:STEPS   unaveraged run, nodesPerLambdar = NODES,
                        stepsPerPeriod = STEPS             [default 12:30 24:60]
  --avg CELLS:STEPS     averaged run, lambdarPerCell = CELLS,
                        stepsPerPeriod = STEPS             [default 1:2]
  --np N                MPI ranks                          [default 4]
  --periods N           undulator periods                  [default 120]
  --writes N            integrated writes over the run     [default 20]

The first --avg run is compared against every --plain run.  Refining the
unaveraged mesh is the point of the scan: the 1D study found the unaveraged
solver is itself several per cent low at nodesPerLambdar = 12, because linear
deposition and linear interpolation each attenuate a carrier sampled at 11
cells per wavelength, so a gap at n12 that closes as the mesh refines is the
unaveraged run converging, not an averaged-mode error.  Adding 48:120 makes
that much clearer and costs roughly 4x the n12 run.

Needs mpirun and h5dump; standard library Python only.  PUFFIN overrides the
executable path.
"""

import os
import re
import shutil
import subprocess
import sys

from compare3d import metrics

HERE = os.path.dirname(os.path.abspath(__file__))
PUFFIN = os.environ.get("PUFFIN",
                        os.path.join(HERE, "..", "..", "build", "puffin", "puffin"))


def sub(text, name, value):
    return re.sub(r"(?m)^(\s*%s\s*=\s*)[^\n!]*" % name, r"\g<1>%s " % value, text)


def run(tag, und, aw, averaged, mesh, steps, nranks, periods, writes):
    workdir = os.path.join(HERE, "run3d_%s" % tag)
    shutil.rmtree(workdir, ignore_errors=True)
    os.makedirs(workdir)
    shutil.copy(os.path.join(HERE, "beam_file3d.in"),
                os.path.join(workdir, "beam_file.in"))
    shutil.copy(os.path.join(HERE, "seed_file3d.in"),
                os.path.join(workdir, "seed_file.in"))

    with open(os.path.join(HERE, "deck3d.in")) as fh:
        deck = fh.read()
    deck = sub(deck, "zundType", "'%s'" % und)
    deck = sub(deck, "saw", aw)
    deck = sub(deck, "nPeriods", periods)
    deck = sub(deck, "stepsPerPeriod", steps)
    deck = sub(deck, "iWriteIntNthSteps", max(1, periods * int(steps) // writes))
    deck = sub(deck, "iWriteNthSteps", periods * int(steps))
    if averaged:
        mode = " qAveraged = .true.\n lambdarPerCell = %s\n" % mesh
    else:
        deck = sub(deck, "nodesPerLambdar", mesh)
        mode = " qAveraged = .false.\n"
    deck = deck.replace("&MDATA\n", "&MDATA\n" + mode)
    with open(os.path.join(workdir, "run.in"), "w") as fh:
        fh.write(deck)

    env = dict(os.environ, OMP_NUM_THREADS="1")
    proc = subprocess.run(["mpirun", "-np", str(nranks), PUFFIN, "run.in"],
                          cwd=workdir, env=env,
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    out = proc.stdout.decode(errors="replace")
    if proc.returncode != 0 or "Tried rearranging" in out:
        sys.stderr.write(out[-2000:])
        raise SystemExit("puffin failed for %s" % tag)
    m = re.search(r"Finished undulator module in\s+([0-9.E+-]+)", out)
    return workdir, float(m.group(1)) if m else float("nan")


def main():
    args = sys.argv[1:]
    if not args:
        raise SystemExit(__doc__)
    und, aw = args.pop(0), "1.0121809"
    nranks, periods, writes = 4, 120, 20
    plains, avgs, which = [], [], None
    while args:
        a = args.pop(0)
        if a == "--aw":
            aw = args.pop(0)
        elif a == "--np":
            nranks = int(args.pop(0))
        elif a == "--periods":
            periods = int(args.pop(0))
        elif a == "--writes":
            writes = int(args.pop(0))
        elif a in ("--plain", "--avg"):
            which = plains if a == "--plain" else avgs
        elif which is None:
            raise SystemExit("stray argument %r before --plain/--avg" % a)
        else:
            which.append(tuple(a.split(":")))
    plains = plains or [("12", "30"), ("24", "60")]
    avgs = avgs or [("1", "2")]

    avg_runs = []
    for cells, steps in avgs:
        wd, secs = run("%s_avg_c%s_s%s" % (und, cells, steps), und, aw, True,
                       cells, steps, nranks, periods, writes)
        avg_runs.append((cells, steps, wd, secs))
        print("  averaged   lambdarPerCell=%-5s steps=%-4s %8.1f s" % (cells, steps, secs),
              flush=True)

    ref_avg = avg_runs[0][2]
    print()
    print("  %-8s %-18s %-10s %-9s %-9s %-9s %s"
          % ("und", "unaveraged", "runtime", "P ratio", "b ratio", "fld sig", "beam sig"))
    for nodes, steps in plains:
        wd, secs = run("%s_plain_n%s_s%s" % (und, nodes, steps), und, aw, False,
                       nodes, steps, nranks, periods, writes)
        res, _ = metrics(wd, ref_avg)

        def ratio(name):
            p, a = res.get(name, (float("nan"),) * 2)
            return a / p if p else float("nan")

        print("  %-8s n=%-4s s=%-9s %8.1f s %-9.4f %-9.4f %-9.4f %.4f"
              % (und, nodes, steps, secs, ratio("power"), ratio("bunching"),
                 ratio("field sigma_x"), ratio("beam sigma_x")),
              flush=True)

    print()
    print("  full tables:  python3 compare3d.py <plain_dir> %s" % ref_avg)


if __name__ == "__main__":
    main()
