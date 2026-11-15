#!/usr/bin/env python3
"""Co-refinement study: refine the z2 mesh, the macroparticle loading and the
step together, and see what each deposition kernel converges to.

run_scan.py varies the step alone, against a reference on the same mesh with
the same beam, so it measures time-step error at a frozen spatial
discretisation.  It cannot say whether a run is close to the physical answer,
nor compare kernels, which converge to slightly different answers at a fixed
mesh.  This script refines everything at once instead, at fixed ratios, and
compares physical observables across levels and kernels.

Usage:
  python3 run_corefine.py OUTDIR UND [UND ...]

  UND is an undulator type (planepole, helical).  For each, and each kernel
  1-3, runs the seeded 1D deck (scan.in, beam_file.in, seed_file.in) at:

    level r = 1, 2, 4, 8   12r mesh cells per resonant wavelength,
                           iMPsZ2PerWave 4r, stepsPerPeriod 30r - so
                           macroparticles per cell and cells crossed per step
                           stay fixed;
    from r = 2, doubling one factor alone - nodes only, macroparticles only,
                           steps only - to see which one carries the error.

  Writes every period, so each run has a power history.  Runs go to
  OUTDIR/<und>/<tag>, many at once (CORE_JOBS, default 12), one MPI rank each.
  Existing runs with a finished last dump are kept.  Analyse with
  analyse_corefine.py.

Environment: PUFFIN, the binary [in-tree build/]; CORE_JOBS; CORE_LEVELS
  (default "1 2 4 8").
"""

import os
import re
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
PUFFIN = os.environ.get("PUFFIN", os.path.join(HERE, "..", "..", "build", "puffin", "puffin"))
JOBS = int(os.environ.get("CORE_JOBS", "12"))
LEVELS = [int(v) for v in os.environ.get("CORE_LEVELS", "1 2 4 8").split()]

BASE_CELLS, BASE_MPW, BASE_STEPS = 12, 4, 30

# Puffin lays nodesPerLambdar - 1 cells in each resonant wavelength (the input
# counts both end nodes), so 12 cells needs nodesPerLambdar = 13.  Doubling
# nodesPerLambdar instead would not double the cell count, and the ratios this
# study holds fixed would drift.


def nodes_for(cells):
    return cells + 1


def configs():
    """(tag, nodes, mpw, steps): the diagonal, then the one-factor variations."""
    out = []
    for r in LEVELS:
        out.append(("r%d" % r, nodes_for(BASE_CELLS * r), BASE_MPW * r, BASE_STEPS * r))
    if 2 in LEVELS:
        c, m, s = BASE_CELLS * 2, BASE_MPW * 2, BASE_STEPS * 2
        out += [("r2_cells2", nodes_for(2 * c), m, s), ("r2_mps2", nodes_for(c), 2 * m, s),
                ("r2_steps2", nodes_for(c), m, 2 * s)]
    return out


def sub(text, key, value):
    new, n = re.subn(r"(?m)^(\s*%s\s*=\s*)\S+" % key, r"\g<1>%s" % value, text)
    if n != 1:
        raise SystemExit("could not set %s" % key)
    return new


def run(und, order, tag, nodes, mpw, steps, outdir):
    wd = os.path.join(outdir, und, "o%d_%s" % (order, tag))
    if os.path.exists(os.path.join(wd, "done")):
        return wd, "kept"
    shutil.rmtree(wd, ignore_errors=True)
    os.makedirs(wd)
    deck = open(os.path.join(HERE, "scan.in")).read()
    deck = sub(deck, "nodesPerLambdar", nodes)
    deck = sub(deck, "stepsPerPeriod", steps)
    deck = sub(deck, "zundType", "'%s'" % und)
    deck = sub(deck, "iWriteNthSteps", steps * 6)     # field and beam every 6 periods
    deck = sub(deck, "iWriteIntNthSteps", steps)      # integrated every period
    deck = deck.replace("&MDATA\n", "&MDATA\n iShapeOrder            = %d\n" % order)
    open(os.path.join(wd, "run.in"), "w").write(deck)
    beam = open(os.path.join(HERE, "beam_file.in")).read()
    open(os.path.join(wd, "beam_file.in"), "w").write(sub(beam, "iMPsZ2PerWave", mpw))
    shutil.copy(os.path.join(HERE, "seed_file.in"), wd)
    cmd = ["mpirun", "-np", "1", PUFFIN, "run.in"]
    p = subprocess.run(cmd, cwd=wd, env=dict(os.environ, OMP_NUM_THREADS="1"),
                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    out = p.stdout.decode(errors="replace")
    open(os.path.join(wd, "stdout.txt"), "w").write("# " + " ".join(cmd) + "\n" + out)
    if p.returncode != 0 or "completed" not in out:
        return wd, "FAILED"
    open(os.path.join(wd, "done"), "w").write("%d %d %d %d\n" % (order, nodes, mpw, steps))
    return wd, "ok"


def main():
    outdir = os.path.abspath(sys.argv[1])
    unds = sys.argv[2:]
    jobs = [(u, k, *c) for u in unds for k in (1, 2, 3) for c in configs()]
    # biggest first, so the long runs are not left until last
    jobs.sort(key=lambda j: -(j[5] * (j[3] + j[4] * 19)))
    with ThreadPoolExecutor(JOBS) as ex:
        for wd, status in ex.map(lambda j: run(*j, outdir), jobs):
            print("%-8s %s" % (status, os.path.relpath(wd, outdir)), flush=True)


if __name__ == "__main__":
    main()
