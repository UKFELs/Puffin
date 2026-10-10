#!/usr/bin/env python3
"""Run the averaged and unaveraged solvers on the same 1D SASE deck and compare.

Usage:
  python3 run_compare_sase.py [--np N] [--seeds S[,S...]] [--periods P]
                              [--sigej V] [--mps M] [--reuse]
                              [--plain NODES:STEPS ...] [--avg CELLS:STEPS ...]

  --np N                MPI ranks, the SAME for every run    [default 4]
  --seeds S[,S...]      iRandSeed values.  One value runs the mesh-refinement
                        comparison; several run an ensemble, one averaged and
                        one unaveraged run per seed at the first --plain and
                        --avg setting, and report mean and spread  [default 1]
  --periods P           undulator periods                     [default 340]
  --sigej V             sSigEj_G, the bunch-end rounding      [default 0.1]
  --mps M               iMPsZ2PerWave                        [default 8]
  --reuse               keep run directories that already hold a finished run
                        at the right zbar instead of re-running them
  --plain NODES:STEPS   unaveraged nodesPerLambdar : stepsPerPeriod
                                               [default 12:30 24:60 48:120]
  --avg CELLS:STEPS     averaged lambdarPerCell : stepsPerPeriod  [default 1:2]

Why the rank count is asserted rather than left to the user: the two runs only
share a shot-noise realisation if they share iRandSeed AND the rank count.
init_random_seed (macroparticle_generation/random.f90) builds the RNG seed as
iRandSeed + 37*[...]*tProcInfo_G%rank, so each rank draws its own stream over
the macroparticles it owns; change the rank count and the draws land on
different particles.  The macroparticle count moves with the rank count too
(the sElectronThreshold cull is per rank), so two rank counts are two
realisations - statistically equivalent, but not the same shot noise.

At a fixed rank count the two solver modes do generate the same realisation:
that is checked, not assumed.  compare_sase.noise_match compares the two
initial macroparticle dumps after sorting them on z2, since the beam is
reordered to follow each mode's own field decomposition and shuntBeam offsets
the whole beam by a tenth of a field cell.  Everything else has to agree to the
last bit, and on this deck it does, at 1 and at 4 ranks.

The writes line up without any work because Puffin indexes a dump by the
undulator period it was written at, not by the step, so the same index is the
same zbar in both modes however stepsPerPeriod differs.  Both write intervals
are set to whole periods here to keep that true.

Keep NODES/STEPS at 0.4 cells per step or finer for the unaveraged runs, or
they are measuring their own step error (see benchmark/convergence).  Needs
mpirun and h5dump; standard library Python only.  PUFFIN overrides the
executable path.
"""

import math
import os
import re
import shutil
import subprocess
import sys

from compare import attr, last_index
from compare_sase import (FITHI, FITLO, bands, gain_length, moments, noise_match,
                          saturation, trace)
from run_compare import PUFFIN, RHO, sub

HERE = os.path.dirname(os.path.abspath(__file__))
WRITE_PERIODS = 4        # integrated writes, in undulator periods
FIELD_PERIODS = 20       # /aperp writes, in undulator periods
PERIODS, SIGEJ, MPS = "340", "0.1", "8"   # the deck's own values; see sase.in


def run(tag, averaged, mesh, steps, seed, periods, sigej, mps, nproc, reuse):
#   Anything that changes the beam or the length of the run goes in the
#   directory name, so a --sigej or --periods scan does not quietly overwrite,
#   or --reuse, a run made at a different setting.
    for value, default, letter in ((periods, PERIODS, "p"), (sigej, SIGEJ, "e"),
                                   (mps, MPS, "m")):
        if value != default:
            tag += "_%s%s" % (letter, value)
    workdir = os.path.join(HERE, "run_sase_%s" % tag)

    if reuse and os.path.isdir(workdir):
        ks = last_index(workdir, "integrated")
        if ks:
            p = os.path.join(workdir, "run_integrated_%d.h5" % ks[-1])
            if abs(attr(p, "zbarTotal") - int(periods) * 4.0 * math.pi * RHO) < 1e-4:
                return workdir, float("nan")
        sys.stderr.write("  %s: no finished run to reuse, re-running\n" % tag)

    shutil.rmtree(workdir, ignore_errors=True)
    os.makedirs(workdir)

    with open(os.path.join(HERE, "sase_beam.in")) as fh:
        beam = fh.read()
    beam = sub(beam, "sSigEj_G", sigej)
    beam = sub(beam, "iMPsZ2PerWave", mps)
    with open(os.path.join(workdir, "sase_beam.in"), "w") as fh:
        fh.write(beam)

    with open(os.path.join(HERE, "sase.in")) as fh:
        deck = fh.read()
    deck = sub(deck, "iRandSeed", seed)
    deck = sub(deck, "nPeriods", periods)
    deck = sub(deck, "stepsPerPeriod", steps)
    deck = sub(deck, "iWriteIntNthSteps", WRITE_PERIODS * int(steps))
    deck = sub(deck, "iWriteNthSteps", FIELD_PERIODS * int(steps))
    if averaged:
        mode = " qAveraged = .true.\n lambdarPerCell = %s\n" % mesh
    else:
        deck = sub(deck, "nodesPerLambdar", mesh)
        mode = " qAveraged = .false.\n"
    deck = deck.replace("&MDATA\n", "&MDATA\n" + mode)
    with open(os.path.join(workdir, "run.in"), "w") as fh:
        fh.write(deck)

    env = dict(os.environ, OMP_NUM_THREADS="1")
    proc = subprocess.run(["mpirun", "-np", str(nproc), PUFFIN, "run.in"],
                          cwd=workdir, env=env, stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT)
    out = proc.stdout.decode(errors="replace")
    if proc.returncode != 0 or "Tried rearranging" in out:
        sys.stderr.write(out[-2000:])
        raise SystemExit("puffin failed for %s" % tag)
    m = re.search(r"Finished undulator module in\s+([0-9.E+-]+)", out)
    return workdir, float(m.group(1)) if m else float("nan")


def observables(d, rho):
    """(gain length, P_sat, zbar_sat, band centre, band RMS width) of one run."""
    tr = trace(d, last_index(d, "integrated"))
    lg = gain_length(tr, FITLO, FITHI)[0]
    psat, zsat = saturation(tr)
    k = last_index(d, "aperp")[-1]
    sp, averaged = bands(d, k, rho)
    half = (min(0.5 / attr(os.path.join(d, "run_aperp_%d.h5" % k), "lambdarPerCell"), 0.5)
            if averaged else 0.5)
    _, centre, width = moments(sp, 1.0 - half, 1.0 + half)
    return lg, psat, zsat, centre, width


def stats(vals):
    vals = [v for v in vals if v == v]
    if not vals:
        return float("nan"), float("nan")
    m = sum(vals) / len(vals)
    if len(vals) < 2:
        return m, 0.0
    return m, math.sqrt(sum((v - m) ** 2 for v in vals) / (len(vals) - 1))


HEAD = "  %-14s %-9s %-10s %-10s %-10s %-11s %s"
ROW = "  %-14s %-9s %-10.4f %-10.4e %-10.3f %-11.6f %.4e"
COLS = ("run", "runtime", "Lg(zbar)", "P_sat", "zbar_sat", "centre w/wr", "rms width")


def main():
    args = sys.argv[1:]
    nproc, seeds, reuse = 4, ["1"], False
    periods, sigej, mps = PERIODS, SIGEJ, MPS
    plains, avgs, which = [], [], None
    while args:
        a = args.pop(0)
        if a == "--np":
            nproc = int(args.pop(0))
        elif a == "--seeds":
            seeds = args.pop(0).split(",")
        elif a == "--periods":
            periods = args.pop(0)
        elif a == "--sigej":
            sigej = args.pop(0)
        elif a == "--mps":
            mps = args.pop(0)
        elif a == "--reuse":
            reuse = True
        elif a in ("--plain", "--avg"):
            which = plains if a == "--plain" else avgs
        elif which is None:
            raise SystemExit(__doc__)
        else:
            which.append(tuple(a.split(":")))
    plains = plains or [("12", "30"), ("24", "60"), ("48", "120")]
    avgs = avgs or [("1", "2")]
    iper = int(periods)

    def go(tag, averaged, mesh, steps, seed):
        return run(tag, averaged, mesh, steps, seed, periods, sigej, mps, nproc, reuse)

    print("  %s ranks, %s periods (zbar %.3f), sSigEj_G = %s, %s MPs per wavelength"
          % (nproc, periods, iper * 4.0 * math.pi * RHO, sigej, mps))

    if len(seeds) > 1:
        cells, asteps = avgs[0]
        nodes, psteps = plains[0]
        print("  ensemble over iRandSeed %s: averaged %s:%s against unaveraged %s:%s"
              % (",".join(seeds), cells, asteps, nodes, psteps))
        print()
        print(HEAD % COLS)
        rows = {"averaged": [], "unaveraged": []}
        for seed in seeds:
            wa, sa = go("avg_c%s_s%s_r%s" % (cells, asteps, seed), True, cells, asteps, seed)
            wp, sp_ = go("plain_n%s_s%s_r%s" % (nodes, psteps, seed), False, nodes, psteps, seed)
            ok, msg = noise_match(wp, wa)
            if not ok:
                raise SystemExit("seed %s: shot noise differs - %s" % (seed, msg))
            for name, wd, secs in (("unaveraged", wp, sp_), ("averaged", wa, sa)):
                o = observables(wd, RHO)
                rows[name].append(o)
                print(ROW % (("%s r=%s" % (name[:5], seed), "%.1f s" % secs) + o), flush=True)
        print()
        print(HEAD % COLS)
        for name in ("unaveraged", "averaged"):
            ms = [stats([r[i] for r in rows[name]]) for i in range(5)]
            print(ROW % ((name, "mean") + tuple(m for m, _ in ms)))
            print(ROW % ((name, "sd") + tuple(s for _, s in ms)))
        return

    seed = seeds[0]
    avg_runs = []
    print()
    print(HEAD % COLS)
    for cells, asteps in avgs:
        wd, secs = go("avg_c%s_s%s_r%s" % (cells, asteps, seed), True, cells, asteps, seed)
        avg_runs.append((cells, asteps, wd, secs))
        print(ROW % (("avg %s:%s" % (cells, asteps), "%.1f s" % secs)
                     + observables(wd, RHO)), flush=True)
    for nodes, psteps in plains:
        wd, secs = go("plain_n%s_s%s_r%s" % (nodes, psteps, seed), False, nodes, psteps, seed)
        print(ROW % (("plain %s:%s" % (nodes, psteps), "%.1f s" % secs)
                     + observables(wd, RHO)), flush=True)
        ok, msg = noise_match(wd, avg_runs[0][2])
        print("    shot noise against avg %s:%s - %s"
              % (avg_runs[0][0], avg_runs[0][1], msg), flush=True)
    print()
    print("  full tables:  python3 compare_sase.py run_sase_plain_nN_sS_r%s "
          "run_sase_avg_c%s_s%s_r%s" % (seed, avgs[0][0], avgs[0][1], seed))


if __name__ == "__main__":
    main()
