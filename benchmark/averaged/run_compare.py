#!/usr/bin/env python3
"""Run the averaged and unaveraged solvers on the same 1D deck and compare.

Usage:
  python3 run_compare.py <helical|planepole|UX,UY> [--aw AW] [--seed AX,AY]
                         [--mpw N] [--every PERIODS]
                         [--plain NODES:STEPS ...] [--avg CELLS:STEPS[:H,H...] ...]

  UX,UY                 the variably polarised 'puffin' undulator, sux = UX,
                        suy = UY (one of them 1) - elliptical, or linear along y
                        with 1,0
  --seed AX,AY          seed sA0_X, sA0_Y.  Default: linear along x for
                        planepole, the resonant helicity (equal) otherwise.
                        Averaged mode carries any seed polarisation.

  --plain NODES:STEPS   unaveraged run, nodesPerLambdar = NODES,
                        stepsPerPeriod = STEPS     [default 12:30 24:60 48:120]
  --avg CELLS:STEPS[:H,H...]
                        averaged run, lambdarPerCell = CELLS,
                        stepsPerPeriod = STEPS, and avgHarmonics = H,H...
                        if given                   [default 1:2]
  --mpw N               iMPsZ2PerWave for every run [default: the beam file's 4].
                        A harmonic band h needs more than 2h.
  --every PERIODS       write /aperp every PERIODS undulator periods, for the
                        harmonic tables [default: first and last only]

Every --avg run is compared against every --plain run: envelope relative L2
difference over both Cartesian envelopes of the fundamental, mid-bunch power
ratio at the last write (total /power, which sums every band), and the
fraction of that power in y for each.  An --avg run with harmonic bands also
gets a table of each band's mid-bunch power against zbar, against the
unaveraged field demodulated at that harmonic (compare.harmonic_power).  Keep
NODES/STEPS at 0.4 cells per step or finer for the unaveraged runs, or they
are measuring their own step error (see benchmark/convergence).  Needs mpirun
and h5dump; standard library Python only.  PUFFIN overrides the executable
path.
"""

import os
import re
import shutil
import subprocess
import sys

from compare import harmonic_power, metrics, print_harmonics

HERE = os.path.dirname(os.path.abspath(__file__))
PUFFIN = os.environ.get("PUFFIN",
                        os.path.join(HERE, "..", "..", "build", "puffin", "puffin"))
RHO = 0.005


def sub(text, name, value):
    return re.sub(r"(?m)^(\s*%s\s*=\s*)[^\n]*$" % name, r"\g<1>%s" % value, text)


def run(tag, und, aw, seed_xy, averaged, mesh, steps, harm="", mpw=None, every=None):
    workdir = os.path.join(HERE, "run_%s" % tag)
    shutil.rmtree(workdir, ignore_errors=True)
    os.makedirs(workdir)

    with open(os.path.join(HERE, "beam_file.in")) as fh:
        beam = fh.read()
    if mpw:
        beam = sub(beam, "iMPsZ2PerWave", mpw)
    with open(os.path.join(workdir, "beam_file.in"), "w") as fh:
        fh.write(beam)

    with open(os.path.join(HERE, "seed_file.in")) as fh:
        seed = fh.read()
    seed = sub(seed, "sA0_X", seed_xy[0])
    seed = sub(seed, "sA0_Y", seed_xy[1])
    with open(os.path.join(workdir, "seed_file.in"), "w") as fh:
        fh.write(seed)

    with open(os.path.join(HERE, "deck.in")) as fh:
        deck = fh.read()
    if "," in und:
        ux, uy = und.split(",")
        deck = sub(deck, "zundType", "'puffin'")
        deck = sub(deck, "sux", ux)
        deck = sub(deck, "suy", uy)
    else:
        deck = sub(deck, "zundType", "'%s'" % und)
    deck = sub(deck, "saw", aw)
    deck = sub(deck, "stepsPerPeriod", steps)
    deck = sub(deck, "iWriteIntNthSteps", steps)     # one integrated write per period
    if every:
        deck = sub(deck, "iWriteNthSteps", int(every) * int(steps))
    if averaged:
        mode = " qAveraged = .true.\n lambdarPerCell = %s\n" % mesh
        if harm:
            mode += " avgHarmonics = %s\n" % harm
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


def tag_of(und):
    return "u" + und.replace(",", "_") if "," in und else und


def main():
    args = sys.argv[1:]
    und, aw = args.pop(0), "1.0121809"
    seed_xy = ("0.01", "0.0") if und == "planepole" else ("0.01", "0.01")
    plains, avgs, which, mpw, every = [], [], None, None, None
    while args:
        a = args.pop(0)
        if a == "--aw":
            aw = args.pop(0)
        elif a == "--seed":
            seed_xy = tuple(args.pop(0).split(","))
        elif a == "--mpw":
            mpw = args.pop(0)
        elif a == "--every":
            every = args.pop(0)
        elif a in ("--plain", "--avg"):
            which = plains if a == "--plain" else avgs
        else:
            which.append(tuple(a.split(":")))
    plains = plains or [("12", "30"), ("24", "60"), ("48", "120")]
    avgs = avgs or [("1", "2")]
    sfx = ("_mpw%s" % mpw if mpw else "") + ("_e%s" % every if every else "")

    avg_runs = []
    for spec in avgs:
        cells, steps, harm = spec[0], spec[1], spec[2] if len(spec) > 2 else ""
        tag = "%s_avg_c%s_s%s%s%s" % (tag_of(und), cells, steps,
                                      "_h" + harm.replace(",", "") if harm else "", sfx)
        wd, secs = run(tag, und, aw, seed_xy, True, cells, steps, harm, mpw, every)
        avg_runs.append((cells, steps, harm, wd, secs))
        print("  averaged   lambdarPerCell=%-5s steps=%-4s harmonics=%-6s %7.1f s"
              % (cells, steps, harm or "-", secs), flush=True)

    for nodes, steps in plains:
        wd, secs = run("%s_plain_n%s_s%s%s" % (tag_of(und), nodes, steps, sfx), und, aw,
                       seed_xy, False, nodes, steps, "", mpw, every)
        print()
        print("  unaveraged n=%s s=%s, %.1f s" % (nodes, steps, secs))
        print("  %-22s %-12s %-10s %s" % ("averaged", "env rel L2", "P_avg/P_un",
                                          "y frac unavg / avg (mid-bunch)"))
        for cells, asteps, harm, awd, _ in avg_runs:
            l2, rows, power = metrics(wd, awd, RHO)
            k, pp, pa = power[-1]
            mid = rows[len(rows) // 2]
            print("  c=%-4s s=%-4s h=%-7s %-12.4e %-10.4f %.4f / %.4f"
                  % (cells, asteps, harm or "-", l2, pa / pp, mid[3], mid[4]), flush=True)
        for cells, asteps, harm, awd, _ in avg_runs:
            if harm:
                hs, hrows = harmonic_power(wd, awd, RHO)
                print()
                print("  band power, averaged c=%s s=%s h=%s against this run:"
                      % (cells, asteps, harm))
                print_harmonics(hs, hrows)


if __name__ == "__main__":
    main()
