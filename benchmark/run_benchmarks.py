#!/usr/bin/env python3
"""Run the Puffin performance benchmarks and record timings.

Each benchmark case is a real Puffin run over an input deck in benchmark/inputs
(or, for the full-lattice case, the deck already committed under test/inputs).
Two numbers are recorded per repetition:

  wall_s       total wall-clock time of the mpirun invocation, i.e. setup +
               beam generation + integration + HDF5 output
  undulator_s  sum of the "Finished undulator module in X seconds" values that
               Puffin itself prints, i.e. the integration loop only

undulator_s is the sharper regression signal (no process startup or MPI
bootstrap noise); wall_s catches regressions in setup and I/O.

Results are written as JSON so two runs can be diffed with
compare_benchmarks.py.

Usage:
    ./run_benchmarks.py --puffin ../build/puffin/puffin --reps 5 -o after.json
    ./run_benchmarks.py --list
"""

import argparse
import json
import os
import platform
import re
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
SRC_ROOT = HERE.parent

# Each case: the main input deck (path relative to the staged working dir) and
# the number of MPI ranks to run it on.
CASES = {
    "1d_serial": {
        "deck": "inputs/bench_1d.in",
        "ranks": 1,
        "desc": "1D flat-top CSE, single rank (isolates serial integration cost)",
    },
    "1d_mpi": {
        "deck": "inputs/bench_1d.in",
        "ranks": 2,
        "desc": "1D flat-top CSE, 2 ranks (adds parallel field/beam exchange)",
    },
    "3d_single": {
        "deck": "inputs/3D/bench_3d.in",
        "ranks": 2,
        "desc": "3D CLARA, single undulator module, 85x85 mesh, 2 ranks",
    },
    "3d_lattice": {
        "deck": "inputs/3D/clara_full.in",
        "ranks": 2,
        "desc": "3D CLARA, full 17-module lattice with quads/drifts/chicanes, 2 ranks",
    },
}

UND_TIME_RE = re.compile(r"Finished undulator module in\s+([0-9.eEdD+-]+)\s+seconds")


def stage_inputs(workdir: Path) -> None:
    """Lay out the input tree the decks expect at ``workdir/inputs``.

    The test inputs go down first, then the benchmark inputs are overlaid on
    top.  That way the benchmark decks reuse the committed beam/seed files and
    the pre-generated HDF5 beam rather than duplicating them.
    """
    dest = workdir / "inputs"
    if dest.exists():
        shutil.rmtree(dest)
    shutil.copytree(SRC_ROOT / "test" / "inputs", dest)
    shutil.copytree(HERE / "inputs", dest, dirs_exist_ok=True)


def run_once(puffin: Path, workdir: Path, deck: str, ranks: int, mpirun: str):
    """Run one repetition; return (wall_s, undulator_s)."""
    cmd = [mpirun, "-n", str(ranks), str(puffin), deck]
    env = dict(os.environ)
    # Puffin is built with OpenMP.  Left unset, the OpenMP runtime spawns one
    # thread per core *per MPI rank*, which oversubscribes the machine and makes
    # timings meaningless (and much slower).  Pin to one thread per rank.
    env["OMP_NUM_THREADS"] = "1"

    t0 = time.perf_counter()
    proc = subprocess.run(
        cmd, cwd=workdir, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT
    )
    wall = time.perf_counter() - t0

    out = proc.stdout.decode("utf-8", errors="replace")
    if proc.returncode != 0:
        sys.stderr.write(out)
        raise RuntimeError(f"puffin exited {proc.returncode} for deck {deck}")

    und = sum(float(m.replace("D", "E")) for m in UND_TIME_RE.findall(out))
    if und == 0.0:
        sys.stderr.write(out)
        raise RuntimeError(f"no undulator timings parsed from run of {deck}")
    return wall, und


def toolchain_info(puffin: Path) -> dict:
    return {
        "host": platform.node(),
        "platform": platform.platform(),
        "machine": platform.machine(),
        "cpu_count": os.cpu_count(),
        "puffin": str(puffin),
        "puffin_mtime": time.strftime(
            "%Y-%m-%d %H:%M:%S", time.localtime(puffin.stat().st_mtime)
        ),
    }


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--puffin", default=str(SRC_ROOT / "build" / "puffin" / "puffin"),
                    help="path to the puffin executable to benchmark")
    ap.add_argument("--mpirun", default="mpirun", help="MPI launcher to use")
    ap.add_argument("--reps", type=int, default=3,
                    help="repetitions per case (the minimum is reported)")
    ap.add_argument("--case", action="append", dest="cases", metavar="NAME",
                    help="run only this case (repeatable); default is all")
    ap.add_argument("-o", "--output", help="write results JSON here")
    ap.add_argument("--label", default="", help="label for this result set, e.g. a git sha")
    ap.add_argument("--workdir", help="scratch dir to run in (default: a temp dir)")
    ap.add_argument("--list", action="store_true", help="list cases and exit")
    args = ap.parse_args()

    if args.list:
        for name, c in CASES.items():
            print(f"{name:12s} {c['ranks']} rank(s)  {c['desc']}")
        return 0

    puffin = Path(args.puffin).resolve()
    if not puffin.is_file():
        ap.error(f"puffin executable not found: {puffin}")

    selected = args.cases or list(CASES)
    for name in selected:
        if name not in CASES:
            ap.error(f"unknown case {name!r}; use --list to see the available cases")

    tmp = None
    if args.workdir:
        workdir = Path(args.workdir).resolve()
        workdir.mkdir(parents=True, exist_ok=True)
    else:
        tmp = tempfile.mkdtemp(prefix="puffin-bench-")
        workdir = Path(tmp)

    try:
        stage_inputs(workdir)

        results = {}
        for name in selected:
            case = CASES[name]
            walls, unds = [], []
            print(f"[{name}] {case['desc']}", flush=True)
            for i in range(args.reps):
                wall, und = run_once(puffin, workdir, case["deck"], case["ranks"],
                                     args.mpirun)
                walls.append(wall)
                unds.append(und)
                print(f"  rep {i + 1}/{args.reps}: wall {wall:7.3f} s   "
                      f"undulator {und:7.3f} s", flush=True)
            results[name] = {
                "desc": case["desc"],
                "deck": case["deck"],
                "ranks": case["ranks"],
                "reps": args.reps,
                "wall_s": walls,
                "undulator_s": unds,
                "wall_min": min(walls),
                "undulator_min": min(unds),
            }
            print(f"  -> min wall {min(walls):7.3f} s   "
                  f"min undulator {min(unds):7.3f} s\n", flush=True)

        payload = {
            "label": args.label,
            "generated": time.strftime("%Y-%m-%d %H:%M:%S"),
            "toolchain": toolchain_info(puffin),
            "results": results,
        }

        if args.output:
            Path(args.output).write_text(json.dumps(payload, indent=2) + "\n")
            print(f"wrote {args.output}")
        else:
            print(json.dumps(payload, indent=2))
        return 0
    finally:
        if tmp:
            shutil.rmtree(tmp, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
