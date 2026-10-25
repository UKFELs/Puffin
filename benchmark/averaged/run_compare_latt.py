#!/usr/bin/env python3
"""Averaged against unaveraged on deck.in with a lattice, a taper or a second seed.

Usage:
  python3 run_compare_latt.py <helical|planepole|UX,UY>
        [--latt "UN NW ALPHA TAPER; DR NW; ..."]  [--nperiods N] [--taper T]
        [--seed AX,AY] [--seed2 F:AX,AY] [--z2len L]
        [--plain NODES:STEPS ...] [--avg CELLS:STEPS ...] [--every K]

  --latt SPEC      a lattice, elements separated by ';'.  'UN NW ALPHA TAPER'
                   is an undulator module of NW periods with alpha = aw/aw0
                   and d(alpha)/d(zbar) = TAPER, of the given undulator type,
                   at each run's own stepsPerPeriod.  ALPHA may be given as
                   fF instead, the module's resonant frequency relative to
                   the carrier (f1.03 is 3% blue), and alpha is solved for.
                   'DR NW' is a drift of NW undulator periods.  Without
                   --latt, a single undulator of --nperiods periods
                   [default 60] with the main-input --taper [default 0].
  --seed2 F:AX,AY  a second flat-top seed at frequency F (freqf), for a
                   two-colour case.
  --z2len L        sFModelLengthZ2 [deck.in's 60].  A red-detuned module
                   slips the beam 1/f wavelengths per period, so a long one
                   needs a longer mesh.
  --plain, --avg   as run_compare.py [defaults 24:60 48:120 and 1:2].
  --every K        print every Kth period's write in the table [default 10].

Each --plain run is compared with each --avg run, write by write, on the
flat-top mean (middle 30% of the mesh) of /power, |/bunchingFundamental| and
/meanGamma, and at the last write on the spectral centre of the middle 30% of
the field and, with --seed2, the power in each colour.  Every --avg run is
compared, so a band-edge scan over lambdarPerCell shares the unaveraged runs.  The
spectra are taken from the fields themselves, not from compare.py's
one-wavelength demodulation, which attenuates off-carrier content (by sinc(df)
at a frequency offset df) and so cannot be used to compare a detuned module's
radiation node by node.  Needs mpirun and h5dump; standard library Python.
"""

import cmath
import math
import os
import re
import shutil
import subprocess
import sys

from compare import attr, field_comps, last_index, read_ds

HERE = os.path.dirname(os.path.abspath(__file__))
PUFFIN = os.environ.get("PUFFIN",
                        os.path.join(HERE, "..", "..", "build", "puffin", "puffin"))
RHO, AW, GAMMA = 0.005, 1.0121809, 456.40


def sub(text, name, value):
    return re.sub(r"(?m)^(\s*%s\s*=\s*)[^\n]*$" % name, r"\g<1>%s" % value, text)


def und_amps(und):
    """(cx, cy) as averaging.f90's getAvgUndAmps."""
    if und == "planepole":
        return 0.0, 1.0
    if und == "helical":
        return 1.0, 1.0
    ux, uy = und.split(",")
    return float(ux), float(uy)


def fp_of(und):
    cx, cy = und_amps(und)
    return 0.5 * (cx ** 2 + cy ** 2)


def p2_ref(alpha, fp):
    """p2 of a reference electron (Gamma = 1, no slow pperp), as getP2Avg."""
    eta = eta_of(1.0, fp)
    u = (1.0 + AW ** 2 * alpha ** 2 * fp) / GAMMA ** 2
    rt = math.sqrt(1.0 - u)
    return u / (eta * rt * (1.0 + rt))


def eta_of(alpha, fp):
    bz = math.sqrt(GAMMA ** 2 - 1.0 - AW ** 2 * alpha ** 2 * fp) / GAMMA
    return (1.0 - bz) / bz


def alpha_for(freq, fp):
    """alpha that puts a module's resonance at freq times the carrier: 1/p2 = freq."""
    eta_m = eta_of(1.0, fp) / freq
    bz = 1.0 / (1.0 + eta_m)
    return math.sqrt(((1.0 - bz ** 2) * GAMMA ** 2 - 1.0) / (AW ** 2 * fp))


def lattice_lines(spec, und, steps):
    """Lattice file text for one run; alphas resolved."""
    if "," in und:
        utype, (ux, uy) = "'puffin'", und.split(",")
    else:
        utype, ux, uy = "'%s'" % und, "1.0", "1.0"
    out = []
    for el in [e.split() for e in spec.split(";") if e.strip()]:
        if el[0] == "UN":
            nw, alpha, taper = el[1], el[2], el[3]
            if alpha.startswith("f"):
                alpha = "%.10f" % alpha_for(float(alpha[1:]), fp_of(und))
            out.append("UN %s %s %s %s %s %s %s 0.0 0.0"
                       % (utype, nw, alpha, taper, steps, ux, uy))
        elif el[0] == "DR":
            out.append("DR %s" % el[1])
        else:
            raise SystemExit("unknown lattice element %s" % el[0])
    return "\n".join(out) + "\n"


def run(tag, und, opts, averaged, mesh, steps):
    workdir = os.path.join(HERE, "run_%s" % tag)
    shutil.rmtree(workdir, ignore_errors=True)
    os.makedirs(workdir)
    shutil.copy(os.path.join(HERE, "beam_file.in"), workdir)

    with open(os.path.join(HERE, "seed_file.in")) as fh:
        seed = fh.read()
    seed = sub(seed, "sA0_X", opts["seed"][0])
    seed = sub(seed, "sA0_Y", opts["seed"][1])
    if opts["seed2"]:
        f2, (ax2, ay2) = opts["seed2"]
        seed = sub(seed, "nseeds", "2")
        seed = sub(seed, "freqf", "1.0, %s" % f2)
        seed = sub(seed, "sA0_X", "%s, %s" % (opts["seed"][0], ax2))
        seed = sub(seed, "sA0_Y", "%s, %s" % (opts["seed"][1], ay2))
        seed = sub(seed, "sSigmaF", ", ".join(["1E8"] * 6))
        for name, val in (("qFlatTop", ".true."), ("meanZ2", "0.0"),
                          ("qRndFj_G", ".false."), ("sSigFj_G", "0.1")):
            seed = sub(seed, name, "%s, %s" % (val, val))
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
    deck = sub(deck, "stepsPerPeriod", steps)
    deck = sub(deck, "iWriteIntNthSteps", steps)     # one integrated write per period
    deck = sub(deck, "nPeriods", opts["nperiods"])
    deck = sub(deck, "taper", opts["taper"])
    deck = sub(deck, "sFModelLengthZ2", opts["z2len"])
    if opts["latt"]:
        with open(os.path.join(workdir, "run.latt"), "w") as fh:
            fh.write(lattice_lines(opts["latt"], und, steps))
        deck = sub(deck, "lattFile", "'run.latt'")
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
    with open(os.path.join(workdir, "stdout.txt"), "w") as fh:
        fh.write(out)
    if proc.returncode != 0 or "Tried rearranging" in out:
        sys.stderr.write(out[-2000:])
        raise SystemExit("puffin failed for %s" % tag)
    secs = sum(float(s) for s in re.findall(r"Finished undulator module in\s+([0-9.E+-]+)", out))
    warned = "WARNING" in out and "resonance" in out
    return workdir, secs, warned


def mid(vals):
    lo, hi = int(0.35 * len(vals)), int(0.65 * len(vals))
    return sum(vals[lo:hi]) / (hi - lo)


def history(d):
    """{round(zbar, 6): (P, |b|, <gamma>)}, flat-top means, one entry per write."""
    out = {}
    for k in last_index(d, "integrated"):
        p = os.path.join(d, "run_integrated_%d.h5" % k)
        out[round(attr(p, "zbarTotal"), 6)] = (mid(read_ds(p, "/power")),
                                                abs(mid(read_ds(p, "/bunchingFundamental"))),
                                                mid(read_ds(p, "/meanGamma")))
    return out


def spectrum(d, averaged, freqs):
    """|FT|^2 of the middle 30% of the field at the last write, summed over x and y,
    at each frequency in freqs (units of the carrier).  Hann-windowed.  Both modes
    are put on the unaveraged field's normalisation: the averaged envelope's
    exp(+i (k - k_r) z2) transform is 1/sqrt(2) of the full field's at k."""
    comps, dz2, _ = field_comps(d, last_index(d, "aperp")[-1])
    n = len(comps[0])
    lo, hi = int(0.35 * n), int(0.65 * n)
    if averaged:
        sig = [comps[0][lo:hi], comps[1][lo:hi]]
        k0, fac = 1.0 / (2.0 * RHO), math.sqrt(0.5)
    else:
        sig = [[a.real for a in comps[0][lo:hi]], [-a.imag for a in comps[0][lo:hi]]]
        k0, fac = 0.0, 1.0
    m = hi - lo
    w = [0.5 - 0.5 * math.cos(2.0 * math.pi * j / (m - 1)) for j in range(m)]
    z0 = lo * dz2
    out = []
    for f in freqs:
        dk = f / (2.0 * RHO) - k0
        tot = 0.0
        for s in sig:
            ph, step, acc = cmath.exp(1j * dk * z0), cmath.exp(1j * dk * dz2), 0j
            for j in range(m):
                acc += w[j] * s[j] * ph
                ph *= step
            tot += abs(fac * acc * dz2) ** 2
        out.append(tot)
    return out


def centre(freqs, spec):
    return sum(f * s for f, s in zip(freqs, spec)) / sum(spec)


def main():
    args = sys.argv[1:]
    und = args.pop(0)
    opts = {"latt": "", "nperiods": "60", "taper": "0.0", "seed2": None, "z2len": "60.0",
            "seed": ("0.01", "0.0") if und == "planepole" else ("0.01", "0.01")}
    plains, avgs, which, every = [], [], None, 10
    while args:
        a = args.pop(0)
        if a in ("--latt", "--nperiods", "--taper", "--z2len"):
            opts[a[2:]] = args.pop(0)
        elif a == "--seed":
            opts["seed"] = tuple(args.pop(0).split(","))
        elif a == "--seed2":
            f, xy = args.pop(0).split(":")
            opts["seed2"] = (f, tuple(xy.split(",")))
        elif a == "--every":
            every = int(args.pop(0))
        elif a in ("--plain", "--avg"):
            which = plains if a == "--plain" else avgs
        else:
            which.append(tuple(a.split(":")))
    plains = plains or [("24", "60"), ("48", "120")]
    avgs = avgs or [("1", "2")]

    if opts["latt"]:
        print(lattice_lines(opts["latt"], und, "S"), end="")
    tag = re.sub(r"[^A-Za-z0-9]+", "_", "%s %s %s %s %s %s" % (
        und, opts["latt"], opts["nperiods"], opts["taper"], opts["seed2"],
        opts["z2len"])).strip("_")[-90:]

    if opts["seed2"]:
        f2 = float(opts["seed2"][0])
        pad = max(0.02, 0.3 * abs(f2 - 1.0))
        freqs = [min(1.0, f2) - pad + i * 0.002
                 for i in range(int((abs(f2 - 1.0) + 2 * pad) / 0.002) + 1)]
        cut = 0.5 * (1.0 + f2)
    else:
        freqs = [0.85 + i * 0.002 for i in range(151)]

    def colours(s):
        """(power at the carrier, power at the second colour) from a spectrum."""
        two = sum(v for f, v in zip(freqs, s) if (f < cut) == (f2 < 1.0))
        return sum(s) - two, two

    avg_runs = []
    for cells, steps in avgs:
        d, secs, warned = run("L_%s_avg_c%s_s%s" % (tag, cells, steps), und, opts, True,
                              cells, steps)
        print("averaged lambdarPerCell=%s steps=%s  %.1f s%s"
              % (cells, steps, secs, "  [band WARNING printed]" if warned else ""))
        avg_runs.append(("c%s s%s" % (cells, steps), history(d), spectrum(d, True, freqs)))

    for nodes, steps in plains:
        wd, secs, _ = run("L_%s_plain_n%s_s%s" % (tag, nodes, steps), und, opts, False,
                          nodes, steps)
        hp = history(wd)
        sp = spectrum(wd, False, freqs)
        for name, ha, sa in avg_runs:
            zs = sorted(set(ha) & set(hp))
            print()
            print("unaveraged nodesPerLambdar=%s steps=%s  %.1f s, against averaged %s"
                  % (nodes, steps, secs, name))
            print("   zbar      P plain     P avg   ratio |  b plain   b avg   ratio |"
                  "  <gam> plain  <gam> avg   diff")
            for i, z in enumerate(zs):
                if i % every and i != len(zs) - 1:
                    continue
                pp, bp, gp = hp[z]
                pa, ba, ga = ha[z]
                print("  %6.3f  %10.4e %10.4e %6.4f | %7.4f %7.4f %6.4f | %10.6f %10.6f %9.2e"
                      % (z, pp, pa, pa / pp, bp, ba, ba / bp if bp else float("nan"),
                         gp, ga, ga - gp))
            print("  spectral centre, middle 30%%: plain %.5f  avg %.5f   (diff %.2e)"
                  % (centre(freqs, sp), centre(freqs, sa),
                     centre(freqs, sa) - centre(freqs, sp)))
            if opts["seed2"]:
                (p1, p2), (a1, a2) = colours(sp), colours(sa)
                print("  by colour, middle 30%%: carrier avg/plain %.4f, second colour %.3f "
                      "avg/plain %.4f; second colour's share plain %.4f avg %.4f"
                      % (a1 / p1, f2, a2 / p2, p2 / (p1 + p2), a2 / (a1 + a2)))
            sys.stdout.flush()

if __name__ == "__main__":
    main()
