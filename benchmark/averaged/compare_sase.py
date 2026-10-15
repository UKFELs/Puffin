#!/usr/bin/env python3
"""Compare a period-averaged SASE run against an unaveraged one (1D).

Usage: python3 compare_sase.py <plain_dir> <avg_dir> [<rho>] [<undtype>]

The seeded 1D harness (compare.py) differences the two fields node by node.
That is the wrong expectation here.  SASE starts from shot noise and amplifies
it by seven orders of magnitude, so even with a bit-identical noise
realisation any difference in the two solvers' coupling is amplified through
the gain and the fields decorrelate long before saturation.  What is worth
comparing is the statistics of the amplification:

  shot noise   the initial macroparticle dump, compared element by element.
               This is the SASE analogue of compare.py's "power ratio at the
               first write must be 1": with no seed the field starts at zero
               in both runs, so the thing that has to be identical is the beam.
               iRandSeed fixes the realisation, but init_random_seed folds the
               MPI rank into the seed, so two runs only agree at equal rank
               count - and the macroparticle layout must agree too, or the
               draws land in different places.  The two dumps are not bit
               identical even then, for two reasons that are not noise: the
               beam is reordered to follow each mode's own field decomposition,
               and shuntBeam shifts the whole beam by a tenth of a field cell.
               noise_match sorts on z2 and allows one common offset; everything
               else has to match exactly.  If it does not, nothing below is a
               comparison of the physics.
  power        sum(/power) * dz2 at each integrated write, so the two z2 meshes
               are comparable.  Total over every frequency the mesh resolves,
               and NOT band limited: a planar undulator radiates odd harmonics
               the averaged mode cannot carry, so read the spectral table for
               the like-for-like number.
  gain length  least squares fit of log(power) against zbar over the window
               where the power is between --fit fractions of its saturation
               value - by default 1e-4 to 1e-2, which is above the startup
               lethargy and below the roll-over.  The one number that says
               whether the two solvers amplify at the same rate.
  saturation   the largest power reached and the zbar it is reached at.
  band power   the integral of |A|^2 over z2 at each common /aperp write, total
               and restricted to the band the envelope mesh can hold.  The
               band column is the like-for-like power comparison: early in a
               SASE run most of the unaveraged field is spontaneous emission
               spread over every frequency its mesh resolves, which the
               averaged mode is not carrying and never claimed to.  Alongside
               it, the normalised overlap of the averaged envelope with the
               demodulated unaveraged field: 1 while the two runs are still
               the same shot, falling towards 0 as the gain amplifies whatever
               they disagree about into two different shots.
  spectrum     from the last common /aperp write.  Unaveraged, the transform
               of the resolved field, whose bin m is at w/wr = |4 pi rho f_m|.
               Averaged, the transform of the envelope, whose bin is an offset
               about the carrier, w/wr = 1 - 4 pi rho f_m (see the W5 notes in
               AVERAGED_MODE_ROADMAP.md).  Reported as the power-weighted
               centre and RMS width over the band an envelope mesh of this
               lambdarPerCell can hold, 1 +/- 1/(2 lambdarPerCell), and over a
               narrow +/-5% window about resonance.  The unaveraged run's
               harmonic shares are listed alongside, since those are power the
               averaged mode is not even trying to carry.
  bunching     current-weighted /bunchingFundamental, which is not diluted by
               the field the way power is early on.

Both runs must end at the same zbar: Puffin exits 0 when it gives up
rearranging its parallel field, so a run that stopped early otherwise looks
finished.

Pure standard library: datasets are pulled out with `h5dump -b LE`, and the
transform is a radix-2 FFT below.
"""

import cmath
import math
import os
import sys

from compare import attr, demod, field_comps, last_index, pol, read_ds

RHO = 0.005
UND = "planepole"    # sase.in's undulator, for the demodulation normalisation
ZTOL = 1e-6          # relative zbar agreement required of the two runs
FITLO, FITHI = 1e-4, 1e-2    # fit window, as fractions of saturation power


# ---------------------------------------------------------------- transform

def fft(a):
    """In-place iterative radix-2 FFT of a list of complex, len a power of 2."""
    n = len(a)
    j = 0
    for i in range(1, n):
        bit = n >> 1
        while j & bit:
            j ^= bit
            bit >>= 1
        j |= bit
        if i < j:
            a[i], a[j] = a[j], a[i]
    ln = 2
    while ln <= n:
        w = cmath.exp(-2j * math.pi / ln)
        for i in range(0, n, ln):
            wk = 1.0 + 0j
            for k in range(i, i + ln // 2):
                u, v = a[k], a[k + ln // 2] * wk
                a[k], a[k + ln // 2] = u + v, u - v
                wk *= w
        ln <<= 1
    return a


def spectrum(A, dz2):
    """[(f, |Ahat|^2 * dz2 / N)] over signed frequencies f in cycles per z2.

    The weight is normalised so that summing it reproduces the integral of
    |A|^2 over z2 (Parseval), making it comparable between meshes, and zero
    padding to the next power of two does not change that sum.
    """
    n = 1
    while n < len(A):
        n <<= 1
    hat = fft(list(A) + [0j] * (n - len(A)))
    return [((m if m < n // 2 else m - n) / (n * dz2), abs(h) ** 2 * dz2 / n)
            for m, h in enumerate(hat)]


def bands(d, k, rho):
    """Spectral power of write k against w/wr: ([(w_over_wr, weight)], mode)."""
    comps, dz2, _ = field_comps(d, k)
    averaged = attr(os.path.join(d, "run_aperp_%d.h5" % k), "qAveraged", 0.0) > 0.5
    sp = spectrum(comps[0], dz2)
    if averaged:
        return [(1.0 - 4.0 * math.pi * rho * f, w) for f, w in sp], True
#   Resolved field: real, so the +k and -k bins are the same physical
#   frequency and both are kept.
    return [(abs(4.0 * math.pi * rho * f), w) for f, w in sp], False


def correlation(plain, kp, avg, ka, rho, undtype=UND):
    """(|correlation|, relative L2) of the two runs' envelopes at one write.

    The unaveraged field is demodulated onto the averaged envelope's
    normalisation exactly as compare.py does it, and sampled at the averaged
    run's nodes.  The correlation is the normalised complex overlap, whose
    MAGNITUDE is taken, and the L2 is taken with the same global phase removed.
    shuntBeam leaves the two beams a fixed fraction of a wavelength apart (see
    noise_match), which is a constant carrier phase between the two envelopes
    and no physical difference at all - here 0.0909 of a wavelength, 33
    degrees, which left in would put 0.58 into a relative L2 whose real content
    is 0.1.  compare.py can difference the seeded runs' envelopes directly
    because there the seed pins the phase; with no seed nothing does.

    Correlation 1 means the two runs are still the same shot; 0 means the gain
    has amplified whatever they disagree about into two different shots with
    the same statistics.  The L2, with the phase removed, still counts
    amplitude and shape, so it is the one that sees a gain-length difference.
    """
    Ap, dzp, _ = field_comps(plain, kp)
    Aa, dza, _ = field_comps(avg, ka)
    env = demod(Ap[0], dzp, rho, int(round(4.0 * math.pi * rho / dzp)), pol(undtype))
    num, da, de = 0j, 0.0, 0.0
    for j, a in enumerate(Aa[0]):
        i = int(round(j * dza / dzp))
        if i >= len(env) or env[i] is None:
            continue
        e = env[i]
        num += a * e.conjugate()
        da += abs(a) ** 2
        de += abs(e) ** 2
    if da <= 0.0 or de <= 0.0:
        return float("nan"), float("nan")
#   sum|a - e exp(i phi)|^2 at phi = arg(num) is da + de - 2|num|.
    return abs(num) / math.sqrt(da * de), math.sqrt(max(da + de - 2 * abs(num), 0.0) / de)


def moments(sp, lo, hi):
    """(total weight, power-weighted mean w/wr, RMS width) inside [lo, hi]."""
    tot = sum(w for x, w in sp if lo <= x <= hi)
    if tot <= 0.0:
        return 0.0, float("nan"), float("nan")
    mean = sum(x * w for x, w in sp if lo <= x <= hi) / tot
    var = sum((x - mean) ** 2 * w for x, w in sp if lo <= x <= hi) / tot
    return tot, mean, math.sqrt(max(var, 0.0))


# ---------------------------------------------------------------- integrated

def totals(d, k):
    """(zbar, integrated power, current-weighted bunching) of integrated write k."""
    p = os.path.join(d, "run_integrated_%d.h5" % k)
    dz2 = attr(p, "sLengthOfElmZ2")
    power = read_ds(p, "/power")
    cur = read_ds(p, "/beamCurrent")
    bun = read_ds(p, "/bunchingFundamental")
    q = sum(cur)
    b = sum(c * v for c, v in zip(cur, bun)) / q if q > 0 else float("nan")
    return attr(p, "zbarTotal"), sum(power) * dz2, b


def trace(d, ks):
    return [totals(d, k) for k in ks]


def zbars(d, stem):
    """{rounded zbar: write index} for every write of this stem.

    A dump's index is the number of integrated writes made before it, not the
    step or the period, so two runs only share indices when their
    iWriteIntNthSteps is the same multiple of their stepsPerPeriod.  Pairing on
    zbar instead holds whatever the write intervals were, and a later write
    overwriting an earlier one at the same zbar is what we want: Puffin writes
    the final dump twice, once on the last step and once on the way out.
    """
    out = {}
    for k in last_index(d, stem):
        p = os.path.join(d, "run_%s_%d.h5" % (stem, k))
        out[round(attr(p, "zbarTotal"), 6)] = k
    return out


def paired(plain, avg, stem):
    """[(zbar, plain index, avg index)] for the writes the two runs share."""
    zp, za = zbars(plain, stem), zbars(avg, stem)
    return [(z, zp[z], za[z]) for z in sorted(set(zp) & set(za))]


def gain_length(tr, lo, hi):
    """(gain length in zbar, number of points) from a log-linear fit.

    P ~ exp(zbar / Lg) over the window where P is between lo and hi times the
    largest power in the trace.
    """
    psat = max(p for _, p, _ in tr)
    pts = [(z, math.log(p)) for z, p, _ in tr if lo * psat <= p <= hi * psat and p > 0]
    if len(pts) < 3:
        return float("nan"), len(pts)
    n = len(pts)
    sz = sum(z for z, _ in pts)
    sl = sum(l for _, l in pts)
    szz = sum(z * z for z, _ in pts)
    szl = sum(z * l for z, l in pts)
    slope = (n * szl - sz * sl) / (n * szz - sz * sz)
    return 1.0 / slope, n


def saturation(tr, frac=0.95):
    """(largest power in the trace, zbar where it first reaches frac of that).

    The position is taken at a fraction of the peak rather than at the peak
    itself: past saturation a SASE run's power oscillates by a few per cent, so
    which write happens to hold the maximum is noise, while the zbar the power
    first gets near it is stable.
    """
    psat = max(p for _, p, _ in tr)
    z = next(z for z, p, _ in tr if p >= frac * psat)
    return psat, z


# ---------------------------------------------------------------- shot noise

NCOL = 7          # /electrons columns: x, y, z2, px, py, gamma, Nk
IZ2 = 2


def noise_match(plain, avg, rho=RHO):
    """Compare the two runs' initial macroparticle dumps: (ok, message).

    Two things make the two dumps differ without the noise differing, and both
    have been checked against the code and on real runs:

    Order.  After generation the macroparticles are redistributed so each rank
    owns the z2 range of its field slice, and the two modes' meshes slice z2
    differently, so on more than one rank the gathered dump holds the same
    particles in a different order.  Sorting on z2 removes that.

    Origin.  shuntBeam (macroparticle_generation/simple_electron_gen.f90)
    slides the whole beam so its leading macroparticle sits a tenth of a FIELD
    cell above the mesh start.  The field cell differs between the modes by the
    ratio of the meshes, so the beams start a fixed fraction of a resonant
    wavelength apart - 0.0909 of one, for lambdarPerCell = 1 against
    nodesPerLambdar = 12.  That is a rigid translation of one realisation, and
    on a temporal mesh with no seed nothing in the run picks out a z2 origin for
    it to matter to.

    So the test, after sorting: the same number of macroparticles, every column
    but z2 identical to the last bit, and all the z2 differences equal.
    Anything else - a different count, a non-constant offset, a different
    energy or weight - means the two runs drew different noise, and nothing
    downstream is a comparison of the physics.
    """
    pp = os.path.join(plain, "run_electrons_0.h5")
    pa = os.path.join(avg, "run_electrons_0.h5")
    for p in (pp, pa):
        if not os.path.exists(p):
            return False, "no %s - set iWriteNthSteps so step 0 is written" % p
    ep, ea = read_ds(pp, "/electrons"), read_ds(pa, "/electrons")
    n = len(ep) // NCOL
    if len(ep) != len(ea):
        return False, ("%d vs %d macroparticles: the two runs did not generate "
                       "the same beam, so the shot noise differs"
                       % (n, len(ea) // NCOL))

    rp = sorted((ep[i * NCOL:(i + 1) * NCOL] for i in range(n)), key=lambda r: r[IZ2])
    ra = sorted((ea[i * NCOL:(i + 1) * NCOL] for i in range(n)), key=lambda r: r[IZ2])

    for j in range(NCOL):
        if j == IZ2:
            continue
        worst = max((abs(x[j] - y[j]) for x, y in zip(rp, ra)), default=0.0)
        if worst != 0.0:
            return False, ("%d macroparticles, but /electrons column %d differs "
                           "by up to %.3e once sorted on z2 - different noise"
                           % (n, j, worst))

    off = [y[IZ2] - x[IZ2] for x, y in zip(rp, ra)]
    spread = max(off) - min(off)
    lamr = 4.0 * math.pi * rho
    if spread > 1e-9 * lamr:
        return False, ("%d macroparticles, z2 offsets spread over %.3e (%.3e of a "
                       "wavelength) - not a rigid shift, so different noise"
                       % (n, spread, spread / lamr))
    return True, ("%d macroparticles, identical realisation shifted by %+.6f "
                  "wavelengths in z2 (shuntBeam; spread %.1e)"
                  % (n, off[0] / lamr, spread))


# ---------------------------------------------------------------- report

def main():
    plain, avg = sys.argv[1], sys.argv[2]
    rho = float(sys.argv[3]) if len(sys.argv) > 3 else RHO

    zp, za = zbars(plain, "integrated"), zbars(avg, "integrated")
    if not zp or not za:
        raise SystemExit("no integrated writes")
    if abs(max(zp) - max(za)) > ZTOL * max(abs(max(zp)), 1.0):
        raise SystemExit("runs ended at different zbar (%.6f vs %.6f): a failed "
                         "field rearrangement exits 0, so check the logs"
                         % (max(zp), max(za)))
    pairs = [(z, zp[z], za[z]) for z in sorted(set(zp) & set(za))]
    if not pairs:
        raise SystemExit("no integrated write at a common zbar")
    tp = trace(plain, [kp for _, kp, _ in pairs])
    ta = trace(avg, [ka for _, _, ka in pairs])

    ok, msg = noise_match(plain, avg)
    print("shot noise: %s" % msg)
    if not ok:
        print("  *** the runs do not share a noise realisation - nothing below "
              "is a comparison of the physics ***")

    print()
    print("  %-8s %-12s %-12s %-9s %-11s %-11s %s"
          % ("zbar", "P unavg", "P avg", "P ratio", "b unavg", "b avg", "b ratio"))
    step = max(1, len(pairs) // 16)
    for i in list(range(0, len(pairs), step)) + [len(pairs) - 1]:
        (z, pp, bp), (_, pa, ba) = tp[i], ta[i]
        print("  %-8.3f %-12.5e %-12.5e %-9.4f %-11.4e %-11.4e %.4f"
              % (z, pp, pa, pa / pp if pp else float("nan"), bp, ba,
                 ba / bp if bp else float("nan")))

    lgp, np_ = gain_length(tp, FITLO, FITHI)
    lga, na = gain_length(ta, FITLO, FITHI)
    sp_, zsp = saturation(tp)
    sa_, zsa = saturation(ta)
    print()
    print("  gain length (zbar, P between %.0e and %.0e of saturation)"
          % (FITLO, FITHI))
    print("    unaveraged %8.4f  (%d points)" % (lgp, np_))
    print("    averaged   %8.4f  (%d points)    ratio %.4f" % (lga, na, lga / lgp))
    print("  saturation (largest power, and the zbar it first reaches 95% of it)")
    print("    unaveraged  P = %.5e at zbar = %.3f" % (sp_, zsp))
    print("    averaged    P = %.5e at zbar = %.3f    P ratio %.4f, dzbar %+.3f"
          % (sa_, zsa, sa_ / sp_, zsa - zsp))

    fpairs = paired(plain, avg, "aperp")
    if not fpairs:
        print()
        print("  no /aperp write at a common zbar - no spectrum")
        return
    zf, kfp, kfa = fpairs[-1]
    spp, _ = bands(plain, kfp, rho)
    spa, isavg = bands(avg, kfa, rho)
    if not isavg:
        raise SystemExit("%s is not an averaged-mode run" % avg)
    lpc = attr(os.path.join(avg, "run_aperp_%d.h5" % kfa), "lambdarPerCell")
    half = min(0.5 / lpc, 0.5)

#   The power table above is total power.  Early on, before the fundamental
#   runs away, most of the unaveraged run's field is spontaneous emission
#   spread over every frequency its mesh resolves, and the averaged mode is
#   only ever carrying the band about the carrier - so the total-power ratio
#   there is a like-for-unlike comparison.  This is the like-for-like one.
    print()
    print("  int |A|^2 against zbar, total and inside the envelope band, and how")
    print("  far the two runs are still the same shot")
    print("  %-8s %-11s %-11s %-8s %-11s %-11s %-8s %-7s %-6s %s"
          % ("zbar", "tot unavg", "tot avg", "ratio", "band unavg", "band avg",
             "ratio", "band sh", "corr", "rel L2"))
    for z, kp, ka in fpairs:
        bp, _ = bands(plain, kp, rho)
        ba, _ = bands(avg, ka, rho)
        tp_, ta_ = sum(w for _, w in bp), sum(w for _, w in ba)
        ep_ = moments(bp, 1.0 - half, 1.0 + half)[0]
        ea_ = moments(ba, 1.0 - half, 1.0 + half)[0]
        c, l2 = correlation(plain, kp, avg, ka, rho)
        print("  %-8.3f %-11.4e %-11.4e %-8.4f %-11.4e %-11.4e %-8.4f %-7.5f %-6.4f %.4f"
              % (z, tp_, ta_, ta_ / tp_ if tp_ else float("nan"), ep_, ea_,
                 ea_ / ep_ if ep_ else float("nan"),
                 ep_ / tp_ if tp_ else float("nan"), c, l2), flush=True)

    print()
    print("  spectrum at zbar = %.3f (/aperp writes %d and %d)" % (zf, kfp, kfa))
    print("    envelope band is w/wr = 1 +/- %.3f (lambdarPerCell = %.4f)" % (half, lpc))
    print("    %-12s %-13s %-10s %-10s %s"
          % ("window", "int |A|^2", "share", "centre w/wr", "rms width"))
    for name, lo, hi in (("band", 1.0 - half, 1.0 + half), ("+/-5%", 0.95, 1.05)):
        for tag, sp in (("unavg", spp), ("avg", spa)):
            tot = sum(w for _, w in sp)
            e, c, s = moments(sp, lo, hi)
            print("    %-12s %-13.5e %-10.5f %-10.5f %.5e"
                  % ("%s %s" % (name, tag), e, e / tot if tot else float("nan"), c, s))

    etot_p = sum(w for _, w in spp)
    etot_a = sum(w for _, w in spa)
    eb_p = moments(spp, 1.0 - half, 1.0 + half)[0]
    print()
    print("    total int |A|^2   unavg %.5e   avg %.5e   ratio %.4f"
          % (etot_p, etot_a, etot_a / etot_p if etot_p else float("nan")))
    print("    in band          unavg %.5e   avg %.5e   ratio %.4f"
          % (eb_p, etot_a, etot_a / eb_p if eb_p else float("nan")))
    print()
    print("    unaveraged harmonic content, +/-%.2f about each harmonic" % half)
    print("    %-10s %s" % ("harmonic", "% of total"))
    for h in (1, 2, 3, 4, 5):
        e = moments(spp, h - half, h + half)[0]
        print("    %-10d %.4f" % (h, 100.0 * e / etot_p if etot_p else float("nan")))


if __name__ == "__main__":
    main()
