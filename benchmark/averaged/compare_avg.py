#!/usr/bin/env python3
"""Compare averaged-mode runs against an averaged reference (1D).

Usage: python3 compare_avg.py <ref_dir> <dir> [<dir> ...]

Both hold the envelope directly, so no demodulation: the reference envelope is
sampled at each run's nodes (mesh spacings are integer multiples of each other
here) and compared in relative L2; the mid-bunch power at the last common
integrated write is compared as a ratio.
"""

import os
import sys

from compare import field, last_index, read_ds


def mid_power(d, k):
    p = read_ds(os.path.join(d, "run_integrated_%d.h5" % k), "/power")
    lo, hi = int(0.35 * len(p)), int(0.65 * len(p))
    return sum(p[lo:hi]) / (hi - lo)


def main():
    ref = sys.argv[1]
    Ar, dzr, _ = field(ref, last_index(ref, "aperp")[-1])
    kr = last_index(ref, "integrated")[-1]
    pr = mid_power(ref, kr)
    print("  %-18s %-9s %-12s %s" % ("run", "dz2/dzr", "env rel L2", "P/P_ref (last write)"))
    for d in sys.argv[2:]:
        A, dz, _ = field(d, last_index(d, "aperp")[-1])
        m = dz / dzr
        num = den = 0.0
        for j, a in enumerate(A):
            i = int(round(j * m))
            if i >= len(Ar):
                break
            num += abs(a - Ar[i]) ** 2
            den += abs(Ar[i]) ** 2
        k = min(kr, last_index(d, "integrated")[-1])
        print("  %-18s %-9.2f %-12.4e %.5f" % (d, m, (num / den) ** 0.5, mid_power(d, k) / pr))


if __name__ == "__main__":
    main()
