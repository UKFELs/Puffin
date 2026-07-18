# Lint suppressions and follow-up notes

Generated while adding `-Wall -Wextra` (`ENABLE_LINT`) to the CMake build.
Two warning classes are suppressed at the compiler-flag level rather than
"fixed" in source, because fixing them risks changing numerical behaviour
in a codebase with a bit-exact (1e-10) E2E test:

- `-Wno-unused-dummy-argument` — many routines share a common call
  signature (lattice element interfaces, callback-style routines) where a
  given implementation legitimately ignores some arguments. Removing the
  argument would break the interface.
- `-Wno-compare-reals` — exact `==`/`/=` comparisons on `REAL(8)`.
  Rewriting these as epsilon-based comparisons could change simulation
  output even though the current tests pass, since the tests don't
  necessarily exercise every branch.
- `-Wno-conversion` — 20 sites across `puffin/lib/macroparticle_generation/random.f90`
  (14, e.g. `l = mu - 1.1484` truncating REAL(8) to an INTEGER local),
  `puffin/lib/io/h5_in.f90` (5, e.g. `nMPs = dims(2)` narrowing an HDF5
  `INTEGER(8)` dimension to `INTEGER(4)`), and
  `puffin/lib/io/av_write.f90` (1, REAL(8) to REAL(4)). All are implicit
  narrowing conversions that match the existing coding idiom throughout
  these files (ported numerical-recipes-style code, HDF5 dimension
  reads) rather than accidental truncation. Wrapping each in an explicit
  `INT()`/`REAL()` call would be behaviourally identical (Fortran
  implicit and explicit conversion truncate the same way) and so purely
  cosmetic; not worth touching 20 sites in numerically-sensitive code
  for no behavioural change.
- `-Wno-intrinsic-shadow` — `puffin/lib/utilities/error_fn.f90` defines
  its own `erf`/`erfc` functions (lines 23, 38), which shadow the
  Fortran 2008 intrinsics of the same name. This is deliberate (a
  Numerical-Recipes-style implementation via the incomplete gamma
  function, predating/replacing the intrinsic); renaming would require
  updating every call site for no behavioural change.
- `-Wno-aliasing` — `puffin/lib/diffraction/transforms.f90` (4 sites,
  lines 190/195/201/206) calls `fftw_mpi_plan_dft_3d(..., Afftw, Afftw,
  ...)`, passing the same buffer as both the `in` and `out` arguments.
  This is deliberate: it requests an FFTW in-place transform. Using a
  separate `out` buffer would create an out-of-place plan instead —
  a different FFTW algorithm/memory layout that could change numerical
  results, not just silence the warning.

## REAL(8) exact-equality/inequality sites (13 total)

Left as exact comparisons, suppressed via `-Wno-compare-reals`. Listed here
for future review in case any of these should become tolerance-based
comparisons.

| File | Line | Code |
|---|---|---|
| `puffin/lib/macroparticle_generation/random.f90` | 1057 | `IF (x /= INT(x)) GO TO 10` |
| `puffin/lib/utilities/error_fn.f90` | 262 | `IF (xx==0) RETURN` |
| `puffin/lib/utilities/error_fn.f90` | 212 | `IF (x==0.0_WP) THEN` |
| `puffin/lib/utilities/error_fn.f90` | 170 | `IF (X == 0.0) THEN` |
| `puffin/lib/setup/setup_transverse.f90` | 357 | `if (kx /= 0_wp) then` |
| `puffin/lib/setup/setup_transverse.f90` | 325 | `if (kbx == 0_wp) then` |
| `puffin/lib/setup/checks.f90` | 643 | `IF (.NOT. (f_x==1.0_WP .OR. f_y==1.0_WP)) THEN` (both operands) |
| `puffin/lib/diffraction/diffraction.f90` | 486 | `if (kz2_loc_G(z2_inc)/=0.0_WP) then` |
| `puffin/lib/diffraction/diffraction.f90` | 155 | `if (kz2_loc_G(z2_inc)/=0.0_WP) then` |
| `puffin/lib/io/read_base_input.f90` | 481 | `if (DFact /= -1000.0_wp) then` |
| `puffin/lib/io/read_base_input.f90` | 505 | `if (speout /= -1000.0_wp) then` |
| `puffin/lib/io/hdf5_puff.f90` | 192 | `where (avGam4Unsc == 0.0_wp) avGam4Unsc = 1.0_wp` |

Most of these compare against sentinel values (`-1000.0`, unset defaults)
or guard against exact zero (division-by-zero guards), which is the
correct use of exact comparison — not a precision bug. `error_fn.f90`
and `random.f90` look like ported numerical-recipes-style code where the
same applies. None looked like an obvious bug on inspection, but flagging
here rather than silently editing per user request.

## Uninitialized-variable warnings (`-Wmaybe-uninitialized`)

Most of the 33 sites were genuine (if often practically unreachable)
gaps — an early-exit/error branch that left a function result or output
variable unset — and were fixed by adding a defensive default value
(e.g. `fn_val = zero` before an early `RETURN` in
`random_von_Mises`/`error_fn.f90:gammln`; a default before an
if/else-if chain that isn't provably exhaustive in
`mp_sequences.f90:interp1` and `para_field.f90` around line 2113; the
`ival`/`fk`/`difmuk`/`omega`/`c0..c3` locals in
`random.f90:random_Poisson`, whose lazy-init block is always reached in
practice but not provably so from the GOTO-heavy control flow). These
defaults only change behaviour on inputs that don't occur in the current
codebase (e.g. an invalid parallel-decomposition selector, or a
malformed/unsorted interpolation grid) — see the added inline comments
at each site for specifics.

One class was left unfixed as a compiler-internal false positive:
`puffin/lib/macroparticle_generation/gen_macros.f90` and
`gen_macros_new.f90` (5 sites each, e.g. `x_2_random.dim[0].ubound`) warn
about the array-descriptor bounds of `ALLOCATABLE` arrays that are
conditionally allocated and conditionally whole-array-assigned, both
guarded by the same `IF (PRESENT(x_2_grid))` check. The two guards are
textually identical, so this is provably safe, but gfortran's flow
analysis doesn't connect the allocation and the assignment. There's no
source change here that would fix this without restructuring the
optional-argument pattern across both files, which wasn't judged worth
the risk for what is a known gfortran false-positive pattern with
conditionally-allocated optional arrays.
