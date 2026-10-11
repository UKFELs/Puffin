# Tests for the pyPlotting field helpers

Checks for the functions in `utilities/pyPlotting` that read a field dump and
work out its polarisation. They are plain scripts: run one and it prints a
line per check and exits non-zero if any failed.

```
python utilities/pyPlotting/tests/testPolarization.py
python utilities/pyPlotting/tests/testPolarizationHilbert.py
```

They are also registered with ctest, labelled `python`, so

```
ctest --test-dir build -L python
```

runs just these and `ctest --test-dir build -LE python` leaves them out.
Registration happens at configure time and only if an interpreter that can
import what they need is found, so a build without numpy prints a note and
skips them rather than failing. CMake prefers an activated environment, then
the first `python3` on `PATH`; `-DPython3_EXECUTABLE=/path/to/python` pins it.

| | needs |
|---|---|
| `testPolarization.py` | numpy |
| `testPolarizationHilbert.py` | numpy, scipy |

`fakeDump.py` holds the stand-ins for an opened dump, so neither test writes
an HDF5 file or runs the solver. pytables and matplotlib are stubbed out —
they are imported by the scripts under test for output the tests never ask
for. scipy is not stubbed: the Hilbert test calls the real transform as its
reference.

## What they cover

Both tests build fields whose polarisation is known analytically and check
what the helpers recover from them. Three states pin the map down between
them: a helical undulator's `Ãy = i Ãx` is circular, a planar run's `Ãy = 0`
is linear along x, and equal envelopes are linear at 45 degrees, which is the
only one of the three that tells P2 from P3. `testPolarization.py` also checks
the reconstructed ellipse, and that the unaveraged path still reads its two
components verbatim.

`testPolarizationHilbert.py` additionally cross-validates: one physical field
is built, the resolved version handed to the existing Hilbert path and the
envelope version to the averaged analytic path, and the two are required to
agree. That is what pins the sign and magnitude of the carrier the averaged
path puts back, which no self-consistent check of the envelope alone can see.

Its test signal is deliberately periodic over the record, because the
reference analytic signal is built by FFT: a window holding a non-integer
number of carrier cycles leaks over its whole length, and the reference is
then worthless as one. That failure looks exactly like a broken
implementation, so it is worth knowing before changing the signal.

## What they do not cover

The field layout only. These are unit checks on functions, with the dump
faked in memory, so they say nothing about whether Puffin's dumps are
physically right — the e2e tests in `test/` and the comparisons in
`benchmark/averaged/` are for that.

They also work in the layout the plotting scripts expect,
`(nx, ny, nz2, 2*nFieldComp)`, which is **not** the layout Puffin writes.
The dumps are Fortran-major, `(2*nFieldComp, nz2, ny, nx)`, and have to be
passed through `ReorderFieldfast.py` first:

```
python utilities/pyPlotting/ReorderFieldfast.py run_aperp_C_5.h5
python utilities/pyPlotting/plotPolarization.py run_aperp_C_5.h5 0.005
```

Handed a raw dump instead, these scripts do not fail. They read the component
axis as x and carry on, so a 41x41 mesh is reported as `nx: 4` and
`nComponents: 41`.
