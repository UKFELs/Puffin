# Puffin

Fortran FEL (Free Electron Laser) simulation code.

## Git workflow

`dev` is the base branch, both here (`origin`, mightylorenzo/Puffin) and
upstream (`upstream`, UKFELs/Puffin). Always sync from and branch off `dev`:

```
git fetch upstream
git checkout dev
git merge upstream/dev    # or: git rebase upstream/dev
git checkout -b my-feature
```

Open pull requests against `dev`, never `master`. `master` upstream is only
updated by periodic merges from `dev` at release points, so do not branch off
it, target it, or sync to it for normal work.

## Build & Test

- Build: `make -j4 -C build/`
- Tests: `ctest --output-on-failure --test-dir build/`
- Two test suites: `puffin_basic_tests` (unit) and `puffin_e2e_tests`/`puffin_e2e_tests_1d_taper`/`puffin_e2e_tests_3d`/`puffin_e2e_tests_3d_slow` (MPI integration tests)
- E2E tests verify numerical results to 1e-10 tolerance (bit-exact)

### Slow and big tests — off by default

Do NOT enable the `PUFFIN_SLOW_TESTS` or `PUFFIN_BIG_TESTS` CMake options
during normal development, and do not run those test suites by default. They
are expensive: the slow tests run the full CLARA 17-module lattice (~30s, ~750
HDF5 dumps), and the big tests use an 85x85x5780 field mesh (~9–12GB RAM and
~9 min at 6 ranks). Both default to OFF — leave them that way, and only
configure with them ON when the user explicitly asks for them.

### Serial (non-MPI) builds

`-DENABLE_PARALLEL=OFF` builds an executable that runs directly, with no
`mpirun` and no MPI, FFTW3-MPI or parallel-HDF5 dependency. It is numerically
identical to `mpirun -n 1` of the parallel build. The switch works by compiling
one of `puffin/lib/backends/mpi/` or `puffin/lib/backends/serial/`, which define
the same module names (`mpi`, `puffin_fftw3`, `puffin_h5_par`) with one-rank
semantics — no call site in the physics, setup or IO code differs between the
two modes, and there are no preprocessor conditionals. When adding a new MPI
call, add its one-rank meaning to `backends/serial/mpi_serial.f90` too, or the
serial build stops compiling. See `doc/SERIAL_BUILD.md`.

A serial build runs `puffin_basic_tests` and `puffin_e2e_tests_serial`; the
`@mpitest` suites are built only for a parallel configure.

### Sandboxed test runs

The MPI-based e2e test targets can spuriously segfault or report "insufficient
processes" when run inside a sandboxed shell — the sandbox interferes with
MPI's inter-process communication (shared memory / sockets). If an e2e test
fails, re-run it with the sandbox disabled before concluding it's a real
failure or regression. `puffin_basic_tests` (no MPI) is unaffected and safe
to run sandboxed.
