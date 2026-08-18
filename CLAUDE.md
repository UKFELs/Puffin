# Puffin

Fortran FEL (Free Electron Laser) simulation code.

## Build & Test

- Build: `make -j4 -C build/`
- Tests: `ctest --output-on-failure --test-dir build/`
- Two test suites: `puffin_basic_tests` (unit) and `puffin_e2e_tests`/`puffin_e2e_tests_3d`/`puffin_e2e_tests_3d_slow` (MPI integration tests)
- E2E tests verify numerical results to 1e-10 tolerance (bit-exact)

### Slow and big tests — off by default

Do NOT enable the `PUFFIN_SLOW_TESTS` or `PUFFIN_BIG_TESTS` CMake options
during normal development, and do not run those test suites by default. They
are expensive: the slow tests run the full CLARA 17-module lattice (~30s, ~750
HDF5 dumps), and the big tests use an 85x85x5780 field mesh (~9–12GB RAM and
~9 min at 6 ranks). Both default to OFF — leave them that way, and only
configure with them ON when the user explicitly asks for them.

### Sandboxed test runs

The MPI-based e2e test targets can spuriously segfault or report "insufficient
processes" when run inside a sandboxed shell — the sandbox interferes with
MPI's inter-process communication (shared memory / sockets). If an e2e test
fails, re-run it with the sandbox disabled before concluding it's a real
failure or regression. `puffin_basic_tests` (no MPI) is unaffected and safe
to run sandboxed.
