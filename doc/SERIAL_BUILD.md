# Serial builds of Puffin

Puffin is an MPI code. It can also be built without MPI:

```sh
cmake -B ../build-serial -DENABLE_PARALLEL=OFF .
cmake --build ../build-serial
../build-serial/puffin/puffin my_deck.in       # note: no mpirun
```

The result is an ordinary executable. It is launched directly, needs no
launcher, and does not link `libmpi` or `libfftw3_mpi`.

This is useful for debugging (`gdb ./puffin` rather than attaching to a rank),
for profiling and single-core benchmarking, for running on a machine with no
MPI installed, and for isolating whether a problem is in the physics or in the
domain decomposition.

It is **not** a performance feature. A serial build is a one-rank run, with all
of the memory and all of the work on one core. Production runs still want MPI.


## What the serial build guarantees

A serial build is numerically identical to `mpirun -n 1` of the same source,
except where FFTW's planner legitimately differs (see below). Verified on the
committed test decks:

| Deck | Serial vs `mpirun -n 1` |
| --- | --- |
| `test/inputs/f1main.in` (1D) | field, electrons and integrated data bit-exact |
| `test/inputs/1D/osc_taper.in` (1D, periodic mesh, 5 modules) | field and electrons bit-exact |
| `test/inputs/3D/clara_test.in` (3D, periodic mesh, diffraction) | electrons bit-exact; field agrees to 4e-15 relative |

The 3D field is not bit-exact because FFTW's serial planner and its MPI planner
pick different codelets for the same transform, and so sum in a different order.
The residual is a few ULP - max absolute difference 5.3e-18 against a field
maximum of 1.3e-3 - which is four orders of magnitude inside the 1e-10 tolerance
the E2E tests use.

Note that a one-rank run is not in general bit-identical to a two-rank run of
the same deck, in either build mode: reductions sum in a different order, and
for decks that generate their own beam each rank seeds its own RNG stream. That
is a pre-existing property of Puffin, not something the serial mode introduces.


## How it works

No call site anywhere in the physics, setup or IO code changes between the two
modes, and there are no preprocessor conditionals in them. Instead, one of two
directories of backend modules is compiled in:

```
puffin/lib/backends/mpi/      compiled when ENABLE_PARALLEL=ON
puffin/lib/backends/serial/   compiled when ENABLE_PARALLEL=OFF
```

Both define the same module names with the same public interfaces, so which one
was built is invisible to everything downstream. `puffin/CMakeLists.txt` picks
the directory; the source globs never see both, because the backends sit two
levels below `lib/` and the globs only reach one.

There are three backend modules.

### `mpi` (serial only: `backends/serial/mpi_serial.f90`)

Puffin's `use mpi, only: ...` statements resolve against a module literally
called `mpi`. In a parallel build that is the MPI library's own; in a serial
build it is this one, which exports exactly the subset of the MPI-3 Fortran API
that Puffin uses, implemented for a communicator of size 1:

- `MPI_COMM_SIZE` returns 1, `MPI_COMM_RANK` returns 0.
- Barriers, waits, `MPI_BCAST`, init and finalize are no-ops - a broadcast from
  the only rank to itself has nothing to move.
- Reductions, gathers, scatters and all-to-alls copy the send buffer to the
  receive buffer at the displacement MPI defines, or do nothing at all when the
  caller passes `MPI_IN_PLACE` (detected by address, as real MPI does).
- Sends and receives are supported **only to and from ourselves**, through a
  small FIFO of buffered messages. Puffin genuinely needs this: with a periodic
  field mesh, rank 0 and rank `size-1` are the same process when `size == 1`,
  and the wrap-around in `para_field.f90` posts an `MPI_ISSEND` to itself and
  then receives it.
- Anything that can only mean something on more than one rank - a send to a
  rank that does not exist, a receive with no matching message already posted -
  stops the run with a diagnostic. That is deliberate. A stub that silently
  skipped a real communication would produce plausible but wrong physics.

Buffer arguments are `type(*), dimension(..)` (assumed-type, assumed-rank), so
one implementation serves every type and rank Puffin passes, mirroring the
`<type> buf(*)` of the real Fortran bindings. Payloads move as raw bytes, the
size coming from the `count` and `datatype` arguments.

### `puffin_fftw3`

Puffin plans its 3D transforms with FFTW3-MPI. The serial backend includes
plain `fftw3.f03` and adds the four `fftw_mpi_*` entry points the transform
layer calls, on top of their serial counterparts. FFTW-MPI distributes a 3D
transform over the slowest dimension, so on one rank the local size is the whole
array, the offset is zero, and the plan is an ordinary in-place serial plan over
the same dimensions. `transforms.f90` is unchanged.

`cmake/Modules/FindFFTW3.cmake` gained an `MPI` component for this: only a
parallel configure requests it, so a serial build never links `libfftw3_mpi`.

### `puffin_h5_par`

HDF5 defines `h5pset_fapl_mpio_f` and `h5pset_dxpl_mpio_f` only when it has
itself been built against MPI, so a serial build cannot import them from the
`hdf5` module at all. Both become no-ops here, leaving the file-access and
transfer property lists at their defaults: on one rank the MPI-IO driver and
the default POSIX one write byte-identical files, and a collective transfer
degenerates to an independent one. The hyperslab selections are untouched, so
the file layout matches the parallel build exactly.

The two IO modules that used these names, `h5_in.f90` and `hdf5_puff_coll.f90`,
now import them from `puffin_h5_par` rather than from `hdf5`. That is the only
change to existing source outside the build files.

If the HDF5 that CMake finds is itself a parallel build, linking it still pulls
`libmpi` in transitively. The executable is still launched directly and never
calls MPI, but the binary is not MPI-free; point `HDF5_ROOT` at a serial HDF5
if that matters. CMake says so at configure time.


## Testing

`ctest` on a serial build runs:

- `puffin_basic_tests` - the same unit tests as the parallel build.
- `puffin_e2e_tests_serial` - `test/testSerialIntegration.pf`, which runs the
  full 1D `f1main` deck in-process and checks it against the same golden files
  the parallel integration test uses. Those references were captured from a
  two-rank run, so agreeing with them to 1e-10 is the statement that matters:
  turning MPI off does not change the physics.

The `puffin_e2e_tests*` suites are declared `@mpitest(npes=[2])` and so are
built only for a parallel configure, as are the opt-in slow and big tiers.

Benchmarks work too, for the single-rank case:

```sh
cmake -B ../build-serial -DENABLE_PARALLEL=OFF -DENABLE_BENCHMARKS=ON .
ctest --test-dir ../build-serial -L benchmark
```

`benchmark/run_benchmarks.py --mpirun ""` runs the executable directly instead
of through a launcher. Only the `1d_serial` case is registered in serial mode;
timing it in both modes is the interesting comparison, since it is the same
physics either way and the difference is what the MPI layer costs on one rank.


## Adding code that communicates

If you add an MPI call that Puffin does not already make, the serial build will
fail to compile: the stub exports only the names in use. Add the routine to
`backends/serial/mpi_serial.f90` with its one-rank meaning, and to the `public`
list. Keep the "abort rather than guess" rule for anything whose one-rank
behaviour is not well defined.
