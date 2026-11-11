# Step-size convergence scans

`run_scan.py` measures how the final radiation field depends on the integration
step, so that a change to the integrator can be judged on accuracy-per-step
rather than on wall clock alone. It was built to judge the B-spline deposition
kernels (`iShapeOrder`), which is what this document reports.

```sh
python3 run_scan.py <undulator-type> <reference-steps> <steps> [<steps> ...]
SCAN_NODES=24 python3 run_scan.py planepole 240 12 20 30   # override nodesPerLambdar
SCAN_ORDER=3 python3 run_scan.py planepole 240 12 20 30    # cubic deposition
```

Every value in `<steps>` is compared against a run at `<reference-steps>`,
which stands in for the exact solution.

`SCAN_ORDER` sets `iShapeOrder`, the B-spline degree of the deposition kernel
(1 linear, 2 quadratic, 3 cubic). The reference is run with the *same* kernel as
the runs measured against it. That matters: a smoother kernel filters the source
slightly differently in space, so it converges to a marginally different answer,
and comparing across kernels would measure that spatial difference rather than
step-size error. It is quantified under "what the smoothing costs" below.

`scan.in` is deliberately boring: seeded rather than SASE, `q_noise` off, one
undulator, no lattice file, no undulator ends. Nothing varies run to run except
the step count, so the differences measured are integrator error and nothing
else. The physical parameters are those of the 1D CLARA deck (rho = 0.005,
lambda_w = 27.5 mm, gamma_r = 456.4).

Needs `mpirun` and `h5dump`; standard library Python only. `PUFFIN=<path>`
overrides which binary is run; the default is the in-tree `build/`. Further
`SCAN_*` variables, listed at the top of the script, pick the deck (the 3D one
below is `SCAN_DECK=test/inputs/3D/clara_test.in SCAN_INPUTS=test`), where the
runs go, a dump every N periods, and a JSON copy of the results.

## Figures

`figures/make_figures.sh SCANS` runs every scan the figures need, all at once,
and draws them; `plot_convergence.py` draws the first, and
`../averaged/plot_compare_latt.py` - the averaged-mode overlay plotter, which
takes `--labels` and `--ratio` to compare any two runs - the other three.

- [`figures/convergence.png`](figures/convergence.png): error against steps per
  period for every kernel in 1D planar, 1D helical and 3D; the observed order
  against the kernel's degree; what the kernel is worth against steps per period
  in each case; and the error against cells crossed per step at two mesh
  densities. Sections 1 to 3 in one picture.
- [`figures/overlay_linear.png`](figures/overlay_linear.png) and
  [`figures/overlay_cubic.png`](figures/overlay_cubic.png): power, energy,
  bunching, profile, spectrum and phase space for the linear and cubic kernels,
  each at 30 and 12 steps/period against its own 240-step reference - what the
  error numbers look like in the observables.
- [`figures/overlay_kernels.png`](figures/overlay_kernels.png): linear against
  cubic, both converged in the step - the kernel's form factor of section 5.
- [`figures/corefine.png`](figures/corefine.png), from
  `figures/make_corefine_figure.sh`: the co-refinement study below - the one
  that says which kernel is closer to the physical answer.

## How the error is measured

One number per run: the relative L2 difference of the final radiation field
against the reference,

    err  =  || A - A_ref ||_2  /  || A_ref ||_2

taken over the final `_aperp_` dump in full - every z2 node, real and imaginary
parts both (21012 doubles for this deck) - rather than over a reduction like
power or bunching. It is an aggregate that mixes amplitude and phase error: a
pure phase slip shows up here as a large error, and these scans do not separate
the two.

The reference is not the exact solution, so it is worth knowing how far the
measurement can be trusted. Re-running the smallest errors against a 480
step/period reference as well as the 240 one:

| | the two references differ by |
| --- | --- |
| planepole, cubic | 4.3e-7 |
| helical, cubic | 6.1e-7 |

So the floor of this measurement is a few times 1e-7. The smallest error quoted
anywhere below is 1.2e-4, more than two orders of magnitude above it, so every
ratio here is measuring the scheme rather than the reference.

## Co-refinement: which kernel is closer to the physical answer

Every scan in the sections after this one varies the step alone, against a
reference with the same mesh, the same macroparticles and the same kernel. That
measures time-step error at a frozen spatial discretisation, and nothing else:
it cannot say whether a run is close to the physical answer, and it cannot
compare kernels, which converge to different answers on a given mesh. The
resolutions interact - the error a run makes depends on the cells per
wavelength, the macroparticles per cell and the cells crossed per step
together - so a claim of convergence needs them refined together.

`run_corefine.py` does that, in 1D, for planepole and helical. Level r has 12r
cells per resonant wavelength, `iMPsZ2PerWave = 4r` and 30r steps per period,
so 6.3 macroparticles per cell and 0.4 cells crossed per step at every level,
for r = 1, 2, 4, 8. From level 2 it also doubles each of the three alone.
`analyse_corefine.py` compares physical observables at the end of the
undulator - pulse energy, peak power, mid-beam bunching, energy loss, and the
complex field envelope (demodulated on the carrier, low-passed, and sampled at
the same z2 points for every mesh) - against a Richardson extrapolation from
levels 2, 4 and 8. The scalars are extrapolated with their fitted order; the
envelope and the energy history point by point, at order 2. No finite run is
used as a reference. The script's docstring defines every quantity exactly. Note that Puffin lays `nodesPerLambdar - 1` cells in each wavelength,
so 12 cells needs `nodesPerLambdar = 13`; the step scans below say "12
nodes/λr" and mean 11 cells.

What it shows ([`figures/corefine.png`](figures/corefine.png)):

- **All three kernels converge to the same answer.** The extrapolated limits
  of pulse energy, peak power, bunching and energy loss agree to 5e-5 across
  kernels, and the extrapolated field envelopes to 2e-6. Every observable,
  the envelope included, converges at second order (observed 1.92-2.04) for
  every kernel.
- **At these ratios the z2 mesh carries all of the error.** From level 2,
  doubling only the cells reproduces the full refinement for every kernel;
  doubling only the macroparticles, or only the steps, changes no observable
  in its third figure. The time-step error the B-spline kernel was built to
  reduce is negligible here.
- **The smoother kernels are less accurate on a given mesh, not more.** Pulse
  energy against the extrapolated limit, planepole:

  | cells per λr | linear | quadratic | cubic |
  | --- | --- | --- | --- |
  | 12 | -6.6% | -10.5% | -14.2% |
  | 24 | -2.0% | -3.0% | -4.0% |
  | 48 | -0.51% | -0.76% | -1.01% |
  | 96 | -0.13% | -0.19% | -0.26% |

  Helical is the same to within 20%. Every kernel undershoots, and the ratio
  is 1 : 1.5 : 2 at every level - the ratio of the B-splines' variances,
  (k + 1) Δ² / 12 for degree k. The leading error is the deposition kernel's
  own spatial smoothing of the source, the linear kernel's included, and a
  smoother kernel smooths more.

- **Total-energy conservation cannot see this error.** The drift in field +
  beam energy, W = ½ Σ|A|² ΔV + Σ χΓ / 2ρ, is 2e-8 to 1.2e-4 of the energy
  exchanged in every run, while the energy exchanged is 7-16% short at 12
  cells per wavelength. The gather and scatter are transposes, so the
  spatially discretised system conserves W exactly at any mesh and kernel; the
  drift measures time-integration error alone. It does confirm the kernel's
  design aim: the cubic kernel drifts about 10x less than the linear one, and
  doubling only the cells (so twice the cells crossed per step) raises the
  drift 15-60x. But that error is three to four orders of magnitude below the
  mesh error.

So in 1D, at practical ratios, a higher `iShapeOrder` makes results worse. The
step scans below are correct as measurements of time-step error, but that is
not the error that limits these runs. Whether 3D differs - the 3D deck runs at
0.7-1.8 cells per step, where its step error is far larger - needs the same
study in 3D, with the transverse cells and macroparticles refined too, and has
not been done.

## Step scans at a fixed spatial discretisation

Everything from here to section 5 measures time-step error only, against a
reference with the same mesh, macroparticles and kernel - read it with the
section above in mind. A factor like "38x" means *time-step error reduced by*
that factor at the same step count - the linear kernel's error divided by this
one's. It has nothing to do with FEL gain. All runs are on one MPI rank, in the
Debug build.

### 1. Two sources of step error, and which one the kernel can touch

Puffin resolves the undulator period, so its step error has two sources:

- **the quiver** - RK4 tracking the fast transverse oscillation, at a fixed
  number of steps per undulator period;
- **node crossings** - the linear (hat) deposition is only C0, so every time a
  macroparticle crosses a field-mesh node the source it scatters gains a kink,
  and RK4 cannot integrate through a kink at fourth order.

The second is visible directly with the linear kernel: at a fixed step, a finer
mesh - more nodes to cross - is *less* accurate. 1D planepole, linear:

| steps/period | 12 nodes/λr | 24 nodes/λr | finer mesh is worse by |
| --- | --- | --- | --- |
| 12 | 7.98e-2 | 2.05e-1 | 2.6x |
| 20 | 1.41e-2 | 3.40e-2 | 2.4x |
| 30 | 2.19e-3 | 3.32e-3 | 1.5x |

That comparison is confounded: the beam was loaded at a fixed 4 slices per
wavelength, so doubling the mesh also halved the macroparticles per cell, and
part of the difference may be a sparser, more granular source rather than more
node crossings. The co-refinement above holds that ratio fixed.

A smoother kernel removes the kink and nothing else. So what it is worth depends
entirely on how much of the error the node crossings account for - which is set
by how many cells a macroparticle sweeps through per step, and that is mostly
the figure-of-eight: on a planar undulator the z2 motion oscillates at twice the
quiver frequency and drags each particle back and forth across nodes. A helical
undulator has no figure-of-eight, and there the kernel is worth nothing in 1D.

### 2. The smoother kernel, in 1D

`iShapeOrder = 2` or `3` replaces the linear (C0) weights with a quadratic (C1)
or cubic (C2) B-spline, in `shape_functions.f90`. Error in the final field, 1D:

| steps/period | planepole 12 nodes/λr: linear / cubic | gain | planepole 24 nodes/λr: linear / cubic | gain | helical 12 nodes/λr: gain |
| --- | --- | --- | --- | --- | --- |
| 12 | 7.98e-2 / 7.26e-2 | 1.1x | 2.05e-1 / 1.38e-1 | 1.5x | 1.0x |
| 20 | 1.41e-2 / 9.47e-3 | 1.5x | 3.40e-2 / 1.18e-2 | 2.9x | 1.0x |
| 30 | 2.19e-3 / 1.87e-3 | 1.2x | 3.32e-3 / 2.03e-3 | 1.6x | 1.1x |
| 40 | 8.82e-4 / 5.91e-4 | 1.5x | 3.42e-3 / 6.54e-4 | 5.2x | 1.2x |
| 60 | 3.15e-4 / 1.17e-4 | 2.7x | 7.57e-4 / 1.27e-4 | 6.0x | 1.7x |

At the shipped operating point - 12 nodes/λr, 30 steps/period, 0.4 cells per
step - the quiver dominates and the kernel is worth 1.2x. The slope from 12 to
20 steps/period is about 4 for every kernel (3.4 / 4.0 / 4.0), RK4's own order:
the step error there is the quiver's, and the deposition is a minor part of it.
On the finer 24-node mesh the crossings matter more and the kernel is worth
1.5-6x, with the slope now ordered by degree (3.5 / 4.5 / 4.8). Quadratic sits
close to cubic throughout.

### 3. In 3D, where the step error is large

The 3D path is a tensor product of the same splines, and the node indices stride
by `nspinDX` and `ntrndsi_G` as well as by one, so it is worth measuring rather
than assuming. Scanned on the 3D e2e deck (`test/inputs/3D/clara_test.in`: 85x85
transverse, 21 nodes per resonant wavelength, periodic, diffraction on, beam
read from HDF5 so the realisation is fixed, undulator ends on), each kernel
against its own 240 step/period reference:

| steps/period | cells/step | linear | quadratic | cubic | cubic gain |
| --- | --- | --- | --- | --- | --- |
| 12 | 1.8 | 6.2e-1 | 3.1e-1 | 2.0e-1 | 3.1x |
| 16 | 1.3 | 3.9e-1 | 1.2e-1 | 5.7e-2 | 6.8x |
| 20 | 1.1 | 3.1e-1 | 6.0e-2 | 1.4e-2 | 22x |
| 24 | 0.9 | 1.5e-1 | 1.9e-2 | 3.7e-3 | 39x |
| 30 | 0.7 | 1.5e-1 | 2.2e-2 | 4.1e-3 | 38x |
| 40 | 0.5 | 9.1e-2 | 1.5e-2 | 3.0e-3 | 31x |

| observed order p, 12 to 20 | linear | quadratic | cubic |
| --- | --- | --- | --- |
| 3D | 1.4 | 3.2 | 5.2 |

Here the node crossings dominate the step error, and the kernel's continuity
class shows up directly as the observed order of convergence in the step. This deck samples 21 nodes per
wavelength and runs at 0.5 to 1.8 cells per step, where the crossings set the
step error; the 1D default sits at 0.4 with half the mesh density, where the
quiver does. Whether reducing it makes a 3D run closer to the physical answer
depends on how large this step error is against the kernel's own smoothing
error, which only a 3D co-refinement can say.

Beyond about 24 steps/period the 3D error stops falling - cubic is 3.7e-3 at 24,
4.1e-3 at 30, 3.0e-3 at 40, and linear is flat between 24 and 30 too. Some other
error source takes over there, the same for every kernel, and it has not been
examined; the split-step diffraction advance is the obvious candidate.

### 3b. Which direction the kernel is worth anything in

In 3D the degree can be set separately along z2 (`iShapeOrder`) and across x
and y (`iShapeOrderXY`, which defaults to `iShapeOrder`). Same deck, each
combination against its own 240 step/period reference:

| z2 / x, y degree | 20 steps/period | 24 | 30 | error reduced by, at 30 | order p, 12 to 20 |
| --- | --- | --- | --- | --- | --- |
| linear / linear | 3.1e-1 | 1.5e-1 | 1.5e-1 | - | 1.4 |
| linear / cubic | 3.1e-1 | 1.4e-1 | 1.6e-1 | 1.0x | 1.1 |
| quadratic / linear | 6.3e-2 | 2.1e-2 | 2.2e-2 | 7.0x | 3.2 |
| cubic / linear | 1.6e-2 | 4.8e-3 | 4.6e-3 | 34x | 5.1 |
| cubic / cubic | 1.4e-2 | 3.7e-3 | 4.1e-3 | 38x | 5.2 |

Essentially all of it is in z2. A cubic kernel across x and y with linear z2
changes nothing; cubic along z2 with linear x and y keeps 90% of the full cubic
gain, and the observed order follows the z2 degree alone. On this deck that is
what the geometry predicts: the transverse quiver excursion, a_w lambda_w /
(2 pi gamma) ~ 10 um, is a quarter of a 39 um transverse cell, so particles
rarely cross transverse nodes within a period, while along z2 the
figure-of-eight sweeps them across several. A deck with a coarser transverse
mesh, or a beam large enough for betatron motion to cross cells quickly, could
need the transverse degree too; none was measured. Like everything in this
part, this is about time-step error at a fixed mesh: it says where the kernel
reduces step error, not that the result is closer to the physical answer.

### 4. What it costs, and whether it is worth turning on

Measured in an optimised build (`-DCMAKE_BUILD_TYPE=Release`), best of three,
timed on the undulator module alone so that setup and IO are out of it. The
Debug build the scans run in exaggerates the stencil cost, so these are the
numbers to quote.

**1D**: cubic costs about 1.5x per step - four nodes touched instead of two,
per gather and per scatter, diluted by the rest of the step (planepole, 12
nodes/λr, 30 steps/period: 4.66 s linear, 7.04 s cubic - the cubic timing taken with the since-removed quiver split on, which cost nothing measurable). At the default mesh it
reduces the step error 1.1-1.5x for that, and the co-refinement shows the step
error is not what limits a 1D run there anyway.

**3D** is a different trade. Same deck as section 3, 40 periods, no dumps:

| z2 / x, y degree | nodes per particle | module time | per step | error reduced by, at 30 steps/period |
| --- | --- | --- | --- | --- |
| linear / linear | 8 | 0.74 s | - | - |
| linear / cubic | 32 | 1.78 s | 2.4x | 1.0x |
| quadratic / linear | 12 | 1.07 s | 1.45x | 7.0x |
| cubic / linear | 16 | 1.25 s | 1.70x | 34x |
| quadratic / quadratic | 27 | 1.59 s | 2.15x | 7.1x |
| cubic / cubic | 64 | 2.96 s | 4.0x | 38x |

The cost follows the nodes touched per particle, for the gather and again for
the scatter. Every scatter is an `!$OMP ATOMIC` into `getSource_3D` - already
the memory-bound routine where an unrelated commit once cost 48% by moving
symbols around (see `benchmark/RESULTS-2026-08-07-globals-refactor.md`).

Cubic along z2 with linear x and y gives 90% of the full cubic kernel's
step-error reduction for 43% of its cost. An earlier version of this section
went on to compare kernels "at equal accuracy". That comparison does not hold:
each kernel's error was measured against its own reference, and the kernels
converge to different answers on a given mesh - by far more, as the
co-refinement shows, than the step errors being compared. Only a co-refinement
in 3D can say whether the kernel's lower step error outweighs the extra
smoothing error it brings.

### 5. What the smoothing costs: a real change to the discretisation

A B-spline filters the source in space as well as smoothing it in time, so at a
fixed mesh the coupling to the radiation is weaker. This section first called
that a discretisation difference rather than an error. The co-refinement above
settles it: it is an error, against an answer every kernel converges to, and
the linear kernel has the same error at two-thirds the size of the quadratic's
and half the cubic's. Final field at 120 steps/period (converged in the step),
planepole, relative to the linear kernel on the same mesh (these runs also
held the macroparticles per wavelength fixed, so the 24-node column has half
the macroparticles per cell):

| kernel | `nodesPerLambdar = 12` | `nodesPerLambdar = 24` |
| --- | --- | --- |
| quadratic | -2.5% | -0.6% |
| cubic | -4.8% | -1.2% |

It falls by 4x when the mesh doubles, i.e. it scales as the square of the cell
size, as a kernel form factor should. Two practical consequences:

- a scan must use a reference computed with its own kernel, which `run_scan.py`
  does;
- changing `iShapeOrder` on an existing deck changes the answer by a few percent
  at typical mesh densities. It is not a drop-in refinement of the same
  discretisation, it is a different one.

The effect is much larger for a run starting from shot noise, where the drive is
broadband and the kernel filters a real part of it. On the 3D CLARA test deck
(zero seed, 5 periods of startup) the final field is 33% lower with quadratic
and 44% lower with cubic. That is expected rather than wrong — the shot-noise
source has content right up to the mesh Nyquist — but it does mean the kernel is
not a free knob on a SASE startup run.

How much of that 44% is physics is an open question, and the number above
probably overstates it. The averaged-mode SASE work (`benchmark/averaged/README.md`)
found that the unaveraged field's out-of-band share at a fine mesh is largely
the mesh's own resolved bandwidth rather than radiation anyone cares about — the
in-band share at zbar = 1.3 falls from 0.87 to 0.41 as the mesh goes from 12 to
48 nodes per wavelength. A band-limited comparison would be the honest
measurement here, and has not been done.

### Tried and removed: splitting out the quiver

The other half of the 1D step error - the quiver - was attacked alongside this
work with an analytic split: subtract the closed-form undulator quiver from
pperp for each RK4 step and integrate only the slow remainder. In 1D, together
with the cubic kernel, it cut the error 100x at the default operating point. It
was removed before merging. It needs the undulator field across each step to be
a closed-form function of zbar, so end ramps, tapers and any field that changes
appreciably within a period fell back to the unsplit scheme - and resolving such
changes is much of the reason to run an unaveraged code. In 3D it was worth
nothing (x and y still carry the quiver), and even in 1D its error was not
monotonic in the step. The kernel has none of those limits and stands on the 3D
result alone.

## The inner transverse mesh, and its margin

`getInNode` sizes the inner transverse mesh - the reduced x, y region the
field/electron interaction actually runs on, a memory-saving measure - from the
outermost macroparticle plus a margin, `inBuf`. That margin was 3 cells for
every case. It is what absorbs betatron motion between calls, and running out of
it costs a rebuild of the parallel layout, or an early exit from the undulator
section if three rebuilds in a row do not fix it (`igoes > 3` in
`undulator.f90`).

Three cells leaves the linear stencil two nodes spare at each transverse edge
and a cubic one node, since the wider stencil reaches further for the same
particle. So `inBuf` now grows with the stencil - `3 + shapeLeftReach(order)`,
which is the historical 3 at `iShapeOrder = 1` - rather than letting a wider
kernel spend headroom that exists for a different purpose.

Worth being clear about what that fix rests on: the arithmetic above, and a unit
test (`testStencilOverrunIsCaught`) that a stencil reaching past the inner mesh
sets `inner_xy_ok` and deposits nothing, rather than writing outside the field
arrays. It is not a fix for an observed failure - none of the 3D runs here ever
tripped the regrow path, because `getInNode` is only called again on demand and
these decks' beams do not move far enough. A beam filling more of the inner mesh
is the case that would have found it.

## Open: the beam has to start inside the mesh

A kernel wider than the linear one reaches one node behind the particle, and
there is no node behind the first one. There are two honest things to do about a
macroparticle in the first cell, and this code now does the second:

- fold the overhanging weight onto the first node. Charge-conserving, and it was
  what the first version of this did - but it quietly imposes a boundary
  condition (the field held constant off the end of the mesh) at the one place
  in the run where the source is already least trustworthy. Removed on
  2026-10-16, because a boundary treatment nobody asked for is worse than an
  error message.
- stop, and say so. `getInterps_*_bspline` prints `Z2 coord is too small for the
  deposition stencil` and halts.

Which makes deck setup the user's problem, and that is not where it belongs.
Every standard Puffin deck trips it: `getOffsets` in `init_conds.f90` replaces
any `bcenter` below half the beam length with exactly half the beam length,
putting the beam front on `z2 = 0` by construction. Both decks here therefore
write `bcenter` as half the beam length plus the offset wanted - 25.03 for the
50-long scan beam, 1.04 for the 2.0-long e2e beam - which is obscure enough to
be worth the comments it carries in those files.

**The fix belongs in the macroparticle generation**: it should leave the first
few nodes of the mesh empty of macroparticles on its own, sized from the
deposition stencil, rather than placing the beam front hard against node 1 and
making every wide-stencil deck work around it. Until then the error message is
the contract. Periodic meshes are already fine, since the wrap leaves the room.

## Using it

`iShapeOrder` goes in `&MDATA`, defaults to 1, and 1 is the historical linear
deposition, on the original arithmetic: all four e2e suites still match their
goldens bit-for-bit. 2 and 3 select the quadratic and cubic B-splines, in 1D and
3D, and work on both the temporal and periodic meshes.

`iShapeOrderXY`, also in `&MDATA`, sets the degree across x and y in 3D
separately; left out, it follows `iShapeOrder`, so `iShapeOrder` alone still
sets the whole stencil. In 3D the linear path, with its own arithmetic, runs
only when both are 1. `iShapeOrderXY` has no effect in 1D, and the input reader
warns if it is set there.

What this evidence supports:

- **1D**: keep `iShapeOrder = 1`. With macroparticles, cells and steps refined
  together, the smoother kernels converge to the same answer as the linear one
  but with 1.5x (quadratic) and 2x (cubic) its error at every resolution, and
  the step error they reduce is negligible at practical ratios.
- **3D**: not yet known. On the 3D test deck the kernel reduces time-step
  error by up to 38x, and almost all of that comes from the z2 degree - but it
  will also bring the extra smoothing error seen in 1D, and which wins has not
  been measured. Until a 3D co-refinement settles it, treat `iShapeOrder > 1`
  as an experimental option, and use it, if at all, along z2 only
  (`iShapeOrderXY = 1`), which buys nearly all of the step-error reduction at
  under half the cost.
- **Either**: do not change `iShapeOrder` or `iShapeOrderXY` on a deck whose
  numbers you are comparing against older runs (see section 5).

The wider stencil reaches one node further left and right than the linear one,
which the parallel field layout has to account for: `rearrElecs` shifts which
macroparticles a rank owns so the stencil stays inside the rank's slice,
`calcBuff` widens the right-hand halo, and the periodic wrap leaves a cell of
room at the front of the mesh. All three are no-ops at `iShapeOrder = 1`.

Results stay rank-independent at the level the code already is, but not better,
and the wider stencil costs something here. On the temporal scan deck, 1, 2 and
3 ranks agree to 6e-6 - 1.4e-5. On the periodic 1D taper deck, which is far more
rank-sensitive to begin with, 2 ranks differ from 1 by 7.9e-5 with the linear
kernel and 2.6e-4 with the cubic - about three times worse. More nodes touched
per particle means more of the source crossing a rank boundary and being summed
in a different order, on top of the shifted ownership moving different particles
between ranks. Neither figure is roundoff, and the linear one is not either, so
this is an existing property of the parallel layout that a wider stencil
amplifies rather than something the kernel introduces. Worth knowing before
trusting a multi-rank run at this level.
