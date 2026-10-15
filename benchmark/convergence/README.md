# Step-size convergence scans

`run_scan.py` measures how the final radiation field depends on the integration
step, so that a change to the integrator can be judged on accuracy-per-step
rather than on wall clock alone.

```sh
python3 run_scan.py <undulator-type> <reference-steps> <steps> [<steps> ...]
SCAN_NODES=24 python3 run_scan.py planepole 240 12 20 30   # override nodesPerLambdar
SCAN_ORDER=3 python3 run_scan.py planepole 240 12 20 30    # cubic deposition
```

Every value in `<steps>` is run twice, once with `qQuiverSplit = .false.` and
once with `.true.`, and both are compared against a single unsplit run at
`<reference-steps>`, which stands in for the exact solution. Using the *same*
reference for both is what makes the two columns comparable, and it doubles as
the correctness check on the split scheme: it solves the same ODE, so at a fine
step it has to converge to the same answer. It does — split and unsplit at 240
steps/period agree to 6.6e-7, inside the reference's own truncation error.

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
overrides which binary is run; the default is the in-tree `build/`.

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

| | the two references differ by | error at 30 steps/period moves |
| --- | --- | --- |
| planepole, cubic + split | 4.3e-7 | 0.3% |
| helical, cubic + split | 6.1e-7 | 0.8% |

So the floor of this measurement is a few times 1e-7. The smallest number quoted
anywhere below is 6.4e-6, an order of magnitude above it and stable to under a
percent when the reference is refined, so even the largest ratios in section 3
are measuring the scheme rather than the reference.

## What the scans showed

### 1. Linear deposition, and the cells-per-step floor

The first round of scans found that with the shipped linear deposition the
accuracy floor is set by macroparticles crossing field-mesh cells rather than by
resolving the undulator quiver. Sorting the runs by cells crossed per step —
`nodesPerLambdar / stepsPerPeriod` — the error collapses onto that one number:

| cells/step | 0.4 | 0.6 | 0.8 | 1.0 | 1.2 | 2.0 |
| --- | --- | --- | --- | --- | --- | --- |
| rel. L2 error, linear + split | 1.0e-3 | 1.0e-2 | 2.5e-3 | 2.3e-2 | 3.2e-2 | 1.8e-1 |

Doubling `nodesPerLambdar` from 12 to 24 at a fixed 12 steps/period makes the
error 8x worse even though the integration step is unchanged. The deposition is
linear, so a particle crossing a node puts a kink in the source, and RK4 cannot
integrate through a kink at fourth order.

That diagnosis predicted a fix — a smoother kernel — and it is now implemented
and measured, which is the rest of this document.

### 2. The smoother kernel, and a correction to the diagnosis

`iShapeOrder = 2` or `3` replaces the linear (C0) weights with a quadratic (C1)
or cubic (C2) B-spline, in `shape_functions.f90`. Rerunning the scans across all
three kernels changes the picture in a way the original write-up got half right.

**The kink is real, and smoothing removes it — but only once the quiver error is
out of the way as well.** At the shipped operating point (`nodesPerLambdar = 12`,
30 steps/period, 0.4 cells/step), relative L2 error in the final field:

Throughout this document a factor like "100x" means *error reduced by* that
factor at the same step count — the default's error divided by this row's. It
has nothing to do with FEL gain.

| configuration | error | error reduced by |
| --- | --- | --- |
| linear, unsplit (the shipped default) | 2.14e-3 | - |
| linear, `qQuiverSplit` | 9.96e-4 | 2.1x |
| cubic, unsplit | 1.87e-3 | 1.1x |
| cubic + `qQuiverSplit` | 2.13e-5 | **100x** |

Neither change alone is worth much; together they are worth two orders of
magnitude. The two error sources — quiver truncation and the deposition kink —
are comparable in size at this operating point, so removing either one leaves
the other in charge. The original conclusion that "the floor is set by
deposition, not the quiver" was drawn from the split column alone, where that is
true; for a default run, both matter about equally.

**The kernel's continuity class shows up directly as the observed order of
convergence.** Fitting `error ~ (1/steps)^p` over 12 to 20 steps/period, with
`qQuiverSplit` on, at `nodesPerLambdar = 12`:

| kernel | continuity | observed p |
| --- | --- | --- |
| linear | C0 | 1.7 |
| quadratic | C1 | 2.7 |
| cubic | C2 | 4.1 |

That is the cleanest confirmation of the diagnosis: with a C2 source, RK4 gets
its design order back. (Over the finer 20 to 30 interval the measured slopes are
steeper still, 5.7 to 7.6, so this is not a single clean power law over the whole
range — the numbers above are the coarse-step slope, which is the regime that
matters for choosing a step.)

**The cells-per-step collapse is an artefact of the C0 kernel, and it breaks.**
Same runs, split column, all three kernels:

| cells/step | 0.4 | 0.6 | 0.8 | 1.0 | 1.2 | 2.0 |
| --- | --- | --- | --- | --- | --- | --- |
| linear | 1.0e-3 | 1.0e-2 | 2.5e-3 | 2.3e-2 | 3.2e-2 | 1.8e-1 |
| quadratic | 9.4e-5 | 2.1e-3 | 4.1e-4 | 8.4e-3 | 1.3e-2 | 1.4e-1 |
| cubic | 2.1e-5 | 4.7e-4 | 8.4e-5 | 3.7e-3 | 5.8e-3 | 1.1e-1 |

The linear row is monotonic in cells/step; the cubic row is not — 0.6 cells/step
(12 nodes, 20 steps) is *worse* than 0.8 (24 nodes, 30 steps). With the cubic
kernel the error is governed mainly by the step, as an ODE integrator's error
should be, and the mesh density is a second-order effect.

**But the floor only moves where the step resolves the crossing.** Answering the
question the kernel was built to answer:

- at 1.0 cells/step the split error drops 6.3x (2.3e-2 to 3.7e-3), and 21x
  against the unsplit default;
- at 2.0 cells/step it drops 1.7x, from 1.8e-1 to 1.1e-1, which is no use to
  anyone — the error is still enormous. At two cells per step the source varies
  on the scale of the step itself and no amount of kernel smoothness fixes that.

So: a smoother kernel buys one to two orders of magnitude below about one cell
per step, and nothing at two.

### 3. The quiver split now pays off on planepole

The secondary check, and it confirms the diagnosis from the other direction.
`qQuiverSplit` was worth about 2x on `planepole` against linear deposition,
versus far more on `helical`, because the planar figure-of-eight excursion drags
the macroparticles back and forth across node boundaries and the resulting kinks
dominated whatever the split saved. With the kink smoothed away, the split is
worth what it should be. Error reduction from turning `qQuiverSplit` on, at
`nodesPerLambdar = 12`:

| steps/period | planepole, linear | planepole, cubic | helical, linear | helical, cubic |
| --- | --- | --- | --- | --- |
| 12 | 3.4x | 19x | 33x | 1594x |
| 20 | 1.4x | 20x | 6.2x | 389x |
| 30 | 2.1x | 88x | 3.4x | 416x |

### 4. What it costs, and whether it is worth turning on

Measured in an optimised build (`-DCMAKE_BUILD_TYPE=Release`), 1D planepole,
`nodesPerLambdar = 12`, best of three runs. The Debug build the scans run in
exaggerates the stencil cost (1.7x there, 1.4x here), so these are the numbers
to quote:

| configuration | steps/period | error | wall clock |
| --- | --- | --- | --- |
| linear, unsplit (default) | 30 | 2.14e-3 | 5.75 s |
| cubic + split | 30 | 2.13e-5 | 8.18 s |
| cubic + split | 14 | 8.83e-4 | 4.43 s |
| cubic + split | 12 | 3.75e-3 | 3.93 s |

Cubic deposition costs about 1.4x per step in 1D (four nodes touched instead of
two, per gather and per scatter); `qQuiverSplit` is free. Two ways to spend that:

- **same step, better answer**: 100x less error for 1.4x the wall clock;
- **same answer, less time**: the default's 2.14e-3 is reached at roughly 13
  steps/period, so about 2.3x fewer steps, or ~1.4x less wall clock end to end.

The first is the interesting one. A 100x accuracy margin at 1.4x cost is the
kind of headroom that makes a result trustworthy rather than merely fast.

### 5. What the smoothing costs: a real change to the discretisation

A B-spline filters the source in space as well as smoothing it in time, so at a
fixed mesh the coupling to the radiation is slightly weaker. This is a
discretisation difference, not an error, and it converges away as the mesh is
refined. Final field at 120 steps/period (converged in the step), planepole,
relative to the linear kernel on the same mesh:

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

## Using it

`iShapeOrder` goes in `&MDATA`, defaults to 1, and 1 is the historical linear
deposition, on the original arithmetic: all four e2e suites still match their
goldens bit-for-bit. 2 and 3 select the quadratic and cubic B-splines, in 1D and
3D, and work on both the temporal and periodic meshes.

Recommended, on this evidence: `iShapeOrder = 3` together with
`qQuiverSplit = .true.`, and do not expect either to rescue a run taking more
than about one cell per step.

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
