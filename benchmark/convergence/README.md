# Step-size convergence scans

`run_scan.py` measures how the final radiation field depends on the integration
step, so that a change to the integrator can be judged on accuracy-per-step
rather than on wall clock alone.

```sh
python3 run_scan.py <undulator-type> <reference-steps> <steps> [<steps> ...]
SCAN_NODES=24 python3 run_scan.py planepole 240 12 20 30   # override nodesPerLambdar
```

Every value in `<steps>` is run twice, once with `qQuiverSplit = .false.` and
once with `.true.`, and both are compared against a single unsplit run at
`<reference-steps>`, which stands in for the exact solution. Using the *same*
reference for both is what makes the two columns comparable, and it doubles as
the correctness check on the split scheme: it solves the same ODE, so at a fine
step it has to converge to the same answer. It does — split and unsplit at 240
steps/period agree to 6.6e-7, inside the reference's own truncation error.

`scan.in` is deliberately boring: seeded rather than SASE, `q_noise` off, one
undulator, no lattice file, no undulator ends. Nothing varies run to run except
the step count, so the differences measured are integrator error and nothing
else. The physical parameters are those of the 1D CLARA deck (rho = 0.005,
lambda_w = 27.5 mm, gamma_r = 456.4).

Needs `mpirun` and `h5dump`; standard library Python only.

## What the scans showed

The headline result is that **the accuracy floor is set by macroparticles
crossing field-mesh cells, not by resolving the undulator quiver.** Sorting
every run by cells crossed per step — `nodesPerLambdar / stepsPerPeriod` — the
error collapses onto that one number:

| cells/step | 0.4 | 0.6 | 0.8 | 1.0 | 1.2 | 2.0 |
| --- | --- | --- | --- | --- | --- | --- |
| rel. L2 error, split | 1.0e-3 | 1.0e-2 | 2.6e-3 | 2.4e-2 | 3.2e-2 | 1.9e-1 |

Doubling `nodesPerLambdar` from 12 to 24 at a fixed 12 steps/period makes the
error 8x worse (2.4e-2 to 1.9e-1) even though the integration step is unchanged.
The deposition in `getSource_1D` is linear, so a particle crossing a node puts a
kink in the source; RK4 cannot integrate through a kink at fourth order, and no
amount of cleverness in the electron equations recovers it.

Two consequences:

- Puffin's shipped defaults (`stepsPerPeriod = 30`, `nodesPerLambdar = 12`, so
  0.4 cells/step) already sit at that floor. The quiver was never what limited
  the step at the default operating point.
- The `qQuiverSplit` integrator therefore pays only where the deposition floor
  is not already binding. Measured on this deck at `nodesPerLambdar = 12`:
  2.5x fewer steps for the same accuracy on a helical undulator, but only about
  1.1x on `planepole`, and nothing at all at `nodesPerLambdar = 24`.

The polarisation difference is itself a deposition effect. A planar undulator
gives the macroparticles a longitudinal figure-of-eight excursion of about a
third of a mesh cell, so they slosh back and forth across node boundaries on top
of their mean drift, adding crossings and therefore kinks. Helical motion has
`|pperp|^2` constant and no such excursion.

Splitting that figure-of-eight out of the integrated `z2` was tried and
removed: it changed the planar error by under 5%, because the transformation
only moves the excursion out of the *integrated* variable — the particle still
physically crosses the same cell boundaries, so the source keeps the same kinks.

The lever that would actually raise this floor is a smoother deposition kernel
(quadratic or cubic B-spline in place of the linear weights), which would let
both the step size and the quiver split pay off. That is untried.
