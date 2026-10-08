# Slow and big tests for pull requests into master

`master` only moves at release points, by a pull request from `dev`. Before one
merges, the gated test suites that normal development leaves switched off must
pass too (UKFELs/Puffin#116):

| Suite | Size | Where it runs |
|---|---|---|
| `puffin_e2e_tests_3d_slow` | full CLARA lattice, ~30 s on 2 ranks | hosted CI (`build.yml` turns on `PUFFIN_SLOW_TESTS` for PRs into master), and again on the HPC machine |
| `puffin_e2e_tests_3d_big` | 85×85×5780 mesh, ~9–12 GB, ~9 min on 6 ranks | an HPC machine, by hand |

The big test does not fit on a hosted runner, so its result comes back to the
pull request as a comment, and the `hpc-evidence` check
(`.github/workflows/hpc-evidence.yml`) reads it. That check fails until there
is a comment that

- was posted by someone with write access to the repository,
- is for the PR's **current head commit**, built from a clean tree,
- shows both suites passing, and
- reports big-test field and beam reductions within tolerance of the golden
  constants in that commit's own `test/testMPIIntegration3DBig.pf`.

Anything pushed to the PR afterwards changes the head commit, so the check
fails again until the tests are re-run. Make `hpc-evidence (slow + big tests)`
a required status check for `master` in the branch protection settings.

## Running it

On the HPC machine, from a clean checkout of the PR's head commit (for a
release PR that is the tip of `dev`):

```bash
git clone -b dev https://github.com/UKFELs/Puffin.git && cd Puffin
scripts/hpc-tests/run_hpc_tests.sh -n "Hartree Scafell Pike, job 12345" \
  -- -DCMAKE_PREFIX_PATH=$HOME/pfunit-install
```

It needs CMake ≥ 3.21, MPI, parallel HDF5, FFTW3-MPI, pFUnit and Python 3,
and must be able to start 6 MPI ranks. Arguments after `--` go to cmake; on a
machine that launches with `srun`, add `-DMPIEXEC_EXECUTABLE=$(which srun)`.
`example.slurm` wraps it as a batch job.

It writes `hpc-evidence-<sha>/`, holding the build and ctest logs, the JUnit
report, `evidence.json` and `comment.md`, and says whether the evidence will
pass. Then, from anywhere `gh` is logged in to GitHub - the login node, or
your own machine after copying that directory back:

```bash
scripts/hpc-tests/hpc_evidence.py submit hpc-evidence-<sha> --pr <N>
```

This refuses if the PR has moved on since the run, posts `comment.md` to the
PR, and re-runs the failed `hpc-evidence` check so it picks the comment up.
(Posting a comment is not an event the check listens for, so without the
re-run it would wait for the next push. "Re-run jobs" on the PR does the same.)

## What it does and does not prove

The check binds the result to an exact commit and to that commit's own
expectations, so a stale run, a run of a different branch, or a run where the
golden constants were edited locally is caught. It cannot tell a genuine
comment from a fabricated one: that rests on only trusting comments from
people with write access, who could merge the PR anyway.

If a run fails, or the PR changes, re-run and submit again. The newest comment
for the current head commit is the one that counts.
