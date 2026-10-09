#!/bin/bash
# Example SLURM job running the slow and big tests for a release PR.
# Adjust the partition, modules and pFUnit path for your machine, then
# submit from a clean checkout of the PR's head commit:
#
#   sbatch scripts/hpc-tests/example.slurm
#
# The big test needs 6 ranks and ~12 GB in total; the build needs a few cores.
#SBATCH --job-name=puffin-hpc-tests
#SBATCH --nodes=1
#SBATCH --ntasks=6
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --time=01:00:00
#SBATCH --output=puffin-hpc-tests-%j.out

# module load gcc openmpi hdf5-parallel fftw-mpi cmake python3

cd "$SLURM_SUBMIT_DIR"
scripts/hpc-tests/run_hpc_tests.sh -j "$SLURM_NTASKS" \
  -n "$(hostname -s), SLURM job $SLURM_JOB_ID" \
  -- -DCMAKE_PREFIX_PATH="$HOME/pfunit-install" \
     -DMPIEXEC_EXECUTABLE="$(command -v srun)"
