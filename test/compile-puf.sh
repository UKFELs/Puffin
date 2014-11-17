#!/bin/bash

# compile-puf.sh
# Compiles Puffin on Linux Mint install.

PUFDIR=~/src/puffin-github/source

mpicc -DONE_UNDERSCORE -c $PUFDIR/*.c -lfftw_mpi -lfftw -lmpi -lm

mpif90 -c $PUFDIR/*.f90 -lfftw_mpi -lfftw -lmpi -lm
mpif90 -c $PUFDIR/*.f90 -lfftw_mpi -lfftw -lmpi -lm
mpif90 -c $PUFDIR/*.f90 -lfftw_mpi -lfftw -lmpi -lm
mpif90 -c $PUFDIR/*.f90 -lfftw_mpi -lfftw -lmpi -lm

mpif90 -o puffin *.o -lfftw_mpi -lfftw -lmpi -lm

