#!/bin/bash

# test-puf.sh
# Runs test cases for Puffin.
# Sets up directories, copies in test files, 
# and runs Puffin for each case.


PUFFIN=./puffin

mkdir shsq
mkdir longsq

cp ../inputs/example/simple/1D/OptCommV165pp65-70/fig1/* ./shsq/
cp ../inputs/example/simple/1D/OptCommV165pp65-70/fig2/* ./longsq/

cd shsq/

mpirun -np 2 $PUFFIN fig1.in

cd ../longsq/

mpirun -np 2 $PUFFIN fig2.in