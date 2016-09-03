# Puffin

Puffin (Parallel Unaveraged Fel INtegrator) simulates a Free Electron 
Laser (FEL). Puffin is a massively parallel numerical solver for an 
unaveraged, 3D FEL system of equations, and is written mostly in 
Fortran 90, using MPI and OpenMP.

The initial publication describing the first version of the code is:-

LT Campbell and BWJ McNeil, Physics of Plasmas 19, 093119 (2012)

The code has undergone many improvements and extended its functionality 
since then. It no longer uses an external linear solver package, and the
only external package now required is FFTW v2.1.5.



## Release Notes

1.5.1:
  - Added use of inner mesh to the parallel model to reduce communication between MPI nodes.
  