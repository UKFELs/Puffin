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

## Features

Puffin is a so-called 'unaveraged' FEL code - meaning it is absent of the
slowly varying envelope approximation (SVEA) and wiggler period averaging
approximations. It does not utilize a 'slicing' model of the beam phase space
and radiation field, and instead utilizes an algorithm which is much more
similar to a Particle-In-Cell (PIC) code methodology.

In addition, some accelerator componenets are included, and together with the
lack of restrictions, means it may model:
  - The full, broad bandwidth frequency spectrum, limited only by the Niquist frequency of the mesh
  - Full electron beam transport
  - Transport of large energy spread beams through the undulator, and the radiation emitted from these beams
  - Tapered undulators
  - Fully 3D undulators, including modelling of the wiggler entries/exits and natural focusing
  - Interleaved undulator-chicane lattices
  - Variably polarized undulators
  - Undulator tuning of each module

It presently does not include the effects of space charge, and ignores emission
of the backwards wave from the e-beam.


## Release Notes

1.5.1:
  - Added use of inner mesh to the parallel model to reduce communication between MPI nodes.
  - Initial python script to auto-gen plots using Visit from HDF5 files.

1.5.0
  - Re-wrote parallel algorithm to distribute the field mesh amongst MPI nodes.
