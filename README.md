# Puffin

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Build Status master](https://img.shields.io/travis/com/UKFELs/Puffin/master.svg?label=master)](https://travis-ci.com/UKFELs/Puffin/branches)
[![Build Status dev](https://img.shields.io/travis/com/UKFELs/Puffin/dev.svg?label=dev)](https://travis-ci.com/UKFELs/Puffin/branches)

Puffin (Parallel Unaveraged Fel INtegrator) simulates a Free Electron Laser (FEL). Puffin is a massively parallel numerical solver for an unaveraged, 3D FEL system of equations, and is written in Fortran 90, using MPI and OpenMP.

The initial publication describing the first version of the code is:-

[LT Campbell and BWJ McNeil, Physics of Plasmas 19, 093119 (2012)](http://aip.scitation.org/doi/10.1063/1.4752743)

The code has undergone many improvements and extended its functionality
since then. It no longer uses an external linear solver package, and the
only external packages now required are FFTW (version 3.3 onwards), and
parallel HDF5 libraries. A more recent description can be found [here](http://ipac2018.vrws.de/papers/thpmk112.pdf).

Documentation is being developed [here](https://ukfels.github.io/puffinDocs/).

----
## Features

Puffin is a so-called 'unaveraged' FEL code - meaning it is absent of the
slowly varying envelope approximation (SVEA) and wiggler period averaging
approximations. It does not utilize a 'slicing' model of the beam phase space
and radiation field, and instead utilizes an algorithm which is much more
similar to a Particle-In-Cell (PIC) code methodology.

In addition, some accelerator components are included for simulation of the
'realistic' undulator line, and together with the lack of restrictions,
means it may model:
  - The full, broad bandwidth frequency spectrum, limited only by the Nyquist frequency of the mesh
  - Full electron beam transport
  - Transport of large energy spread beams through the undulator, and the radiation emitted from these beams
  - Tapered undulators
  - Fully 3D undulators, including modelling of the wiggler entries/exits and natural focusing
  - Interleaved undulator-chicane lattices
  - Variably polarized undulators
  - Tuning of each undulator module

It presently does not include the effects of space charge, and ignores emission
of the backwards wave from the e-beam.

----
## Docker Images

[![Docker Hub](http://dockeri.co/image/ukfels/puffin-user)](https://hub.docker.com/r/ukfels/puffin-user)

Docker images can be fetched from [here](https://hub.docker.com/u/ukfels/).
There are currently two - a 'full' container intended for development purposes, 
which has all tests built, along with the testing infrastructure (pFUnit) and
developer and user documentation, etc. The other, the 'user' container, is
run like an executable. You pass it the number of processors you want to use,
and the name of the input file in the current directory to run.

The images are not configured for or intended for use on a cluster; rather, they allow you to get Puffin up and running quickly for smaller, 1D or few-slice runs on a local machine (e.g. your laptop or desktop).

To grab an image, do e.g.
```
docker pull ukfels/puffin-user
```
then, for the user image,
```
docker run -v $(pwd):/home/puffin_user/project ukfels/puffin-user 2 main.in
```
will run Puffin using 2 processes with the main input file in the current directory.

[![Docker Hub](http://dockeri.co/image/ukfels/puffin-dev)](https://hub.docker.com/r/ukfels/puffin-dev)

For the 'full' container, for using interactively, grab the `puffin-dev` image, like so:
```
docker pull ukfels/puffin-dev
```
Start the container, whilst mounting the current directory to the project area in the container, with
```
docker run -it -v $(pwd):/home/puffin_user/project dev/puffin-dev
```
You'll find Puffin built in the `/home/puffin_user/built/puffin` directory, with the executable `/home/puffin_user/built/puffin/bin/puffin`. To run, do 

```
mpirun -np 2 /home/puffin_user/built/puffin/bin/puffin main.in
```
assuming the main input file `main.in` is in your current directory.

----

## How to run

Puffin requires at least 2 input files to run. The 2 required files are:
- Main input file: This contains info like the base FEL parameters (undulator parameter, mean beam energy, and so on) which sets up the scaled frame to be used in the simulation, the data writing frequency, and various simualtion options and switches. It also contains the names of the other input files, of which, the beam file **must** be specified.
- Beam file: This describes the electron beam, and can take 3 different forms: a *simple* beam file, which describes a simple homogeneous beam; a *dist* format, which describes a more complex distribution which varies with the temporal coordinate, which is homogeneous in every dimension but the temporal; a *particle* format, which in hdf5 format, and is a direct description of the macroparticles in the Puffin output format.

The other, optional input files are:
 - Radiation seed file: This describes an externally injected radiation seed. This is described only as a simple homogeneous gaussian or flat top intensity distribution in each of the 3 spatial dimensions.
 - Lattice file: The main input file can describe a simple, single, long undulator, with a linear taper if required. On the other hand, a lattice file can be supplied to model several undulator modules with variable tuning and tapering, with free-space drifts between them, along with a variety of beam optical elements such as chicanes or focusing quadrupoles.

Several example input decks are included in the `inputs/simple` directory in this repo.

To run Puffin, pass it the name of the main input file as an argument. In the `inputs/simple/3D/CLARA/single-slice` directory, you'll find an example deck which models the UK test FEL CLARA in 'periodic' mode, which is relatively quick to run. Puffin is an MPI code, so when compiled, you'll need to run with `mpirun`, specifying the number of MPI processes with the `-np` flag. After Puffin is built, with all files in the current (working) directory, do
```
mpirun -np 2 puffin clara.in
```
to run the CLARA example on 2 MPI processes.

Alternatively, if using the user Docker image, use the invocation described in the `Docker` section in this document *i.e.* after downloading the files in the CLARA deck to the working directory, do
```
docker run -v $(pwd):/home/puffin_user/project mightylorenzo/puffin-user 2 clara.in
```
to start the container, running Puffin on 2 MPI processes (it will download the `puffin-user` image if not already explicitly downloaded with `docker pull`). The `-v` flag mounts the working directory to the `project` area in the container. The output files will be left intact in the (hosts) working directory when the container finishes running Puffin.

----

## Release Notes

1.9.0
  - Added periodic mesh mode
  - 1D runs now read in the transverse beam radii to give proper SI power

1.8.0
  - Can now input with the Puffin HDF5 output format
  - More metadata in the output files
  - Can now resume from previous runs using HDF5 Puffin dumps
  - Twiss parameters can be used with the simple beam input
  - Communication time reduced in reorganisation stage of the parallel field algorithm
  - Using doxygen for dev documentation
  - The example auto-plotting python scripts for Visit have been refactored and can be called separately
  - Added python script (powPrep.py) to collect the power data together
  - Better point transforms for quads, now including energy dependence
  - Fixed bug of extra half-period diffraction per undulator module
  - HDF5 output is now the default data output format
  - Can now specify specific writing steps for data dumps using a 'write file'
  - Can now choose to use estimated or measured FFTW plans from input file

1.6.0:
  - Revamped the form of the lattice file, both to include more components, and to
    enhance the flexibility. Multiple undulator types with different polarizations,
    tunings, and tapers can now be used in the one run. Quads, drifts, and better
    chicane modelling (with proper diffraction of the radiation field) are now
    included.
  - Now uses FFTW v3.3 onwards. FFTW 2.1.5 support removed.
  - Supplying the radiation seed file is now optional.

1.5.1:
  - Added use of inner mesh to the parallel model to reduce communication between MPI nodes.
  - Initial python script to auto-gen plots using Visit from HDF5 files.

1.5.0
  - Re-wrote parallel algorithm to distribute the field mesh amongst MPI nodes.
