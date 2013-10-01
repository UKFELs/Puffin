==================== Puffin v1.4.0 ======================

A program for solving an unaveraged 3D FEL system. This 
code requires FTW_2.5.1 (MPI).

Puffin (Parallel Unaveraged Fel INtegrator) is described in:-

LT Campbell and BWJ McNeil, Physics of Plasmas 19, 093119 (2012)

The code has been improved since then, and no longer uses an
external linear solver, removing many of the dependencies.
The only external package now required is FFTW.

In the sub-directories you will find:

  source/  -  This contains the main code.
  compile/ -  Some example compilation and linking scripts.
  submit/  -  An example subission script used on Archie-WEST.  
  inputs/  -  Some example input files.

Bear in mind you must load FFTW v2.1.5 and MPI compilers into
your environment before compiling. e.g. on Archie, do

module load compilers/gcc/4.6.2
module load mpi/gcc/openmpi/1.6
module load /libs/gcc/fftw2/double-mpi/2.1.5

You may wish to put these commands into your .bashrc script (or
equivalent) to save typing this every time you log in.

The job submission script lanches a job on 1 node, using 12 
processes. The line

#$ -pe mpi-verbose 1

specifies the number of nodes. There are 12 processors per node,
so we use 12 MPI processes per node here. However, you can use
less than this if you desire. You can technically use more 
MPI processes than physical processors, but this is probably
not a good idea. The MPI job is launched with 

mpirun -np 12 ~/bin/puffin1.4 hidding.in

where the 2nd last argument points to the executable, and the
last argument is the input file for Puffin. The option -np 
specifies the number of MPI processes.

The input files consist of a main parameter file, a beam file,
and a seed file. The beam and seed files enable you to input
multiple electron beams with different energies and 
distributions, and multiple seeds with different frequencies
and different distributions. The parameter file specifies
the name of the seed and beam files to be used.

For a 1D run it is unlikely you will need more than a node
to improve the computation time. A full 3D run will
require many nodes, and plenty of memory. You may wish, on a
larger machine, to use 1 MPI process per node, and lots of 
nodes, to maximize the RAM for each process.


 -Dr Lawrence Campbell                    17th September 2013
  University of Strathclyde, Glasgow
  lawrence.campbell@strath.ac.uk

=========================HISTORY==========================

Puffin is the result of work performed by different 
individuals at the University of Strathclyde over the period
2005-2013. In order to give proper credit, a brief history
now follows.

An early version of Puffin was originally developed by Dr 
Cynthia Nam, Dr Pamela Johnston and Dr Brian McNeil. This 
version is reported in:-

Unaveraged Three-Dimensional Modelling of the FEL, Proceedings 
of the 30th International Free Electron Laser Conference, 
Gyeongju, Korea (2008)

It was redeveloped by myself a further two times to try
different algortithms to improve the parallelism with MPI.
The different iterations have been reported in the proceedings
of FEL conferences:-

L.T. Campbell, R. Martin and B.W.J. McNeil, A Fully Unaveraged, 
Non-localised, Parallelized Computational Model of the FEL, 
Proceedings of the 31st International Free Electron Laser 
Conference, Liverpool, United Kingdom (2009)

L.T. Campbell and B.W.J. McNeil, An Unaveraged Computational 
Model of a Variably Polarized Undulator FEL, Proceedings of
the 32nd International Free Electron Laser Conference, Malmo, 
Sweden (2010)

L.T. Campbell and B.W.J. McNeil, Generation of Variable 
Polarization in a Short Wavelength FEL Amplifier, Proceedings
of the 32nd International Free Electron Laser Conference, 
Malmo, Sweden (2010)

Final working equations and algorithm was reported in:

LT Campbell and BWJ McNeil, Physics of Plasmas 19, 093119 (2012)

 -Dr Lawrence Campbell                    30th November 2012
  University of Strathclyde, Glasgow
  lawrence.campbell@strath.ac.uk

=============================================================
