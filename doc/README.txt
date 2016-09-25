The sub-directories include:


  doc/       -  Some documentation - see 'puffin_notes.pdf'

  inputs/    -  Some example input files.

  scripts/   -  Example job submission scripts

  source/    -  This contains the main source code written in C 
                and Fortran 90.
    
  utilities/ -  Python scripts to manipulate output files


Basic operation involves 3 input files. The three files are:

  1) The main input file, describing free parameters and the 
     integration sampling

  2) The beam input file, which can describe multiple electron 
     beams if necessary

  3) The seed input file, which can describe multiple radiation 
     seeds if necessary

The main input file points to the beam and seed file to be used.
This main input file is then passed to Puffin at run time.
This approach is (hopefully) quite flexible, allowing Puffin to 
generate a simple, more conventional FEL, or a more esoteric 
configuration with multiple seeds and electron beams, with different 
powers, frequencies, energies, and distributions.

The input parameters are in terms of the scaled, dimensionless 
variables. The system has been updated from the original 2012
physics of plasmas reference and is described in the distributed
file: Scaling.pdf

A python script, 'write_df.py' is located in the examples/gen
directory which can generate the input files from commonly used
SI experimental parameters.

In addition, for more fun, a lattice file may be constructed to 
describe a series of undulator modules seperated by chicane/slippage 
sections. The name of this lattice file should be entered in the 
'lattfile' variable in the input file. 2 or 3 example lattice 
files can be found at inputs/example/simple/1D/lattice/ - the
examples simulate a 2 colour scheme. The file header contains 
the number of undulator modules. Each line of the lattice file 
contains 4 numbers describing a complete undulator-chicane pair, 
in the following format:

Nw   Nc  aw_fact   nperlam  taper

where 

Nw            is the number of undulator periods

Nc            is the length of the chicane slippage section
              expressed as the number of resonant wavelengths

aw_f          is the undulator parameter expressed as a fraction
              of the aw in the input file. So if aw_0 is the aw
              given in the input file, then the undulator 
              parameter for this module is aw = aw_f * aw_0.

nperlam       is the number of integration steps per undulator 
              period to be used in this undulator section. The 
              mesh describing the radiation field does not alter
              as the undulator tuning changes, but a larger aw 
              will cause the beam to propagate more quickly in 
              the radiation frame i.e. the beam slips more quickly
              behind the radiation field. This option will be 
              necessary to stop the beam skipping over nodes, 
              see e.g. the Courant number.

taper         d/dz of aw_f across this undulator module. This is 
              a single number - i.e. only linear tapers may be
              specified.


Also note that if aw is decreased in an undulator module you should
ensure the radiation mesh is fine enough to model the higher frequency
content which will arise.

The strength of the dispersive chicanes can be given in the 'Dfact' 
variable in the main input file. Currently, only this one strength 
factor may be specified which will be shared by all chicanes.
 

COMPILATION ON HPC MACHINES
===========================

FFTW v2.1.5 and MPI compilers must be loaded into
your environment before compiling. e.g. on Archie, do

module load compilers/gcc/4.6.2
module load mpi/gcc/openmpi/1.6
module load /libs/gcc/fftw2/double-mpi/2.1.5

By default, Puffin will create SDDS[1] files, which may be
postprocessed using the SDDS toolkit from Argonne National Lab.
If you have the hdf5[2] library available, Puffin can be built to 
produce hdf5 output, which aids visualisation using VisIt[3], 
paraview, matplotlib, MATLAB (R) or other tools.

You may wish to put these commands to set your environment into 
your .bashrc script (or equivalent) to save typing this every time
you log in.

If you do not have these built, you may either contact your
system administrator or build these yourself using the bilder[4]
package management system.

To build the software itself you can use cmake, or the scripts
provided.

The example job submission script lanches a job on 1 node, using 12 
processes. The line

#$ -pe mpi-verbose 1

specifies the number of nodes. There are 12 processors per node,
so we use 12 MPI processes per node here. However, you can use
less than this if you desire. You can technically use more 
MPI processes than physical processors, but this is probably
not a good idea. The MPI job is launched with 

mpirun -np 12 ~/bin/puffin1.4 inputfile.in

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

=========================HISTORY==========================

Puffin is the result of work performed by a group of people 
at the University of Strathclyde over the period
2005-2016. A brief history is as follows:

The code had its origins in a 1D version briefly described in:
BWJ McNeil, GRM Robb and D Jaroszynski,
‘Self Amplification of Coherent Spontaneous Emission in the Free Electron Laser',
Optics Comm., 165, p 65, 1999

The first 3D version of the code was originally developed by 
Dr Cynthia Nam, Dr Pamela Johnston and Dr Brian McNeil and was 
reported in:
Unaveraged Three-Dimensional Modelling of the FEL, Proceedings 
of the 30th International Free Electron Laser Conference, 
Gyeongju, Korea (2008)

It was significantly redeveloped by Lawrence Campbell (et al) a further two times
improving the algortithms (Fourier method back to Finite Element) and the 
parallelism with MPI. This development has been reported in the 
following proceedings of FEL conferences:

L.T. Campbell, R. Martin and B.W.J. McNeil, 
'A Fully Unaveraged, Non-localised, Parallelized Computational Model of the FEL', 
Proceedings of the 31st International Free Electron Laser Conference, 
Liverpool, United Kingdom (2009)

L.T. Campbell and B.W.J. McNeil, 
'An Unaveraged Computational Model of a Variably Polarized Undulator FEL', 
Proceedings of the 32nd International Free Electron Laser Conference, 
Malmo, Sweden (2010)

L.T. Campbell and B.W.J. McNeil, 
'Generation of Variable Polarization in a Short Wavelength FEL Amplifier', 
Proceedings of the 32nd International Free Electron Laser Conference, 
Malmo, Sweden (2010)

The final working equations and scaling were reported in:
LT Campbell and BWJ McNeil, 
'Puffin: A three dimensional, unaveraged free electron laser simulation code'
Physics of Plasmas 19, 093119 (2012)

The code algorithm has been improved since and no longer needs an
external linear solver for the coupled electron-radiation field equations.
The only external package now required by Puffin is FFTW.

=============================================================
Contact Information:
Dr Lawrence Campbell: lawrence.campbell@strath.ac.uk
Dr Brian McNeil:    b.w.j.mcneil@strath.ac.uk

[1] http://www.aps.anl.gov/Accelerator_Systems_Division/Accelerator_Operations_Physics/SDDSInfo.shtml
[2] https://www.hdfgroup.org/HDF5/
[3] https://visit.llnl.gov/
[4] https://ice.txcorp.com:3000/projects/bilder/wiki
