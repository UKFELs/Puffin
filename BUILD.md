# Build Instructions

Puffin is written mainly in modern fortran and one C file, and the 
post-processing scripts are in Python, using pytables and numpy. Puffin 
can be built using CMake, and uses SciMake (bundled with Puffin) to find
and link the external libraries.

The below guide is for use on linux using the bash terminal. Installation 
on Windows or OS X is not currently supported.

## To build Puffin

To build Puffin you will need parallel hdf5 and fftw3 libs already installed. 
If you're installing on a system with different compilers, you'll need to 
make sure these libraries are built with the same build chain as you're going
to use for Puffin. If you wish, fftw3 and hdf5 may be obtained and built 
using Bilder (see bottom of this guide). 

1. Fetch Puffin to /path/to/Puffin

2. Make a new dir for the build:

`mkdir /path/to/puffin-install; cd /path/to/puffin-install`

3. Run cmake, e.g.

`cmake -DCMAKE_INSTALL_PREFIX:PATH=/path/to/puffin-install -DFftw3_ROOT_DIR='/path/to/fftw3 -DHdf5_ROOT_DIR='/path/to/hdf5' /path/to/Puffin`

4. Do `make && make install`. You should get a puffin binary in /path/to/puffin-install

## Building hdf5 and fftw3 using bilder

Hdf5 and fftw3 parallel libraries may be combersome to build individually. The most 
popular linux distros have them in the official repositories. They can also be obtained 
and built by using bilder, which provides a stable base for building Puffin. This is
usually what is used for Puffin development.  

First, check out bilder from the repository.

`svn co http://ice.txcorp.com/svnrepos/code/bilder/visitall/trunk bilder-visit`

Then go into the created bilder-visit directory, and to build the desired libararies, do

`./mkvisitall.sh -k ../contrib -i ../install -b ../build fftw,cmake,autotools,hdf5,numpy,tables`

Here, we are also building numpy and tables for the Python post-processing scripts.
The requested libraries will be in ../contrib, along with a bash script
to add them to your path. So do 

`source /path/to/contrib/visitall.sh`

and then you are ready to build Puffin as above. Note that you should link to 
the `fftw3-par` and `hdf5-par` libs built, and not the serial versions.


# Known Issues

  - If you are using a system with mutiple compilers (e.g. HPC systems usually 
    have a variety of Fortran and C compilers to choose), then it is usually a 
    good idea to make sure environment variables are set up to tell SciMake and 
    Bilder which ones to use. These can be explicitly set using CC, F90, etc.
    So for example, the following example specifies the intel compilers and
    paths to the OpenMPI wrappers on that system:
    
    `export CC=icc; export CXX=icpc; export F90=ifort; export F77=ifort; export FC=ifort'

    'export MPICC="/opt/intel-openmpi/1.8.1/bin/mpicc"; export MPICXX="/opt/intel-openmpi/1.8.1/bin/mpicxx"; export MPIFC="/opt/intel-openmpi/1.8.1/bin/mpif90"; export MPIF77="/opt/intel-openmpi/1.8.1/bin/mpif77" `
  
  - The most recent versions of HDF5 built by bilder have 2 module files installed 
    to a different directory than expected. They are built, however, in the hdf5 
    sub-directory of the `build` sub-directory specified in the `./mkvisitall.sh`
    command above, and just need copied over to the correct place. Do
    
    `cp /path/to/build/hdf5-1.8.13/par/bin/h5f_provisional.mod /path/to/contrib/hdf5-par/include/`

    `cp /path/to/build/hdf5-1.8.13/par/bin/h5fdmpio.mod /path/to/contrib/hdf5-par/include/`

    Replace the version number in the above with that from your own install, 
    which may be more recent.




