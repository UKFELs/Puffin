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

1. Fetch Puffin to /path/to/Puffin. To clone from Github, do

`git clone git@github.com:UKFELs/Puffin.git /path/to/Puffin`

where `/path/to/Puffin` is where you want the top level of the Puffin source to be downloaded to.

2. Make a new dir for the build:

`mkdir /path/to/puffin-install; cd /path/to/puffin-install`

3. Run cmake, e.g.

`cmake -DCMAKE_INSTALL_PREFIX:PATH=/path/to/puffin-install -DFftw3_ROOT_DIR='/path/to/fftw3' -DHdf5_ROOT_DIR='/path/to/hdf5' /path/to/Puffin`

4. Do `make && make install`. You should get a puffin binary in /path/to/puffin-install

## Building on Ubuntu 16.04

To build on Ubuntu, first install gfortran, openmpi, cmake, Git, and the parallel
hdf5 and fftw libraries from the repositories.

`sudo apt install gfortran libhdf5-mpi-dev libhdf5-openmpi-10 libhdf5-dev \
libfftw3-dev libfftw3-mpi-dev cmake libopenmpi-dev git`

For some reason, the hdf5 headers aren't automatically added to your path, so 
you need to manually add them so that CMake can find them:

`export PATH=/usr/include/hdf5/openmpi:$PATH`

Grab Puffin from Github:

`git clone git@github.com:UKFELs/Puffin.git path/to/Puffin`

`path/to/Puffin` in the above cammand is the location you want the files placed.

Then, run CMake to find the libraries and create the Makefile (note that
`path/to/desired/puffin/install` is the location you want the Puffin build 
installed to, and `path/to/puffin/` is the location of the directory
that you downloaded Puffin to, same as in the `git clone` command above):

`cmake -DCMAKE_INSTALL_PREFIX:PATH=path/to/desired/puffin/install -DENABLE_PARALLEL:BOOL=TRUE -DHdf5_MODULE_DIRS='/usr/include/hdf5/openmpi' -DHdf5_LIBRARY_DIRS='/usr/lib/x86_64-linux-gnu/hdf5/openmpi;/usr/lib/x86_64-linux-gnu/' -DHdf5_INCLUDE_DIRS='/usr/include/hdf5/openmpi' -DHdf5_LIBRARY_NAMES='hdf5_openmpi_fortran;hdf5_openmpi' -DHdf5_LIBRARIES='/usr/lib/x86_64-linux-gnu/libhdf5_openmpi_fortran.so;/usr/lib/x86_64-linux-gnu/libhdf5_openmpi.so' -DHdf5_STLIBS='/usr/lib/x86_64-linux-gnu/libhdf5_openmpi_fortran.a;/usr/lib/x86_64-linux-gnu/libhdf5_openmpi.a'  path/to/puffin/`

Then build:

`make`

`make install`

You'll find the Puffin executable in the subdirectory `bin` in the directory you
told CMake to install Puffin to (i.e. after the above cmake command the location
would be `path/to/desired/puffin/install/bin`).

## Building hdf5 and fftw3 (and more) using Bilder

Hdf5 and fftw3 parallel libraries may be cumbersome to build individually. The most 
popular linux distros have them in the official repositories. They can also be obtained 
and built by using bilder, which provides a stable base for building Puffin. This is
usually what is used for Puffin running and development on HPC clusters. Bilder
also allows one to build other packages needed for building and post-processing,
like CMake and Python packages, and so on. In the below, we build fftw3, hdf5,
and numpy and tables for Python.

First, check out bilder from the repository.

`svn co http://ice.txcorp.com/svnrepos/code/bilder/visitall/trunk bilder-visit`

Then go into the created bilder-visit directory, and to build the desired libararies, do

`env CMAKE_BLDRVERSION=3.4.1 env HDF5_BLDRVERSION=1.8.13 ./mkvisitall.sh -k ../contrib -i ../software -b ../builddir fftw3,cmake,autotools,hdf5,numpy,tables`

Note we are specifying a specific version of CMake here. The default version of
CMake installed using Bilder has a few known issues with SciMake, so until these
are fixed, we use the older version here. We are also grabbing a specific HDF5 build,
since unfortunately Bilder does not build the later versions correctly. This can
be fixed manually (see issues section below), but for now we recommend just 
using the earlier version with Bilder if you can do that.

The requested libraries will be in ../contrib, along with a bash script
to add them to your path. So do 

`source /path/to/contrib/visitall.sh`

and then you are ready to build Puffin as above. Note that you should link to 
the `fftw3-par` and `hdf5-par` libs built, and not the serial versions.


## Known Issues

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


  - Ubuntu 16.04 and Bilder

    An issue with building with Bilder on Ubuntu is that Bilder does not 
    currently recognise the machine hostname on Ubuntu. This may cause problems
    with the install process. We recommend doing

    `export FQHOSTNAME=$HOSTNAME`

    to make Bilder recognise the machine hostname.

    Another issue is with the OpenMPI/Fortran compilers compatibility with Bilder.
    The HDF5 libs built by Bilder do not work with the particular configuration on 
    Ubuntu 16.04. So we recommend using the HDF5 libs supplied in the Ubuntu
    repositories. They are called `libhdf5-openmpi-dev` - e.g. do 
    `sudo apt-get install libhdf5-openmpi-dev` to install them. For some reason
    the headers aren't automatically added to your path, so you need to do 

    `export PATH=/usr/include/hdf5/openmpi:$PATH`

    Then the CMake command to configure the Puffin Makefile becomes:

    `cmake -DCMAKE_INSTALL_PREFIX:PATH=/path/to/puffin-install -DCMAKE_BUILD_TYPE:STRING=Release -DCMAKE_COLOR_MAKEFILE:BOOL=TRUE -DCMAKE_VERBOSE_MAKEFILE:BOOL=TRUE -DENABLE_PARALLEL:BOOL=ON -DDEBUG_CMAKE:BOOL=TRUE -DFftw3_ROOT_DIR='/path/to/fftw3-par' -DHdf5_ROOT_DIR='/usr' -DHdf5_MODULE_DIRS='/usr/include/hdf5/openmpi' -DHdf5_LIBRARY_DIRS='/usr/lib/x86_64-linux-gnu/hdf5/openmpi;/usr/lib/x86_64-linux-gnu' -DHdf5_INCLUDE_DIRS='/usr/include/hdf5/openmpi' -DHdf5_LIBRARY_NAMES='hdf5_openmpi_fortran;hdf5_openmpi' -DHdf5_LIBRARIES='/usr/lib/x86_64-linux-gnu/libhdf5_openmpi_fortran.so;/usr/lib/x86_64-linux-gnu/libhdf5_openmpi.so' -DHdf5_STLIBS='/usr/lib/x86_64-linux-gnu/libhdf5_openmpi_fortran.a;/usr/lib/x86_64-linux-gnu/libhdf5_openmpi.a' /path/to/Puffin`