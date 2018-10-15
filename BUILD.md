# How to Install

The source code is hosted [here](https://github.com/UKFELs/Puffin), along with binaries for Ubuntu 16.04 in the releases section. Docker container images are now being hosted [here](https://hub.docker.com/u/mightylorenzo/).

## Building from source

Puffin is written in modern fortran, and the post-processing scripts are in Python, using pytables and numpy. Puffin can be built using CMake, and uses SciMake (bundled with Puffin) to find and link the external libraries.

The below guide is for use on linux using the bash terminal. Installation on native Windows or OS X is not currently supported. Installation on OS X with MacPorts using the below general instructions has been achieved, however, and similarly with Cygwin on Windows 7. The Ubuntu instructions below have been successfully run on Ubuntu for Windows on Windows 10. As noted above, Docker images for running containers are now available [here](https://hub.docker.com/u/mightylorenzo/), which are the best way to get quickly up and running on a local machine with Windows or Mac OS X.

Specific instructions for building on Jureca are [here](BUILD-jureca.md).

### General Instructions

To build Puffin you will need parallel hdf5 and fftw3 libs already installed. If you're installing on a system with different compilers, you'll need to make sure these libraries are built with the same build chain as you're going to use for Puffin. If you wish, fftw3 and hdf5 may be obtained and built using Bilder (see later in this guide).

1. Fetch Puffin to /path/to/Puffin. To clone from Github, do

    ```
    git clone git@github.com:UKFELs/Puffin.git /path/to/Puffin
    ```

    where `/path/to/Puffin` is where you want the top level of the Puffin source to be downloaded to.

2. Make a new dir for the build:

    ```
    mkdir /path/to/puffin-install; cd /path/to/puffin-install
    ```

3. Run cmake, e.g.

    ```
    cmake -DENABLE_PARALLEL:BOOL=TRUE -DCMAKE_INSTALL_PREFIX:PATH=/path/to/puffin-install -DFftw3_ROOT_DIR='/path/to/built/fftw3' -DHdf5_ROOT_DIR='/path/to/built/hdf5' /path/to/Puffin
    ```
    
    To specify compilers, you can pass them as environment varables in the above command *e.g.*

    ```
    env MPI_CXX=mpicxx CC=mpicc CXX=mpicxx F90=mpif90 F77=mpif90 FC=mpif90 cmake -DENABLE_PARALLEL:BOOL=TRUE -DCMAKE_INSTALL_PREFIX:PATH=/path/to/puffin-install -DFftw3_ROOT_DIR='/path/to/built/fftw3' -DHdf5_ROOT_DIR='/path/to/built/hdf5' /path/to/Puffin
    ```
    
    See the *Common Issues* section below to see an alternative (possibly better) way of specifying them.
    
4. Do `make && make install`. You should get a puffin binary in /path/to/puffin-install

#### Common issues

1. Sometimes CMake/SciMake does not pick up the environment variables which set which compilers to use. These can be explicitly set by doing:

    ```
    cmake -DMPI_C_COMPILER=$MPICC -DMPI_CXX_COMPILER=$MPICXX -DMPI_FORTRAN_COMPILER=$MPIFC -DENABLE_PARALLEL:BOOL=TRUE -DCMAKE_INSTALL_PREFIX:PATH=/path/to/puffin-install -DFftw3_ROOT_DIR='/path/to/fftw3' -DHdf5_ROOT_DIR='/path/to/hdf5' /path/to/Puffin
    ```
    where `$MPICC` *etc* should be pointing to your MPI C/Fortran compilers.
    
2. Depending on your setup, the OpenMP flags may be picked up by SciMake and added to the compile flags, but not the link flags. When `make`ing, this results in successful individual compilation of modules, but it will fail at the link stage complaining about not being able to recognise OpenMP commands (which will have the form \*omp\* *e.g.* the name of one is omp_get_num_threads). To fix this, you need to modify the file `/where/you/ran/cmake/source/CMakeFiles/puffin.dir/link.txt`, and add the OpenMP flag for your compiler, usually `-fopenmp` for gfortran, next to the `-O3` flag. Then do `make` again (then `make install`).

3. If you have built hdf5 yourself (see section on building hdf5 and fftw below), then you may find that everything compiles and links, but when you run it says that it can't find the shared hdf5 libraries, usually called something like `libhdf5_fortran.so.10`. You'll need to add the built hdf5 libs to your LD_LIBRARY_PATH (which should be located in the `lib` subdirectory of the top-level of the built hdf5). So do 
    ```
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/shared/hdf5/lib/directory/
    ```



### Building on Ubuntu 16.04

To build on Ubuntu, first install gfortran, openmpi, cmake, Git, and the parallel hdf5 and fftw libraries from the repositories.

```
sudo apt install gfortran libhdf5-mpi-dev libhdf5-openmpi-10 libhdf5-dev libfftw3-dev libfftw3-mpi-dev cmake libopenmpi-dev git
```

For some reason, the hdf5 headers aren't automatically added to your path, so you need to manually add them so that CMake can find them:

```
export PATH=/usr/include/hdf5/openmpi:$PATH
```

Grab Puffin from Github (`path/to/Puffin` in the below command is the location you want the source files placed):

```
git clone git@github.com:UKFELs/Puffin.git path/to/Puffin
```

Then, run CMake to find the libraries and create the Makefile (note that `path/to/desired/puffin/install` is the location you want the Puffin build installed to, and `path/to/puffin/` is the location of the directory that you downloaded the Puffin source to, same as in the `git clone` command above):

```
cmake -DCMAKE_INSTALL_PREFIX:PATH=path/to/desired/puffin/install -DENABLE_PARALLEL:BOOL=TRUE -DHdf5_MODULE_DIRS='/usr/include/hdf5/openmpi' -DHdf5_LIBRARY_DIRS='/usr/lib/x86_64-linux-gnu/hdf5/openmpi;/usr/lib/x86_64-linux-gnu/' -DHdf5_INCLUDE_DIRS='/usr/include/hdf5/openmpi' -DHdf5_LIBRARY_NAMES='hdf5_openmpi_fortran;hdf5_openmpi' -DHdf5_LIBRARIES='/usr/lib/x86_64-linux-gnu/libhdf5_openmpi_fortran.so;/usr/lib/x86_64-linux-gnu/libhdf5_openmpi.so' -DHdf5_STLIBS='/usr/lib/x86_64-linux-gnu/libhdf5_openmpi_fortran.a;/usr/lib/x86_64-linux-gnu/libhdf5_openmpi.a'  path/to/puffin/
```

Then build:

```
make
make install
```

You'll find the Puffin executable in the subdirectory `bin` in the directory you told CMake to install Puffin to (i.e. after the above cmake command the location would be `path/to/desired/puffin/install/bin`).

## Building HDF5 and FFTW3

### Building hdf5 and fftw3 manually

In the simplest form, hdf5 can be built by first downloading and unzipping the source from e.g. [here](https://www.hdfgroup.org/downloads/hdf5/source-code/). Then, going into the unzipped hdf5 source directory, one can do:

```
./configure --enable-parallel --enable-fortran --prefix=/home/users/username/bin/hdf5/build
make
make install
```

where the destination specified by `prefix` is the destination the built libraries and header files will be installed to. 

Similarly for fftw3, the libraries can be built after extracting the source and moving into the top-level source directory by doing 

```
./configure --prefix=/home/users/username/bin/fftw3/build --enable-mpi --enable-openmp
make
make install
```

where, again, the `prefix` specifies where the built library will be placed.

Often, it is required to specify the compilers to be used by the packages. For example, it is common on HPC machines that there will be multiple compilers, which can be loaded into your working environment with use of a `module` command. If the following envirnment variables are not already configured by your system (*e.g.* after doing a `module load` to load the compilers on your HPC machine) (try `echo $MPICC` to print to screen to see if/how `MPICC` is specified), then specifying the environment variables by hand to point to the compilers is sometimes needed:

```
export CC=mpicc
export CXX=mpicxx
export F90=mpif90
export F77=mpif90
export F9X=mpif90
export FC=mpif90
export MPICC=mpicc
export MPICXX=mpicxx
export MPIF90=mpif90
export MPI_CXX=mpicxx
```

Sometimes the parallel C compiler is `mpiicc` rather than `mpicc`, and the parallel Fortran compiler is `mpiifort` - please check this yourself. Sometimes you may need to give the full path to the compiler, depending on how your system is setup! *e.g.* you may need:

```
export CC=/path/to/mpiicc
```
*etc.*

Environment variables can also be specified in the `configure` call in each case. So, for hdf5, do:

```
CC=/path/to/mpicc FC=/path/to/mpif90 F90=/path/to/mpif90 F77=/path/to/mpif90 ./configure --enable-parallel --enable-fortran --prefix=/home/users/username/bin/hdf5/build
```

and for fftw3, do *e.g.*

```
./configure --prefix=/home/users/username/bin/fftw3/build MPICC=mpiicc --enable-mpi --enable-openmp
```

will tell the configure routines to use the specified compilers.


### Building hdf5 and fftw3 (and more) using Bilder

Hdf5 and fftw3 parallel libraries may be cumbersome to build individually. The most popular linux distros have them in the official repositories. They can also be obtained and built by using Bilder, which provides a stable base for building Puffin. This is usually what is used for Puffin running and development on HPC clusters. Bilder also allows one to build other packages needed for building and post-processing, like CMake and Python packages, and so on. In the below, we build fftw3, hdf5, and numpy and tables for Python.

First, check out bilder from the repository.

```
svn co http://ice.txcorp.com/svnrepos/code/bilder/visitall/trunk bilder-visit
```

Then go into the created bilder-visit directory, and to build the desired libararies, do

```
env CMAKE_BLDRVERSION=3.4.1 env HDF5_BLDRVERSION=1.8.13 ./mkvisitall.sh -k ../contrib -i ../software -b ../builddir fftw3,cmake,autotools,hdf5,numpy,tables
```

Note we are specifying a specific version of CMake here. The default version of CMake installed using Bilder has a few known issues with SciMake, so until these are fixed, we use the older version here. We are also grabbing a specific HDF5 build, since unfortunately Bilder does not build the later versions correctly. This can be fixed manually (see issues section below), but for now we recommend just using the earlier version with Bilder if you can do that.

The requested libraries will be in ../contrib, along with a bash script to add them to your path. So do 

```
source /path/to/contrib/visitall.sh
```

and then you are ready to build Puffin as above. Note that you should link to the `fftw3-par` and `hdf5-par` libs built, and not the serial versions.


### Known Issues

  - If you are using a system with mutiple compilers (e.g. HPC systems usually have a variety of Fortran and C compilers to choose), then it is usually a good idea to make sure environment variables are set up to tell SciMake and Bilder which ones to use. These can be explicitly set using CC, F90, etc. So for example, the following example specifies the intel compilers and paths to the OpenMPI wrappers on that system:
    
    ```
    export CC=icc; export CXX=icpc; export F90=ifort; export F77=ifort; export FC=ifort

    export MPICC="/opt/intel-openmpi/1.8.1/bin/mpicc"; export MPICXX="/opt/intel-openmpi/1.8.1/bin/mpicxx"; export MPIFC="/opt/intel-openmpi/1.8.1/bin/mpif90"; export MPIF77="/opt/intel-openmpi/1.8.1/bin/mpif77"
    ```
  
  - The most recent versions of HDF5 built by bilder have 2 module files installed to a different directory than expected. They are built, however, in the hdf5 sub-directory of the `build` sub-directory specified in the `./mkvisitall.sh` command above, and just need copied over to the correct place. Do

    ```    
    cp /path/to/build/hdf5-1.8.13/par/bin/h5f_provisional.mod /path/to/contrib/hdf5-par/include/

    cp /path/to/build/hdf5-1.8.13/par/bin/h5fdmpio.mod /path/to/contrib/hdf5-par/include/
    ```

    Replace the version number in the above with that from your own install, which may be more recent.


  - Ubuntu 16.04 and Bilder

    An issue with building with Bilder on Ubuntu is that Bilder does not currently recognise the machine hostname on Ubuntu. This may cause problems with the install process. We recommend doing

    ```
    export FQHOSTNAME=$HOSTNAME
    ```

    to make Bilder recognise the machine hostname.

    Another issue is with the OpenMPI/Fortran compilers compatibility with Bilder. The HDF5 libs built by Bilder do not work with the particular configuration on Ubuntu 16.04. So we recommend using the HDF5 libs supplied in the Ubuntu repositories. They are called `libhdf5-openmpi-dev` - e.g. do `sudo apt-get install libhdf5-openmpi-dev` to install them. For some reason the headers aren't automatically added to your path, so you need to do 

    ```
    export PATH=/usr/include/hdf5/openmpi:$PATH
    ```

    Then the CMake command to configure the Puffin Makefile becomes:

    ```
    cmake -DCMAKE_INSTALL_PREFIX:PATH=/path/to/puffin-install -DCMAKE_BUILD_TYPE:STRING=Release -DCMAKE_COLOR_MAKEFILE:BOOL=TRUE -DCMAKE_VERBOSE_MAKEFILE:BOOL=TRUE -DENABLE_PARALLEL:BOOL=ON -DDEBUG_CMAKE:BOOL=TRUE -DFftw3_ROOT_DIR='/path/to/fftw3-par' -DHdf5_ROOT_DIR='/usr' -DHdf5_MODULE_DIRS='/usr/include/hdf5/openmpi' -DHdf5_LIBRARY_DIRS='/usr/lib/x86_64-linux-gnu/hdf5/openmpi;/usr/lib/x86_64-linux-gnu' -DHdf5_INCLUDE_DIRS='/usr/include/hdf5/openmpi' -DHdf5_LIBRARY_NAMES='hdf5_openmpi_fortran;hdf5_openmpi' -DHdf5_LIBRARIES='/usr/lib/x86_64-linux-gnu/libhdf5_openmpi_fortran.so;/usr/lib/x86_64-linux-gnu/libhdf5_openmpi.so' -DHdf5_STLIBS='/usr/lib/x86_64-linux-gnu/libhdf5_openmpi_fortran.a;/usr/lib/x86_64-linux-gnu/libhdf5_openmpi.a' /path/to/Puffin
    ```

## Using Puffin with Docker

Docker images are hosted [here](https://hub.docker.com/u/mightylorenzo/). There are currently 2 images being hosted - a 'full' container intended for development purposes, which has all tests built, along with the testing infrastructure (pFUnit) and developer and user documentation, etc. By default, it runs the unit tests in Puffin. The other, the 'user' container, is run like an executable, and is about half the size of the 'test' image, since it does not include the tests etc. By default, you pass it the number of processors you want to use, and the name of the input file in the current directory to run.

### Puffin-test container

Contains all tests and testing infrastructure, and builds all dev documentation. Can be built from the dev branch in Puffin.

To download:

```
docker pull mightylorenzo/puffin-test
```

To run, do:
```
docker run -it -v $(pwd):/home/puffin_user/project mightylorenzo/puffin-test /bin/bash
```
...which will open a shell in the container interactively, mounting the current directory on the host to the /home/puffin_user/tmp/puffin-test directory in the container.

### Puffin-user container

More lightweight, no tests or documentation built, intended more as an executable. The dockerfile to build it is in the dev-user branch.

To download:
```
docker pull mightylorenzo/puffin-user
```

To run, do:
```
docker run -v $(pwd):/home/puffin_user/project mightylorenzo/puffin-user 2 clara.in
```
which will run on 2 processes, and pass the file in the current directory 'clara.in' to Puffin.


### Building the Puffin Docker image from source:

If you want, you can build the Docker image locally yourself from source, rather than pulling it from Dockerhub. Just do the standard Docker build command:

```
docker build -t imagename /path/to/puffin/
```
