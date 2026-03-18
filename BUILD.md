# How to Install

The source code is hosted [here](https://github.com/UKFELs/Puffin).

## Building from source

Puffin is written in modern fortran, uses MPI for parallelism, and the post-processing scripts are in Python, using pytables and numpy. Puffin can be built using CMake. It requires the parallel (MPI) versions of the FFTW and HDF5 libraries. 

The below guide is for use on linux and other *nix environments. It has been built on numerous HPC clusters and large servers. On Mac OS, Homebrew can be used to install MPI, a Fortran compiler, hdf5 and fftw. In the past Puffin has been built on Windows by way of both Cygwin and WSL.


### Getting Puffin

Fetch Puffin from the repo: here we'll copy it to `/home/user/gitroot/puffin/src`. To clone from Github, do

```sh
git clone git@github.com:UKFELs/Puffin.git /home/user/gitroot/puffin/src
```


### General instructions for building 

To build Puffin you will need parallel hdf5 and fftw3 libs already installed. If you're installing on a system with different compilers, you'll need to make sure these libraries are built with the same build chain as you're going to use for Puffin.

Then build using CMake in the usual way - here we're using a dedicated build directory and install location a level up from the source:
```sh
cmake -B ../build .
cmake --build ../build
cmake --install ../build --prefix ../install
```

Puffin uses PFUnit for testing. If you want to run tests, you can modify the above instructions like so:
```sh
cmake -B ../build -DENABLE_TESTING=TRUE -DCMAKE_PREFIX_PATH='/path/to/pfunit/installed' .
cmake --build ../build
cmake --build ../build --target test
cmake --install ../build --prefix ../install
```

### Install dependencies using conda-forge

Installing dependencies via conda-forge is simple and fast, and consistent across different systems. First create the puffin environment, installing compilers and libs, and then activate it:
```sh
conda create -y -n puffin compilers openmpi hdf5=*=mpi_openmpi* fftw=*=mpi_openmpi* cmake doxygen
conda activate puffin
```
Unfortunately, PFUnit is not (yet) distributed on conda-forge, so you'll still have to build it yourself if you want tests to run.


### Common issues

HPC clusters are unique beasts, and compilation problems vary significantly from machine-to-machine. Here are some issues encountered:

1. Sometimes CMake does not pick up the environment variables which set which compilers to use. These can be explicitly set by doing:

```sh
cmake -DMPI_C_COMPILER=$MPICC -DMPI_CXX_COMPILER=$MPICXX -DMPI_FORTRAN_COMPILER=$MPIFC -DENABLE_PARALLEL:BOOL=TRUE -DCMAKE_INSTALL_PREFIX:PATH=/path/to/puffin-install ...etc
```
where `$MPICC` *etc* should be pointing to your MPI C/Fortran compilers. Also, sometimes setting the following environment variables for the MPI C and Fortran compilers helps:
```sh
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
...etc
```

2. If CMake can't find the parallel libs, the HDF5_ROOT option hints at where to find your HDF5 install if it's having trouble finding it. For example, on Mac OS using dependencies installed with Homebrew, this is needed, and the MPI HDF5 variant is in `/usr/local/opt/hdf5-parallel`, so for the initial cmake call you would do:
```sh
cmake -DENABLE_PARALLEL:BOOL=TRUE -DCMAKE_INSTALL_PREFIX:PATH=/home/user/gitroot/puffin/install -B /home/user/gitroot/puffin/build -DHDF5_ROOT=/path/to/hdf5-parallel /home/user/gitroot/puffin/src
```

3. Speaking of Mac OS, you will see in the CMake logs which compilers have been identified. On Mac with Homebrew, we sometimes see mixups between the Apple C compiler and the Homebrew-installed gcc compiler. To resolve, set `CMAKE_C_COMPILER` and `CMAKE_CXX_COMPILER` options in that initial `cmake` command - e.g. `-DCMAKE_C_COMPILER:FILEPATH=/usr/local/bin/gcc-15` and `-DCMAKE_CXX_COMPILER:FILEPATH=/usr/local/bin/g++-15`. 

4. Depending on your setup, the OpenMP flags may be picked up by CMake and added to the compile flags, but not the link flags. When `make`ing, this results in successful individual compilation of modules, but it will fail at the link stage complaining about not being able to recognise OpenMP commands (which will have the form \*omp\* *e.g.* the name of one is omp_get_num_threads). To fix this, you need to modify the file `/where/you/ran/cmake/source/CMakeFiles/puffin.dir/link.txt`, and add the OpenMP flag for your compiler, usually `-fopenmp` for gfortran, next to the `-O3` flag. Then do `make` again (then `make install`).

5. If you have built hdf5 yourself (see section on building hdf5 and fftw below), then you may find that everything compiles and links, but when you run it says that it can't find the shared hdf5 libraries, usually called something like `libhdf5_fortran.so.10`. You'll need to add the built hdf5 libs to your LD_LIBRARY_PATH (which should be located in the `lib` subdirectory of the top-level of the built hdf5). So do 
```sh
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/shared/hdf5/lib/directory/
```

## Building HDF5 and FFTW3 manually

If running on a HPC machine, you'll likely have admins maintaining these libraries. On local machines for 1D runs or smaller 3D runs, it'll usually be easier to install these libraries via the distribution's package manager. However, if you really need to, for total control, you can build and install these yourself from source.

NOTE: THE BEST DOCUMENTATION FOR INSTALLING FFTW AND HDF5 ARE IN THOSE PROJECTS THEMSELVES. THE BELOW IS JUST A BRIEF OVERVIEW.

In the simplest form, hdf5 can be built by first downloading and unzipping the source from e.g. [here](https://www.hdfgroup.org/downloads/hdf5/source-code/). Then, going into the unzipped hdf5 source directory, one can do:

```sh
./configure --enable-parallel --enable-fortran --prefix=/home/users/username/bin/hdf5/build
make
make install
```

where the destination specified by `prefix` is the destination the built libraries and header files will be installed to. 

Similarly for fftw3, the libraries can be built after extracting the source and moving into the top-level source directory by doing 

```sh
./configure --prefix=/home/users/username/bin/fftw3/build --enable-mpi --enable-openmp
make
make install
```

where, again, the `prefix` specifies where the built library will be placed.

Often, it is required to specify the compilers to be used by the packages. For example, it is common on HPC machines that there will be multiple compilers, which can be loaded into your working environment with use of a `module` command. If the following envirnment variables are not already configured by your system (*e.g.* after doing a `module load` to load the compilers on your HPC machine) (try `echo $MPICC` to print to screen to see if/how `MPICC` is specified), then specifying the environment variables by hand to point to the compilers is sometimes needed:

```sh
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

```sh
export CC=/path/to/mpiicc
```
*etc.*

Environment variables can also be specified in the `configure` call in each case. So, for hdf5, do:

```sh
CC=/path/to/mpicc FC=/path/to/mpif90 F90=/path/to/mpif90 F77=/path/to/mpif90 ./configure --enable-parallel --enable-fortran --prefix=/home/users/username/bin/hdf5/build
```

and for fftw3, do *e.g.*

```sh
./configure --prefix=/home/users/username/bin/fftw3/build MPICC=mpiicc --enable-mpi --enable-openmp
```

will tell the configure routines to use the specified compilers.
