# How to Install

The source code is hosted [here](https://github.com/UKFELs/Puffin).

## Building from source

Puffin is written in modern fortran, uses MPI for parallelism, and the post-processing scripts are in Python, using pytables and numpy. Puffin can be built using CMake. It requires the parallel (MPI) versions of the FFTW and HDF5 libraries. 

The below guide is for use on linux and other *nix environments. It has been built on numerous HPC clusters and large servers. On Mac OS, Homebrew can be used to install MPI, a Fortran compiler, hdf5 and fftw. In the past Puffin has been built on Windows by way of both Cygwin and WSL.

### General Instructions

To build Puffin you will need parallel hdf5 and fftw3 libs already installed. If you're installing on a system with different compilers, you'll need to make sure these libraries are built with the same build chain as you're going to use for Puffin. If you wish, fftw3 and hdf5 may be obtained and built using Bilder (see later in this guide).

1. Fetch Puffin to wherever you would like: here we'll use `/home/user/gitroot/puffin/src`. To clone from Github, do

    ```sh
    git clone git@github.com:UKFELs/Puffin.git /home/user/gitroot/puffin/src
    ```

2. Run cmake, e.g.

    ```sh
    cmake -DENABLE_PARALLEL:BOOL=TRUE -DCMAKE_INSTALL_PREFIX:PATH=/home/user/gitroot/puffin/install -B /home/user/gitroot/puffin/build -DHDF5_ROOT=/path/to/hdf5-parallel /home/user/gitroot/puffin/src
    ```
    
    With the above, under the `puffin/` directory we're going for the directory structure `src/` for the source repo, `build/` for the build files (e.g. the Makefile + headers/compile artifacts/etc), and `install/` for the actual final executable etc. But you can pick whatever structure you'd like. The HDF5_ROOT option hints at where to find your HDF5 install if it's having trouble finding it (on Mac with Homebrew, this is needed, and the MPI HDF5 is in `/usr/local/opt/hdf5-parallel`).

    To specify compilers, you can pass them as environment varables *e.g.*

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
    
4. Go to the build directory (`cd /home/user/gitroot/puffin/build`) and do `make && make install`. You should get a puffin binary in `/home/user/gitroot/puffin/install/bin`.


### Build using Conda-forge

Users have reported install dependencies via conda-forge with e.g.

```sh
conda create -y -n puffin compilers openmpi hdf5=*=mpi_openmpi* fftw=*=mpi_openmpi* cmake doxygen
conda activate puffin
cmake -B build .
cmake --build build
```


#### Common issues

HPC clusters are unique beasts, and compilation problems vary significantly from machine-to-machine. Here are some issues encountered:

1. Sometimes CMake does not pick up the environment variables which set which compilers to use. These can be explicitly set by doing:

    ```sh
    cmake -DMPI_C_COMPILER=$MPICC -DMPI_CXX_COMPILER=$MPICXX -DMPI_FORTRAN_COMPILER=$MPIFC -DENABLE_PARALLEL:BOOL=TRUE -DCMAKE_INSTALL_PREFIX:PATH=/path/to/puffin-install ...etc
    ```
    where `$MPICC` *etc* should be pointing to your MPI C/Fortran compilers.
    
2. Depending on your setup, the OpenMP flags may be picked up by CMake and added to the compile flags, but not the link flags. When `make`ing, this results in successful individual compilation of modules, but it will fail at the link stage complaining about not being able to recognise OpenMP commands (which will have the form \*omp\* *e.g.* the name of one is omp_get_num_threads). To fix this, you need to modify the file `/where/you/ran/cmake/source/CMakeFiles/puffin.dir/link.txt`, and add the OpenMP flag for your compiler, usually `-fopenmp` for gfortran, next to the `-O3` flag. Then do `make` again (then `make install`).

3. If you have built hdf5 yourself (see section on building hdf5 and fftw below), then you may find that everything compiles and links, but when you run it says that it can't find the shared hdf5 libraries, usually called something like `libhdf5_fortran.so.10`. You'll need to add the built hdf5 libs to your LD_LIBRARY_PATH (which should be located in the `lib` subdirectory of the top-level of the built hdf5). So do 
    ```sh
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/shared/hdf5/lib/directory/
    ```

4. You will see in the CMake logs which compilers have been identified. On Mac with Homebrew, we sometimes see mixups between the Apple C compiler and the Homebrew-installed gcc compiler. To resolve, set `CMAKE_C_COMPILER` and `CMAKE_CXX_COMPILER` options in `CMake` - e.g. `-DCMAKE_C_COMPILER:FILEPATH=/usr/local/bin/gcc-15` and `-DCMAKE_CXX_COMPILER:FILEPATH=/usr/local/bin/g++-15`.

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
