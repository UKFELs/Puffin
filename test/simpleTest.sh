rm *.o *.x *.F90 *.mod
export CONTRIB_DIR=$HOME/contrib-gcc540
export PFUNIT=$CONTRIB_DIR/pFUnit-3.2.8-ser
$PFUNIT/bin/pFUnitParser.py helloworld.pf  helloworld.F90
#$F90 -c -g -O0 -fbacktrace -fbounds-check -fcheck=mem -I$PFUNIT/include -DINCLUDE_INTENTIONALLY_BROKEN -I/home/jonny/build-gcc540/pFUnit-3.2.8/Examples/Simple/src -I/home/jonny/contrib-gcc540/pFUnit-3.2.8-ser/mod -DGNU -DBUILD_ROBUST helloworld.F90
$F90 -c -g -O0 -fbacktrace -fbounds-check -fcheck=mem -I$PFUNIT/include -I$PFUNIT/mod -DGNU -DBUILD_ROBUST helloworld.F90
gfortran -o tests.x -I$PFUNIT/mod -I$PFUNIT/include $PFUNIT/include/driver.F90 *.o -L$PFUNIT/lib -I. -lpfunit -fopenmp -g -O0 -fbacktrace -fbounds-check -fcheck=mem -I$PFUNIT/include -DGNU -DBUILD_ROBUST
./tests.x
