#enable_language(Fortran)
enable_language (Fortran)
##set (PFUNIT /home/jonny/contrib-gcc540/pFUnit-3.2.8-ser)
set (CMAKE_VERBOSE_MAKEFILE "On")
set (PFUNIT_SOURCE_DIR "/home/jonny/build-gcc540/pFUnit-3.2.8/source")
find_library(PFUNITLIB NAMES pfunit
  PATHS ${PFUNIT} ${PFUNIT}/lib
  )

include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../source
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}/../source
  ${PFUNIT}/include
  ${PFUNIT}/mod
  )
message(STATUS "PFUNIT ${PFUNIT}")
set (PFTESTS testTests testFFunctions)
FILE(GLOB PFUNIT_MODULES ${PFUNIT}/mod/*.mod)
FILE(GLOB PFUNIT_TOOLS ${PFUNIT_SOURCE_DIR}/*F90)
FILE(GLOB F_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/../source/*f90)
FILE(GLOB C_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/../source/*.c)
foreach(PFTEST ${PFTESTS})
#  execute_process (COMMAND ("$PFUNIT/bin/pFUnitParser.py" ${PFTEST}".pf" ${PFTEST}".F90")
#    RESULT_VARIABLE PFPREPROCESS_RESULT
#    OUTPUT_VARIABLE PFPREPROCESS_OUTPUT
#  )
set (PFTEST_FORTRAN_FILE "${PFTEST}.F90")
add_custom_command( OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${PFTEST_FORTRAN_FILE}
  COMMAND "${PFUNIT}/bin/pFUnitParser.py"
  ARGS ${CMAKE_CURRENT_SOURCE_DIR}/${PFTEST}.pf ${CMAKE_CURRENT_BINARY_DIR}/${PFTEST_FORTRAN_FILE}
  COMMENT "Generating ${PFTEST_FORTRAN_FILE} from ${PFTEST}"
  VERBATIM
  )
#  VERBATIM

set (PFTEST_FORTRAN_FILES ${PFTEST_FORTRAN_FILES} ${PFTEST_FORTRAN_FILE})
message(STATUS "Trying to run custom command over ${PFTEST}")
message(STATUS "Source dir ${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "pFUNIT preprocessing : ${PFTEST_FORTRAN_FILES}")
message(STATUS "Include dirs: ${CMAKE_INCLUDE_DIRECTORIES}")
message(STATUS "CMAKE lib path: ${CMAKE_LIBRARY_PATH}")
message(STATUS "F_SOURCE_LIBS : ${F_SOURCES}")
#add_dependencies(tests "${PFTEST}.F90")

endforeach(PFTEST)


### DON'T COMPILE AGAIN WHEN YOU CAN COPY! ###
### GET F90 FILES FROM PUFFIN SOURCE TO TEST ####
foreach(F_SOURCE_PATH ${F_SOURCES})
  get_filename_component(F_SOURCE ${F_SOURCE_PATH} NAME)
  if (NOT ((${F_SOURCE} STREQUAL "NMain.f90") OR (${F_SOURCE} STREQUAL "otherLibToAvoid")))
  message(STATUS "working on ${F_SOURCE}")
add_custom_command(OUTPUT ${F_SOURCE}.o
  COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/../source/CMakeFiles/puffin.dir/${F_SOURCE}.o .
  COMMENT "Looking to copy ${CMAKE_CURRENT_BINARY_DIR}/../source/CMakeFiles/puffin.dir/${F_SOURCE}.o"
  VERBATIM
  )
  set(F_SOURCE_MODS ${F_SOURCE_MODS} "${F_SOURCE}.o")  
  endif(NOT ((${F_SOURCE} STREQUAL "NMain.f90") OR (${F_SOURCE} STREQUAL "otherLibToAvoid")))
endforeach(F_SOURCE_PATH)

### AND AIO, which is a PITA ###
add_custom_command(OUTPUT "AIO.c.o"
  COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/../source/CMakeFiles/AIO.dir/AIO.c.o .
  COMMENT "Copying libAIO over"
  VERBATIM
  )

###### FOR TESTING ONLY ###########
set(F_SOURCE_MODS "Ffunctions.f90.o" "DParallelSetUp.f90.o" "Ferror_fn.f90.o" "BCIOWrapper.f90.o" "Bnrutil.f90.o" "BParallelInfoType.f90.o" "CIO.f90.o" "FgToP2.f90.o" "Hgrids.f90.o" "FparBeam.f90.o" "IelectronGrid.f90.o" "GparticleFunctions.f90.o" "beamPrep.f90.o" "EDerivGlobals.f90.o" "GenMacros.f90.o" "noise.f90.o" "Erandom.f90.o" "CinitConds.f90.o" "pseqs.f90.o" "CremLow.f90.o")


message(STATUS "F_SOURCE_MODS : ${F_SOURCE_MODS}")

add_custom_command(OUTPUT libpfunit.a
  COMMAND cp ${PFUNIT}/lib/libpfunit.a .
  COMMENT "Looking to copy of libpfunit.a"
  VERBATIM
  )

add_custom_target(fortran_test_files DEPENDS ${PFTEST_FORTRAN_FILES})
#build-gcc540/puffin/par/source/CMakeFiles/puffin.dir
add_custom_target(fortran_source_modules DEPENDS ${F_SOURCE_MODS})

message(STATUS "fortran test files: " ${PFTEST_FORTRAN_FILES})
message(STATUS "include directories: ${PFUNIT} and ${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "PFUNIT MODULES: ${PFUNIT_MODULES}")
#add_executable(testexe ${PFUNIT}/include/driver.F90 ${PFTEST_FORTRAN_FILES} ${PFUNIT_TOOLS})

#Nearly working with these two.
add_executable(testexe ${PFUNIT}/include/driver.F90 ${PFTEST_FORTRAN_FILES} "AIO.c.o" ${F_SOURCE_MODS})
#link_directories(${PFUNIT}/mod/ ${PFUNIT}/include ${CMAKE_CURRENT_SOURCE_DIR}/../source ${CMAKE_CURRENT_BINARY_DIR}/../source ${Fftw3_LIBRARY_DIRS} ${Hdf5_LIBRARY_DIRS} ${MPI_Fortran_LIBRARIES})
link_directories(${PFUNIT}/mod/ ${PFUNIT}/include ${CMAKE_CURRENT_SOURCE_DIR}/../source ${CMAKE_CURRENT_BINARY_DIR}/../source  ${MPI_Fortran_LIBRARIES})


set_target_properties(testexe PROPERTIES
   COMPILE_FLAGS "-c -g -O0 -DGNU -DBUILD_ROBUST -fbacktrace -fbounds-check -fcheck=mem -I${PFUNIT}/include -I${PFUNIT}/mod -I${CMAKE_CURRENT_BINARY_DIR}/../source"
   LINK_FLAGS "-g -O0 -fbacktrace -fbounds-check -fcheck=mem -DGNU -DBUILD_ROBUST -I.  -I${PFUNIT}/mod -I${CMAKE_CURRENT_BINARY_DIR}/../source"
   )
#   LINK_FLAGS "-g -O0 -fbacktrace -fbounds-check -fcheck=mem -DGNU -DBUILD_ROBUST -I.  -I${PFUNIT}/mod -I${CMAKE_CURRENT_BINARY_DIR}/../source -I${Fftw3_INCLUDE_DIRS} -I${Hdf5_INCLUDE_DIRS}"

#target_link_libraries(testexe ${PFUNITLIB} ${Fftw3_LIBRARIES} ${Hdf5_LIBRARIES} ${MPI_Fortran_LIBRARIES} dl m)
target_link_libraries(testexe ${PFUNITLIB}  ${MPI_Fortran_LIBRARIES} dl m)
add_dependencies(testexe puffin fortran_test_files fortran_source_modules)

add_test(testall testexe)
