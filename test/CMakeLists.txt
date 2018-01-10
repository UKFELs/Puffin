#enable_language(Fortran)
enable_language (Fortran)
##set (PFUNIT /home/jonny/contrib-gcc540/pFUnit-3.2.8-ser)
# set (PFUNIT /home/tml/src/pFUnit)
set (PFUNIT /home/tml/bin/pfunit-parallel)
set (CMAKE_VERBOSE_MAKEFILE "On")
find_library(PFUNITLIB NAMES pfunit
  PATHS ${PFUNIT} ${PFUNIT}/lib
  )

include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../source
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}/../source
  ${PFUNIT}/mod
  ${PFUNIT}/include
  )


#  Status message:

message(STATUS "PFUNIT ${PFUNIT}")


# Define tests do build:

#set (PFTESTS testTests testFFunctions testHdf5Puff)
set (PFTESTS testTests testFFunctions)


# 

#FILE(GLOB PFUNIT_MODULES ${PFUNIT}/mod/*.mod)
FILE(GLOB F_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/../source/*f90)
FILE(GLOB C_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/../source/*.c)
set(F_SOURCES ${F_SOURCES} "${CMAKE_CURRENT_SOURCE_DIR}/../source/hdf5_puff.f90")


# For every test suite...:

foreach(PFTEST ${PFTESTS})
#  execute_process (COMMAND ("$PFUNIT/bin/pFUnitParser.py" ${PFTEST}".pf" ${PFTEST}".F90")
#    RESULT_VARIABLE PFPREPROCESS_RESULT
#    OUTPUT_VARIABLE PFPREPROCESS_OUTPUT
#  )

# Name of fortran test file to be created by python test driver:
    set (PFTEST_FORTRAN_FILE "${PFTEST}.F90")


# Custom command to pre-process .pf -> .F90 file...

    add_custom_command( OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${PFTEST_FORTRAN_FILE}
      COMMAND "${PFUNIT}/bin/pFUnitParser.py"
      ARGS ${CMAKE_CURRENT_SOURCE_DIR}/${PFTEST}.pf ${CMAKE_CURRENT_BINARY_DIR}/${PFTEST_FORTRAN_FILE}
      COMMENT "Generating ${PFTEST_FORTRAN_FILE} from ${PFTEST}"
      VERBATIM
    ) # VERBATIM

    set (PFTEST_FORTRAN_FILES ${PFTEST_FORTRAN_FILES} ${PFTEST_FORTRAN_FILE})
    message(STATUS "Trying to run custom command over ${PFTEST}")
    message(STATUS "Source dir ${CMAKE_CURRENT_SOURCE_DIR}")
    message(STATUS "pFUNIT preprocessing : ${PFTEST_FORTRAN_FILES}")
    message(STATUS "Include dirs: ${CMAKE_INCLUDE_DIRECTORIES}")
    message(STATUS "CMAKE lib path: ${CMAKE_LIBRARY_PATH}")
    message(STATUS "F_SOURCE_LIBS : ${F_SOURCES}")
    #add_dependencies(tests "${PFTEST}.F90")

endforeach(PFTEST)  # end loop


### DON'T COMPILE AGAIN WHEN YOU CAN COPY! ###
### GET F90 FILES FROM PUFFIN SOURCE TO TEST ####

# Loop around every puffin f90 module and copy

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
# add_custom_command(OUTPUT "AIO.c.o"
#   COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/../source/CMakeFiles/AIO.dir/AIO.c.o .
#   COMMENT "Copying libAIO over"
#   VERBATIM
#   )

message(STATUS "F_SOURCE_MODS : ${F_SOURCE_MODS}")

# PFUnit must be built! :

add_custom_command(OUTPUT libpfunit.a
  COMMAND cp ${PFUNIT}/source/libpfunit.a .
  COMMENT "Looking to copy of libpfunit.a"
  VERBATIM
  )

add_custom_target(fortran_test_files DEPENDS ${PFTEST_FORTRAN_FILES})
add_custom_target(fortran_source_modules DEPENDS ${F_SOURCE_MODS})

message(STATUS "fortran test files: " ${PFTEST_FORTRAN_FILES})
message(STATUS "include directories: ${PFUNIT} and ${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "PFUNIT MODULES: ${PFUNIT_MODULES}")


# add_executable(testexe ${PFUNIT}/include/driver.F90 ${PFTEST_FORTRAN_FILES} "AIO.c.o" ${F_SOURCE_MODS})
add_executable(testexe ${PFUNIT}/include/driver.F90 ${PFTEST_FORTRAN_FILES} ${F_SOURCE_MODS})
#link_directories(${PFUNIT}/mod/ ${PFUNIT}/include ${CMAKE_CURRENT_SOURCE_DIR}/../source ${CMAKE_CURRENT_BINARY_DIR}/../source ${Fftw3_LIBRARY_DIRS} ${Hdf5_LIBRARY_DIRS} ${MPI_Fortran_LIBRARIES})
link_directories(${PFUNIT}/bin/ ${PFUNIT}/include ${CMAKE_CURRENT_SOURCE_DIR}/../source ${CMAKE_CURRENT_BINARY_DIR}/../source  ${MPI_Fortran_LIBRARIES})


set_target_properties(testexe PROPERTIES
   COMPILE_FLAGS "-c -g -O0 -DGNU -DBUILD_ROBUST -fbacktrace -fbounds-check -fcheck=mem -I${PFUNIT}/include -I${PFUNIT}/include -I${CMAKE_CURRENT_BINARY_DIR}/../source"
   LINK_FLAGS "-g -O0 -fbacktrace -fbounds-check -fcheck=mem -DGNU -DBUILD_ROBUST -I.  -I${PFUNIT}/include -I${CMAKE_CURRENT_BINARY_DIR}/../source  -I${Hdf5_INCLUDE_DIRS} -I${Fftw3_INCLUDE_DIRS}"
   )

 ###
 #  Duplicate Fftw3 libs seems like a typo but it isn't.
 # Currently these are being set in the wrong order, but by
 # including twice the FFTW_MPI lib before and after the other lib
 # (which this implicitly does) it is able to resolve its symbols correctly
 ###
target_link_libraries(testexe ${PFUNITLIB} ${Hdf5_LIBRARIES} ${Fftw3_LIBRARIES} ${Fftw3_LIBRARIES} ${MPI_Fortran_LIBRARIES} dl m)
add_dependencies(testexe puffin fortran_test_files fortran_source_modules)

add_test(testall testexe)
