#enable_language(Fortran)
enable_language (Fortran)
##set (PFUNIT /home/jonny/contrib-gcc540/pFUnit-3.2.8-ser)
# set (PFUNIT /home/tml/src/pFUnit)
# set (PFUNIT ${PFUNIT_INSTALL})
set (CMAKE_VERBOSE_MAKEFILE "On")

find_package(PFUNIT REQUIRED)
enable_testing()

include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../puffin
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}/../puffin
  )
  

link_directories(
  ${Fftw3_LIBRARY_DIRS}
)

# Define tests to build:

#set (PFTESTS testTests testFFunctions testHdf5Puff)

# add_pfunit_ctest (puffin_tests
#   TEST_SOURCES testTests.pf testFFunctions.pf testHdf5Puff.pf # your application library
# )

# Copy test input files to build directory so tests can access them
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/inputs/f1main.in"
  "${CMAKE_CURRENT_BINARY_DIR}/inputs/f1main.in"
  COPYONLY
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/inputs/beam_file.in"
  "${CMAKE_CURRENT_BINARY_DIR}/inputs/beam_file.in"
  COPYONLY
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/inputs/seed_file.in"
  "${CMAKE_CURRENT_BINARY_DIR}/inputs/seed_file.in"
  COPYONLY
)

add_pfunit_ctest (puffin_basic_tests
  TEST_SOURCES testTests.pf testFFunctions.pf # your application library
  LINK_LIBRARIES puffin_mpi # your application library
)


add_pfunit_ctest (puffin_e2e_tests
  TEST_SOURCES testMPIIntegration.pf # your application library
  LINK_LIBRARIES puffin_mpi # your application library
  MAX_PES 2
)



# ^^ For examples of adding more tests, see:
# https://github.com/Goddard-Fortran-Ecosystem/pFUnit_demos/blob/main/Basic/tests/CMakeLists.txt
