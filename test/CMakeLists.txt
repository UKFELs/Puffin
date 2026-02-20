#enable_language(Fortran)
enable_language (Fortran)
##set (PFUNIT /home/jonny/contrib-gcc540/pFUnit-3.2.8-ser)
# set (PFUNIT /home/tml/src/pFUnit)
set (PFUNIT ${PFUNIT_INSTALL})
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

add_pfunit_ctest (puffin_tests
  TEST_SOURCES testTests.pf testFFunctions.pf testHdf5Puff.pf # your application library
  LINK_LIBRARIES puffin_mpi # your application library
)


# ^^ For examples of adding more tests, see:
# https://github.com/Goddard-Fortran-Ecosystem/pFUnit_demos/blob/main/Basic/tests/CMakeLists.txt
