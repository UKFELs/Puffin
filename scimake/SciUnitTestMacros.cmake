######################################################################
#
# SciUnitTestMacros: Macros for adding unit tests of various types.
#
# $Id: SciUnitTestMacros.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2014-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# Add the specified directories to the shared libraries path
macro(SciAddSharedLibDirs)
# parse the path argument
  set(multiValArgs ADDPATH)
  cmake_parse_arguments(SHLIB_DIRS "${opts}" "${oneValArgs}" "${multiValArgs}" ${ARGN})
# if 1+ directories were specified add it/them to the path in the parent scope
  if (SHLIB_DIRS_ADDPATH)
    set(SHLIB_CMAKE_PATH_VAL ${SHLIB_DIRS_ADDPATH} ${SHLIB_CMAKE_PATH_VAL})
    if (NOT "${CMAKE_CURRENT_BINARY_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
# it only makes sense to set the variable in the parent scope if not
# in the top level directory
      set(SHLIB_CMAKE_PATH_VAL "${SHLIB_CMAKE_PATH_VAL}" PARENT_SCOPE)
    endif (NOT "${CMAKE_CURRENT_BINARY_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")

# make a system native path var containing all of the shared libraries
# directories
    makeNativePath(INPATH "${SHLIB_CMAKE_PATH_VAL}" OUTPATH SCIMAKE_SHLIB_NATIVE_PATH_VAL)
  endif (SHLIB_DIRS_ADDPATH)
endmacro()

# Add current binary dir to shared lib path var. This is needed when doing
# shared builds in order for executables to run.
macro(SciAddCurrentBinaryDir)
  SciAddSharedLibDirs(ADDPATH "${CMAKE_CURRENT_BINARY_DIR}")
endmacro()

# make a macro for converting a cmake path into a platform specific path
macro(makeNativePath)
  set(oneValArgs OUTPATH)
  set(multiValArgs INPATH)
  cmake_parse_arguments(TO_NATIVE "${opts}" "${oneValArgs}" "${multiValArgs}" ${ARGN})
  file(TO_NATIVE_PATH "${TO_NATIVE_INPATH}" NATIVE_OUTPATH)
  if (WIN32)
    string(REPLACE ";" "\\;" ${TO_NATIVE_OUTPATH} "${NATIVE_OUTPATH}")
  else ()
    string(REPLACE ";" ":" ${TO_NATIVE_OUTPATH} "${NATIVE_OUTPATH}")
  endif ()
endmacro()

message("")
message("--------- Setting up testing ---------")

# Set test environment
if (WIN32)
  set(SHLIB_PATH_VAR PATH)
elseif (APPLE)
  set(SHLIB_PATH_VAR DYLD_LIBRARY_PATH)
elseif (LINUX)
  set(SHLIB_PATH_VAR LD_LIBRARY_PATH)
endif ()
message(STATUS "SHLIB_PATH_VAR = ${SHLIB_PATH_VAR}.")

file(TO_CMAKE_PATH "$ENV{${SHLIB_PATH_VAR}}" SHLIB_CMAKE_PATH_VAL)
makeNativePath(INPATH "${SHLIB_CMAKE_PATH_VAL}" OUTPATH SCIMAKE_SHLIB_NATIVE_PATH_VAL)

message(STATUS "In SciAddUnitTestMacros.cmake, SHLIB_CMAKE_PATH_VAL = ${SHLIB_CMAKE_PATH_VAL}")

# Add a unit test. If the test needs to compare its results against some
# expected results, then RESULTS_DIR and RESULTS (or STDOUT) must be set.
#
# Args
#
#   NAME          = the name of this test (which may or may not be the same
#                   as the executable)
#   COMMAND       = test executable (typically same as NAME, but need not be)
#   SOURCES       = 1+ source files to be compiled
#   LIBS          = libraries needed to link test
#   ARGS          = arguments to test
#   RESULTS_FILES = Files to be compared against golden results. If this
#                   var is empty, no comparisons will be done (but see
#                   STDOUT_FILE below)
#   RESULTS_DIR   = directory which contains expected results
#   STDOUT_FILE   = Name of file into which stdout should be captured. This
#                   file will be added to $RESULTS so it will be compared
#                   against expected output.

macro(SciAddUnitTest)
  set(oneValArgs NAME COMMAND DIFFER RESULTS_DIR TEST_DIR DIFF_DIR STDOUT_FILE ARGS NUMPROCS MPIEXEC_PROG)
  set(multiValArgs RESULTS_FILES TEST_FILES DIFF_FILES SOURCES LIBS
                           PROPERTIES ATTACHED_FILES)
  cmake_parse_arguments(TEST "${opts}" "${oneValArgs}" "${multiValArgs}" ${ARGN})
  if (NOT TEST_COMMAND)
    set (TEST_COMMAND ${TEST_NAME})
  endif ()
  if (IS_ABSOLUTE ${TEST_COMMAND})
    set(TEST_EXECUTABLE "${TEST_COMMAND}")
  else ()
    set(TEST_EXECUTABLE "${CMAKE_CURRENT_BINARY_DIR}/${TEST_COMMAND}")
  endif ()
  # make sure there is a diff directory
  if (NOT TEST_DIFF_DIR)
    set(TEST_DIFF_DIR ${TEST_RESULTS_DIR})
  endif ()
  # make sure there are test and diff files
  if (NOT TEST_TEST_FILES)
    set(TEST_TEST_FILES ${TEST_RESULTS_FILES})
  endif ()
  if (NOT TEST_DIFF_FILES)
    foreach (fname ${TEST_TEST_FILES})
      get_filename_component(TEST_DIFF_FILE "${fname}" NAME)
      set(TEST_DIFF_FILES ${TEST_DIFF_FILES} "${TEST_DIFF_FILE}")
    endforeach ()
  endif ()
  # if parallel set the mpiexec argument
  if (TEST_NUMPROCS AND ENABLE_PARALLEL AND MPIEXEC)
    set(TEST_MPIEXEC "${MPIEXEC} -np ${TEST_NUMPROCS}")
  else ()
    set(TEST_MPIEXEC)
  endif (TEST_NUMPROCS AND ENABLE_PARALLEL AND MPIEXEC)
  if (TEST_SOURCES)
    add_executable(${TEST_COMMAND} ${TEST_SOURCES})
  endif ()
  if (TEST_LIBS)
    target_link_libraries(${TEST_COMMAND} ${TEST_LIBS})
  endif ()
  add_test(NAME ${TEST_NAME} COMMAND ${CMAKE_COMMAND}
      -DTEST_DIFFER:STRING=${TEST_DIFFER}
      -DTEST_PROG:FILEPATH=${TEST_EXECUTABLE}
      -DTEST_MPIEXEC:STRING=${TEST_MPIEXEC}
      -DTEST_ARGS:STRING=${TEST_ARGS}
      -DTEST_STDOUT_FILE:STRING=${TEST_STDOUT_FILE}
      -DTEST_TEST_FILES:STRING=${TEST_TEST_FILES}
      -DTEST_TEST_DIR:PATH=${TEST_TEST_DIR}
      -DTEST_DIFF_FILES:STRING=${TEST_DIFF_FILES}
      -DTEST_DIFF_DIR:PATH=${TEST_DIFF_DIR}
      -DTEST_SCIMAKE_DIR:PATH=${SCIMAKE_DIR}
      -P ${SCIMAKE_DIR}/SciTextCompare.cmake
  )

# $ATTACHED_FILES is a list of files to attache and if non-empty, it
# overrides the default, which is ${TEST_RESULTS_FILES}.
  if (TEST_ATTACHED_FILES)
    set(FILES_TO_ATTACH ${TEST_ATTACHED_FILES})
  else ()
    set(FILES_TO_ATTACH ${TEST_RESULTS_FILES})
  endif ()
  set_tests_properties(${TEST_NAME}
    PROPERTIES ENVIRONMENT
      "${SHLIB_PATH_VAR}=${SCIMAKE_SHLIB_NATIVE_PATH_VAL}" ${TEST_PROPERTIES}
    ATTACHED_FILES_ON_FAIL "${FILES_TO_ATTACH}"
  )
endmacro()

#
# Check the source with cppcheck
#
macro(SciCppCheckSource build)
  if (("${build}" STREQUAL "") OR (CppCheck_cppcheck AND ${CMAKE_INSTALL_PREFIX} MATCHES "${build}$"))
    message(STATUS "Source code checking enabled.")
    add_test(NAME cppcheck COMMAND ${CMAKE_COMMAND}
      -DCppCheck_cppcheck:FILEPATH=${CppCheck_cppcheck}
      -DCPPCHECK_SOURCE_DIR:PATH=${CMAKE_SOURCE_DIR}
      -P ${SCIMAKE_DIR}/SciCppCheck.cmake
    )
    set_tests_properties(cppcheck
      PROPERTIES ENVIRONMENT
        "${SHLIB_PATH_VAR}=${SCIMAKE_SHLIB_NATIVE_PATH_VAL}"
    )
  else ()
    message(STATUS "Source code checking not enabled.")
  endif ()
endmacro()

