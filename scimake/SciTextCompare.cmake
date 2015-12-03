######################################################################
#
# SciTextCompare: Run an executable and check for differences between
#                 current and accepted results.
#
# $Id: SciTextCompare.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (NOT SCIMAKE_DIR)
  set(SCIMAKE_DIR "${TEST_SCIMAKE_DIR}")
endif ()

include(${SCIMAKE_DIR}/SciDiffMacros.cmake)

string(REPLACE "\"" "" ARGS_LIST "${TEST_ARGS}")
string(REPLACE " " ";" ARGS_LIST "${ARGS_LIST}")

# make sure the file differ is set
if (TEST_DIFFER)
  separate_arguments(TEST_DIFFER)
else ()
  set(TEST_DIFFER diff --strip-trailing-cr)
endif ()
if (TEST_MPIEXEC)
  separate_arguments(TEST_MPIEXEC)
endif (TEST_MPIEXEC)

# if TEST_STDOUT_FILE is non-empty, then we use it as the output file
# into for the execute_process(), and we add it to the ${TEST_TEST_FILES}
# to be compared. This allows us to have a test which generates one or
# more files which are to be compared, while also comparing the stdout
# of the test.

if (TEST_STDOUT_FILE)
  execute_process(COMMAND ${TEST_MPIEXEC} ${TEST_PROG} ${ARGS_LIST}
    RESULT_VARIABLE EXEC_ERROR
    OUTPUT_FILE ${TEST_STDOUT_FILE})
  SciDiffFiles("${TEST_STDOUT_FILE}" "${TEST_STDOUT_FILE}" ARE_FILES_EQUAL
               DIFF_DIR ${TEST_DIFF_DIR})
  if (ARE_FILES_EQUAL)
    message(FATAL_ERROR "Comparison failure: ${TEST_STDOUT_FILE} differ.")
  else ()
    message(STATUS "Comparison of ${TEST_STDOUT_FILE} succeeded.")
  endif ()
else ()
  execute_process(COMMAND ${TEST_MPIEXEC} ${TEST_PROG} ${ARGS_LIST}
    RESULT_VARIABLE EXEC_ERROR)
endif ()


if (EXEC_ERROR)
  message(STATUS "EXEC_ERROR      = ${EXEC_ERROR}")
  message(STATUS "RESULT_VARIABLE = ${RESULT_VARIABLE}")
  message(FATAL_ERROR "Execution failure.")
endif ()
message(STATUS "Execution succeeded.")

if (TEST_TEST_FILES)
  # Test all the output
  # There must be an easier way to pass a list
  # message(STATUS "TEST_TEST_FILES = ${TEST_TEST_FILES}.")
  string(REPLACE "\"" "" TEST_FILES_LIST "${TEST_TEST_FILES}")
  string(REPLACE " " ";" TEST_FILES_LIST "${TEST_FILES_LIST}")
  string(REPLACE "\"" "" DIFF_FILES_LIST "${TEST_DIFF_FILES}")
  string(REPLACE " " ";" DIFF_FILES_LIST "${DIFF_FILES_LIST}")

  list(LENGTH TEST_FILES_LIST tlen)
  list(LENGTH DIFF_FILES_LIST dlen)
  if (NOT ${tlen} MATCHES ${dlen})
    message(FATAL_ERROR "The comparison lists are different sizes.")
  elseif (${tlen} MATCHES 0)
    #message(STATUS "Nothing to compare.")
    return()
  endif ()
  message(STATUS "TEST_FILES_LIST = ${TEST_FILES_LIST}.")
  message(STATUS "DIFF_FILES_LIST = ${DIFF_FILES_LIST}.")
  math(EXPR loopLen "${tlen} - 1")

  foreach (ifile RANGE ${loopLen})
    list(GET TEST_FILES_LIST ${ifile} testFile)
    list(GET DIFF_FILES_LIST ${ifile} diffFile)
    SciDiffFiles("${testFile}" "${diffFile}" ARE_FILES_EQUAL
                 COMMAND  ${TEST_DIFFER}
                 TEST_DIR ${TEST_TEST_DIR}
                 DIFF_DIR ${TEST_DIFF_DIR}
    )
    if (ARE_FILES_EQUAL)
      set(diffres "${testFile}")
    else ()
       message(STATUS "Comparison of ${testFile} succeeded.")
    endif ()
  endforeach ()
  if (diffres)
    message(FATAL_ERROR "Comparison failure: ${diffres} differ.")
  endif ()
  message(STATUS "Comparison succeeded.")
endif ()
