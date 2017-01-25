######################################################################
#
# SciTextCompare: Run an executable and check for differences between
#                 current and accepted results.
#
# $Id: SciTextCompare.cmake 1087 2016-09-25 13:44:28Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
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
  # separate_arguments(TEST_DIFFER)
else ()
  set(TEST_DIFFER diff --strip-trailing-cr)
endif ()
if (TEST_SORTER)
  message(STATUS "Sorting is on.")
  set(SORTER_ARGS SORTER ${TEST_SORTER})
endif ()
message(STATUS "[SciTextCompare] DIFFER IS = ${TEST_DIFFER}.")
message(STATUS "[SciTextCompare] SORTER IS = ${TEST_SORTER}.")
if (TEST_MPIEXEC)
  separate_arguments(TEST_MPIEXEC)
endif (TEST_MPIEXEC)
set(DIR_ARGS)
if (TEST_TEST_DIR)
  set(DIR_ARGS ${DIR_ARGS} TEST_DIR ${TEST_TEST_DIR})
endif ()
if (TEST_DIFF_DIR)
  set(DIR_ARGS ${DIR_ARGS} DIFF_DIR ${TEST_DIFF_DIR})
endif ()

# if TEST_STDOUT_FILE is non-empty, then we use it as the output file
# for the execute_process(), and we add it to the ${TEST_TEST_FILES}
# to be compared. This allows us to have a test which generates one or
# more files which are to be compared, while also comparing the stdout
# of the test.

string(REGEX REPLACE "([^\\]|^);" "\\1 " tmpStr "${ARGS_LIST}")
string(REGEX REPLACE "[\\](.)" "\\1" tmpStr "${tmpStr}")
set(argStr "${tmpStr}")
message(STATUS "[SciTextCompare] EXECUTING ... ${TEST_MPIEXEC} ${TEST_PROG} ${argStr}")
message(STATUS "[SciTextCompare] OUTPUT_FILE = ${TEST_STDOUT_FILE}")
set(errarg)
if (TEST_STDERR_FILE)
  set(errarg ERROR_FILE ${TEST_STDERR_FILE})
  message(STATUS "[SciTextCompare] ERROR_FILE = ${TEST_STDERR_FILE}")
endif ()
if (TEST_STDOUT_FILE)
  execute_process(COMMAND ${TEST_MPIEXEC} ${TEST_PROG} ${ARGS_LIST}
    RESULT_VARIABLE EXEC_ERROR
    OUTPUT_FILE ${TEST_STDOUT_FILE} ${errarg}
  )
# Assume stdout is not out of order by threading
  SciDiffFiles("${TEST_STDOUT_FILE}" "${TEST_STDOUT_FILE}" ARE_FILES_EQUAL
      DIFFER ${TEST_DIFFER}
      ${DIR_ARGS}
      ${SORTER_ARGS}
  )
  if (ARE_FILES_EQUAL)
    message(STATUS "Comparison of ${TEST_STDOUT_FILE} succeeded.")
  else ()
    message(FATAL_ERROR "Comparison failure: ${TEST_STDOUT_FILE} differ.")
  endif ()
else ()
  execute_process(COMMAND ${TEST_MPIEXEC} ${TEST_PROG} ${ARGS_LIST}
    RESULT_VARIABLE EXEC_ERROR
    # ERROR_VARIABLE errvar
  )
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
    message(STATUS "[SciTextCompare] Comparing ${testFile} and ${diffFile} using ${TEST_DIFFER} with ${SORTER_ARGS}.")
    SciDiffFiles("${testFile}" "${diffFile}" ARE_FILES_EQUAL
        DIFFER ${TEST_DIFFER}
        ${DIR_ARGS}
        ${SORTER_ARGS}
    )
    if (ARE_FILES_EQUAL)
      message(STATUS "Comparison of ${testFile} succeeded.")
    else ()
      set(diffres "${testFile}")
    endif ()
  endforeach ()
  if (diffres)
    message(FATAL_ERROR "Comparison failure: ${diffres} differ.")
  endif ()
  message(STATUS "Comparison succeeded.")
endif ()

