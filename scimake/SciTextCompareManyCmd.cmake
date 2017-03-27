######################################################################
#
# @file    SciTextCompareManyCmd
#
# @brief   Run an executable and check for differences between
#          current and accepted results.
#
# @version $Id: SciTextCompareManyCmd.cmake 1082 2016-09-10 16:11:13Z cary $
#
# Copyright 2016-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

if (NOT SCIMAKE_DIR)
  set(SCIMAKE_DIR "${TEST_SCIMAKE_DIR}")
endif ()

include(${SCIMAKE_DIR}/SciDiffMacros.cmake)

# Fix up args for CMDs
string(REPLACE "\"" "" ARGS1_LIST "${TEST_ARGS1}")
string(REPLACE " " ";" ARGS1_LIST "${ARGS1_LIST}")
string(REPLACE "\"" "" ARGS2_LIST "${TEST_ARGS2}")
string(REPLACE " " ";" ARGS2_LIST "${ARGS2_LIST}")

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
message(STATUS "[SciTextCompareManyCmd] DIFFER IS = ${TEST_DIFFER}.")
message(STATUS "[SciTextCompareManyCmd] SORTER IS = ${TEST_SORTER}.")
if (TEST_MPIEXEC)
  separate_arguments(TEST_MPIEXEC)
endif (TEST_MPIEXEC)
set(DIR_ARGS)
if (TEST_TEST_DIR)
  set(DIR_ARGS ${DIR_ARGS} TEST_DIR ${TEST_TEST_DIR})
endif ()
if (TEST_ACCEPTED_DIR)
  set(DIR_ARGS ${DIR_ARGS} ACCEPTED_DIR ${TEST_ACCEPTED_DIR})
endif ()

# if TEST_STDOUT_FILE is non-empty, then we use it as the output file
# into for the execute_process(), and we add it to the ${TEST_TEST_FILES}
# to be compared. This allows us to have a test which generates one or
# more files which are to be compared, while also comparing the stdout
# of the test.

string(REGEX REPLACE "([^\\]|^);" "\\1 " arg1Str "${ARGS1_LIST}")
string(REGEX REPLACE "[\\](.)" "\\1" arg1Str "${arg1Str}")
string(REGEX REPLACE "([^\\]|^);" "\\1 " arg2Str "${ARGS2_LIST}")
string(REGEX REPLACE "[\\](.)" "\\1" arg2Str "${arg2Str}")
message(STATUS "[SciTextCompareManyCmd] EXECUTING ... ${TEST_MPIEXEC} ${TEST_PROG1} ${arg1Str} && ${TEST_PROG2} ${arg2Str}")
message(STATUS "[SciTextCompareManyCmd] OUTPUT_FILE = ${TEST_STDOUT_FILE}")

# Execute first command, quitting on error
message(STATUS "Executing '${TEST_MPIEXEC} ${TEST_PROG1} ${arg1Str}'")
execute_process(COMMAND ${TEST_MPIEXEC} ${TEST_PROG1} ${ARGS1_LIST}
  RESULT_VARIABLE EXEC_ERROR
)
if (EXEC_ERROR)
  message(STATUS "EXEC_ERROR      = ${EXEC_ERROR}")
  message(STATUS "RESULT_VARIABLE = ${RESULT_VARIABLE}")
  message(FATAL_ERROR "Execution failure.")
endif ()
message(STATUS "Execution of ${CMD1} succeeded.")

# Execute second command quitting on error possibly diffing stdout
message(STATUS "Executing '${TEST_MPIEXEC} ${TEST_PROG2} ${arg2Str}'")
if (TEST_STDOUT_FILE)
  execute_process(
    COMMAND ${TEST_MPIEXEC} ${TEST_PROG2} ${ARGS2_LIST}
    RESULT_VARIABLE EXEC_ERROR
    OUTPUT_FILE ${TEST_STDOUT_FILE}
  )
else ()
  execute_process(
    COMMAND ${TEST_MPIEXEC} ${TEST_PROG2} ${ARGS2_LIST}
    RESULT_VARIABLE EXEC_ERROR
  )
endif ()
if (EXEC_ERROR)
  message(STATUS "EXEC_ERROR      = ${EXEC_ERROR}")
  message(STATUS "RESULT_VARIABLE = ${RESULT_VARIABLE}")
  message(FATAL_ERROR "Execution failure.")
endif ()
message(STATUS "Execution succeeded.")

# Diff stdout
if (TEST_STDOUT_FILE)
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
endif ()

if (TEST_TEST_FILES)
  # Test all the output
  # There must be an easier way to pass a list
  # message(STATUS "TEST_TEST_FILES = ${TEST_TEST_FILES}.")
  string(REPLACE "\"" "" TEST_FILES_LIST "${TEST_TEST_FILES}")
  string(REPLACE " " ";" TEST_FILES_LIST "${TEST_FILES_LIST}")
  string(REPLACE "\"" "" ACCEPTED_FILES_LIST "${TEST_ACCEPTED_FILES}")
  string(REPLACE " " ";" ACCEPTED_FILES_LIST "${ACCEPTED_FILES_LIST}")

  list(LENGTH TEST_FILES_LIST tlen)
  list(LENGTH ACCEPTED_FILES_LIST dlen)
  if (NOT ${tlen} MATCHES ${dlen})
    message(FATAL_ERROR "The comparison lists are different sizes.")
  elseif (${tlen} MATCHES 0)
    #message(STATUS "Nothing to compare.")
    return()
  endif ()
  message(STATUS "TEST_FILES_LIST = ${TEST_FILES_LIST}.")
  message(STATUS "ACCEPTED_FILES_LIST = ${ACCEPTED_FILES_LIST}.")
  math(EXPR loopLen "${tlen} - 1")

  foreach (ifile RANGE ${loopLen})
    list(GET TEST_FILES_LIST ${ifile} testFile)
    list(GET ACCEPTED_FILES_LIST ${ifile} diffFile)
# If diffFile is a relative path, add TEST_ACCEPTED_DIR
    message(STATUS "diffFile = ${diffFile}.")
    if ((NOT diffFile MATCHES "^/") AND (NOT diffFile MATCHES "^[A-Z]:/"))
      set(diffFile "${TEST_ACCEPTED_DIR}/${diffFile}")
    endif ()
    message(STATUS "[SciTextCompareManyCmd] Comparing ${testFile} and ${diffFile} using ${TEST_DIFFER} with ${SORTER_ARGS}.")
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

