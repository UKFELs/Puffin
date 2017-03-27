######################################################################
#
# SciCppCheck: Run cppcheck on a source directory.
#
# $Id: SciCppCheck.cmake 1016 2016-03-15 17:42:51Z swsides $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

if (NOT SCIMAKE_DIR)
  set(SCIMAKE_DIR "${TEST_SCIMAKE_DIR}")
endif ()

# Execute cppcheck
set(CppCheck_suppargs)
if (EXISTS ${CPPCHECK_SOURCE_DIR}/cppchecksupp.txt)
  set(CppCheck_suppargs
    --suppressions-list=${CPPCHECK_SOURCE_DIR}/cppchecksupp.txt
  )
endif ()
# Run with --xml to get error ids
# set(cmd ${CppCheck_cppcheck} --inline-suppr --enable=warning --xml ${CppCheck_suppargs} ${CPPCHECK_SOURCE_DIR})
set(cmd ${CppCheck_cppcheck} --inline-suppr --enable=warning ${CppCheck_suppargs} ${CPPCHECK_SOURCE_DIR})
# Convert to list for printing
string(REPLACE ";" " " cmdstr "${cmd}")
message(STATUS "Executing ${cmdstr}")
execute_process(COMMAND ${cmd}
  RESULT_VARIABLE EXEC_ERROR
  OUTPUT_FILE ${CMAKE_BINARY_DIR}/cppcheck.out
  ERROR_FILE ${CMAKE_BINARY_DIR}/cppcheck.err
  WORKING_DIRECTORY ${CPPCHECK_SOURCE_DIR}
)

# Make sure cppcheck succeeded
if (EXEC_ERROR)
  message(STATUS "EXEC_ERROR      = ${EXEC_ERROR}")
  message(FATAL_ERROR "Execution failure.")
endif ()
message(STATUS "Execution succeeded.")

# Look for error messages
file(STRINGS cppcheck.err CPPCHECK_ERRORS REGEX "(error)")
string(LENGTH "${CPPCHECK_ERRORS}" errlen)
if (errlen)
  message(STATUS "cppcheck errors:")
  string(REPLACE ";" "\n" CPPCHECK_ERRORS "${CPPCHECK_ERRORS}")
  message(FATAL_ERROR ${CPPCHECK_ERRORS})
endif ()

# Look for warning messages
file(STRINGS cppcheck.err CPPCHECK_WARNINGS REGEX "(warning)")
string(LENGTH "${CPPCHECK_WARNINGS}" errlen)
if (errlen)
  message(STATUS "cppcheck warnings:")
  string(REPLACE ";" "\n" CPPCHECK_WARNINGS "${CPPCHECK_WARNINGS}")
  message(WARNING ${CPPCHECK_WARNINGS})
endif ()

