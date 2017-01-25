######################################################################
#
# SciDiffMacros: A collection of macros for diffing files
#
# $Id: SciDiffMacros.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2014-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

include(CMakeParseArguments)

# macro for determining the file type
macro(SciGetFileType FILENAME OUTVAR)
  get_filename_component(FILETYPE_SUFFIX "${FILENAME}" EXT)
  string(TOLOWER "${FILETYPE_SUFFIX}" FILETYPE_SUFFIX)
  if (FILETYPE_SUFFIX STREQUAL ".h5" OR FILETYPE_SUFFIX STREQUAL ".vsh5")
    set(${OUTVAR} "h5File")
  elseif (FILETYPE_SUFFIX STREQUAL ".stl" OR FILETYPE_SUFFIX STREQUAL ".ply" OR FILETYPE_SUFFIX STREQUAL ".vtk")
    set(${OUTVAR} "polysFile")
  elseif (FILETYPE_SUFFIX STREQUAL ".stp" OR FILETYPE_SUFFIX STREQUAL ".step")
    set(${OUTVAR} "stepFile")
  else ()
    set(${OUTVAR} "unknown")
  endif ()
endmacro()

# macro for diffing two files
macro(SciDiffFiles DIFF_TEST_FILE DIFF_DIFF_FILE DIFF_FILES_EQUAL)
# specify optional arguments
  # set(opts SORT)
  # message(STATUS "SciDiffFiles called with ${ARGN}.")
  set(oneValArgs TEST_DIR DIFF_DIR)
  set(multiValArgs DIFFER SORTER)
# parse optional arguments
  cmake_parse_arguments(DIFF "${opts}" "${oneValArgs}" "${multiValArgs}"
    ${ARGN}
  )
  message(STATUS "[SciDiffFiles] DIFF_DIFFER = ${DIFF_DIFFER}.")
  message(STATUS "[SciDiffFiles] DIFF_SORTER = ${DIFF_SORTER}.")
  # message(STATUS "[SciDiffFiles] PATH = $ENV{PATH}.")
  if (WIN32)
    execute_process(COMMAND where sort
      COMMAND head -1
      OUTPUT_VARIABLE sortloc
      OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    file(TO_CMAKE_PATH "${sortloc}" sortloc)
  else ()
    set(sortloc sort)
  endif ()
  message(STATUS "[SciDiffFiles] sortloc = ${sortloc}.")

# if no diff file specified use the test file name with the results directory
  set(DIFF_TEST_FILEPATH "${DIFF_TEST_FILE}")
  set(DIFF_DIFF_FILEPATH "${DIFF_DIFF_FILE}")
  if (DIFF_TEST_DIR)
    set(DIFF_TEST_FILEPATH "${DIFF_TEST_DIR}/${DIFF_TEST_FILE}")
  endif ()
  if (DIFF_DIFF_DIR)
    set(DIFF_DIFF_FILEPATH "${DIFF_DIFF_DIR}/${DIFF_DIFF_FILE}")
  endif ()

# make sure both files exist
  message(STATUS "DIFF_TEST_FILEPATH = ${DIFF_TEST_FILEPATH}.")
  if (NOT EXISTS "${DIFF_TEST_FILEPATH}")
    set(${DIFF_FILES_EQUAL} FALSE)
    message(FATAL_ERROR "TEST FILE ${DIFF_TEST_FILEPATH} does not exist.")
  endif ()
  message(STATUS "DIFF_DIFF_FILEPATH = ${DIFF_DIFF_FILEPATH}.")
  if (NOT EXISTS "${DIFF_DIFF_FILEPATH}")
    set(${DIFF_FILES_EQUAL} FALSE)
    message(FATAL_ERROR "DIFF FILE ${DIFF_DIFF_FILEPATH} does not exist.")
  endif ()

# Sort the new file if requested
  if (DIFF_SORTER)
    # string(REPLACE ";" " " sorter "${DIFF_SORTER}")
# Sort is a unix command, so need to pull off drive.  If we get a windows command,
# will need to do differently
    message(STATUS "[SciDiffFiles] Executing ${sortloc} -df \"${DIFF_TEST_FILEPATH}\".")
    execute_process(COMMAND ${sortloc} -df "${DIFF_TEST_FILEPATH}"
      OUTPUT_FILE "${DIFF_TEST_FILEPATH}.sorted"
      RESULT_VARIABLE res
    )
    message (STATUS "res = ${res}.")
    if (res EQUAL 0)
      file(RENAME "${DIFF_TEST_FILEPATH}.sorted" "${DIFF_TEST_FILEPATH}")
    else ()
      message(STATUS "[SciDiffFiles] Execution failed.")
    endif ()
  endif ()

# make sure a diff command is specified
  if (NOT DIFF_DIFFER)
    set(DIFF_DIFFER diff --strip-trailing-cr)
  endif ()

# SciAddUnitTest requires the differ to be passed as a single string, so
# here we must separate the args for cmake.
  separate_arguments(DIFF_DIFFER)
  # message(STATUS "[SciDiffFiles] DIFF_DIFFER = ${DIFF_DIFFER}.")
# execute the diff process
  execute_process(COMMAND ${DIFF_DIFFER}
    "${DIFF_TEST_FILEPATH}" "${DIFF_DIFF_FILEPATH}"
    RESULT_VARIABLE DIFF_FILES_DIFFER)
# return results in results variable
  # message(STATUS "DIFF_FILES_DIFFER = ${DIFF_FILES_DIFFER}.")
  if (DIFF_FILES_DIFFER)
    set(${DIFF_FILES_EQUAL} FALSE)
  else ()
    set(${DIFF_FILES_EQUAL} TRUE)
  endif ()
  # message(STATUS "${DIFF_FILES_EQUAL} = ${${DIFF_FILES_EQUAL}}.")

endmacro()

