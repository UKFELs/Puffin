######################################################################
#
# SciDiffMacros: A collection of macros for diffing files
#
# $Id: SciDiffMacros.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
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
  set(oneValArgs TEST_DIR DIFF_DIR)
  set(multiValArgs COMMAND)
  # parse optional arguments
  cmake_parse_arguments(DIFF "${opts}" "${oneValArgs}" "${multiValArgs}" ${ARGN})
  # if no diff file was specified use the test file name with the results directory
  set(DIFF_TEST_FILEPATH "${DIFF_TEST_FILE}")
  set(DIFF_DIFF_FILEPATH "${DIFF_DIFF_FILE}")
  if (DIFF_TEST_DIR)
    set(DIFF_TEST_FILEPATH "${DIFF_TEST_DIR}/${DIFF_TEST_FILE}")
  endif ()
  if (DIFF_DIFF_DIR)
    set(DIFF_DIFF_FILEPATH "${DIFF_DIFF_DIR}/${DIFF_DIFF_FILE}")
  endif ()
  # make sure both files exist
  if (NOT EXISTS "${DIFF_TEST_FILE}")
    message(FATAL_ERROR "TEST FILE ${DIFF_TEST_FILE} does not exist.")
  endif ()
  if (NOT EXISTS "${DIFF_DIFF_FILE}")
    message(FATAL_ERROR "DIFF FILE ${DIFF_DIFF_FILE} does not exist.")
  endif ()
  # make sure a diff command is specified
  if (NOT DIFF_COMMAND)
    set(DIFF_COMMAND diff --strip-trailing-cr)
  endif ()

  # execute the diff process
  execute_process(COMMAND ${DIFF_COMMAND}
    "${DIFF_TEST_FILEPATH}" "${DIFF_DIFF_FILEPATH}"
    RESULT_VARIABLE DIFF_FILES_DIFFER)
  # return results in results variable
  if (DIFF_FILES_DIFFER)
    set(${DIFF_FILES_EQUAL} TRUE)
  else ()
    set(${DIFF_FILES_EQUAL} FALSE)
  endif ()
endmacro()
