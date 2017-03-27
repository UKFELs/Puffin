######################################################################
#
# SciFindCompilerVersion: Determine compiler version for any compiler
#
# $Id: SciFindCompilerVersion.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

macro(SciFindCompilerVersion COMPLANG)

  set(${COMPLANG}_COMPILER ${CMAKE_${COMPLANG}_COMPILER})
  set(${COMPLANG}_COMPILER_ID ${CMAKE_${COMPLANG}_COMPILER_ID})
  message(STATUS "Checking ${COMPLANG} compiler.")
  SciPrintVar(${COMPLANG}_COMPILER)
  SciPrintVar(${COMPLANG}_COMPILER_ID)

  if (CMAKE_${COMPLANG}_COMPILER_ID STREQUAL GNU OR
      CMAKE_${COMPLANG}_COMPILER_ID STREQUAL Clang)
  # Get first line of version string
    execute_process(
      COMMAND ${CMAKE_${COMPLANG}_COMPILER} --version
      OUTPUT_FILE ${PROJECT_BINARY_DIR}/_version.txt
    )
    file(STRINGS ${PROJECT_BINARY_DIR}/_version.txt _version_strlist)
    file(REMOVE ${PROJECT_BINARY_DIR}/_version.txt)
    list(GET _version_strlist 0 _version_str)
  # New approach, just try to match a three-number version
  # then a two-number version
    # message(STATUS "_version_str = '${_version_str}'")
    string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+"
      _version_tmp "${_version_str}"
    )
    if (NOT _version_tmp)
      string(REGEX MATCH "[0-9]+\\.[0-9]+" _version_tmp "${_version_str}")
    endif ()
    if (NOT _version_tmp)
      message(ERROR "Unable to extract version from '${_version_str}'")
    endif ()

  elseif (CMAKE_${COMPLANG}_COMPILER_ID STREQUAL Cray)
    exec_program(${CMAKE_${COMPLANG}_COMPILER}
      ARGS -V
      OUTPUT_VARIABLE _version_tmp
    )
    string(REGEX MATCH
      "Version [0-9]+\\.[0-9]+\\.[0-9]+"
      _version_tmp
      ${_version_tmp}
    )
    # MESSAGE("_version_tmp = ${_version_tmp}.")
    string(REPLACE "Version " "" _version_tmp ${_version_tmp})
    # MESSAGE("_version_tmp = ${_version_tmp}.")
  elseif (CMAKE_${COMPLANG}_COMPILER_ID STREQUAL Intel)
    # message(STATUS "Intel: CMAKE_${COMPLANG}_COMPILER = ${CMAKE_${COMPLANG}_COMPILER}")
    if (CMAKE_${COMPLANG}_COMPILER MATCHES "icc"
        OR CMAKE_${COMPLANG}_COMPILER MATCHES "icpc"
        OR CMAKE_${COMPLANG}_COMPILER MATCHES "mpi"
        OR CMAKE_${COMPLANG}_COMPILER MATCHES "CC"
        OR CMAKE_${COMPLANG}_COMPILER MATCHES "cc")
      execute_process(
        COMMAND ${CMAKE_${COMPLANG}_COMPILER} --version
        OUTPUT_VARIABLE _version_str
      )
      # Works on verus: '(ICC) m.n.r' no trailing space on stix
      # string(REGEX MATCH '(GCC) [0-9]+\\.[0-9]+\\.[0-9]+'
      string(REGEX MATCH ".ICC. [0-9]+\\.[0-9]+"
        _version_tmp "${_version_str}"
      )
      if (DEBUG_CMAKE)
        message(STATUS "Evaluating '${_version_str}'")
        message(STATUS "Got '${_version_tmp}'")
      endif ()

      if (NOT _version_tmp)
        message(ERROR "Unable to extract version from '${_version_str}'")
      endif ()
      string(REPLACE "(ICC) " "" _version_tmp "${_version_tmp}")
      string(STRIP ${_version_tmp} _version_tmp)
    elseif (CMAKE_${COMPLANG}_COMPILER MATCHES "icl")
      exec_program(${CMAKE_${COMPLANG}_COMPILER}
        OUTPUT_VARIABLE _version_tmp
      )
      string(REGEX MATCH
        "Version [0-9][0-9]\\.[0-9]\\.[0-9]\\.[0-9][0-9][0-9]"
        _version_tmp
        ${_version_tmp}
      )
      string(REPLACE "Version " "" _version_tmp ${_version_tmp})
    endif ()

  elseif (CMAKE_${COMPLANG}_COMPILER MATCHES "cl")

    exec_program(${CMAKE_${COMPLANG}_COMPILER}
      OUTPUT_VARIABLE _version_tmp
    )
    string(REGEX MATCH
      "Version [0-9]+\\.[0-9]+\\.[0-9]+(\\.[0-9]+)? for"
      _version_tmp
      ${_version_tmp}
    )
    string(REPLACE "Version " "" _version_tmp ${_version_tmp})
    string(REPLACE " for" "" _version_tmp ${_version_tmp})

  elseif ("${CMAKE_${COMPLANG}_COMPILER_ID}" STREQUAL PathScale)

    exec_program(${CMAKE_${COMPLANG}_COMPILER}
      ARGS --version
      OUTPUT_VARIABLE _version_tmp
    )
    # ARGS -v # This used to work above?
    # MESSAGE("_version_tmp = ${_version_tmp}.")
    string(REGEX MATCH
      "Version [0-9]+\\.[0-9]"
      _version_tmp
      ${_version_tmp}
    )
    string(REPLACE "Version " "" _version_tmp ${_version_tmp})
    # MESSAGE("_version_tmp = ${_version_tmp}.")

  elseif ("${CMAKE_${COMPLANG}_COMPILER_ID}" STREQUAL PGI)

    exec_program(${CMAKE_${COMPLANG}_COMPILER}
      ARGS -V
      OUTPUT_VARIABLE _version_tmp
    )
    string(REGEX MATCH
      "pgCC [0-9]+\\.[0-9]+-[0-9]+"
      _version_tmp
      ${_version_tmp}
    )
    # MESSAGE("_version_tmp = ${_version_tmp}.")
    string(REPLACE "pgCC " "" _version_tmp ${_version_tmp})
    # MESSAGE("_version_tmp = ${_version_tmp}.")

  elseif ("${CMAKE_${COMPLANG}_COMPILER_ID}" STREQUAL XL)

    exec_program(${CMAKE_${COMPLANG}_COMPILER}
      ARGS -qversion
      OUTPUT_VARIABLE _version_tmp
    )
    # MESSAGE("_version_tmp = ${_version_tmp}.")
    string(REGEX MATCH
      "Version: .*"
      _version_tmp
      ${_version_tmp}
    )
    # MESSAGE("_version_tmp = ${_version_tmp}.")
    string(REPLACE "Version: " "" _version_tmp ${_version_tmp})
    # MESSAGE("_version_tmp = ${_version_tmp}.")

  else ()

    message(FATAL_ERROR "Unknown compiler ID, ${CMAKE_${COMPLANG}_COMPILER_ID}.")

  endif ()

  set(${COMPLANG}_VERSION ${_version_tmp})
  SciPrintVar(${COMPLANG}_VERSION)

endmacro()

