######################################################################
#
# SciCxxFindVersion: Determine compiler version for any compiler
#
# $Id: SciCxxFindVersion.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

SciPrintString("CMAKE_CXX_COMPILER_ID = '${CMAKE_CXX_COMPILER_ID}'.")

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL GNU OR
    "${CMAKE_CXX_COMPILER_ID}" STREQUAL Clang)
# Get first line of version string
  execute_process(
    COMMAND ${CMAKE_CXX_COMPILER} --version
    # COMMAND head -1
    # OUTPUT_VARIABLE cxx_version_str
    OUTPUT_FILE ${PROJECT_BINARY_DIR}/cxx_version.txt
  )
  file(STRINGS ${PROJECT_BINARY_DIR}/cxx_version.txt cxx_version_strlist)
  file(REMOVE ${PROJECT_BINARY_DIR}/cxx_version.txt)
  list(GET cxx_version_strlist 0 cxx_version_str)
  # message(STATUS "cxx_version_str = '${cxx_version_str}'")
  if (0)
# Test for ubuntu
  # set(cxx_version_str "gcc(Ubuntu 4.4.3-4ubuntu5) 4.4.3 ")
  # message(STATUS "cxx_version_str = '${cxx_version_str}'")
# Works on stix: '(GCC) m.n.r' no trailing space on stix
  # string(REGEX MATCH '(GCC) [0-9]+\\.[0-9]+\\.[0-9]+'
  string(REGEX MATCH ".GCC. [0-9]+\\.[0-9]+\\.[0-9]+"
    cxx_version_tmp "${cxx_version_str}"
  )
  # message(STATUS "cxx_version_tmp = '${cxx_version_tmp}'")
  if (NOT cxx_version_tmp)
# Works for Ubuntu
    string(REGEX MATCH " [0-9]+\\.[0-9]+\\.[0-9]+ *\$"
      cxx_version_tmp "${cxx_version_str}"
    )
  endif ()
  if (NOT cxx_version_tmp)
# Now try to get Gentoo...not very robust either, but nothing else is working.
# Could do with more sophisticated options such as multiline regex/ \s
    string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+"
      cxx_version_tmp "${cxx_version_str}"
    )
  endif ()
  # message(STATUS "cxx_version_tmp = '${cxx_version_tmp}'")
  if (NOT cxx_version_tmp)
    message(ERROR "Unable to extract version from '${cxx_version_str}'")
  endif ()
  # message(STATUS "cxx_version_tmp = '${cxx_version_tmp}'")
  # message(STATUS "cxx_version_tmp = '${cxx_version_tmp}'")
  string(REPLACE "(GCC) " "" cxx_version_tmp "${cxx_version_tmp}")
  string(STRIP ${cxx_version_tmp} cxx_version_tmp)
  endif ()
# New approach, just try to match a three-number version
# then a two-number version
  # message(STATUS "cxx_version_str = '${cxx_version_str}'")
  string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+"
    cxx_version_tmp "${cxx_version_str}"
  )
  if (NOT cxx_version_tmp)
    string(REGEX MATCH "[0-9]+\\.[0-9]+" cxx_version_tmp "${cxx_version_str}")
  endif ()
  if (NOT cxx_version_tmp)
    message(ERROR "Unable to extract version from '${cxx_version_str}'")
  endif ()

elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL Cray)
  exec_program(${CMAKE_CXX_COMPILER}
    ARGS -V
    OUTPUT_VARIABLE cxx_version_tmp
  )
  string(REGEX MATCH
    "Version [0-9]+\\.[0-9]+\\.[0-9]+"
    cxx_version_tmp
    ${cxx_version_tmp}
  )
  # MESSAGE("cxx_version_tmp = ${cxx_version_tmp}.")
  string(REPLACE "Version " "" cxx_version_tmp ${cxx_version_tmp})
  # MESSAGE("cxx_version_tmp = ${cxx_version_tmp}.")
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL Intel)
  if (CMAKE_CXX_COMPILER MATCHES "icc" OR CMAKE_CXX_COMPILER MATCHES "icpc" OR
  CMAKE_CXX_COMPILER MATCHES "mpi" OR CMAKE_CXX_COMPILER MATCHES "CC")
    execute_process(
      COMMAND ${CMAKE_CXX_COMPILER} --version
      OUTPUT_VARIABLE cxx_version_str
    )
    # Works on verus: '(ICC) m.n.r' no trailing space on stix
    # string(REGEX MATCH '(GCC) [0-9]+\\.[0-9]+\\.[0-9]+'
    string(REGEX MATCH ".ICC. [0-9]+\\.[0-9]+"
      cxx_version_tmp "${cxx_version_str}"
    )
    if (DEBUG_CMAKE)
      message(STATUS "Evaluating '${cxx_version_str}'")
      message(STATUS "Got '${cxx_version_tmp}'")
    endif ()

    if (NOT cxx_version_tmp)
      message(ERROR "Unable to extract version from '${cxx_version_str}'")
    endif ()
    string(REPLACE "(ICC) " "" cxx_version_tmp "${cxx_version_tmp}")
    string(STRIP ${cxx_version_tmp} cxx_version_tmp)
  elseif (CMAKE_CXX_COMPILER MATCHES "icl")
    exec_program(${CMAKE_CXX_COMPILER}
      OUTPUT_VARIABLE cxx_version_tmp
    )
    string(REGEX MATCH
      "Version [0-9][0-9]\\.[0-9]\\.[0-9]\\.[0-9][0-9][0-9]"
      cxx_version_tmp
      ${cxx_version_tmp}
    )
    string(REPLACE "Version " "" cxx_version_tmp ${cxx_version_tmp})
  endif ()
elseif (CMAKE_CXX_COMPILER MATCHES "cl")
  exec_program(${CMAKE_CXX_COMPILER}
    OUTPUT_VARIABLE cxx_version_tmp
  )
  string(REGEX MATCH
    "Version [0-9]+\\.[0-9]+\\.[0-9]+(\\.[0-9]+)? for"
    cxx_version_tmp
    ${cxx_version_tmp}
  )
  string(REPLACE "Version " "" cxx_version_tmp ${cxx_version_tmp})
  string(REPLACE " for" "" cxx_version_tmp ${cxx_version_tmp})
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL PathScale)
  exec_program(${CMAKE_CXX_COMPILER}
    ARGS --version
    OUTPUT_VARIABLE cxx_version_tmp
  )
  # ARGS -v # This used to work above?
  # MESSAGE("cxx_version_tmp = ${cxx_version_tmp}.")
  string(REGEX MATCH
    "Version [0-9]+\\.[0-9]"
    cxx_version_tmp
    ${cxx_version_tmp}
  )
  string(REPLACE "Version " "" cxx_version_tmp ${cxx_version_tmp})
  # MESSAGE("cxx_version_tmp = ${cxx_version_tmp}.")
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL PGI)
  exec_program(${CMAKE_CXX_COMPILER}
    ARGS -V
    OUTPUT_VARIABLE cxx_version_tmp
  )
  string(REGEX MATCH
    "pgCC [0-9]+\\.[0-9]+-[0-9]+"
    cxx_version_tmp
    ${cxx_version_tmp}
  )
  # MESSAGE("cxx_version_tmp = ${cxx_version_tmp}.")
  string(REPLACE "pgCC " "" cxx_version_tmp ${cxx_version_tmp})
  # MESSAGE("cxx_version_tmp = ${cxx_version_tmp}.")
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL XL)
  exec_program(${CMAKE_CXX_COMPILER}
    ARGS -qversion
    OUTPUT_VARIABLE cxx_version_tmp
  )
  # MESSAGE("cxx_version_tmp = ${cxx_version_tmp}.")
  string(REGEX MATCH
    "Version: .*"
    cxx_version_tmp
    ${cxx_version_tmp}
  )
  # MESSAGE("cxx_version_tmp = ${cxx_version_tmp}.")
  string(REPLACE "Version: " "" cxx_version_tmp ${cxx_version_tmp})
  # MESSAGE("cxx_version_tmp = ${cxx_version_tmp}.")
# This does not work, as it sets Boost_INCLUDE_DIR
else ()
  message(FATAL_ERROR "Unknown compiler ID, ${CMAKE_CXX_COMPILER_ID}.")
endif ()
set(CXX ${CMAKE_CXX_COMPILER})
set(CXX_VERSION ${cxx_version_tmp})

