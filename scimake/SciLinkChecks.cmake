######################################################################
#
# SciLinkChecks: check/set various link flags
#
# $Id: SciLinkChecks.cmake 790 2015-04-15 21:42:06Z techxdws $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################

######################################################################
#
# rpaths
#
######################################################################

if (BUILD_SHARED_LIBS)
  if (APPLE)
# Follow convention that libraries are installed with full path
    set(CMAKE_MACOSX_RPATH FALSE)
    if (NOT DEFINED CMAKE_INSTALL_NAME_DIR)
# Set library directory as needed by package managers
      set(CMAKE_INSTALL_NAME_DIR ${CMAKE_INSTALL_PREFIX}/lib)
    endif ()
  elseif (LINUX)
    set(CMAKE_EXE_LINKER_FLAGS "-Wl,-rpath,\$ORIGIN ${CMAKE_EXE_LINKER_FLAGS}")
    set(CMAKE_SHARED_LINKER_FLAGS "-Wl,-rpath,\$ORIGIN ${CMAKE_SHARED_LINKER_FLAGS}")
  endif ()
# Add the automatically determined parts of the RPATH that
# point to directories outside the build tree to the install RPATH
# See: http://www.itk.org/Wiki/CMake_RPATH_handling
  if (NOT DEFINED CMAKE_INSTALL_RPATH_USE_LINK_PATH)
# Add the automatically determined parts of the RPATH that
# point to directories outside the build tree to the install RPATH
    set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
  endif ()
  message(STATUS "After further modification:")
  foreach (type EXE SHARED)
    SciPrintVar(CMAKE_${type}_LINKER_FLAGS)
  endforeach ()
endif ()

######################################################################
#
# This works around the bug discussed in
# http://www.personal.psu.edu/stm134/Software.html
# where one can download DynamicCastTest.zip
# This is known to be a problem with Snow Leopard and Mountain Lion
#
######################################################################

if ("${CMAKE_SYSTEM_NAME}" STREQUAL Darwin)
# Get major version
  string(REPLACE "Darwin-" "" SCI_SYSTEM_VERSION "${CMAKE_SYSTEM}")
  message(STATUS "SCI_SYSTEM_VERSION = ${SCI_SYSTEM_VERSION}.")
  string(REGEX REPLACE "\\..*$" "" SCI_SYSTEM_MAJVER "${SCI_SYSTEM_VERSION}")
  if ("${SCI_SYSTEM_MAJVER}" LESS 11)   # Before Lion (Snow Leopard or before)
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -mmacosx-version-min=10.4")
    set(CMAKE_SKIP_RPATH TRUE)        # -rpath isn't available until 10.5
    set(CMAKE_SKIP_BUILD_RPATH TRUE)
    set(CMAKE_SKIP_INSTALL_RPATH TRUE)
  elseif ("${SCI_SYSTEM_MAJVER}" LESS 13) # Before Mavericks
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -mmacosx-version-min=10.5")
  else ()
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -mmacosx-version-min=10.7")
  endif ()
endif ()

######################################################################
#
# Need to add library paths from compiler for rpath
#
######################################################################

if ("${CMAKE_SYSTEM_NAME}" STREQUAL Linux)

  message("")
  message("--------- Determining version of glibc ---------")
  execute_process(
    COMMAND ldd --version
    COMMAND head -1
    COMMAND sed "s/^.* //"
    OUTPUT_VARIABLE GLIBC_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  SciPrintVar(GLIBC_VERSION)

  if ("${CMAKE_C_COMPILER_ID}" STREQUAL GNU)
    message("")
    message("--------- Adding location of libstdc++ to rpath ---------")
    execute_process(COMMAND ${CMAKE_C_COMPILER} -print-file-name=libstdc++.so
      OUTPUT_VARIABLE libcxx)
    message("libcxx is ${libcxx}")
    if (${libcxx} MATCHES "^/")
      get_filename_component(CXX_LIBDIR ${libcxx}/.. REALPATH)
      message(STATUS "libstdc++ is in ${CXX_LIBDIR}.")
# Add to build rpath
      set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath,${CXX_LIBDIR}")
      set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,-rpath,${CXX_LIBDIR}")
    endif ()
  endif ()

endif ()

######################################################################
#
# Print results
#
######################################################################

message(STATUS "")
message(STATUS "Link flags:")
foreach (type EXE SHARED)
  foreach (bld FULL RELEASE RELWITHDEBINFO MINSIZEREL DEBUG)
    SciPrintVar(CMAKE_${type}_LINKER_FLAGS_${bld})
  endforeach ()
  SciPrintVar(CMAKE_${type}_LINKER_FLAGS)
  message(STATUS "")
endforeach ()

