######################################################################
#
# SciMultiArchKernels: Capabilities for building multiarch libraries
#
# $Id: SciMultiArchKernels.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2015-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

message("")
message(STATUS "--------- Setting up multi-arch capabilities ---------")

######################################################################
# Define the set of architectures we're building for
######################################################################
set(ALL_INSTRUCTION_SETS Generic SSE2 AVX AVX2 AVX512)
if (SCI_MULTIARCH_INSTRUCTION_SETS)
# If SCI_MULTIARCH_INSTRUCTION_SETS has been explicitly set we use that
# value
else ()
  option(SCI_BUILD_ALL_INSTRUCTION_SETS
         "Whether to build for all architectures"
         FALSE)
  if (SCI_BUILD_ALL_INSTRUCTION_SETS)
    SciPrintString("Building multiarch kernels for maximal set of ISAs")
    set(SCI_MULTIARCH_INSTRUCTION_SETS Generic)
    foreach(instSet ${ALL_INSTRUCTION_SETS})
      if (${instSet}_COMPILES)
        list(APPEND SCI_MULTIARCH_INSTRUCTION_SETS ${instSet})
      endif()
    endforeach()
  else ()
    SciPrintString("Building multiarch kernels for minimal set of ISAs")
    set(SCI_MULTIARCH_INSTRUCTION_SETS Generic ${SCI_MOST_POWERFUL_ISA})
  endif ()
endif ()
list(REMOVE_DUPLICATES SCI_MULTIARCH_INSTRUCTION_SETS)
SciPrintVar(SCI_MULTIARCH_INSTRUCTION_SETS)

# We use the SCI_MULTI_ARCH_cmp_FLAGS variables to construct compiler
# flags for the multi-architecture libraries.  We start from the FULL
# flags and remove all architecture specific flags.  The architecture
# specific flags are later put back one by one in a controlled way for
# each supported architecture.
foreach(cmp C CXX)
  set(SCI_MULTI_ARCH_${cmp}_FLAGS ${CMAKE_${cmp}_FLAGS_FULL})
  foreach(instSet ${ALL_INSTRUCTION_SETS})
    string(REPLACE "${${instSet}_FLAG}" " "
           SCI_MULTI_ARCH_${cmp}_FLAGS
           ${SCI_MULTI_ARCH_${cmp}_FLAGS})
  endforeach()
  SciPrintVar(SCI_MULTI_ARCH_${cmp}_FLAGS)
endforeach()

#
# SciAddMultiArchLibrary: Create a multi architecture library
#
# Multi architecture libraries are libraries containing code for
# multiple CPU instruction set architectures (ISAs).  They are created
# by compiling the source files of the library multiple times.  This
# process leads to multiple libraries, one for each ISA.  For each
# library appropriate compiler flags are set to create binaries for one
# specific ISA.  The set of ISAs for which binaries are created is
# controlled by the SCI_MULTIARCH_INSTRUCTION_SETS architecture.
#
# Typical usage of this function is as follows:
#
# add_library(example_lib generic.cpp)
# set(EXAMPLE_LIB_SRCS foo.cpp bar.cpp)
# SciAddMultiArchLibrary(
#   example_lib_multiarch example_lib ${EXAMPLE_LIB_SRCS})
# add_executable(main main.cpp)
# target_link_libaries(main example_lib ${example_lib_multiarch})
#
#
# Args:
#   multiarch_libraries: On entry the name of the multi architecture
#                        library.  On exit this variable contains the
#                        list of libraries that constitute the multi
#                        architecture libraries.
#   library:             The generic library associated with the multi
#                        architecture library.  This controls the
#                        -Dlib_EXPORTS macro for symbol visibility.
#
function(SciAddMultiArchLibrary multiarch_libraries library)
  set(library_targets)
  foreach(ia ${SCI_MULTIARCH_INSTRUCTION_SETS})
    set(library_name ${multiarch_libraries}_${ia})
    add_library(${library_name} ${ARGN})
    if (${${ia}_FLAG})
      set_target_properties(${library_name} PROPERTIES
                            COMPILE_FLAGS ${${ia}_FLAG})
    endif ()
    target_compile_definitions(${library_name} PRIVATE
                               -DSCI_ARCH=${ia} -DSCI_BUILDING_${ia} -D${library}_EXPORTS)
    list(APPEND library_targets ${library_name})
  endforeach()
  set(${multiarch_libraries} ${library_targets} PARENT_SCOPE)
endfunction()

