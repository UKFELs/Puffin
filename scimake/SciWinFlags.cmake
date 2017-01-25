######################################################################
#
# Include for common Windows flags and settings.
#
# $Id: SciWinFlags.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright &copy; 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

if (WIN32)
# ICL needs to be defined here (Intel compiler and Visual Studio) for
# trilinos: ml_utils.h
  add_definitions(-DWIN32)
  set(_USE_MATH_DEFINES 1
    CACHE STRING "Define whether to use math defines(for Windows)")
  string(REGEX MATCH "^.*icl\\.*" USING_ICL "${CMAKE_C_COMPILER}")
  string(REGEX MATCH "^.*cl\\.*" USING_CL "${CMAKE_C_COMPILER}")
  string(REGEX MATCH "^.*mingw.*" USING_MINGW "${CMAKE_C_COMPILER}")
  if (USING_ICL)
    add_definitions(-DICL)
    set(_TIMEVAL_DEFINED 1
        CACHE STRING "Define whether system has timeval(for Windows)")
    foreach (i DEBUG RELEASE MINSIZERELEASE REWITHDEBINFO)
      set(CMAKE_C_FLAGS_${i} "${CMAKE_C_FLAGS_${i}} /Qstd:c99")
    endforeach ()
  elseif (USING_CL)

    # There are so many things wrong with giving "-DCL" to the compiler
    # just because the compiler is cl.exe.  So commenting out this
    # blanket macro.  Rationale:
    #
    # - Non standard.  _MSC_VER is the existing standard.
    # - Applies to Fortran code also, perhaps one that we cannot change.
    #   Fortran code will have small identifiers and capitalized too.
    # - Not easy to grep where it is used (imagine PTCL, PARTICLE, etc).
    # - Weird error messages when CL is a variable name.
    # - The compiler/preprocessor does not need to be told its identity!
    #
    # If some code breaks, use builtin _MSC_VER there instead of CL.
    #
    # -- CJ

    # add_definitions(-DCL)

    set(_TIMEVAL_DEFINED 1
        CACHE STRING "Define whether system has timeval(for Windows)")
  endif ()
endif ()

