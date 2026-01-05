######################################################################
#
# Variables that change the behavior of this script
#
#  Some variables use scipkguc, and some use scipkgreg.  See
#  http://www.cmake.org/pipermail/cmake/2004-July/005283.html.
#    "scipkgreg" is the regularized package name
#      (package name with all "." and "-" replaced with "_")
#    "scipkguc" is the UPPERCASE regularized package name
#
#  DEBUG_CMAKE - if true, outputs verbose debugging information
#  (default false)
#
#  ENABLE_${scipkguc} (default = true) - if false, will not search for package
#  DISABLE_${scipkguc} - if true, sets ENABLE_${scipkguc} to false
#
#  ${scipkgreg}_FIND_QUIETLY - if true, will succeed silently
#  (default - not defined, which is treated as false)
#
#  ${scipkgreg}_FIND_REQUIRED (default = false) - if true, will issue a
#    fatal error if #    package not found
#
#  ${scipkgreg}_ROOT_DIR - search directory hints
#
#  SUPRA_SEARCH_PATH - used to specify various top-level search directories
#
######################################################################
#
#  Variables defined by this script
#    (In the event that a variable has no valid value, it is set to
#    "${varname}-NOTFOUND")
#
#    ${scipkguc}_FOUND - true if package found
#
#  PROGRAMS:
#    ${scipkgreg}_PROGRAMS - list of found executables, including full
#      path to each.  Only defined for specifically requested executables.
#    ${scipkgreg}_yyy - full name & path to executable "yyy"
#      Only defined for specifically requested executables.
#
#  HEADERS:
#    ${scipkgreg}_INCLUDE_DIRS - a list of all include directories found
#    ${scipkgreg}_yyy - full path to individual header yyy
#      Only defined for specifically requested headers
#
#  MODULES:
#    ${scipkgreg}_MODULE_DIRS - a list of all module directories found
#    ${scipkgreg}_yyy_MOD - full path to individual module yyy.{mod,MOD}
#      Only defined for specifically requested modules
#
#  LIBRARIES:
#    ${scipkgreg}_{library_name}_LIBRARY - full path to the individual
#      $library_name library. Only defined for specifically requested libraries.
#    ${scipkgreg}_LIBRARIES - list of found libraries, including full path to
#      each.  Only defined for specifically requested libraries.
#    ${scipkgreg}_STLIBS - list of all found static libraries.
#      Only defined for libraries existing in ${scipkgreg}_LIBRARIES
#    ${scipkgreg}_DLLS - windows only, list of all found dlls
#      Only defined for libraries existing in ${scipkgreg}_LIBRARIES
#
#  FILES:
#    ${scipkgreg}_FILES - list of found files, including a full path to each
#    ${scipkgreg}_yyy - full name and path to file "yyy"
#
#######################################################################

######################################################################
#
# SciFindPackage: find includes and libraries of a package
#
# $Id: SciFindPackage.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#

# SciGetStaticLibs
#
# Given a list of libraries, create a new list, where for any
# dynamic library with a static library next to it, the new list
# contains the static library.  Otherwise it contains the original
# dynamic library.
#
# Args:
# origlibs: the original libraries
# statlibsvar: the variable holding the static libs when found
#
function(SciGetStaticLibs origlibs statlibsvar)
  # set(origlibs ${${origlibsvar}})
  set(statlibs)
  if (DEBUG_CMAKE)
    message(STATUS "[SciFindPackage]: original libs = ${origlibs}.")
    message(STATUS "[SciFindPackage]: statlibsvar = ${statlibsvar}.")
  endif ()
  foreach (lib ${origlibs})
    set(newlib ${lib})
    set(havestlib FALSE)
    if (${lib} MATCHES "\\.a$" OR ${lib} MATCHES "\\.lib$")
      if (DEBUG_CMAKE)
        message(STATUS "${lib} is a static library.")
      endif ()
      set(havestlib TRUE)
    endif ()
# If not static, try replacing suffix
    if (NOT havestlib)
      foreach (sfx so;dylib;dll)
        if (${lib} MATCHES "\\.${sfx}$")
          if (DEBUG_CMAKE)
            message(STATUS "${lib} is a shared library.")
          endif ()
          get_filename_component(libdir ${lib}/.. REALPATH)
# NAME_WE takes from first .  We generally need from last
          get_filename_component(libname "${lib}" NAME)
          string(REGEX REPLACE "\\.[^\\.]*$" "" libname "${libname}")
          if (${sfx} STREQUAL "dll")
            set(newsfx "lib")
          else ()
            set(newsfx "a")
          endif ()
          if (EXISTS ${libdir}/${libname}.${newsfx})
            set(newlib ${libdir}/${libname}.${newsfx})
            set(havestlib TRUE)
          endif ()
          break()
        endif ()
      endforeach ()
    endif ()
# If still do not have static, try pulling out of library flags
    if (havestlib)
      list(APPEND statlibs ${newlib})
    elseif (${lib} MATCHES "^-L")
      if (DEBUG_CMAKE)
        message(STATUS "${lib} defined by flags.")
      endif ()
      # set(libargs ${lib}) # Create a list
      string(REPLACE " " ";" libargs "${lib}")
      if (DEBUG_CMAKE)
        message(STATUS "libargs = ${libargs}.")
      endif ()
      set(libdirargs)
      set(libdirs)
      set(libnames)
      foreach (libarg ${libargs})
        if (DEBUG_CMAKE)
          message(STATUS "libarg = ${libarg}.")
        endif ()
        if (${libarg} MATCHES "^-L")
          list(APPEND libdirargs ${libarg})
          string(REGEX REPLACE "^-L" "" libdir ${libarg})
          list(APPEND libdirs ${libdir})
        elseif (${libarg} MATCHES "^-l")
          string(REGEX REPLACE "^-l" "" libname ${libarg})
          list(APPEND libnames ${libname})
        endif ()
      endforeach ()
      if (DEBUG_CMAKE)
        message(STATUS "libdirs = ${libdirs}.")
        message(STATUS "libnames = ${libnames}.")
      endif ()
      set(stlibs)
      foreach (libname ${libnames})
        set(lib)
        foreach (libdir ${libdirs})
          if (EXISTS ${libdir}/lib${libname}.a)
            set(lib ${libdir}/lib${libname}.a)
            break()
          elseif (EXISTS ${libdir}/${libname}.lib)
            set(lib ${libdir}/${libname}.lib)
            break()
          endif ()
        endforeach ()
        if (lib)
          list(APPEND stlibs ${lib})
        else ()
          list(APPEND stlibs "${libdirargs} -l${libname}")
        endif ()
      endforeach ()
      list(APPEND statlibs ${stlibs})
    else ()
# If still not static, look in system dirs
      foreach (libdir /usr/lib64;/usr/lib)
        if (EXISTS ${libdir}/lib${lib}.a)
          set(newlib ${libdir}/lib${lib}.a)
          set(havestlib TRUE)
          break()
        endif ()
      endforeach ()
      list(APPEND statlibs ${newlib})
    endif ()
  endforeach ()
  if (DEBUG_CMAKE)
    message(STATUS "[SciFindPackage]: static libs = ${statlibs}.")
  endif ()
  set(${statlibsvar} ${statlibs} PARENT_SCOPE)
endfunction()

# SciGetRealDir
#
# Given a directory name, find the real directory name after
#  first trying to resolve it as a shortcut if on Windows,
#  then, if that does not work, trying to resolve it as a soft
#  link.
#
# Args:
#  canddir: the variable holding the directory candidate
#  realdirvar: the variable holding the directory after following any shortcuts
#
function(SciGetRealDir canddir realdirvar)

  # MESSAGE("realdirvar = ${realdirvar}.")
  set(${realdirvar})
  if (canddir)
    if (DEBUG_CMAKE)
      message(STATUS "Finding the directory for ${canddir}.")
    endif ()
# Follow an existing Windows shortcut
    if (WIN32)
      if (EXISTS ${canddir}.lnk)
        exec_program(readshortcut ARGS ${canddir}.lnk
          OUTPUT_VARIABLE idir
          return_VALUE ires)
        if (ires)
          message(FATAL_ERROR "readshortcut error on ${canddir}.lnk.")
        endif ()
        exec_program(cygpath ARGS -am ${idir} OUTPUT_VARIABLE rd)
      else ()
        set(rd ${canddir})
      endif ()
    else ()
       get_filename_component(rd ${canddir} REALPATH)
    endif ()
  endif ()
  set(${realdirvar} ${rd} PARENT_SCOPE)
endfunction()

# SciGetInstSubdirs
#
# Given a package name, set the possible installation subdirs
#
# Args:
#  pkgnamelc: the variable holding the package name in lower case
#  instdirsvar: the variable holding possible installation directories
#
function(SciGetInstSubdirs pkgname instdirsvar)
  string(TOLOWER ${pkgname} pkgnamelc)
  if (DEBUG_CMAKE)
    message(STATUS "SciGetInstSubdirs called with pkgname = ${pkgname} and instdirsvar = ${instdirsvar}.")
    message(STATUS "${pkgname}_ROOT_DIR = ${${pkgname}_ROOT_DIR}.")
    message(STATUS "USE_PYC_LIBS = ${USE_PYC_LIBS}.")
    message(STATUS "USE_SHARED_LIBS = ${USE_SHARED_LIBS}.")
    message(STATUS "BUILD_WITH_SHARED_RUNTIME = ${BUILD_WITH_SHARED_RUNTIME}")
  endif ()
  if (ENABLE_PARALLEL)
    if (USE_SHARED_LIBS)
      set(instdirs ${pkgnamelc}-parsh ${pkgnamelc}-sersh)
      if (ALLOW_SERIAL_WITH_PARALLEL)
        set(instdirs ${instdirs} ${pkgnamelc}-sersh ${pkgnamelc}-sermd)
        if (NOT WIN32)
          set(instdirs ${instdirs} ${pkgnamelc})
        endif ()
      endif ()
    else ()
      set(instdirs ${pkgnamelc}-par ${pkgnamelc}-ben ${pkgnamelc})
    endif ()
  else ()
    if (USE_PYC_LIBS)
      if (USE_SHARED_LIBS)
        set(instdirs ${pkgnamelc}-pycsh ${pkgnamelc}-pycmd ${pkgnamelc}-pycst ${pkgnamelc}-sersh ${pkgnamelc}-sermd)
        if (NOT WIN32)
          set(instdirs ${instdirs} ${pkgnamelc})
        endif ()
      else ()
        set(instdirs ${pkgnamelc}-pycmd ${pkgnamelc}-pycst ${pkgnamelc}-pycsh ${pkgnamelc}-sermd)
        if (NOT WIN32)
          set(instdirs ${instdirs} ${pkgnamelc})
        endif ()
        set(instdirs ${instdirs} ${pkgnamelc}-sersh)
      endif ()
    elseif (USE_SHARED_LIBS)
      set(instdirs ${pkgnamelc}-sersh)
      if (WIN32)
        set(instdirs ${instdirs} ${pkgnamelc}-sermd)
      else ()
        set(instdirs ${instdirs} ${pkgnamelc})
      endif ()
    elseif (BUILD_WITH_SHARED_RUNTIME)
      set(instdirs ${pkgnamelc}-sermd ${pkgnamelc})
    else ()
      set(instdirs ${pkgnamelc})
    endif ()
  endif ()
  # message(STATUS "SciGetInstSubdirs finds instdirs = ${instdirs}.")
  set(${instdirsvar} ${instdirs} PARENT_SCOPE)
endfunction()

# SciGetRootPath
#
# Construct a path, collection of directories, that one is to search
# to find the root dir.
#
# Args:
#  pkgname: the variable holding the package name with regular capitalization
#  instsubdirs: possible installation subdirs
#  rootpathvar: on output, holds possible installation directories
#
function(SciGetRootPath pkgname instsubdirs rootpathvar)

# Get the installation subdirs
  # string(TOLOWER ${pkgname} pkgnamelc)
  # SciGetInstSubdirs(${pkgnamelc} instsubdirs)
  if (DEBUG_CMAKE)
    message(STATUS "SciGetRootPath: instsubdirs = ${instsubdirs}.")
  endif ()
  string(TOUPPER ${pkgname} pkgnameuc)

# Find the possible directories and put them into path
# Order: command-line define, environment variable, supra-search-path subdirs,
# supra-search-path dirs
  if (DEBUG_CMAKE)
    message(STATUS "${pkgname}_ROOT_DIR = ${${pkgname}_ROOT_DIR}.")
    message("[SciFindPackage] SUPRA_SEARCH_PATH = ${SUPRA_SEARCH_PATH}")
  endif ()
  set(rootpath)

# Command-line define overrides all.
  if (${pkgname}_ROOT_DIR)
    SciGetRealDir(${${pkgname}_ROOT_DIR} rootpath)
  else ()
    if (${pkgname}_DIR)
# JRC 20120617: Remove this July 31, 2012.
      message(STATUS "${pkgname}_DIR = ${${pkgname}_DIR}.")
      message(FATAL_ERROR "Use of ${pkgname}_DIR define is removed.  Please use ${pkgname}_ROOT_DIR")
      # SciGetRealDir(${${pkgname}_DIR} rootpath)
    endif ()
# The deprecated variable name is commonly ${scipkguc}_DIR
# MD 20120619: Remove this July 31, 2012.
    if (${pkgnameuc}_DIR)
      message(STATUS "${pkgnameuc}_DIR = ${${pkgnameuc}_DIR}.")
      message(FATAL_ERROR "Use of ${pkgnameuc}_DIR define is removed.  Please use ${pkgname}_ROOT_DIR")
      # SciGetRealDir(${${pkgnameuc}_DIR} rootpath)
    endif ()
  endif ()
  if (DEBUG_CMAKE)
    message(STATUS "${pkgname}_ROOT_DIR = ${${pkgname}_ROOT_DIR}.")
    message(STATUS "USE_PYC_LIBS = ${USE_PYC_LIBS}.")
    message(STATUS "USE_SHARED_LIBS = ${USE_SHARED_LIBS}.")
  endif ()

# Next try environment variable.  Should this be appended regardless?
  if (NOT DEFINED ${rootpath})
    if ($ENV{${pkgname}_ROOT_DIR})
      SciGetRealDir($ENV{${pkgname}_ROOT_DIR} rootpath)
    else ()
      if ($ENV{${pkgname}_DIR})
# JRC 20120617: Remove this July 31, 2012.
        message(FATAL_ERROR "Use of ${pkgname}_DIR environment variable is removed .  Please use ${pkgname}_ROOT_DIR")
        # SciGetRealDir($ENV{${pkgname}_DIR} rootpath)
      endif ()
      if ($ENV{${pkgnameuc}_DIR})
# MD 20120619: Remove this July 31, 2012.
        message(FATAL_ERROR"Use of ${pkgnameuc}_DIR environment variable is removed.  Please use ${pkgname}_ROOT_DIR")
        # SciGetRealDir($ENV{${pkgnameuc}_DIR} rootpath)
      endif ()
    endif ()
  endif ()
  if (DEBUG_CMAKE)
    message(STATUS "After looking for ${pkgname}_ROOT_DIR in the environment, rootpath = ${rootpath}.")
  endif ()

# Supra-search-path dirs
  foreach (instdir ${instsubdirs})
    foreach (spdir ${SUPRA_SEARCH_PATH})
      set(idir ${spdir}/${instdir})
      if (EXISTS ${idir})
        SciGetRealDir(${idir} scidir)
        set(rootpath ${rootpath} ${scidir})
      else ()
        if (DEBUG_CMAKE)
          message(STATUS "${idir} does not exist.")
        endif ()
      endif ()
    endforeach (spdir ${SUPRA_SEARCH_PATH})
  endforeach ()

# Supra-search-path dirs
  foreach (spdir ${SUPRA_SEARCH_PATH})
    set(idir ${spdir})
    if (EXISTS ${idir})
      SciGetRealDir(${idir} scidir)
      set(rootpath ${rootpath} ${scidir})
    endif ()
  endforeach (spdir ${SUPRA_SEARCH_PATH})

# Any found?
  list(LENGTH rootpath rootpathlen)
  if (DEBUG_CMAKE)
    if (rootpathlen)
      list(REMOVE_DUPLICATES rootpath)
      message(STATUS "rootpath = ${rootpath}")
    else ()
      message(FATAL_ERROR "rootpath is empty.")
    endif ()
  endif ()

# Return value
  set(${rootpathvar} ${rootpath} PARENT_SCOPE)
endfunction()

# SciFindPkgFiles
#
# Find the files of a type for a package.
#
# Args:
#  pkgname: the variable holding the package name with regular capitalization
#  pkgfiles: the files to search for
#  rootpath: contains possible installation root directories
#  filesubdirs: possible file subdirs
#  singularsfx: singular version of the file type
#  pluralsfx: plural version of the file type
#  allfoundvar: whether all files were found
#  allowdups: whether duplicates are allowed in list
#  rootpathvar: the variable in which the rootpath is stored
#
function(SciFindPkgFiles pkgname pkgfiles
  rootpath filesubdirs
  singularsfx pluralsfx allfoundvar rootpathvar)

  if (DEBUG_CMAKE)
    message(STATUS "Looking for files of type, ${singularsfx} under ${rootpath} with filesubdirs = ${filesubdirs}.")
  endif ()

# Set the package path

# Loop over list of executables and try to find each one
  set(abspkgfiles)
  set(pkgdirs)
  set(allfound TRUE)
  foreach (pkgfile ${pkgfiles})

# Get real name
    if (${singularsfx} STREQUAL MODULE)
      if (NOT SCI_FC_MODULE_SUFFIX)
        message(FATAL_ERROR "SCI_FC_MODULE_SUFFIX not set.  Fortran compiler defined?")
      endif ()
      set(realpkgfile ${pkgfile}.${SCI_FC_MODULE_SUFFIX})
    elseif (${singularsfx} STREQUAL DLL)
      set(realpkgfile ${pkgfile}.dll)
    else ()
      set(realpkgfile ${pkgfile})
    endif ()

# Create the variable that's specific to this executable
    string(REGEX REPLACE "[./-]" "_" pkgfilevar ${realpkgfile})
    set(pkgfilevar ${pkgname}_${pkgfilevar})
    set(pkgfilevarfile)
    if (${singularsfx} STREQUAL LIBRARY)
      set(pkgfilevar ${pkgfilevar}_LIBRARY)
    elseif (${singularsfx} STREQUAL INCLUDE)
      set(pkgfilevarfile ${pkgfilevar})
      set(pkgfilevar ${pkgfilevar}_INCLUDE_DIR)
    endif ()
    if (DEBUG_CMAKE)
      message(STATUS "Searching for pkgfile = ${pkgfile}.")
    endif ()

# First look in specified path
    set(basesrchargs ${pkgfilevar} ${realpkgfile})
    set(pkgfiledir)
    set(fullsrchargs "${basesrchargs}"
      PATHS "${rootpath}"
      PATH_SUFFIXES "${filesubdirs}"
      NO_DEFAULT_PATH
    )
    if (DEBUG_CMAKE)
      message(STATUS "fullsrchargs = ${fullsrchargs}.")
    endif ()
    if (${singularsfx} STREQUAL PROGRAM)
      find_program(${fullsrchargs}
        DOC " The ${realpkgfile} ${singularsfx} file"
      )
    elseif (${singularsfx} STREQUAL INCLUDE)
      find_path(${fullsrchargs}
        DOC " Directory containing the ${realpkgfile} ${singularsfx}"
      )
      if (${pkgfilevar})
        set(pkgfiledir ${${pkgfilevar}})
        set(${pkgfilevarfile} ${pkgfiledir}/${realpkgfile})
      endif ()
    elseif (${singularsfx} STREQUAL LIBRARY)
      find_library(${fullsrchargs}
        DOC " The ${realpkgfile} ${singularsfx} file"
      )
    else ()
      find_file(${fullsrchargs}
        DOC " The ${realpkgfile} ${singularsfx} file"
      )
    endif ()
    if (DEBUG_CMAKE)
      message(STATUS "From first search: ${pkgfilevar} = ${${pkgfilevar}}.")
    endif ()

# If not found, try again with default paths
    if (NOT ${pkgfilevar})
      if (DEBUG_CMAKE)
        message(STATUS "Failed to find ${realpkgfile} in search path, trying default paths.")
      endif ()
      if (${singularsfx} STREQUAL PROGRAM)
        find_program(${basesrchargs}
          DOC " The ${realpkgfile} ${singularsfx} file"
        )
      elseif (${singularsfx} STREQUAL INCLUDE)
        find_path(${basesrchargs}
          DOC " Directory containing the ${realpkgfile} ${singularsfx}"
        )
        if (${pkgfilevar})
          set(pkgfiledir ${${pkgfilevar}})
          set(${pkgfilevarfile} ${pkgfiledir}/${realpkgfile})
        endif ()
      elseif (${singularsfx} STREQUAL LIBRARY)
        find_library(${basesrchargs}
          DOC " The ${realpkgfile} ${singularsfx} file"
        )
      else ()
        find_file(${basesrchargs}
          DOC " The ${realpkgfile} ${singularsfx} file"
        )
      endif ()
      if (DEBUG_CMAKE)
        message(STATUS "From second search: ${pkgfilevar} = ${${pkgfilevar}}.")
      endif ()
    endif ()

    if (${pkgfilevar})
# Add to list of all files of this type for this package
      set(abspkgfiles ${abspkgfiles} ${${pkgfilevar}})
      if (NOT pkgfiledir)
        get_filename_component(pkgfiledir ${${pkgfilevar}}/.. REALPATH)
      endif ()
      if (${singularsfx} STREQUAL INCLUDE)
        set(${pkgfilevar} ${pkgfiledir}
          CACHE STRING " Directory containing ${pkgfiledir}"
        )
        set(${pkgfilevarfile} ${${pkgfilevarfile}}
          CACHE STRING " Full path to ${realpkgfile}}"
        )
      endif ()
      set(pkgdirs ${pkgdirs} ${pkgfiledir})
    else ()
# The WARNING option will actually give a scimake stack trace.
# Not wanted, so use NO option, and start the string with WARNING
      message("WARNING - Unable to locate file, ${realpkgfile}.")
      set(allfound FALSE)
    endif ()
  endforeach ()

# Clean up the lists of files and directories
  list(LENGTH abspkgfiles numpkgfiles)
  if (numpkgfiles)
    if (NOT ${ALLOWDUPS})
      list(REMOVE_DUPLICATES abspkgfiles)
    endif ()
    list(REMOVE_DUPLICATES pkgdirs)
  endif ()
  list(LENGTH pkgdirs lenpkgdirs)

# For libraries, get names
  if (${singularsfx} STREQUAL LIBRARY)
    set(pkgfilenames)
    foreach (pkgfile ${abspkgfiles})
      get_filename_component(pkgfilename ${pkgfile} NAME_WE)
      string(REGEX REPLACE "^lib" "" pkgfilename "${pkgfilename}")
      set(pkgfilenames ${pkgfilenames} ${pkgfilename})
    endforeach ()
  endif ()

# Print results if in debug mode
  if (DEBUG_CMAKE)
    message(STATUS "Setting:")
    message(STATUS "  ${pkgname}_${pluralsfx} = ${abspkgfiles}.")
    if (${singularsfx} STREQUAL LIBRARY)
      message(STATUS "  ${pkgname}_${singularsfx}_NAMES = ${pkgfilenames}.")
    endif ()
    message(STATUS "  ${pkgname}_${singularsfx}_DIRS = ${pkgdirs}.")
  endif ()

# Set the return vars
  set(${pkgname}_${pluralsfx} ${abspkgfiles}
    CACHE STRING " List of all files of type, ${singularsfx}, for ${pkgname}"
  )
  set(${pkgname}_${singularsfx}_DIRS ${pkgdirs}
    CACHE STRING " List of all directories for files of type, ${singularsfx}, for ${pkgname}"
  )
  if (${singularsfx} STREQUAL LIBRARY)
    set(${pkgname}_${singularsfx}_NAMES ${pkgfilenames}
      CACHE STRING " List of all file names for files of type, ${singularsfx}, for ${pkgname}"
    )
    set(${pkgname}_${singularsfx}_NAMES ${pkgfilenames} PARENT_SCOPE)
  endif ()
  set(${pkgname}_${pluralsfx} ${abspkgfiles} PARENT_SCOPE)
  set(${pkgname}_${singularsfx}_DIRS ${pkgdirs} PARENT_SCOPE)
  set(${allfoundvar} ${allfound} PARENT_SCOPE)

endfunction()

#
# SciFindPackage
#
# Args:
# NOPRINT: Do not print the results
# PACKAGE: the prefix for the defined variables
# INSTALL_DIRS: the names for the installation subdirectory.  Defaults
#   to lower cased scipkgname
# PROGRAMS: the executables to look for
# HEADERS: the header files to look for.
# LIBRARIES: the libraries
# FILES: other files to look for
# MODULES: Fortran module files
# PROGRAM_SUBDIRS: executable subdirs
# INCLUDE_SUBDIRS: include subdirectories
# LIBRARY_SUBDIRS: library subdirectories
# FILE_SUBDIRS: file subdirectories
#
# NOTE: One cannot reset calling variables
# NOTE: lists should be delimited by semicolons.
#(which is the default format used by scimake)
#

include(CMakeParseArguments)
macro(SciFindPackage)
# Set default values
  set(TFP_FIND_QUIETLY)
  CMAKE_PARSE_ARGUMENTS(TFP
    "FIND_QUIETLY;ALLOW_LIBRARY_DUPLICATES;FIND_CONFIG_FILE;CONFIG_FILE_ONLY;USE_CONFIG_FILE"
    "PACKAGE;INSTALL_DIR;CONFIG_FILE_NAME"
    "INSTALL_DIRS;PROGRAMS;HEADERS;LIBRARIES;FILES;MODULES;CONFIG_SUBDIRS;PROGRAM_SUBDIRS;INCLUDE_SUBDIRS;MODULE_SUBDIRS;LIBRARY_SUBDIRS;FILE_SUBDIRS"
    ${ARGN}
  )

  set(TFP_DLL_SUBDIRS ${TFP_PROGRAM_SUBDIRS} bin lib .)

# This message is purposefully NOT a STATUS message
# To provide more readable output
  if (NOT DEBUG_CMAKE AND TFP_FIND_QUIETLY)
    message(STATUS "Looking for ${TFP_PACKAGE}.")
  else ()
    message("")
    message("--------- SciFindPackage looking for ${TFP_PACKAGE} ---------")
  endif ()

  if (DEBUG_CMAKE)
    message(STATUS "Outputting debug information.")
    message(STATUS "${TFP_PACKAGE}_ROOT_DIR=${${TFP_PACKAGE}_ROOT_DIR}")
    message(STATUS "SciFindPackage called with arguments:
      PACKAGE          = ${TFP_PACKAGE}
      CONFIG_FILE_NAME = ${TFP_CONFIG_FILE_NAME}
      INSTALL_DIR      = ${TFP_INSTALL_DIR}
      INSTALL_DIRS     = ${TFP_INSTALL_DIRS}
      PROGRAMS         = ${TFP_PROGRAMS}
      HEADERS          = ${TFP_HEADERS}
      LIBRARIES        = ${TFP_LIBRARIES}
      MODULES          = ${TFP_MODULES}
      CONFIG_SUBDIRS   = ${TFP_CONFIG_SUBDIRS}
      PROGRAM_SUBDIRS  = ${TFP_PROGRAM_SUBDIRS}
      INCLUDE_SUBDIRS  = ${TFP_INCLUDE_SUBDIRS}
      LIBRARY_SUBDIRS  = ${TFP_LIBRARY_SUBDIRS}
      FILE_SUBDIRS     = ${TFP_FILE_SUBDIRS}
      FIND_QUIETLY     = ${TFP_FIND_QUIETLY}
      ALLOW_LIBRARY_DUPLICATES = ${TFP_ALLOW_LIBRARY_DUPLICATES}
      FIND_CONFIG_FILE = ${TFP_USE_CONFIG_FILE}
      CONFIG_FILE_ONLY = ${TFP_CONFIG_FILE_ONLY}
      USE_CONFIG_FILE  = ${TFP_USE_CONFIG_FILE}"
    )
  endif ()

  if (${TFP_PACKAGE}_FOUND)
    if (DEBUG_CMAKE)
      message(STATUS "Already found this package, skipping...")
    endif ()
  endif ()

# Construct various names(upper/lower case) for package
  string(REGEX REPLACE "[./-]" "_" scipkgreg ${TFP_PACKAGE})
# scipkgreg is the regularized package name
  string(TOUPPER ${scipkgreg} scipkguc)
  if (DEBUG_CMAKE)
    message(STATUS "ENABLE_${scipkguc} = ${ENABLE_${scipkguc}}")
  endif ()
  if (DISABLE_${scipkguc})
    set(ENABLE_${scipkguc} FALSE)
  endif ()
  if (NOT DEFINED ENABLE_${scipkguc})
    set(ENABLE_${scipkguc} TRUE)
  endif ()
  if (NOT ENABLE_${scipkguc})
    message(STATUS "Disabling ${scipkgreg}.")
    set(${scipkguc}_FOUND FALSE)
    if (DEBUG_CMAKE)
      message(STATUS "${scipkguc}_FOUND set to FALSE.")
    endif ()
    return()
  endif ()
# Set to true until not something not found
  set(${scipkguc}_FOUND TRUE)
  string(TOLOWER ${scipkgreg} scipkglc)
  set(scipkginst ${TFP_INSTALL_DIR} ${TFP_INSTALL_DIRS})
  if (NOT scipkginst)
    SciGetInstSubdirs(${scipkglc} scipkginst)
  endif ()
  if (DEBUG_CMAKE)
    message(STATUS "scipkginst = ${scipkginst}.")
  endif ()

# Find the set of possible root installation dirs
  SciGetRootPath(${scipkgreg} "${scipkginst}" origscipath)
  if (DEBUG_CMAKE)
    message(STATUS "scipath = ${scipath}")
  endif ()
  set(scipath "${origscipath}")

#######################################################################
#
# Look for cmake configuration files
# Variables defined:
#   Xxx_CONFIG_FILE: path to the PkgConfig.cmake
#   Xxx_CONFIG_VERSION_FILE: path to the PkgConfigVersion.cmake
#
#######################################################################

  if (TFP_FIND_CONFIG_FILE OR TFP_USE_CONFIG_FILE OR TFP_CONFIG_FILE_ONLY)

# Get the config file
    set(sciconfigcmvar "${scipkgreg}_CONFIG_CMAKE")
    if (CONFIG_FILE_NAMES)
      set(confnames ${CONFIG_FILE_NAMES})
    else ()
      set(confnames ${scipkgreg}Config.cmake ${scipkglc}-config.cmake)
    endif ()
    set(confdirs ${TFP_CONFIG_SUBDIRS} lib/cmake/${scipkgreg} share/cmake/${scipkglc} cmake/${scipkglc})
    message(STATUS "Looking for ${confnames} in ${confdirs}.")
    find_file(${sciconfigcmvar}
      NAMES ${confnames}
      PATHS ${scipath}
      PATH_SUFFIXES ${confdirs}
      NO_DEFAULT_PATH
      DOC "Path to the hdf5 config files."
    )

# If not found, look in system directories
    if (NOT ${scipkgreg}_CONFIG_CMAKE)
      find_file(${scipkgreg}_CONFIG_CMAKE
        NAMES ${confnames}
      )
    endif ()

# If found, source
    if (${scipkgreg}_CONFIG_CMAKE)
      get_filename_component(${scipkgreg}_CONFIG_DIR ${${scipkgreg}_CONFIG_CMAKE}/.. REALPATH)
      set(confvernames ${scipkgreg}ConfigVersion.cmake ${scipkglc}-config-version.cmake)
      if (DEBUG_CMAKE)
        message(STATUS "Looking for ${confvernames} in ${${scipkgreg}_CONFIG_DIR}.")
      endif ()
      find_file(${scipkgreg}_CONFIG_VERSION_CMAKE
        NAMES ${confvernames}
        PATHS ${${scipkgreg}_CONFIG_DIR}
      )
      if (TFP_USE_CONFIG_FILE AND ${scipkgreg}_CONFIG_CMAKE)
        include(${${scipkgreg}_CONFIG_CMAKE})
      endif ()
    endif ()
    if (DEBUG_CMAKE)
      SciPrintVar(${scipkgreg}_CONFIG_CMAKE)
      SciPrintVar(${scipkgreg}_CONFIG_VERSION_CMAKE)
    endif ()
    if (TFP_CONFIG_FILE_ONLY)
      if (DEBUG_CMAKE OR NOT TFP_FIND_QUIETLY)
        SciPrintCMakeResults(${scipkgreg})
      endif ()
    endif ()

  endif ()

#######################################################################
#
# Look for TYPES = PROGRAM, INCLUDE, MODULE, LIBRARY, FILE
# Variables defined:
#   For PROGRAM, INCLUDE, MODULE, FILE
#     Xxx_yyy - CACHED
#       Where to find the yyy file that comes with Xxx.
#   For LIBRARY
#     Xxx_yyy_LIBRARY - CACHED
#       Where to find the yyy file that comes with Xxx.
#   Xxx_${TYPE_PLURAL} - CACHED
#     List of all files of that type found for package Xxx.
#
#######################################################################

  if (NOT TFP_CONFIG_FILE_ONLY)
# Create the search paths
    set(scitypes PROGRAM INCLUDE MODULE LIBRARY FILE)
# Not clear what is right here
    # if (WIN32 AND (BUILD_SHARED_LIBS))
    if (WIN32)
      set(scitypes ${scitypes} DLL)
    endif ()
    foreach (scitype ${scitypes})

# Get plural
      if (${scitype} STREQUAL LIBRARY)
        set(scitypeplural LIBRARIES)
      elseif (${scitype} STREQUAL INCLUDE)
        set(scitypeplural HEADERS)
      else ()
        set(scitypeplural ${scitype}S)
      endif ()

# Get list containing files to search for
      if (${scitype} STREQUAL DLL)
        set(srchfilesvar TFP_LIBRARIES)
      else ()
        set(srchfilesvar TFP_${scitypeplural})
      endif ()

# Set whether optional search
      set(srchoptional FALSE)
      if (${scitype} STREQUAL DLL)
# DLL search always optional
        set(srchoptional TRUE)
      elseif (${srchfilesvar})
# Matters only if there is something to search for
        list(FIND ${srchfilesvar} OPTIONAL optind)
        if (NOT ${optind} EQUAL -1)
# If find OPTIONAL, then remove that from list and set optional true
          list(REMOVE_AT ${srchfilesvar} ${optind})
          set(srchoptional TRUE)
        endif ()
      endif ()

# If list not empty, search for files
      list(LENGTH ${srchfilesvar} scisrchlen)
      if (scisrchlen)

# Create lists for search
        list(LENGTH TFP_${scitype}_SUBDIRS scilen)
        if (scilen)
          set(scifilesubdirs ${TFP_${scitype}_SUBDIRS})
        else ()
# Default search subdirectories
          if (${scitype} STREQUAL PROGRAM)
            set(scifilesubdirs bin)
          elseif (${scitype} STREQUAL FILE)
            set(scifilesubdirs share)
          elseif (${scitype} STREQUAL INCLUDE)
            set(scifilesubdirs include)
          elseif (${scitype} STREQUAL LIBRARY)
            set(scifilesubdirs lib)
          elseif (${scitype} STREQUAL DLL)
            set(scifilesubdirs bin lib .)
          elseif (${scitype} STREQUAL MODULE)
            set(scifilesubdirs include)
          else ()
            message(WARNING "Default subdir not known for ${scitype}.")
          endif ()
        endif ()
        message(STATUS "Looking for ${scitypeplural}, ${${srchfilesvar}}, in ${scifilesubdirs} with OPTIONAL = ${srchoptional}.")

# Find the files
        SciFindPkgFiles(${scipkgreg} "${${srchfilesvar}}"
          "${scipath}" "${scifilesubdirs}"
          ${scitype} ${scitypeplural} ${scipkgreg}_${scitypeplural}_FOUND
          FALSE    # formerly ${TFP_ALLOW_LIBRARY_DUPLICATES} - Lawrence
        )
# Okay not to find dlls
        if (NOT ${scipkgreg}_${scitypeplural}_FOUND)
          if (NOT ${scitype} STREQUAL DLL)
            message(WARNING "${scipkgreg}_${scitypeplural}_FOUND = ${${scipkgreg}_${scitypeplural}_FOUND}.")
          endif ()
          if (NOT ${srchoptional})
            set(${scipkguc}_FOUND FALSE)
          endif ()
        endif ()
      endif ()

# At the end of the LIBRARY include, find the rootdir, and use it
# for further searches
      if ((${scitype} STREQUAL LIBRARY) AND ${scipkgreg}_INCLUDE_DIRS)
        list(GET ${scipkgreg}_INCLUDE_DIRS 0 rootdir)
        get_filename_component(rootdir "${rootdir}" DIRECTORY)
        get_filename_component(subdir "${rootdir}" NAME)
        if (${subdir} STREQUAL visit OR ${subdir} MATCHES "^vtk-6.1")
          get_filename_component(rootdir "${rootdir}" DIRECTORY)
        endif ()
        get_filename_component(subdir "${rootdir}" NAME)
        if (${subdir} STREQUAL include)
          get_filename_component(rootdir "${rootdir}" DIRECTORY)
        endif ()
# We should require rootdir to be a part of all library dirs, removing
# subdirs until this happens.
        foreach (libdir ${${scipkgreg}_LIBRARY_DIRS})
        endforeach ()
        set(scipath ${rootdir})
        message(STATUS "scipath = ${scipath}.")
      endif ()

    endforeach ()

# Find static libraries
    if (${scipkgreg}_LIBRARIES)
      SciGetStaticLibs("${${scipkgreg}_LIBRARIES}" ${scipkgreg}_STLIBS)
    endif ()

    if (${scipkgreg}_DLLS)
      set(${scipkgreg}_DEFINITIONS -D${scipkguc}_DLL)
    endif ()

  endif ()

####################################################################
#
# For the package to be marked as found, all requested objects must
# have been found, as coded above.
# Per http://www.cmake.org/Wiki/scimake:How_To_Find_Installed_Software,
# The convention is to capitalize the _FOUND variable.
#
####################################################################

  if (DEBUG_CMAKE OR NOT TFP_FIND_QUIETLY)
    SciPrintCMakeResults(${scipkgreg})
  endif ()

# If this was marked as a required package, fail
  if (${scipkgreg}_FIND_REQUIRED AND NOT ${scipkguc}_FOUND)
    message(FATAL_ERROR "Unable to find required package ${scipkgreg} - failing")
  endif ()

  if (DEBUG_CMAKE OR NOT TFP_FIND_QUIETLY)
    message(STATUS "${scipkguc}_FOUND = ${${scipkguc}_FOUND}.")
    message("--------- SciFindPackage done with ${TFP_PACKAGE} -----------")
  endif ()

endmacro(SciFindPackage)
