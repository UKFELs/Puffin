# - SciSphinxFunctions:
# Useful functions for simplifying the setting up of sphinx targets
#
# All functions assume that FindSciSphinx was used and the following are
# defined:
#   Sphinx_EXECUTABLE     = The path to the sphinx command.
#   Sphinx_OPTS           = Options for sphinx
#
# The SciSphinxTarget automates the creation of build and install
# targets.  The make targets are not added to all, and the install's are
# optional.  To make the install options work with install, use the
# add_dependencies command.  For example:
#  add_dependencies(install install-userdocs)
#

#################################################################
#
# SciSphinxFunction
#
# $Id: SciSphinxFunctions.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright &copy; 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#################################################################

include(CMakeParseArguments)

# SciSphinxTarget.cmake
# Automate the defining of the CMake targets
# Args:
#   TARGET:        Target basename.  Targets will be ${TARGET}-<build>,
#                  where <build> is one of html, latex, or pdf.
#   RST_FILE_BASE: Root name of Latex file.  From conf.py
#   SOURCE_DIR:    Directory containing the index.rst.  Defaults
#                  to CMAKE_CURRENT_SOURCE_DIR
#   SPHINX_ADDL_OPTS: Additional options to Sphinx
#   SPHINX_DOCTREE_DIR: Select cache directory (default .doctrees)
#   FILE_DEPS:      Files that are the dependencies.
#   SPHINX_BUILDS:  Which builds to include.  Default is "html latex pdf"
#                   Possible choices are "html latex pdf singlehtml man"
#   SPHINX_INSTALLS: Which builds to install.  Default is same as builds
#   NOWARN_NOTMATCH_DIR: Do not warn if file base does not match install dir
#   INSTALL_SUPERDIR: Name of installation directory up to this one.
#                     Should not be absolute (not include prefix).
#                     Overridden by INSTALL_SUBDIR.
#   INSTALL_SUBDIR:   Name of this subdir for installation.
#                     Should not be absolute (not include prefix).
#
macro(SciSphinxTarget)

# Parse out the args
  set(opts DEBUG;NOWARN_NOTMATCH_DIR) # no-value args
  set(oneValArgs RST_FILE_BASE;TARGET;SPHINX_ADDL_OPTS;SPHINX_DOCTREE_DIR;
        SOURCE_DIR;INSTALL_SUPERDIR;INSTALL_SUBDIR)
  set(multValArgs FILE_DEPS;ALL_BUILDS) # e.g., lists
  cmake_parse_arguments(FD "${opts}" "${oneValArgs}" "${multValArgs}" ${ARGN})
  #
  # Defaults
  #
  if (NOT DEFINED FD_SOURCE_DIR)
    set(FD_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
  endif ()
  if (NOT DEFINED FD_SPHINX_BUILDS)
    set(FD_SPHINX_BUILDS)
    list(APPEND FD_SPHINX_BUILDS html latex pdf)
  endif ()
  if (NOT DEFINED FD_SPHINX_INSTALLS)
    set(FD_SPHINX_INSTALLS ${FD_SPHINX_BUILDS})
  endif ()
  if (FD_INSTALL_SUBDIR)
    set(instdir ${FD_INSTALL_SUBDIR})
  elseif (FD_INSTALL_SUPERDIR)
    set(instdir ${FD_INSTALL_SUPERDIR}/${thissubdir})
  else ()
    set(instdir ${CMAKE_INSTALL_PREFIX})
  endif ()
  if (NOT DEFINED USE_WHOOSH )
    set(USE_WHOOSH false)
  endif ()

  #
  #  Basic sanity checks
  #
  get_filename_component(thissubdir ${CMAKE_CURRENT_SOURCE_DIR} NAME)
  if (NOT NOWARN_NOTMATCH_DIR)
    set(WARN_NOTMATCH_DIR)
  endif ()
  if (WARN_NOTMATCH_DIR)
    if (NOT "${thissubdir}" STREQUAL "${FD_RST_FILE_BASE}")
      message(WARNING "Main rst file base, ${FD_RST_FILE_BASE}, does not match subdirectory name, ${thissubdir}.")
    endif ()
  endif ()

  if (NOT DEFINED FD_TARGET)
    message(WARNING "SciSphinxTarget called without specifying the target name")
    return()
  endif ()
  if (NOT DEFINED FD_FILE_DEPS)
    message(WARNING "SciSphinxTarget called without specifying the file dependencies")
    return()
  endif ()
  if (NOT DEFINED FD_RST_FILE_BASE)
    message(WARNING "SciSphinxTarget called without specifying the latex root from conf.py")
    return()
  endif ()
  if (NOT DEFINED Sphinx_EXECUTABLE)
    message(WARNING "SciSphinxTarget called without defining Sphinx_EXECUTABLE")
    return()
  endif ()
  if (FD_DEBUG)
    message("")
    message("--------- SciSphinxTarget defining targets for ${FD_TARGET} ---------")
    message(STATUS "[SciSphinxFunctions]: TARGET= ${FD_TARGET} ")
    message(STATUS "[SciSphinxFunctions]: RST_FILE_BASE= ${FD_RST_FILE_BASE} ")
    message(STATUS "[SciSphinxFunctions]: Sphinx_EXECUTABLE= ${Sphinx_EXECUTABLE} ")
    message(STATUS "[SciSphinxFunctions]: Sphinx_OPTS= ${Sphinx_OPTS} ")
    message(STATUS "[SciSphinxFunctions]: SPHINX_ADDL_OPTS= ${FD_SPHINX_ADDL_OPTS} ")
    message(STATUS "[SciSphinxFunctions]: SPHINX_DOCTREE_DIR= ${FD_SPHINX_DOCTREE_DIR} ")
  endif ()

  #
  #  Do the standard builds
  #
  if (NOT EXISTS ${FD_SOURCE_DIR}/${FD_RST_FILE_BASE}.rst)
     set(html_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/html/${FD_RST_FILE_BASE}.html)
  else ()
     set(html_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/html/index.html)
  endif ()
  set(singlehtml_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/singlehtml/index.html)
  set(latex_OUTPUT ${BLDDIR}/pdf/${FD_RST_FILE_BASE}.tex)
  set(pdf_OUTPUT ${BLDDIR}/pdf/${FD_RST_FILE_BASE}.pdf)
  set(man_OUTPUT ${BLDDIR}/man/index.man)

  foreach (build ${FD_SPHINX_BUILDS})
    set (${build}_DIR ${CMAKE_CURRENT_BINARY_DIR}/${build})
# Latex is actually for pdf which is below
    if (${build} STREQUAL latex)
      set (${build}_DIR ${CMAKE_CURRENT_BINARY_DIR}/pdf)
    endif ()

# There is something weird about passing blank spaces into COMMAND
# so this method fixes the problems that arise if Sphinx_OPTS is not defined
    set(all_opts -b ${build} -c ${CMAKE_CURRENT_BINARY_DIR} ${Sphinx_OPTS} ${FD_SPHINX_ADDL_OPTS} ${FD_SPHINX_DOCTREE_DIR})
    if (NOT ${build} STREQUAL pdf)
      add_custom_command(
        OUTPUT ${${build}_OUTPUT}
        COMMAND ${Sphinx_EXECUTABLE}
        ARGS ${all_opts} ${FD_SOURCE_DIR} ${${build}_DIR}
        DEPENDS ${FD_FILE_DEPS}
      )
      add_custom_target(${FD_TARGET}-${build} DEPENDS ${${build}_OUTPUT})
      if (USE_WHOOSH AND "${build}" STREQUAL html)
        message(STATUS "XXXXXXXXXXX")
        set(whoosh_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/html/whoosh.txt)
        set(all_opts -b whoosh -c ${CMAKE_CURRENT_BINARY_DIR} ${Sphinx_OPTS} ${FD_SPHINX_ADDL_OPTS} ${FD_SPHINX_DOCTREE_DIR})
        add_custom_command(
          OUTPUT ${whoosh_OUTPUT}
          COMMAND ${Sphinx_EXECUTABLE}
          ARGS ${all_opts} ${FD_SOURCE_DIR} ${${build}_DIR}
          DEPENDS ${FD_FILE_DEPS}
        )
        add_custom_target(${FD_TARGET}-whoosh
                          DEPENDS ${whoosh_OUTPUT})
       endif ()
    endif ()
  endforeach ()

  #
  #  PDF is special
  #   This must be make, as sphinx generates a unix makefile
  #
  add_custom_command(
    OUTPUT ${pdf_OUTPUT}
    COMMAND make all-pdf
    DEPENDS ${latex_OUTPUT}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/pdf
  )
  add_custom_target(${FD_TARGET}-pdf DEPENDS ${pdf_OUTPUT})

  #
  #  Each install is a one-off
  #
  list(FIND FD_SPHINX_INSTALLS "pdf" indx)
  if (NOT indx EQUAL -1)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/pdf/${FD_RST_FILE_BASE}.pdf
      DESTINATION "${instdir}"
      PERMISSIONS OWNER_WRITE OWNER_READ GROUP_WRITE GROUP_READ WORLD_READ
      OPTIONAL
    )
  endif ()
  list(FIND FD_SPHINX_INSTALLS "html" indx)
  if (NOT indx EQUAL -1)
    install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html
      DESTINATION ${instdir}
      FILE_PERMISSIONS OWNER_WRITE OWNER_READ GROUP_WRITE GROUP_READ WORLD_READ
      OPTIONAL
    )
  endif ()
  list(FIND FD_SPHINX_INSTALLS "man" indx)
  if (NOT indx EQUAL -1)
    install(
      DIRECTORY ${BLDDIR}/man
      OPTIONAL
      DESTINATION ${instdir}/man
      COMPONENT userdocs
      OPTIONAL
    )
  endif ()

endmacro()

