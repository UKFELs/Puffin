######################################################################
#
# $Id: SciSvnInfo.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright &copy; 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
# For getting the svn revision of a directory
#
######################################################################
if (0)
macro(Subversion_GET_VERSION dir var1 var2)
  message(WARNING "Subversion_GET_VERSION is deprecated.  Use SciGetSvnInfo.")
  if (EXISTS ${dir}/.svn)
    message(STATUS "In ${dir}, executing ${SVNVERSION_BIN} -c")
    execute_process(COMMAND ${SVNVERSION_BIN} -c
      OUTPUT_VARIABLE ${var1}
      OUTPUT_STRIP_TRAILING_WHITESPACE
      WORKING_DIRECTORY ${dir}
    )
    string(REGEX REPLACE ".*:" "" ${var1} "${${var1}}")
    message(STATUS "In ${dir}, executing ${SVN_BIN} info")
    execute_process(COMMAND ${SVN_BIN} info
      OUTPUT_FILE ${dir}/svninfo.txt
      OUTPUT_STRIP_TRAILING_WHITESPACE
      WORKING_DIRECTORY ${dir}
    )
  else ()
    if (EXISTS ${dir}/svninfo.txt)
      file(READ ${dir}/svninfo.txt ${var1})
      string(REGEX REPLACE "^(.*\n)?Revision:([^\n]+).*"
        "\\2" ${var1} "${${var1}}")
    else ()
      set(${var1} "unknown")
    endif ()
  endif ()
  if (EXISTS ${dir}/svninfo.txt)
      file(READ ${dir}/svninfo.txt SVNINFO)
      string(REGEX REPLACE "^(.*\n)?URL:([^\n]+).*"
        "\\2" ${var2} "${SVNINFO}")
  else ()
      set(${var2} "unknown")
  endif ()
endmacro(Subversion_GET_VERSION)
endif ()

# The new version
macro(SciGetSvnInfo dir revvar urlvar usechanged)
  # message(STATUS "usechanged = ${usechanged}.")
  if (NOT DEFINED usechanged)
    set(usechanged TRUE)
  endif ()
  if (EXISTS ${dir}/.svn)
    if (usechanged)
      # message(STATUS "In ${dir}, executing ${SVNVERSION_BIN} -c")
      execute_process(COMMAND ${SVNVERSION_BIN} -c
        OUTPUT_VARIABLE ${revvar}
        OUTPUT_STRIP_TRAILING_WHITESPACE
        WORKING_DIRECTORY ${dir}
      )
      string(REGEX REPLACE ".*:" "" ${revvar} "${${revvar}}")
    else ()
      # message(STATUS "In ${dir}, executing ${SVNVERSION_BIN}")
      execute_process(COMMAND ${SVNVERSION_BIN}
        OUTPUT_VARIABLE ${revvar}
        OUTPUT_STRIP_TRAILING_WHITESPACE
        WORKING_DIRECTORY ${dir}
      )
    endif ()
    # message(STATUS "In ${dir}, executing ${SVN_BIN} info")
    execute_process(COMMAND ${SVN_BIN} info
      OUTPUT_FILE ${dir}/svninfo.txt
      OUTPUT_STRIP_TRAILING_WHITESPACE
      WORKING_DIRECTORY ${dir}
    )
  else ()
    if (EXISTS ${dir}/svninfo.txt)
      file(READ ${dir}/svninfo.txt ${revvar})
      if (${usechanged})
        string(REGEX REPLACE "^(.*\n)?Last Changed Rev:([^\n]+).*"
          "\\2" ${revvar} "${${revvar}}")
      else ()
        string(REGEX REPLACE "^(.*\n)?Revision:([^\n]+).*"
          "\\2" ${revvar} "${${revvar}}")
      endif ()
    else ()
      set(${revvar} "unknown")
    endif ()
  endif ()
  if (EXISTS ${dir}/svninfo.txt)
    file(READ ${dir}/svninfo.txt SVNINFO)
    string(REGEX REPLACE "^(.*\n)?URL:([^\n]+).*"
        "\\2" ${urlvar} "${SVNINFO}")
  else ()
    set(${urlvar} "unknown")
  endif ()
  if (NOT ${revvar})
    set(${revvar} "unknown")
  endif ()
  if (NOT ${urlvar})
    set(${urlvar} "unknown")
  endif ()
endmacro()

if (SVN_BINDIR)
  set(SVN_PATH ${SVN_BINDIR} ${PATH})
  message("Looking for svn in ${SVN_PATH}")
  find_program(SVN_BIN NAME svn PATHS ${SVN_PATH}
    DOC "subversion command line client" NO_DEFAULT_PATH)
  find_program(SVNVERSION_BIN NAME svnversion PATHS ${SVN_PATH}
    DOC "subversion version command line client" NO_DEFAULT_PATH)
endif ()
# If not found, search in system paths
if (NOT SVN_BIN)
  find_program(SVN_BIN NAME svn PATHS ${SVN_PATH}
    DOC "subversion command line client")
  find_program(SVNVERSION_BIN NAME svnversion PATHS ${SVN_PATH}
    DOC "subversion version command line client")
endif ()
if (DEBUG_CMAKE)
  message(STATUS "SVN_BIN is ${SVN_BIN}")
  message(STATUS "SVNVERSION_BIN is ${SVNVERSION_BIN}")
endif ()
if (SVN_BIN AND SVNVERSION_BIN)
  SciGetSvnInfo(${PROJECT_SOURCE_DIR} PROJECT_REV PROJECT_URL TRUE)
  SciPrintVar(PROJECT_REV)
  SciPrintVar(PROJECT_URL)
  if (EXISTS ${SCIMAKE_DIR})
    SciGetSvnInfo(${SCIMAKE_DIR} SCIMAKE_REV SCIMAKE_URL TRUE)
    SciPrintVar(SCIMAKE_REV)
    SciPrintVar(SCIMAKE_URL)
  endif ()
  if (EXISTS ${PROJECT_SOURCE_DIR}/config)
    SciGetSvnInfo(${PROJECT_SOURCE_DIR}/config CONFIG_REV CONFIG_URL TRUE)
    SciPrintVar(CONFIG_REV)
    SciPrintVar(CONFIG_URL)
  endif ()
endif ()

