# - FindSciMathJax: This module looks for MathJax java code.
# MathJax is a web capable, javascript display engine for mathematics.
# See http://www.mathjax.org
#
# This module works from the variable
#
#  MATHJAXJS =  the full path or url to MathJax.js
#
# This modules defines the following variables:
#
#  MathJax_MathJax_js = the full path or url to MathJax.js
#  MathJax_ROOT_DIR        = If not a url, the directory containing MathJax.js
#

######################################################################
#
# FindSciMathJax
#
# $Id: FindSciMathJax.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# if MATHJAXJS is defined, use that
if (MATHJAXJS)
  message(STATUS "MATHJAXJS = ${MATHJAXJS}.  Will use that.")
  set(MathJax_MathJax_js ${MATHJAXJS})
  if (NOT "${MATHJAXJS}" MATCHES "^http")
    get_filename_component(MathJax_ROOT_DIR ${MathJax_MathJax_js}/.. REALPATH)
  endif ()
  set(MATHJAX_FOUND TRUE)
else ()
  message(STATUS "MATHJAXJS not defined.  Will have to find it.")
# Key of build to find same kind
  if (MATHJAX_ROOTDIR)
    # set(instdirs "INSTALL_DIRS ${MATHJAX_ROOTDIR}")
    SciFindPackage(PACKAGE MathJax INSTALL_DIRS ${MATHJAX_ROOTDIR} FILES MathJax.js)
  elseif ("${CMAKE_INSTALL_PREFIX}" MATCHES "-lite\$")
    # set(instdirs "INSTALL_DIRS MathJax-lite")
    SciFindPackage(PACKAGE MathJax INSTALL_DIRS MathJax-lite FILES MathJax.js)
  else ()
    # set(instdirs "INSTALL_DIRS MathJax")
    SciFindPackage(PACKAGE MathJax INSTALL_DIRS MathJax FILES MathJax.js)
  endif ()
  # SciFindPackage(PACKAGE MathJax ${instdirs} FILES MathJax.js)
  get_filename_component(MathJax_ROOT_DIR ${MathJax_MathJax_js}/.. REALPATH)
endif ()
message(STATUS "  MathJax_ROOT_DIR          = ${MathJax_ROOT_DIR}")
message(STATUS "  MathJax_MathJax_js   = ${MathJax_MathJax_js}")

