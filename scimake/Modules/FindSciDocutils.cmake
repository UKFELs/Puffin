######################################################################
#
# FindSciDocutils
#
# $Id: FindSciDocutils.cmake 1079 2016-09-09 00:05:24Z cary $
#
# Copyright 2013-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################
include(FindPackageHandleStandardArgs)

find_program(RST2HTML_EXECUTABLE NAMES rst2html rst2html.py)
find_package_handle_standard_args(Docutils DEFAULT_MSG RST2HTML_EXECUTABLE)

find_program(RST2LATEX_EXECUTABLE NAMES rst2latex rst2latex.py)
find_package_handle_standard_args(Docutils DEFAULT_MSG RST2LATEX_EXECUTABLE)

