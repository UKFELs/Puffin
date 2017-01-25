# - FindSciOpenSsl: Module to find include directories and
#   libraries for OpenSsl.
#
# Module usage:
#   find_package(SciOpenSsl ...)
#
# This module will define the following variables:
#  HAVE_OPENSSL, OPENSSL_FOUND   = Whether libraries and includes are found
#  OpenSsl_INCLUDE_DIRS       = Location of OpenSsl includes
#  OpenSsl_LIBRARY_DIRS       = Location of OpenSsl libraries
#  OpenSsl_LIBRARIES          = Required libraries
#  OpenSsl_DLLS               =

######################################################################
#
# FindOpenSsl: find includes and libraries for openssl
#
# $Id: FindSciOpenSsl.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# OpenSSL builds its static libs inside sersh
if (WIN32)
  if (NOT OpenSsl_ROOT_DIR)
    set(OpenSsl_ROOT_DIRS C:/OpenSSL C:/OpenSSL-Win64 C:/OpenSSL-Win32)
    foreach (rd ${OpenSsl_ROOT_DIRS})
      if (EXISTS ${rd})
        set(OpenSsl_ROOT_DIR ${rd})
        break ()
      endif ()
    endforeach ()
  endif ()
  if (NOT OpenSsl_ROOT_DIR)
    message(WARNING "OpenSsl_ROOT_DIR not found.")
  endif ()
  set(instdirs .)
  set(ssl_libs ssleay32 libeay32)
else ()
  # SciGetInstSubdirs(openssl instdirs)
  set(instdirs openssl openssl-sersh)
  set(ssl_libs ssl crypto)
  if (LINUX)
# gssapi_krb5 required for libssh-0.6.4
    set(ssl_libs ${ssl_libs} gssapi_krb5)
  endif ()
endif ()

SciFindPackage(PACKAGE "OpenSsl"
  INSTALL_DIRS ${instdirs}
  PROGRAMS openssl
  HEADERS openssl/ssl.h
  LIBRARIES ${ssl_libs}
)

# Correct static libraries on Windows
if (WIN32)
  set(srchlibs ${OpenSsl_LIBRARIES})
  set(OpenSsl_STLIBS)
  set(OpenSsl_MDLIBS)
  foreach (lib ${srchlibs})
    get_filename_component(openssl_libdir ${lib}/.. REALPATH)
    get_filename_component(openssl_libname ${lib} NAME_WE)
    find_library(mdlib ${openssl_libname}MD PATHS ${openssl_libdir}/VC/static
      NO_DEFAULT_PATH
    )
    set(OpenSsl_MDLIBS ${OpenSsl_MDLIBS} ${mdlib})
    find_library(stlib ${openssl_libname}MT PATHS ${openssl_libdir}/VC/static
      NO_DEFAULT_PATH
    )
    set(OpenSsl_STLIBS ${OpenSsl_STLIBS} ${stlib})
  endforeach ()
  message(STATUS "After windows search for static libs:")
  SciPrintVar(OpenSsl_STLIBS)
  SciPrintVar(OpenSsl_MDLIBS)
endif ()

# Finish up
if (OPENSSL_FOUND)
  # message(STATUS "Found OpenSsl")
  set(HAVE_OPENSSL 1 CACHE BOOL "Whether have the OPENSSL library")
else ()
  message(STATUS "Did not find OpenSsl.  Use -DOpenSsl_ROOT_DIR to specify the installation directory.")
  if (SciOpenSsl_FIND_REQUIRED)
    message(FATAL_ERROR "Failing.")
  endif ()
endif ()

