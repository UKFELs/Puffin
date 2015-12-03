######################################################################
#
# Polyswift.cmake: Compute Polyswift specific options
#
# $Id: SciMpiLauncher.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

# First look in specified path
find_program(${sciexecvar}
  "${sciexec}"
  PATHS ${scipath}
  PATH_SUFFIXES "${sciexecsubdirs}"
  NO_DEFAULT_PATH
  DOC "Path to the ${sciexec} executable"
  )

# MPILAUNCHER for parallel runs
if (NOT DEFINED MPILAUNCHER)
    set(MPILAUNCHER ${MPILAUNCHER:FILEPATH})
endif ()

if (NOT DEFINED NPROCS)
  if (ENABLE_PARALLEL)
    set(NPROCS "2")
  endif ()
endif ()

