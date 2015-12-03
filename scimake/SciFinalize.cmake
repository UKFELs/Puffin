######################################################################
#
# SciFinalize: Do the final stuff for any package
#
# $Id: SciFinalize.cmake 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright 2010-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
######################################################################

######################################################################
#
# Configure standard files
#
######################################################################

if (NOT DEFINED INSTALL_CONFIG_HEADERS)
  set(INSTALL_CONFIG_HEADERS TRUE)
endif ()

foreach (configfile config configrev)
  if (EXISTS ${CMAKE_SOURCE_DIR}/${configfile}-cmake.h.in)
    configure_file(${CMAKE_SOURCE_DIR}/${configfile}-cmake.h.in ${configfile}.h)
    if (INSTALL_CONFIG_HEADERS)
      install(FILES ${CMAKE_BINARY_DIR}/${configfile}.h DESTINATION include
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                    GROUP_READ ${SCI_GROUP_WRITE} GROUP_EXECUTE
                    ${SCI_WORLD_PROGRAM_PERMS}
        RENAME ${CMAKE_PROJECT_NAME}_${configfile}.h
      )
    endif ()
    if (CMAKE_Fortran_COMPILER_WORKS)
      execute_process(
        COMMAND sed -f ${SCIMAKE_DIR}/rmcomms.sed
        INPUT_FILE ${CMAKE_BINARY_DIR}/${configfile}.h
        OUTPUT_FILE ${CMAKE_BINARY_DIR}/${configfile}.f
      )
      if (INSTALL_CONFIG_HEADERS)
        install(FILES ${CMAKE_BINARY_DIR}/${configfile}.f DESTINATION include
          PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                      GROUP_READ ${SCI_GROUP_WRITE} GROUP_EXECUTE
                      ${SCI_WORLD_PROGRAM_PERMS}
          RENAME ${CMAKE_PROJECT_NAME}_${configfile}.f
        )
      endif ()
    endif ()
  elseif (EXISTS ${CMAKE_SOURCE_DIR}/${configfile}-cmake.f.in)
    configure_file(${CMAKE_SOURCE_DIR}/${configfile}-cmake.f.in ${configfile}.f)
    if (INSTALL_CONFIG_HEADERS)
      install(FILES ${CMAKE_BINARY_DIR}/${configfile}.f DESTINATION include
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                    GROUP_READ ${SCI_GROUP_WRITE} GROUP_EXECUTE
                    ${SCI_WORLD_PROGRAM_PERMS}
        RENAME ${CMAKE_PROJECT_NAME}_${configfile}.f
      )
    endif ()
  endif ()
endforeach ()
if (EXISTS ${CMAKE_BINARY_DIR}/config.h)
  set(HAVE_CONFIG_H TRUE)
endif ()
if (EXISTS ${CMAKE_BINARY_DIR}/config.f)
  set(HAVE_CONFIG_F TRUE)
endif ()

######################################################################
#
# Create additional files
#
######################################################################

######################################################################
#
# Install files
#
######################################################################

set(CONFIG_FILES ${CONFIG_FILES}
  ${CONFIG_SUMMARY} ${CMAKE_BINARY_DIR}/scimakeCache.txt
  ${CMAKE_SOURCE_DIR}/svninfo.txt
)

file(GLOB CONFIG_SCRIPTS
  ${CMAKE_BINARY_DIR}/*-${CMAKE_PROJECT_NAME}-*-config.sh
  ${CMAKE_BINARY_DIR}/*-${CMAKE_PROJECT_NAME}-*-build.sh
)

if (COMMON_INSTALL) # Whether all builds install in one place
  if (${ENABLE_PARALLEL})
    set(sharedir share-par)
  else ()
    set(sharedir share-ser)
  endif ()
else ()
  set(sharedir share)
endif ()

install(FILES ${CONFIG_FILES} DESTINATION ${sharedir}
  PERMISSIONS OWNER_READ OWNER_WRITE
              GROUP_READ ${SCI_GROUP_WRITE}
              ${SCI_WORLD_FILE_PERMS}
  OPTIONAL
)
install(PROGRAMS ${CONFIG_SCRIPTS} DESTINATION ${sharedir}
  PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
              GROUP_READ ${SCI_GROUP_WRITE} GROUP_EXECUTE
              ${SCI_WORLD_PROGRAM_PERMS}
)

# Generate autotools files for inclusion
# Important for transitioning between autotools and scimake.
# Used for fciowrappers, ntcc_transport, netlib_lite, fmcfm, facets, etc.
if (EXISTS ${CMAKE_SOURCE_DIR}/configure.ac)
  message(STATUS "make dist will generating autotools files for inclusion in distribution.")
  add_custom_target(cleanconf
    COMMAND config/cleanconf.sh
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  )
  add_dependencies(dist cleanconf)
endif ()

######################################################################
#
# Package source.  Package is responsible for binary packaging.
#
######################################################################

# CPack version numbers for release tarball name.
if (NOT CPACK_PACKAGE_VERSION_PATCH)
  set(CPACK_PACKAGE_VERSION_MAJOR ${VERSION_MAJOR})
  set(CPACK_PACKAGE_VERSION_MINOR ${VERSION_MINOR})
  set(CPACK_PACKAGE_VERSION_PATCH ${VERSION_PATCH}-r${PROJECT_REV})
endif ()

if (NOT DEFINED CPACK_SOURCE_PACKAGE_FILE_NAME)
  set(CPACK_SOURCE_PACKAGE_FILE_NAME
    "${CMAKE_PROJECT_NAME}-${PROJECT_VERSION}-r${PROJECT_REV}"
    CACHE INTERNAL "tarball basename"
  )
endif ()
set(CPACK_SOURCE_GENERATOR TGZ)
set(CPACK_SOURCE_IGNORE_FILES
  "/CVS/;/.svn/;.swp$;.#;/#;/build/;/serial/;/ser/;/parallel/;/par/;~;/preconfig.out;/autom4te.cache/;/.config")
include(CPack)

# add make dist target
add_custom_target(dist COMMAND ${CMAKE_MAKE_PROGRAM} package_source)

######################################################################
#
# Conclude
#
######################################################################

SciPrintString("")
SciPrintString("${CMAKE_PROJECT_NAME} configured with scimake.")

