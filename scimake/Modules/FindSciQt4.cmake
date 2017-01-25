######################################################################
#
# FindSciQt4: find includes and libraries for Qt4
#
# $Id: FindSciQt4.cmake 975 2016-01-09 20:04:17Z cary $
#
# Copyright 2012-2016, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
#
# The cmake find of Qt4 needs cleaning up.
#
######################################################################

# Determined how to do this from reading documentation of FindQt4.cmake
#   find_package(Qt4 4.4.3 COMPONENTS QtCore QtGui QtXml REQUIRED )
#   include(${QT_USE_FILE})
#   add_executable(myexe main.cpp)
#   target_link_libraries(myexe ${QT_LIBRARIES})

message("")
message("--------- FindSciQt4 looking for Qt4 ---------")

if (SciQt4_FIND_COMPONENTS)
  message(STATUS "Looking for Qt with version greater than 4.7.1 with components: ${SciQt4_FIND_COMPONENTS}")
  find_package(Qt4 4.7.1 COMPONENTS ${SciQt4_FIND_COMPONENTS} REQUIRED)
else ()
  message(STATUS "Looking for Qt with version greater than 4.7.1, components not specified")
  find_package(Qt4 4.7.1 REQUIRED)
endif ()

# Use file sets up variables
if (DEBUG_CMAKE)
  message(STATUS "QT_USE_FILE = ${QT_USE_FILE}")
endif ()
include(${QT_USE_FILE})
# Regularize the variable the FindQt4 sets
set(QT_INCLUDE_DIRS ${QT_INCLUDES})
set(QT_LIBRARY_DIRS ${QT_LIBRARY_DIR})
if (EXISTS ${QT_BINARY_DIR}/qmake)
  set(QT_QMAKE_EXECUTABLE ${QT_BINARY_DIR}/qmake)
endif ()
set(QT_PROGRAMS
  ${QT_QMAKE_EXECUTABLE}
  ${QT_MOC_EXECUTABLE}
  ${QT_UIC_EXECUTABLE}
)

# Add in optional libaries to QT_LIBARIES, if they are found
foreach (qtoptlib ${QT_OPTIONAL_LIBRARIES})
 string(TOUPPER ${qtoptlib} _uppercaseoptlib )
 if (QT_${_uppercaseoptlib}_FOUND)
   set(QT_LIBRARIES ${QT_LIBRARIES} ${QT_${_uppercaseoptlib}_LIBRARY})
 endif ()
endforeach ()

get_filename_component(QT_DIR ${QT_LIBRARY_DIR}/.. REALPATH)

# The QT_LIBRARIES variable can come back from scimake's FindQt4
# with a list of libary paths mixed in with the words
# "optimized" and "debug".  We need to pull out a pure list
# of the libraries of only one type based on the value
# CMAKE_BUILD_TYPE, which is either DEBUG or RELEASE.
if (NOT "${QT_LIBRARIES}" MATCHES "optimized")
  set(QT_LIBS ${QT_LIBRARIES})
else ()
  set(QT_LIBS)
  set(libtype)
  foreach (qtlib ${QT_LIBRARIES})
    if (${qtlib} MATCHES "optimized" OR ${qtlib} MATCHES "debug")
      set(libtype ${qtlib})
    else ()
      if ("${libtype}" MATCHES "optimized" AND
           ("${CMAKE_BUILD_TYPE}" MATCHES "Release" OR
               "${CMAKE_BUILD_TYPE}" MATCHES "RELEASE" OR
               "${CMAKE_BUILD_TYPE}" MATCHES "RelWithDebInfo" OR
               "${CMAKE_BUILD_TYPE}" MATCHES "RELWITHDEBINFO"
           )
         )
        set(QT_LIBS ${QT_LIBS} ${qtlib})
      elseif ("${libtype}" MATCHES "debug" AND
               ("${CMAKE_BUILD_TYPE}" MATCHES "Debug" OR
                 "${CMAKE_BUILD_TYPE}" MATCHES "DEBUG")
               )
        set(QT_LIBS ${QT_LIBS} ${qtlib})
      endif ()
    endif ()
  endforeach ()
endif ()

# Find dlls
if (WIN32)
  set(QT_DLLS)
  foreach (qtlib ${QT_LIBS})
    get_filename_component(qtname ${qtlib} NAME_WE)
    if (EXISTS ${QT_BINARY_DIR}/${qtname}.dll)
      set(QT_DLLS ${QT_DLLS} ${QT_BINARY_DIR}/${qtname}.dll)
    else ()
      message(STATUS "${qtname} has no dll.")
    endif ()
  endforeach ()
endif ()

# Print results
SciPrintCMakeResults(QT)
SciPrintVar(QT_LIBS)
SciPrintVar(QT_QMAKE_EXECUTABLE)
SciPrintVar(QT_MOC_EXECUTABLE)
SciPrintVar(QT_UIC_EXECUTABLE)

message("--------- FindSciQt4 done with Qt4 -----------")
message("")

