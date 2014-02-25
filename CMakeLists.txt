# CMake script for building debian packages:
#
# Run the following commands:
#   - cd /path/to/src
#   - mkdir build && build
#   - cmake -DCPACK_GENERATOR=DEB ..
#   - make package
#
# To upload a PPA package, adapt DPUT_HOST to point to your ppa then run
# make dput
#
cmake_minimum_required( VERSION 2.8 )

# ##############################################################

set(PROJECT_NAME open-cobol-ide)
project(${PROJECT_NAME})
set(VERSION_MAJOR 2)
set(VERSION_MINOR 1)
set(VERSION_PATCH 0)
set(VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH})

# Make the scripts available in the 'cmake' directory available for the
# 'include()' command, 'find_package()' command.

set( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_LIST_DIR}/cmake )

# ##############################################################

set(DEPENDS "python3, python3-pyqode.core, python3-pyqode.widgets, open-cobol, python3-pyqt4")
find_package(PythonInterp 3)
set(CPACK_TARGET_INTERPRETER_VERSION "python3")

# ##############################################################

configure_file(
  "${PROJECT_SOURCE_DIR}/share/${PROJECT_NAME}.desktop.in"
  "${PROJECT_BINARY_DIR}/${PROJECT_NAME}.desktop"
  )
execute_process(COMMAND chmod 0644 ${PROJECT_BINARY_DIR}/${PROJECT_NAME}.desktop)

# ##############################################################

configure_file(
  "${PROJECT_SOURCE_DIR}/share/${PROJECT_NAME}.1"
  "${PROJECT_BINARY_DIR}/${PROJECT_NAME}.1"
  )
execute_process(COMMAND gzip -f -9 ${PROJECT_BINARY_DIR}/${PROJECT_NAME}.1)

# ##############################################################
if(CPACK_GENERATOR STREQUAL "DEB")
    set(CHANGELOG_NAME "changelog")
else()
    set(CHANGELOG_NAME "changelog.Debian")
endif()

set(RELEASE_PACKAGE_VERSION "${VERSION}")
set(RELEASE "all")
configure_file(
  "${PROJECT_SOURCE_DIR}/share/changelog"
  "${PROJECT_BINARY_DIR}/${CHANGELOG_NAME}"
  )
execute_process(COMMAND gzip -f -9 ${PROJECT_BINARY_DIR}/${CHANGELOG_NAME})

# ##############################################################

install(PROGRAMS "${PROJECT_SOURCE_DIR}/${PROJECT_NAME}" DESTINATION bin)
file(GLOB OCI_FILES "oci/*.py")
file(GLOB OCI_UI_FILES "oci/ui/*.py")
install(FILES ${OCI_FILES} DESTINATION lib/oci)
install(FILES ${OCI_UI_FILES} DESTINATION lib/oci/ui)
install(FILES "${PROJECT_BINARY_DIR}/${PROJECT_NAME}.desktop"
        DESTINATION share/applications/ )
install(FILES "${PROJECT_SOURCE_DIR}/share/OpenCobolIDE.png"
        DESTINATION share/pixmaps/ )
install(FILES ${PROJECT_BINARY_DIR}/${PROJECT_NAME}.1.gz
        DESTINATION share/man/man1/ )
install(FILES ${PROJECT_BINARY_DIR}/${CHANGELOG_NAME}.gz
        DESTINATION share/doc/open-cobol-ide/ )

# ##############################################################

if(CMAKE_CPACK_COMMAND AND UNIX)
    set(DPUT_HOST "ppa:${PROJECT_NAME}/stable" CACHE STRING "PPA repository to upload the debian sources")

    # Packing information
    set(CPACK_PACKAGE_NAME ${PROJECT_NAME})

    INSTALL(FILES "${PROJECT_SOURCE_DIR}/share/copyright"
            DESTINATION share/doc/${CPACK_PACKAGE_NAME}/)

    set(CPACK_PACKAGE_CONTACT "Colin Duquesnoy <colin.duquesnoy@gmail.com>" CACHE STRING "Package maintainer and PGP signer.")
    set(CPACK_PACKAGE_VENDOR "https://github.com/OpenCobolIDE/OpenCobolIDE")
    set(CPACK_PACKAGE_DISPLAY_NAME "${PROJECT_NAME} ${VERSION}")
    set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Simple and lightweight Cobol IDE")
    set(CPACK_PACKAGE_VERSION "${VERSION}")
    set(CPACK_PACKAGE_VERSION_MAJOR ${VERSION_MAJOR})
    set(CPACK_PACKAGE_VERSION_MINOR ${VERSION_MINOR})
    set(CPACK_PACKAGE_VERSION_PATCH ${VERSION_PATCH})
    set(DEB_LONG_DESCRIPTION
        " OpenCobolIDE is a simple cobol IDE based on the OpenCOBOL compiler \n"
        " and the pyqode text editor widget\n")
    string(REPLACE ";" "" DEB_LONG_DESCRIPTION ${DEB_LONG_DESCRIPTION})
    set(CPACK_DEBIAN_PACKAGE_DESCRIPTION
        "${CPACK_PACKAGE_DESCRIPTION_SUMMARY}\n${DEB_LONG_DESCRIPTION}")
    set(CPACK_RESOURCE_FILE_LICENSE ${CMAKE_CURRENT_SOURCE_DIR}/LICENSE)

    # base
    set(CPACK_DEBIAN_BUILD_DEPENDS_UBUNTU debhelper cmake ${CPACK_TARGET_INTERPRETER_VERSION})

    # get architecture
    set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "all")

    # debian
    set(CPACK_DEBIAN_PACKAGE_PRIORITY optional)
    set(CPACK_DEBIAN_PACKAGE_SECTION editors)
    set(CPACK_DEBIAN_PACKAGE_DEPENDS ${DEPENDS})
    set(CPACK_DEBIAN_PACKAGE_SOURCE_COPY "${PYTHON_EXECUTABLE} /tools/copydebfiles.py")
    set(CPACK_DEBIAN_PACKAGE_REMOVE_SOURCE_FILES )
    execute_process(COMMAND lsb_release -is
        OUTPUT_VARIABLE _lsb_distribution OUTPUT_STRIP_TRAILING_WHITESPACE
        RESULT_VARIABLE _lsb_release_failed)
    if(${_lsb_distribution})
        set(CPACK_DEBIAN_DISTRIBUTION_NAME ${_lsb_distribution} CACHE STRING "Name of the distribution")
    else()
        # lsb_release fail when building the package in a ppa, as ubuntu is always the target release
        # on launchpad, we can safely set the distribution name to ubuntu
        set(CPACK_DEBIAN_DISTRIBUTION_NAME "ubuntu")
    endif()
    string(TOLOWER ${CPACK_DEBIAN_DISTRIBUTION_NAME} CPACK_DEBIAN_DISTRIBUTION_NAME)
    if( ${CPACK_DEBIAN_DISTRIBUTION_NAME} STREQUAL "ubuntu" )
        set(CPACK_DEBIAN_DISTRIBUTION_RELEASES precise quantal raring saucy CACHE STRING "Release code-names of the distrubiton release")
    endif()
    # The debian generator does not seem to use a correct name for the debian packages
    set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}_${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}_${CPACK_DEBIAN_PACKAGE_ARCHITECTURE}")

    include(CPack)
    include(DebSourcePPA)
endif()