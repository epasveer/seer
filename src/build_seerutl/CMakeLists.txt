cmake_minimum_required(VERSION 3.1.0)

project(seerutl VERSION 1.0 LANGUAGES CXX)

set(PROJECT_NAME seerutl)

if(NOT DEFINED QTVERSION)
    set(QTVERSION "QT6" CACHE STRING "" FORCE)
endif()

if(${QTVERSION} STREQUAL "QT6")
    message("-- Compile with QT6")
elseif(${QTVERSION} STREQUAL "QT5")
    message("-- Compile with QT5")
else()
    message(FATAL_ERROR "-- Use -DQTVERSION=QT5|QT6 not \"${QTVERSION}\"")
endif()

# Find includes in the build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_PREFIX_PATH ${QTDIR})

if(${QTVERSION} STREQUAL "QT6")
    find_package(Qt6 COMPONENTS REQUIRED Core)

    set(CMAKE_CXX_STANDARD 17)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
    set(CMAKE_CXX_EXTENSIONS OFF)

elseif(${QTVERSION} STREQUAL "QT5")
    find_package(Qt5 COMPONENTS REQUIRED Core)

    set(CMAKE_CXX_STANDARD 11)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
    set(CMAKE_CXX_EXTENSIONS OFF)
endif()


set(HEADER_FILES
    ../SeerUtl.h
    )

set(SOURCE_FILES
    ../seerutl.cpp
    ../SeerUtl.cpp
    )

# Set non-Debug build as GUI application.
# Debug build remains consle application.
if(NOT CMAKE_BUILD_TYPE MATCHES Debug) #Release, RelWithDebInfo and MinSizeRel
    if(WIN32) # Check if we are on Windows
        set(SYSTEM_TYPE WIN32)
    endif()
    message("-- System type is " ${SYSTEM_TYPE})
endif()

# for Linux, BSD, Solaris, Minix
if(UNIX AND NOT APPLE)
    add_compile_options(-Wall -Wextra -Wunused-result -pedantic)   # -Werror -Wdeprecated-copy
endif()

add_definitions(-DQT_DEPRECATED_WARNINGS)

add_executable(${PROJECT_NAME} ${SYSTEM_TYPE} ${SOURCE_FILES})

install(TARGETS ${PROJECT_NAME} RUNTIME DESTINATION bin)

if(${QTVERSION} STREQUAL "QT6")
    target_link_libraries(${PROJECT_NAME} Qt6::Core)
elseif(${QTVERSION} STREQUAL "QT5")
    target_link_libraries(${PROJECT_NAME} Qt5::Core)
endif()

