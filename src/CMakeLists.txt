cmake_minimum_required(VERSION 3.1.0)

project(seer VERSION 1.0.1 LANGUAGES CXX)

set(PROJECT_NAME seer)

# Find includes in the build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_PREFIX_PATH ${QTDIR})
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTORCC ON)

find_package(Qt5Core REQUIRED)
find_package(Qt5Gui REQUIRED)
find_package(Qt5Widgets REQUIRED)
find_package(Qt5PrintSupport REQUIRED)

set(HEADER_FILES
    GdbMonitor.h
    SeerArgumentsDialog.h
    SeerBreakpointCreateDialog.h
    SeerBreakpointsBrowserWidget.h
    SeerCaretAsteriskLogWidget.h
    SeerConsoleWidget.h
    SeerDebugDialog.h
    SeerDirectoryFilterProxyModel.h
    SeerEditorManagerEntry.h
    SeerEditorManagerWidget.h
    SeerEditorOptionsBarWidget.h
    SeerEditorWidget.h
    SeerExecutableFilterProxyModel.h
    SeerGdbWidget.h
    SeerHexWidget.h
    SeerLogWidget.h
    SeerMainWindow.h
    SeerMemoryVisualizerWidget.h
    SeerRegisterValuesBrowserWidget.h
    SeerRunStatusIndicator.h
    SeerSharedLibraryBrowserWidget.h
    SeerSourceBrowserWidget.h
    SeerSourceLibraryManagerWidget.h
    SeerStackArgumentsBrowserWidget.h
    SeerStackFramesBrowserWidget.h
    SeerStackLocalsBrowserWidget.h
    SeerStackManagerWidget.h
    SeerThreadFramesBrowserWidget.h
    SeerThreadIdsBrowserWidget.h
    SeerThreadManagerWidget.h
    SeerTildeEqualAmpersandLogWidget.h
    SeerTildeLogWidget.h
    SeerUtl.h
    SeerVariableLoggerBrowserWidget.h
    SeerVariableManagerWidget.h
    SeerVariableTrackerBrowserWidget.h
    SeerWatchpointCreateDialog.h
    SeerWatchpointsBrowserWidget.h
    )

set(SOURCE_FILES
    GdbMonitor.cpp
    SeerArgumentsDialog.cpp
    SeerBreakpointCreateDialog.cpp
    SeerBreakpointsBrowserWidget.cpp
    SeerCaretAsteriskLogWidget.cpp
    SeerConsoleWidget.cpp
    seer.cpp
    SeerDebugDialog.cpp
    SeerEditorManagerWidget.cpp
    SeerEditorOptionsBarWidget.cpp
    SeerEditorWidgetBreakPointArea.cpp
    SeerEditorWidget.cpp
    SeerEditorWidgetLineNumberArea.cpp
    SeerEditorWidgetMiniMapArea.cpp
    SeerEditorWidgetSourceArea.cpp
    SeerGdbWidget.cpp
    SeerHexWidget.cpp
    SeerLogWidget.cpp
    SeerMainWindow.cpp
    SeerMemoryVisualizerWidget.cpp
    SeerRegisterValuesBrowserWidget.cpp
    SeerRunStatusIndicator.cpp
    SeerSharedLibraryBrowserWidget.cpp
    SeerSourceBrowserWidget.cpp
    SeerSourceLibraryManagerWidget.cpp
    SeerStackArgumentsBrowserWidget.cpp
    SeerStackFramesBrowserWidget.cpp
    SeerStackLocalsBrowserWidget.cpp
    SeerStackManagerWidget.cpp
    SeerThreadFramesBrowserWidget.cpp
    SeerThreadIdsBrowserWidget.cpp
    SeerThreadManagerWidget.cpp
    SeerTildeEqualAmpersandLogWidget.cpp
    SeerTildeLogWidget.cpp
    SeerUtl.cpp
    SeerVariableLoggerBrowserWidget.cpp
    SeerVariableManagerWidget.cpp
    SeerVariableTrackerBrowserWidget.cpp
    SeerWatchpointCreateDialog.cpp
    SeerWatchpointsBrowserWidget.cpp
    )

# Set non-Debug build as GUI application.
# Debug build remains consle application.
if(NOT CMAKE_BUILD_TYPE MATCHES Debug) #Release, RelWithDebInfo and MinSizeRel
    if(WIN32) # Check if we are on Windows
        set(SYSTEM_TYPE WIN32)
    elseif(APPLE) # MacOS X
        set(SYSTEM_TYPE MACOSX_BUNDLE)
    else() # UNIX
        set(SYSTEM_TYPE EXCLUDE_FROM_ALL)
    endif()
    message("System type is" ${SYSTEM_TYPE})
endif()

add_executable(${PROJECT_NAME} ${SYSTEM_TYPE} ${SOURCE_FILES})

install(TARGETS ${PROJECT_NAME} DESTINATION bin)

target_link_libraries(${PROJECT_NAME} Qt5::Widgets Qt5::Gui Qt5::Core Qt5::PrintSupport)

