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

set(CMAKE_CXX_FLAGS_DEBUG_INIT   "-Wall")       # Remove if your compiler
set(CMAKE_CXX_FLAGS_RELEASE_INIT "-Wall")       # doesn't support the -Wall flag.

find_package(Qt5Core         REQUIRED)
find_package(Qt5Gui          REQUIRED)
find_package(Qt5Widgets      REQUIRED)
find_package(Qt5PrintSupport REQUIRED)
find_package(Qt5Charts       REQUIRED)

set(HEADER_FILES
    GdbMonitor.h
    SeerArgumentsDialog.h
    SeerBreakpointCreateDialog.h
    SeerBreakpointsBrowserWidget.h
    SeerCatchpointCreateDialog.h
    SeerCatchpointsBrowserWidget.h
    SeerPrintpointCreateDialog.h
    SeerPrintpointsBrowserWidget.h
    SeerSeerLogWidget.h
    SeerConsoleWidget.h
    SeerConfigDialog.h
    SeerGdbConfigPage.h
    SeerEditorConfigPage.h
    SeerSourceConfigPage.h
    SeerSeerConfigPage.h
    SeerKeysConfigPage.h
    SeerDebugDialog.h
    SeerDirectoryFilterProxyModel.h
    SeerEditorManagerEntry.h
    SeerEditorManagerWidget.h
    SeerEditorOptionsBarWidget.h
    SeerBreakpointsOptionsBarWidget.h
    SeerEditorWidget.h
    SeerExecutableFilterProxyModel.h
    SeerGdbWidget.h
    SeerHexWidget.h
    SeerArrayWidget.h
    SeerLogWidget.h
    SeerMainWindow.h
    SeerArrayVisualizerWidget.h
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
    SeerGdbLogWidget.h
    SeerTildeLogWidget.h
    SeerUtl.h
    SeerVariableLoggerBrowserWidget.h
    SeerVariableManagerWidget.h
    SeerVariableTrackerBrowserWidget.h
    SeerWatchpointCreateDialog.h
    SeerWatchpointsBrowserWidget.h
    SeerCppSourceHighlighter.h
    SeerAboutDialog.h
    SeerSlashProcDialog.h
    SeerHighlighterSettings.h
    SeerKeySettings.h
    QProcessInfo.h
    QProcessInfoWidget.h
    QProgressIndicator.h
    QColorButton.h
    QDetachTabWidget.h
    QZoomChartView.h
    QZoomChart.h
    )

set(SOURCE_FILES
    seer.cpp
    GdbMonitor.cpp
    SeerArgumentsDialog.cpp
    SeerBreakpointCreateDialog.cpp
    SeerBreakpointsBrowserWidget.cpp
    SeerCatchpointCreateDialog.cpp
    SeerCatchpointsBrowserWidget.cpp
    SeerPrintpointCreateDialog.cpp
    SeerPrintpointsBrowserWidget.cpp
    SeerSeerLogWidget.cpp
    SeerConsoleWidget.cpp
    SeerConfigDialog.cpp
    SeerGdbConfigPage.cpp
    SeerEditorConfigPage.cpp
    SeerSourceConfigPage.cpp
    SeerSeerConfigPage.cpp
    SeerKeysConfigPage.cpp
    SeerDebugDialog.cpp
    SeerEditorManagerWidget.cpp
    SeerEditorOptionsBarWidget.cpp
    SeerBreakpointsOptionsBarWidget.cpp
    SeerEditorWidgetBreakPointArea.cpp
    SeerEditorWidget.cpp
    SeerEditorWidgetLineNumberArea.cpp
    SeerEditorWidgetMiniMapArea.cpp
    SeerEditorWidgetSourceArea.cpp
    SeerGdbWidget.cpp
    SeerHexWidget.cpp
    SeerArrayWidget.cpp
    SeerLogWidget.cpp
    SeerMainWindow.cpp
    SeerArrayVisualizerWidget.cpp
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
    SeerGdbLogWidget.cpp
    SeerTildeLogWidget.cpp
    SeerUtl.cpp
    SeerVariableLoggerBrowserWidget.cpp
    SeerVariableManagerWidget.cpp
    SeerVariableTrackerBrowserWidget.cpp
    SeerWatchpointCreateDialog.cpp
    SeerWatchpointsBrowserWidget.cpp
    SeerCppSourceHighlighter.cpp
    SeerAboutDialog.cpp
    SeerSlashProcDialog.cpp
    SeerHighlighterSettings.cpp
    SeerKeySettings.cpp
    QProcessInfo.cpp
    QProcessInfoWidget.cpp
    QProgressIndicator.cpp
    QColorButton.cpp
    QDetachTabWidget.cpp
    QZoomChartView.cpp
    QZoomChart.cpp
    )

qt5_add_resources(SOURCE_FILES resource.qrc)

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

add_compile_definitions(QT_DEPRECATED_WARNINGS)

add_executable(${PROJECT_NAME} ${SYSTEM_TYPE} ${SOURCE_FILES})

install(TARGETS ${PROJECT_NAME} DESTINATION bin)

target_link_libraries(${PROJECT_NAME} Qt5::Widgets Qt5::Gui Qt5::Core Qt5::PrintSupport Qt5::Charts)

