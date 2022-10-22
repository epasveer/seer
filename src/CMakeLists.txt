cmake_minimum_required(VERSION 3.1.0)

project(seergdb VERSION 1.13.0 LANGUAGES CXX)

set(PROJECT_NAME seergdb)

# Find includes in the build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_PREFIX_PATH ${QTDIR})
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTORCC ON)

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
    SeerAssemblyConfigPage.h
    SeerSeerConfigPage.h
    SeerKeysConfigPage.h
    SeerCloseSourceDialog.h
    SeerDebugDialog.h
    SeerDirectoryFilterProxyModel.h
    SeerEditorManagerEntry.h
    SeerEditorManagerWidget.h
    SeerEditorWidgetSource.h
    SeerEditorWidgetAssembly.h
    SeerExecutableFilterProxyModel.h
    SeerGdbWidget.h
    SeerHexWidget.h
    SeerAsmWidget.h
    SeerArrayWidget.h
    SeerLogWidget.h
    SeerMainWindow.h
    SeerArrayVisualizerWidget.h
    SeerMemoryVisualizerWidget.h
    SeerStructVisualizerWidget.h
    SeerVarVisualizerWidget.h
    SeerRegisterValuesBrowserWidget.h
    SeerRegisterEditValueDialog.h
    SeerRunStatusIndicator.h
    SeerLibraryBrowserWidget.h
    SeerSourceBrowserWidget.h
    SeerFunctionBrowserWidget.h
    SeerTypeBrowserWidget.h
    SeerStaticBrowserWidget.h
    SeerSourceSymbolLibraryManagerWidget.h
    SeerStackArgumentsBrowserWidget.h
    SeerStackFramesBrowserWidget.h
    SeerStackLocalsBrowserWidget.h
    SeerStackManagerWidget.h
    SeerThreadFramesBrowserWidget.h
    SeerThreadIdsBrowserWidget.h
    SeerThreadGroupsBrowserWidget.h
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
    SeerPlainTextEdit.h
    SeerHelpPageWidget.h
    SeerProgressIndicator.h
    QProcessInfo.h
    QProcessInfoWidget.h
    QProgressIndicator.h
    QColorButton.h
    QDetachTabWidget.h
    QZoomChartView.h
    QZoomChart.h
    QClearLineEdit.h
    QIndexTreeWidget.h
    QStringPair.h
    QHistoryLineEdit.h
    QEditDelegate.h
    QHContainerWidget.h
    )

set(SOURCE_FILES
    seergdb.cpp
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
    SeerAssemblyConfigPage.cpp
    SeerSeerConfigPage.cpp
    SeerKeysConfigPage.cpp
    SeerCloseSourceDialog.cpp
    SeerDebugDialog.cpp
    SeerEditorManagerWidget.cpp
    SeerEditorWidgetSource.cpp
    SeerEditorWidgetSourceAreas.cpp
    SeerEditorWidgetAssembly.cpp
    SeerEditorWidgetAssemblyAreas.cpp
    SeerGdbWidget.cpp
    SeerHexWidget.cpp
    SeerAsmWidget.cpp
    SeerArrayWidget.cpp
    SeerLogWidget.cpp
    SeerMainWindow.cpp
    SeerArrayVisualizerWidget.cpp
    SeerMemoryVisualizerWidget.cpp
    SeerStructVisualizerWidget.cpp
    SeerVarVisualizerWidget.cpp
    SeerRegisterValuesBrowserWidget.cpp
    SeerRegisterEditValueDialog.cpp
    SeerRunStatusIndicator.cpp
    SeerLibraryBrowserWidget.cpp
    SeerSourceBrowserWidget.cpp
    SeerFunctionBrowserWidget.cpp
    SeerTypeBrowserWidget.cpp
    SeerStaticBrowserWidget.cpp
    SeerSourceSymbolLibraryManagerWidget.cpp
    SeerStackArgumentsBrowserWidget.cpp
    SeerStackFramesBrowserWidget.cpp
    SeerStackLocalsBrowserWidget.cpp
    SeerStackManagerWidget.cpp
    SeerThreadFramesBrowserWidget.cpp
    SeerThreadIdsBrowserWidget.cpp
    SeerThreadGroupsBrowserWidget.cpp
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
    SeerPlainTextEdit.cpp
    SeerHelpPageWidget.cpp
    SeerProgressIndicator.cpp
    QProcessInfo.cpp
    QProcessInfoWidget.cpp
    QProgressIndicator.cpp
    QColorButton.cpp
    QDetachTabWidget.cpp
    QZoomChartView.cpp
    QZoomChart.cpp
    QClearLineEdit.cpp
    QIndexTreeWidget.cpp
    QHistoryLineEdit.cpp
    QEditDelegate.cpp
    QHContainerWidget.cpp
    )

qt5_add_resources(SOURCE_FILES resource.qrc)

# Set non-Debug build as GUI application.
# Debug build remains consle application.
if(NOT CMAKE_BUILD_TYPE MATCHES Debug) #Release, RelWithDebInfo and MinSizeRel
    if(WIN32) # Check if we are on Windows
        set(SYSTEM_TYPE WIN32)
    endif()
    message("System type is " ${SYSTEM_TYPE})
endif()

# for Linux, BSD, Solaris, Minix
if(UNIX AND NOT APPLE)
    add_compile_options(-Wall -Wextra -Wunused-result -pedantic)   # -Werror -Wdeprecated-copy
endif()

add_definitions(-DQT_DEPRECATED_WARNINGS)

add_executable(${PROJECT_NAME} ${SYSTEM_TYPE} ${SOURCE_FILES})

install(TARGETS ${PROJECT_NAME} RUNTIME DESTINATION bin)

target_link_libraries(${PROJECT_NAME} Qt5::Widgets Qt5::Gui Qt5::Core Qt5::PrintSupport Qt5::Charts)

