cmake_minimum_required(VERSION 3.1.0)

project(seergdb VERSION 2.4.0 LANGUAGES CXX)

set(PROJECT_NAME seergdb)

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
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTORCC ON)

if(${QTVERSION} STREQUAL "QT6")
    find_package(Qt6 COMPONENTS REQUIRED Core)
    find_package(Qt6 COMPONENTS REQUIRED Gui)
    find_package(Qt6 COMPONENTS REQUIRED Widgets)
    find_package(Qt6 COMPONENTS REQUIRED PrintSupport)
    find_package(Qt6 COMPONENTS REQUIRED Charts)
    find_package(Qt6 COMPONENTS REQUIRED Svg)

    set(CMAKE_CXX_STANDARD 17)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
    set(CMAKE_CXX_EXTENSIONS OFF)

elseif(${QTVERSION} STREQUAL "QT5")
    find_package(Qt5 COMPONENTS REQUIRED Core)
    find_package(Qt5 COMPONENTS REQUIRED Gui)
    find_package(Qt5 COMPONENTS REQUIRED Widgets)
    find_package(Qt5 COMPONENTS REQUIRED PrintSupport)
    find_package(Qt5 COMPONENTS REQUIRED Charts)

    set(CMAKE_CXX_STANDARD 11)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
    set(CMAKE_CXX_EXTENSIONS OFF)
endif()

set(HEADER_FILES
    GdbMonitor.h
    SeerArgumentsDialog.h
    SeerBreakpointCreateDialog.h
    SeerMessagesBrowserWidget.h
    SeerBreakpointsBrowserWidget.h
    SeerCatchpointCreateDialog.h
    SeerCatchpointsBrowserWidget.h
    SeerPrintpointCreateDialog.h
    SeerPrintpointsBrowserWidget.h
    SeerSeerLogWidget.h
    SeerConsoleWidget.h
    SeerConfigDialog.h
    SeerGdbConfigPage.h
    SeerRRConfigPage.h
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
    SeerImageVisualizerWidget.h
    SeerRegisterValuesBrowserWidget.h
    SeerRegisterEditValueDialog.h
    SeerRegisterProfileDialog.h
    SeerRegisterTreeWidgetItem.h
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
    SeerAdaTasksBrowserWidget.h
    SeerAdaExceptionsBrowserWidget.h
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
    SeerHelpPageDialog.h
    SeerProgressIndicator.h
    SeerMessagesDialog.h
    SeerAssemblyPreferenceDialog.h
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
    QImageViewer.h
    )

set(SOURCE_FILES
    seergdb.cpp
    GdbMonitor.cpp
    SeerArgumentsDialog.cpp
    SeerBreakpointCreateDialog.cpp
    SeerMessagesBrowserWidget.cpp
    SeerBreakpointsBrowserWidget.cpp
    SeerCatchpointCreateDialog.cpp
    SeerCatchpointsBrowserWidget.cpp
    SeerPrintpointCreateDialog.cpp
    SeerPrintpointsBrowserWidget.cpp
    SeerSeerLogWidget.cpp
    SeerConsoleWidget.cpp
    SeerConfigDialog.cpp
    SeerGdbConfigPage.cpp
    SeerRRConfigPage.cpp
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
    SeerImageVisualizerWidget.cpp
    SeerRegisterValuesBrowserWidget.cpp
    SeerRegisterEditValueDialog.cpp
    SeerRegisterProfileDialog.cpp
    SeerRegisterTreeWidgetItem.cpp
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
    SeerAdaTasksBrowserWidget.cpp
    SeerAdaExceptionsBrowserWidget.cpp
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
    SeerHelpPageDialog.cpp
    SeerProgressIndicator.cpp
    SeerMessagesDialog.cpp
    SeerAssemblyPreferenceDialog.cpp
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
    QImageViewer.cpp
    )

if(${QTVERSION} STREQUAL "QT6")
    qt6_add_resources(SOURCE_FILES resource.qrc resources/qdarkstyle/dark/darkstyle.qrc resources/qdarkstyle/light/lightstyle.qrc)
elseif(${QTVERSION} STREQUAL "QT5")
    qt5_add_resources(SOURCE_FILES resource.qrc resources/qdarkstyle/dark/darkstyle.qrc resources/qdarkstyle/light/lightstyle.qrc)
endif()

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
    target_link_libraries(${PROJECT_NAME} Qt6::Widgets Qt6::Gui Qt6::Core Qt6::PrintSupport Qt6::Charts Qt6::Svg)
elseif(${QTVERSION} STREQUAL "QT5")
    target_link_libraries(${PROJECT_NAME} Qt5::Widgets Qt5::Gui Qt5::Core Qt5::PrintSupport Qt5::Charts)
endif()

