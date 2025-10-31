// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerGdbWidget.h"
#include "SeerLogWidget.h"
#include "SeerMemoryVisualizerWidget.h"
#include "SeerArrayVisualizerWidget.h"
#include "SeerMatrixVisualizerWidget.h"
#include "SeerStructVisualizerWidget.h"
#include "SeerVarVisualizerWidget.h"
#include "SeerImageVisualizerWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include "SeerOpenOCDWidget.h"
#include "QHContainerWidget.h"
#include "SeerOpenOCDDebugOnInit.h"
#include <QtGui/QFont>
#include <QtGui/QGuiApplication>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtCore/QLoggingCategory>
#include <QtCore/QSettings>
#include <QtCore/QProcess>
#include <QtCore/QRegularExpression>
#include <QtCore/QDir>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QFileInfoList>
#include <QtCore/QThread>
#include <QtCore/QDebug>
#include <QtGlobal>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <QMutex>
#include <QWaitCondition>
#include <QTextStream>

#include "ui_SeerGdbWidget.h"
static QLoggingCategory LC("seer.gdbwidget");

SeerGdbWidget::SeerGdbWidget (QWidget* parent) : QWidget(parent) {

    _executableName                     = "";
    _executableArguments                = "";
    _executableWorkingDirectory         = "";
    _executableBreakpointsFilename      = "";
    _executableBreakpointFunctionName   = "";
    _executableConnectHostPort          = "";
    _executableRRTraceDirectory         = "";
    _executableCoreFilename             = "";
    _executablePid                      = 0;

    _gdbMonitor                         = 0;
    _gdbProcess                         = 0;
    _consoleWidget                      = 0;
    _consoleIndex                       = -1;
    _breakpointsBrowserWidget           = 0;
    _watchpointsBrowserWidget           = 0;
    _catchpointsBrowserWidget           = 0;
    _gdbOutputLog                       = 0;
    _seerOutputLog                      = 0;
    _gdbProgram                         = "/usr/bin/gdb";
    _gdbArguments                       = "--interpreter=mi";
    _gdbASyncMode                       = true;
    _gdbNonStopMode                     = false;
    _gdbServerDebug                     = false;
    _assemblyShowAssemblyTabOnStartup   = false;
    _assemblyDisassemblyFlavor          = "att";
    _gdbHandleTerminatingException      = true;
    _gdbRandomizeStartAddress           = false;
    _gdbEnablePrettyPrinting            = true;
    _gdbRemoteTargetType                = "extended-remote";
    _gdbRecordMode                      = "";
    _gdbRecordDirection                 = "";
    _consoleScrollLines                 = 1000;
    _rememberManualCommandCount         = 10;
    _currentFrame                       = -1;

    // for openOCD gdb-multiarch support
    _gdbMultiarchProgram                = "/usr/bin/gdb-multiarch";
    _gdbMultiarchArguments              = "--interpreter=mi";
    _isBuildInDocker                    = false;
    _absoluteBuildPath                  = "";
    _dockerBuildPath                    = "";
    _newHBreakFlag                      = false;
    _isTargetRunning                    = false;
    _debugOnInitFlag                    = false;

    setIsQuitting(false);
    setNewExecutableFlag(true);

    setupUi(this);

    // Setup the widgets
    QFont font;
    font.setFamily("monospace [Consolas]");
    font.setFixedPitch(true);
    font.setStyleHint(QFont::TypeWriter);

    _messagesBrowserWidget    = new SeerMessagesBrowserWidget(this);
    _breakpointsBrowserWidget = new SeerBreakpointsBrowserWidget(this);
    _watchpointsBrowserWidget = new SeerWatchpointsBrowserWidget(this);
    _catchpointsBrowserWidget = new SeerCatchpointsBrowserWidget(this);
    _printpointsBrowserWidget = new SeerPrintpointsBrowserWidget(this);
    _checkpointsBrowserWidget = new SeerCheckpointsBrowserWidget(this);

    _gdbOutputLog             = new SeerGdbLogWidget(this);
    _seerOutputLog            = new SeerSeerLogWidget(this);
    _gdbOutputLog->setPlaceholderText("[gdb output]");
    _seerOutputLog->setPlaceholderText("[seer output]");

    logsTabWidget->addTab(_messagesBrowserWidget,    "Messages");
    logsTabWidget->addTab(_breakpointsBrowserWidget, "Breakpoints");
    logsTabWidget->addTab(_watchpointsBrowserWidget, "Watchpoints");
    logsTabWidget->addTab(_catchpointsBrowserWidget, "Catchpoints");
    logsTabWidget->addTab(_printpointsBrowserWidget, "Printpoints");
    logsTabWidget->addTab(_checkpointsBrowserWidget, "Checkpoints");
    logsTabWidget->addTab(_gdbOutputLog,             "GDB output");
    logsTabWidget->addTab(_seerOutputLog,            "Seer output");

    logsTabWidget->addTab(_seerOutputLog,            "Seer output");
    logsTabWidget->setCurrentIndex(0);

    // Create the console tab.
    // Each RUN method will create and connect to the console's terminal.
    createConsole();

    // Create editor options bar.
    QToolButton* breakpointsLoadToolButton = new QToolButton(logsTabWidget);
    breakpointsLoadToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/document-open.svg"));
    breakpointsLoadToolButton->setToolTip("Load previously saved breakpoints.");

    QToolButton* breakpointsSaveToolButton = new QToolButton(logsTabWidget);
    breakpointsSaveToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/document-save-as.svg"));
    breakpointsSaveToolButton->setToolTip("Save breakpoints to a file.");

    QToolButton* helpToolButton = new QToolButton(logsTabWidget);
    helpToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpToolButton->setToolTip("Help on Breakpoint/Gdb/Seer information.");

    QHContainerWidget* hcontainer = new QHContainerWidget(this);
    hcontainer->setSpacing(3);
    hcontainer->addWidget(breakpointsLoadToolButton);
    hcontainer->addWidget(breakpointsSaveToolButton);
    hcontainer->addWidget(helpToolButton);

    logsTabWidget->setCornerWidget(hcontainer, Qt::TopRightCorner);

    // Set manual command settings.
    manualCommandComboBox->setFont(font);
    manualCommandComboBox->setEditable(true);
    manualCommandComboBox->lineEdit()->setPlaceholderText("Manually enter a gdb/mi command...");
    manualCommandComboBox->lineEdit()->setToolTip("Manually enter a gdb/mi command.");
    manualCommandComboBox->lineEdit()->setClearButtonEnabled(true);

    // Create the gdb process.
    _gdbProcess = new QProcess(this);

    // Create the gdb monitor.
    _gdbMonitor = new GdbMonitor(this);
    _gdbMonitor->setProcess(_gdbProcess);

    // Restore tab ordering.
    readLogsSettings();

    // Handle the app's 'quit' event, in case we want to do things before exiting.
    QObject::connect(QCoreApplication::instance(),                              &QCoreApplication::aboutToQuit,                                                             this,                                                           &SeerGdbWidget::handleAboutToQuit);

    // Connect things.
    QObject::connect(logsTabWidget->tabBar(),                                   &QTabBar::tabMoved,                                                                         this,                                                           &SeerGdbWidget::handleLogsTabMoved);
    QObject::connect(logsTabWidget->tabBar(),                                   &QTabBar::currentChanged,                                                                   this,                                                           &SeerGdbWidget::handleLogsTabChanged);
    QObject::connect(_messagesBrowserWidget,                                    &SeerMessagesBrowserWidget::showMessages,                                                   this,                                                           &SeerGdbWidget::handleRaiseMessageTab);

    QObject::connect(manualCommandComboBox->lineEdit(),                         &QLineEdit::returnPressed,                                                                  this,                                                           &SeerGdbWidget::handleManualCommandExecute);

    QObject::connect(_gdbProcess,                                               &QProcess::readyReadStandardOutput,                                                         _gdbMonitor,                                                    &GdbMonitor::handleReadyReadStandardOutput);
    QObject::connect(_gdbProcess,                                               &QProcess::readyReadStandardError,                                                          _gdbMonitor,                                                    &GdbMonitor::handleReadyReadStandardError);
    QObject::connect(_gdbProcess,                                               static_cast<void (QProcess::*)(int, QProcess::ExitStatus)>(&QProcess::finished),            this,                                                           &SeerGdbWidget::handleGdbProcessFinished); // ??? Do we care about the gdb process ending? For now, terminate Seer.
    QObject::connect(_gdbProcess,                                               static_cast<void (QProcess::*)(QProcess::ProcessError)>(&QProcess::errorOccurred),          this,                                                           &SeerGdbWidget::handleGdbProcessErrored);

    QObject::connect(_gdbMonitor,                                               &GdbMonitor::tildeTextOutput,                                                               _gdbOutputLog,                                                  &SeerGdbLogWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::ampersandTextOutput,                                                           _gdbOutputLog,                                                  &SeerGdbLogWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::atsignTextOutput,                                                              _gdbOutputLog,                                                  &SeerGdbLogWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::equalTextOutput,                                                               _seerOutputLog,                                                 &SeerSeerLogWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               _seerOutputLog,                                                 &SeerSeerLogWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::astrixTextOutput,                                                              _seerOutputLog,                                                 &SeerSeerLogWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::astrixTextOutput,                                                              editorManagerWidget,                                            &SeerEditorManagerWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               editorManagerWidget,                                            &SeerEditorManagerWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               sourceLibraryManagerWidget->sourceBrowserWidget(),              &SeerSourceBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               sourceLibraryManagerWidget->functionBrowserWidget(),            &SeerFunctionBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               sourceLibraryManagerWidget->typeBrowserWidget(),                &SeerTypeBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               sourceLibraryManagerWidget->staticBrowserWidget(),              &SeerStaticBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               sourceLibraryManagerWidget->libraryBrowserWidget(),             &SeerLibraryBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               sourceLibraryManagerWidget->adaExceptionsBrowserWidget(),       &SeerAdaExceptionsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               sourceLibraryManagerWidget->skipBrowserWidget(),                &SeerSkipBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               stackManagerWidget->stackFramesBrowserWidget(),                 &SeerStackFramesBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               stackManagerWidget->stackLocalsBrowserWidget(),                 &SeerStackLocalsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               stackManagerWidget->stackArgumentsBrowserWidget(),              &SeerStackArgumentsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               stackManagerWidget->stackDumpBrowserWidget(),                   &SeerStackDumpBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               stackManagerWidget,                                             &SeerStackManagerWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               _breakpointsBrowserWidget,                                      &SeerBreakpointsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               _watchpointsBrowserWidget,                                      &SeerWatchpointsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               _catchpointsBrowserWidget,                                      &SeerCatchpointsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               _printpointsBrowserWidget,                                      &SeerPrintpointsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               _checkpointsBrowserWidget,                                      &SeerCheckpointsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               variableManagerWidget->registerValuesBrowserWidget(),           &SeerRegisterValuesBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               variableManagerWidget->variableTrackerBrowserWidget(),          &SeerVariableTrackerBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               variableManagerWidget->variableLoggerBrowserWidget(),           &SeerVariableLoggerBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::astrixTextOutput,                                                              _watchpointsBrowserWidget,                                      &SeerWatchpointsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::astrixTextOutput,                                                              this,                                                           &SeerGdbWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::equalTextOutput,                                                               this,                                                           &SeerGdbWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               this,                                                           &SeerGdbWidget::handleText);

    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               threadManagerWidget->threadFramesBrowserWidget(),               &SeerThreadFramesBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::equalTextOutput,                                                               threadManagerWidget->threadFramesBrowserWidget(),               &SeerThreadFramesBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::astrixTextOutput,                                                              threadManagerWidget->threadFramesBrowserWidget(),               &SeerThreadFramesBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               threadManagerWidget->threadIdsBrowserWidget(),                  &SeerThreadIdsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               threadManagerWidget->threadGroupsBrowserWidget(),               &SeerThreadGroupsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::equalTextOutput,                                                               threadManagerWidget->threadGroupsBrowserWidget(),               &SeerThreadGroupsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               threadManagerWidget->adaTasksBrowserWidget(),                   &SeerAdaTasksBrowserWidget::handleText);

    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::refreshBreakpointsList,                                           this,                                                           &SeerGdbWidget::handleGdbGenericpointList);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::refreshStackFrames,                                               this,                                                           &SeerGdbWidget::handleGdbStackListFrames);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::insertBreakpoint,                                                 this,                                                           &SeerGdbWidget::handleGdbBreakpointInsert);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::insertPrintpoint,                                                 this,                                                           &SeerGdbWidget::handleGdbPrintpointInsert);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::deleteBreakpoints,                                                this,                                                           &SeerGdbWidget::handleGdbBreakpointDelete);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::enableBreakpoints,                                                this,                                                           &SeerGdbWidget::handleGdbBreakpointEnable);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::disableBreakpoints,                                               this,                                                           &SeerGdbWidget::handleGdbBreakpointDisable);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::infoBreakpoint,                                                   this,                                                           &SeerGdbWidget::handleGdbBreakpointInfo);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::runToLine,                                                        this,                                                           &SeerGdbWidget::handleGdbRunToLine);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::runToAddress,                                                     this,                                                           &SeerGdbWidget::handleGdbRunToAddress);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::addVariableLoggerExpression,                                      variableManagerWidget->variableLoggerBrowserWidget(),           &SeerVariableLoggerBrowserWidget::addVariableExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::addVariableTrackerExpression,                                     this,                                                           &SeerGdbWidget::handleGdbDataAddExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::refreshVariableTrackerValues,                                     this,                                                           &SeerGdbWidget::handleGdbDataListValues);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::addMemoryVisualizer,                                              this,                                                           &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::addArrayVisualizer,                                               this,                                                           &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::addMatrixVisualizer,                                              this,                                                           &SeerGdbWidget::handleGdbMatrixAddExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::addStructVisualizer,                                              this,                                                           &SeerGdbWidget::handleGdbVarAddExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::evaluateVariableExpression,                                       this,                                                           &SeerGdbWidget::handleGdbDataEvaluateExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::evaluateVariableExpression,                                       variableManagerWidget->variableLoggerBrowserWidget(),           &SeerVariableLoggerBrowserWidget::handleEvaluateVariableExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::requestAssembly,                                                  this,                                                           &SeerGdbWidget::handleGdbGetAssembly);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::requestSourceAndAssembly,                                         this,                                                           &SeerGdbWidget::handleGdbGetSourceAndAssembly);

    QObject::connect(sourceLibraryManagerWidget->sourceBrowserWidget(),         &SeerSourceBrowserWidget::refreshSourceList,                                                this,                                                           &SeerGdbWidget::handleGdbExecutableSources);
    QObject::connect(sourceLibraryManagerWidget->sourceBrowserWidget(),         &SeerSourceBrowserWidget::selectedFile,                                                     editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);
    QObject::connect(sourceLibraryManagerWidget->functionBrowserWidget(),       &SeerFunctionBrowserWidget::refreshFunctionList,                                            this,                                                           &SeerGdbWidget::handleGdbExecutableFunctions);
    QObject::connect(sourceLibraryManagerWidget->functionBrowserWidget(),       &SeerFunctionBrowserWidget::insertBreakpoint,                                               this,                                                           &SeerGdbWidget::handleGdbBreakpointInsert);
    QObject::connect(sourceLibraryManagerWidget->functionBrowserWidget(),       &SeerFunctionBrowserWidget::selectedFile,                                                   editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);
    QObject::connect(sourceLibraryManagerWidget->typeBrowserWidget(),           &SeerTypeBrowserWidget::refreshTypeList,                                                    this,                                                           &SeerGdbWidget::handleGdbExecutableTypes);
    QObject::connect(sourceLibraryManagerWidget->typeBrowserWidget(),           &SeerTypeBrowserWidget::selectedFile,                                                       editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);
    QObject::connect(sourceLibraryManagerWidget->staticBrowserWidget(),         &SeerStaticBrowserWidget::refreshVariableList,                                              this,                                                           &SeerGdbWidget::handleGdbExecutableVariables);
    QObject::connect(sourceLibraryManagerWidget->staticBrowserWidget(),         &SeerStaticBrowserWidget::selectedFile,                                                     editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);
    QObject::connect(sourceLibraryManagerWidget->libraryBrowserWidget(),        &SeerLibraryBrowserWidget::refreshLibraryList,                                              this,                                                           &SeerGdbWidget::handleGdbExecutableLibraries);
    QObject::connect(sourceLibraryManagerWidget->adaExceptionsBrowserWidget(),  &SeerAdaExceptionsBrowserWidget::refreshAdaExceptions,                                      this,                                                           &SeerGdbWidget::handleGdbAdaListExceptions);
    QObject::connect(sourceLibraryManagerWidget->adaExceptionsBrowserWidget(),  &SeerAdaExceptionsBrowserWidget::insertCatchpoint,                                          this,                                                           &SeerGdbWidget::handleGdbCatchpointInsert);
    QObject::connect(sourceLibraryManagerWidget->skipBrowserWidget(),           &SeerSkipBrowserWidget::refreshSkipList,                                                    this,                                                           &SeerGdbWidget::handleGdbSkipList);
    QObject::connect(sourceLibraryManagerWidget->skipBrowserWidget(),           &SeerSkipBrowserWidget::addSkip,                                                            this,                                                           &SeerGdbWidget::handleGdbSkipAdd);
    QObject::connect(sourceLibraryManagerWidget->skipBrowserWidget(),           &SeerSkipBrowserWidget::deleteSkips,                                                        this,                                                           &SeerGdbWidget::handleGdbSkipDelete);
    QObject::connect(sourceLibraryManagerWidget->skipBrowserWidget(),           &SeerSkipBrowserWidget::enableSkips,                                                        this,                                                           &SeerGdbWidget::handleGdbSkipEnable);
    QObject::connect(sourceLibraryManagerWidget->skipBrowserWidget(),           &SeerSkipBrowserWidget::disableSkips,                                                       this,                                                           &SeerGdbWidget::handleGdbSkipDisable);

    QObject::connect(stackManagerWidget->stackFramesBrowserWidget(),            &SeerStackFramesBrowserWidget::refreshStackFrames,                                          this,                                                           &SeerGdbWidget::handleGdbStackListFrames);
    QObject::connect(stackManagerWidget->stackFramesBrowserWidget(),            &SeerStackFramesBrowserWidget::selectedFrame,                                               this,                                                           &SeerGdbWidget::handleGdbStackSelectFrame);
    QObject::connect(stackManagerWidget->stackFramesBrowserWidget(),            &SeerStackFramesBrowserWidget::selectedFile,                                                editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);
    QObject::connect(stackManagerWidget->stackFramesBrowserWidget(),            &SeerStackFramesBrowserWidget::selectedAddress,                                             editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenAddress);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::refreshStackArguments,                                    this,                                                           &SeerGdbWidget::handleGdbStackListArguments);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::addVariableLoggerExpression,                              variableManagerWidget->variableLoggerBrowserWidget(),           &SeerVariableLoggerBrowserWidget::addVariableExpression);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::addVariableTrackerExpression,                             this,                                                           &SeerGdbWidget::handleGdbDataAddExpression);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::addMemoryVisualizer,                                      this,                                                           &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::addArrayVisualizer,                                       this,                                                           &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::addMatrixVisualizer,                                      this,                                                           &SeerGdbWidget::handleGdbMatrixAddExpression);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::addStructVisualizer,                                      this,                                                           &SeerGdbWidget::handleGdbVarAddExpression);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::refreshVariableTrackerValues,                             this,                                                           &SeerGdbWidget::handleGdbDataListExpressions);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::refreshStackLocals,                                          this,                                                           &SeerGdbWidget::handleGdbStackListLocals);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::addVariableLoggerExpression,                                 variableManagerWidget->variableLoggerBrowserWidget(),           &SeerVariableLoggerBrowserWidget::addVariableExpression);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::addVariableTrackerExpression,                                this,                                                           &SeerGdbWidget::handleGdbDataAddExpression);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::addMemoryVisualizer,                                         this,                                                           &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::addArrayVisualizer,                                          this,                                                           &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::addMatrixVisualizer,                                         this,                                                           &SeerGdbWidget::handleGdbMatrixAddExpression);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::addStructVisualizer,                                         this,                                                           &SeerGdbWidget::handleGdbVarAddExpression);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::refreshVariableTrackerValues,                                this,                                                           &SeerGdbWidget::handleGdbDataListExpressions);
    QObject::connect(stackManagerWidget->stackDumpBrowserWidget(),              &SeerStackDumpBrowserWidget::refreshStackPointer,                                           this,                                                           &SeerGdbWidget::handleGdbDataEvaluateExpression);
    QObject::connect(stackManagerWidget->stackDumpBrowserWidget(),              &SeerStackDumpBrowserWidget::refreshStackDump,                                              this,                                                           QOverload<int,QString,int,int>::of(&SeerGdbWidget::handleGdbMemoryEvaluateExpression));
    QObject::connect(stackManagerWidget->stackDumpBrowserWidget(),              &SeerStackDumpBrowserWidget::addMemoryVisualizer,                                           this,                                                           &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(stackManagerWidget,                                        &SeerStackManagerWidget::refreshThreadFrames,                                               this,                                                           &SeerGdbWidget::handleGdbThreadListFrames);

    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::refreshVariableTrackerNames,                             this,                                                           &SeerGdbWidget::handleGdbDataListExpressions);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::refreshVariableTrackerValues,                            this,                                                           &SeerGdbWidget::handleGdbDataListValues);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::addVariableExpression,                                   this,                                                           &SeerGdbWidget::handleGdbDataAddExpression);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::deleteVariableExpressions,                               this,                                                           &SeerGdbWidget::handleGdbDataDeleteExpressions);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::addMemoryVisualizer,                                     this,                                                           &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::addArrayVisualizer,                                      this,                                                           &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::addMatrixVisualizer,                                     this,                                                           &SeerGdbWidget::handleGdbMatrixAddExpression);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::addStructVisualizer,                                     this,                                                           &SeerGdbWidget::handleGdbVarAddExpression);
    QObject::connect(variableManagerWidget->variableLoggerBrowserWidget(),      &SeerVariableLoggerBrowserWidget::evaluateVariableExpression,                               this,                                                           &SeerGdbWidget::handleGdbDataEvaluateExpression);
    QObject::connect(variableManagerWidget->variableLoggerBrowserWidget(),      &SeerVariableLoggerBrowserWidget::addMemoryVisualizer,                                      this,                                                           &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(variableManagerWidget->variableLoggerBrowserWidget(),      &SeerVariableLoggerBrowserWidget::addArrayVisualizer,                                       this,                                                           &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(variableManagerWidget->variableLoggerBrowserWidget(),      &SeerVariableLoggerBrowserWidget::addMatrixVisualizer,                                      this,                                                           &SeerGdbWidget::handleGdbMatrixAddExpression);
    QObject::connect(variableManagerWidget->variableLoggerBrowserWidget(),      &SeerVariableLoggerBrowserWidget::addStructVisualizer,                                      this,                                                           &SeerGdbWidget::handleGdbVarAddExpression);
    QObject::connect(variableManagerWidget->registerValuesBrowserWidget(),      &SeerRegisterValuesBrowserWidget::refreshRegisterNames,                                     this,                                                           &SeerGdbWidget::handleGdbRegisterListNames);
    QObject::connect(variableManagerWidget->registerValuesBrowserWidget(),      &SeerRegisterValuesBrowserWidget::refreshRegisterValues,                                    this,                                                           &SeerGdbWidget::handleGdbRegisterListValues);
    QObject::connect(variableManagerWidget->registerValuesBrowserWidget(),      &SeerRegisterValuesBrowserWidget::setRegisterValue,                                         this,                                                           &SeerGdbWidget::handleGdbRegisterSetValue);

    QObject::connect(threadManagerWidget->threadFramesBrowserWidget(),          &SeerThreadFramesBrowserWidget::refreshThreadIds,                                           this,                                                           &SeerGdbWidget::handleGdbThreadListIds);
    QObject::connect(threadManagerWidget->threadFramesBrowserWidget(),          &SeerThreadFramesBrowserWidget::refreshThreadFrames,                                        this,                                                           &SeerGdbWidget::handleGdbThreadListFrames);
    QObject::connect(threadManagerWidget->threadFramesBrowserWidget(),          &SeerThreadFramesBrowserWidget::selectedFile,                                               editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);
    QObject::connect(threadManagerWidget->threadFramesBrowserWidget(),          &SeerThreadFramesBrowserWidget::selectedThread,                                             this,                                                           &SeerGdbWidget::handleGdbThreadSelectId);
    QObject::connect(threadManagerWidget->threadFramesBrowserWidget(),          &SeerThreadFramesBrowserWidget::nextThreadId,                                               this,                                                           &SeerGdbWidget::handleGdbNextThreadId);
    QObject::connect(threadManagerWidget->threadFramesBrowserWidget(),          &SeerThreadFramesBrowserWidget::stepThreadId,                                               this,                                                           &SeerGdbWidget::handleGdbStepThreadId);
    QObject::connect(threadManagerWidget->threadFramesBrowserWidget(),          &SeerThreadFramesBrowserWidget::finishThreadId,                                             this,                                                           &SeerGdbWidget::handleGdbFinishThreadId);
    QObject::connect(threadManagerWidget->threadFramesBrowserWidget(),          &SeerThreadFramesBrowserWidget::continueThreadId,                                           this,                                                           &SeerGdbWidget::handleGdbContinueThreadId);
    QObject::connect(threadManagerWidget->threadFramesBrowserWidget(),          &SeerThreadFramesBrowserWidget::interruptThreadId,                                          this,                                                           &SeerGdbWidget::handleGdbInterruptThreadId);

    QObject::connect(threadManagerWidget->threadIdsBrowserWidget(),             &SeerThreadIdsBrowserWidget::refreshThreadIds,                                              this,                                                           &SeerGdbWidget::handleGdbThreadListIds);
    QObject::connect(threadManagerWidget->threadIdsBrowserWidget(),             &SeerThreadIdsBrowserWidget::selectedThread,                                                this,                                                           &SeerGdbWidget::handleGdbThreadSelectId);
    QObject::connect(threadManagerWidget->threadIdsBrowserWidget(),             &SeerThreadIdsBrowserWidget::nextThreadId,                                                  this,                                                           &SeerGdbWidget::handleGdbNextThreadId);
    QObject::connect(threadManagerWidget->threadIdsBrowserWidget(),             &SeerThreadIdsBrowserWidget::stepThreadId,                                                  this,                                                           &SeerGdbWidget::handleGdbStepThreadId);
    QObject::connect(threadManagerWidget->threadIdsBrowserWidget(),             &SeerThreadIdsBrowserWidget::finishThreadId,                                                this,                                                           &SeerGdbWidget::handleGdbFinishThreadId);
    QObject::connect(threadManagerWidget->threadIdsBrowserWidget(),             &SeerThreadIdsBrowserWidget::continueThreadId,                                              this,                                                           &SeerGdbWidget::handleGdbContinueThreadId);
    QObject::connect(threadManagerWidget->threadIdsBrowserWidget(),             &SeerThreadIdsBrowserWidget::interruptThreadId,                                             this,                                                           &SeerGdbWidget::handleGdbInterruptThreadId);

    QObject::connect(threadManagerWidget->threadGroupsBrowserWidget(),          &SeerThreadGroupsBrowserWidget::refreshThreadGroups,                                        this,                                                           &SeerGdbWidget::handleGdbThreadListGroups);
    QObject::connect(threadManagerWidget->threadGroupsBrowserWidget(),          &SeerThreadGroupsBrowserWidget::runThreadGroup,                                             this,                                                           &SeerGdbWidget::handleGdbRunThreadGroup);
    QObject::connect(threadManagerWidget->threadGroupsBrowserWidget(),          &SeerThreadGroupsBrowserWidget::startThreadGroup,                                           this,                                                           &SeerGdbWidget::handleGdbStartThreadGroup);
    QObject::connect(threadManagerWidget->threadGroupsBrowserWidget(),          &SeerThreadGroupsBrowserWidget::continueThreadGroup,                                        this,                                                           &SeerGdbWidget::handleGdbContinueThreadGroup);
    QObject::connect(threadManagerWidget->threadGroupsBrowserWidget(),          &SeerThreadGroupsBrowserWidget::interruptThreadGroup,                                       this,                                                           &SeerGdbWidget::handleGdbInterruptThreadGroup);

    QObject::connect(threadManagerWidget->adaTasksBrowserWidget(),              &SeerAdaTasksBrowserWidget::refreshAdaTasks,                                                this,                                                           &SeerGdbWidget::handleGdbAdaListTasks);
    QObject::connect(threadManagerWidget->adaTasksBrowserWidget(),              &SeerAdaTasksBrowserWidget::selectedThread,                                                 this,                                                           &SeerGdbWidget::handleGdbThreadSelectId);

    QObject::connect(threadManagerWidget,                                       &SeerThreadManagerWidget::schedulerLockingModeChanged,                                      this,                                                           &SeerGdbWidget::handleGdbSchedulerLockingMode);
    QObject::connect(threadManagerWidget,                                       &SeerThreadManagerWidget::scheduleMultipleModeChanged,                                      this,                                                           &SeerGdbWidget::handleGdbScheduleMultipleMode);
    QObject::connect(threadManagerWidget,                                       &SeerThreadManagerWidget::forkFollowsModeChanged,                                           this,                                                           &SeerGdbWidget::handleGdbForkFollowMode);

    QObject::connect(_breakpointsBrowserWidget,                                 &SeerBreakpointsBrowserWidget::refreshBreakpointsList,                                      this,                                                           &SeerGdbWidget::handleGdbGenericpointList);
    QObject::connect(_breakpointsBrowserWidget,                                 &SeerBreakpointsBrowserWidget::deleteBreakpoints,                                           this,                                                           &SeerGdbWidget::handleGdbBreakpointDelete);
    QObject::connect(_breakpointsBrowserWidget,                                 &SeerBreakpointsBrowserWidget::enableBreakpoints,                                           this,                                                           &SeerGdbWidget::handleGdbBreakpointEnable);
    QObject::connect(_breakpointsBrowserWidget,                                 &SeerBreakpointsBrowserWidget::disableBreakpoints,                                          this,                                                           &SeerGdbWidget::handleGdbBreakpointDisable);
    QObject::connect(_breakpointsBrowserWidget,                                 &SeerBreakpointsBrowserWidget::insertBreakpoint,                                            this,                                                           &SeerGdbWidget::handleGdbBreakpointInsert);
    QObject::connect(_breakpointsBrowserWidget,                                 &SeerBreakpointsBrowserWidget::addBreakpointCondition,                                      this,                                                           &SeerGdbWidget::handleGdbBreakpointCondition);
    QObject::connect(_breakpointsBrowserWidget,                                 &SeerBreakpointsBrowserWidget::addBreakpointIgnore,                                         this,                                                           &SeerGdbWidget::handleGdbBreakpointIgnore);
    QObject::connect(_breakpointsBrowserWidget,                                 &SeerBreakpointsBrowserWidget::addBreakpointCommands,                                       this,                                                           &SeerGdbWidget::handleGdbBreakpointCommands);
    QObject::connect(_breakpointsBrowserWidget,                                 &SeerBreakpointsBrowserWidget::selectedFile,                                                editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);
    QObject::connect(_breakpointsBrowserWidget,                                 &SeerBreakpointsBrowserWidget::selectedAddress,                                             editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenAddress);

    QObject::connect(_watchpointsBrowserWidget,                                 &SeerWatchpointsBrowserWidget::refreshWatchpointsList,                                      this,                                                           &SeerGdbWidget::handleGdbGenericpointList);
    QObject::connect(_watchpointsBrowserWidget,                                 &SeerWatchpointsBrowserWidget::deleteWatchpoints,                                           this,                                                           &SeerGdbWidget::handleGdbWatchpointDelete);
    QObject::connect(_watchpointsBrowserWidget,                                 &SeerWatchpointsBrowserWidget::enableWatchpoints,                                           this,                                                           &SeerGdbWidget::handleGdbWatchpointEnable);
    QObject::connect(_watchpointsBrowserWidget,                                 &SeerWatchpointsBrowserWidget::disableWatchpoints,                                          this,                                                           &SeerGdbWidget::handleGdbWatchpointDisable);
    QObject::connect(_watchpointsBrowserWidget,                                 &SeerWatchpointsBrowserWidget::insertWatchpoint,                                            this,                                                           &SeerGdbWidget::handleGdbWatchpointInsert);
    QObject::connect(_watchpointsBrowserWidget,                                 &SeerWatchpointsBrowserWidget::addBreakpointCondition,                                      this,                                                           &SeerGdbWidget::handleGdbBreakpointCondition);
    QObject::connect(_watchpointsBrowserWidget,                                 &SeerWatchpointsBrowserWidget::addBreakpointIgnore,                                         this,                                                           &SeerGdbWidget::handleGdbBreakpointIgnore);
    QObject::connect(_watchpointsBrowserWidget,                                 &SeerWatchpointsBrowserWidget::addBreakpointCommands,                                       this,                                                           &SeerGdbWidget::handleGdbBreakpointCommands);
    QObject::connect(_watchpointsBrowserWidget,                                 &SeerWatchpointsBrowserWidget::selectedFile,                                                editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);

    QObject::connect(_catchpointsBrowserWidget,                                 &SeerCatchpointsBrowserWidget::refreshCatchpointsList,                                      this,                                                           &SeerGdbWidget::handleGdbGenericpointList);
    QObject::connect(_catchpointsBrowserWidget,                                 &SeerCatchpointsBrowserWidget::deleteCatchpoints,                                           this,                                                           &SeerGdbWidget::handleGdbCatchpointDelete);
    QObject::connect(_catchpointsBrowserWidget,                                 &SeerCatchpointsBrowserWidget::enableCatchpoints,                                           this,                                                           &SeerGdbWidget::handleGdbCatchpointEnable);
    QObject::connect(_catchpointsBrowserWidget,                                 &SeerCatchpointsBrowserWidget::disableCatchpoints,                                          this,                                                           &SeerGdbWidget::handleGdbCatchpointDisable);
    QObject::connect(_catchpointsBrowserWidget,                                 &SeerCatchpointsBrowserWidget::insertCatchpoint,                                            this,                                                           &SeerGdbWidget::handleGdbCatchpointInsert);
    QObject::connect(_catchpointsBrowserWidget,                                 &SeerCatchpointsBrowserWidget::addBreakpointCondition,                                      this,                                                           &SeerGdbWidget::handleGdbBreakpointCondition);
    QObject::connect(_catchpointsBrowserWidget,                                 &SeerCatchpointsBrowserWidget::addBreakpointIgnore,                                         this,                                                           &SeerGdbWidget::handleGdbBreakpointIgnore);
    QObject::connect(_catchpointsBrowserWidget,                                 &SeerCatchpointsBrowserWidget::addBreakpointCommands,                                       this,                                                           &SeerGdbWidget::handleGdbBreakpointCommands);

    QObject::connect(_printpointsBrowserWidget,                                 &SeerPrintpointsBrowserWidget::refreshPrintpointsList,                                      this,                                                           &SeerGdbWidget::handleGdbGenericpointList);
    QObject::connect(_printpointsBrowserWidget,                                 &SeerPrintpointsBrowserWidget::deletePrintpoints,                                           this,                                                           &SeerGdbWidget::handleGdbPrintpointDelete);
    QObject::connect(_printpointsBrowserWidget,                                 &SeerPrintpointsBrowserWidget::enablePrintpoints,                                           this,                                                           &SeerGdbWidget::handleGdbPrintpointEnable);
    QObject::connect(_printpointsBrowserWidget,                                 &SeerPrintpointsBrowserWidget::disablePrintpoints,                                          this,                                                           &SeerGdbWidget::handleGdbPrintpointDisable);
    QObject::connect(_printpointsBrowserWidget,                                 &SeerPrintpointsBrowserWidget::insertPrintpoint,                                            this,                                                           &SeerGdbWidget::handleGdbPrintpointInsert);
    QObject::connect(_printpointsBrowserWidget,                                 &SeerPrintpointsBrowserWidget::addBreakpointCondition,                                      this,                                                           &SeerGdbWidget::handleGdbBreakpointCondition);
    QObject::connect(_printpointsBrowserWidget,                                 &SeerPrintpointsBrowserWidget::addBreakpointIgnore,                                         this,                                                           &SeerGdbWidget::handleGdbBreakpointIgnore);
    QObject::connect(_printpointsBrowserWidget,                                 &SeerPrintpointsBrowserWidget::addBreakpointCommand,                                        this,                                                           &SeerGdbWidget::handleGdbBreakpointCommand);

    QObject::connect(_checkpointsBrowserWidget,                                 &SeerCheckpointsBrowserWidget::refreshCheckpointsList,                                      this,                                                           &SeerGdbWidget::handleGdbCheckpointList);
    QObject::connect(_checkpointsBrowserWidget,                                 &SeerCheckpointsBrowserWidget::insertCheckpoint,                                            this,                                                           &SeerGdbWidget::handleGdbCheckpointInsert);
    QObject::connect(_checkpointsBrowserWidget,                                 &SeerCheckpointsBrowserWidget::selectCheckpoint,                                            this,                                                           &SeerGdbWidget::handleGdbCheckpointSelect);
    QObject::connect(_checkpointsBrowserWidget,                                 &SeerCheckpointsBrowserWidget::deleteCheckpoints,                                           this,                                                           &SeerGdbWidget::handleGdbCheckpointDelete);

    QObject::connect(this,                                                      &SeerGdbWidget::assemblyConfigChanged,                                                      editorManagerWidget,                                            &SeerEditorManagerWidget::handleAssemblyConfigChanged);

    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       stackManagerWidget,                                             &SeerStackManagerWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       stackManagerWidget->stackFramesBrowserWidget(),                 &SeerStackFramesBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       stackManagerWidget->stackLocalsBrowserWidget(),                 &SeerStackLocalsBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       stackManagerWidget->stackArgumentsBrowserWidget(),              &SeerStackArgumentsBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       stackManagerWidget->stackDumpBrowserWidget(),                   &SeerStackDumpBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       threadManagerWidget->threadFramesBrowserWidget(),               &SeerThreadFramesBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       threadManagerWidget->threadIdsBrowserWidget(),                  &SeerThreadIdsBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       threadManagerWidget->adaTasksBrowserWidget(),                   &SeerAdaTasksBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       variableManagerWidget->registerValuesBrowserWidget(),           &SeerRegisterValuesBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       variableManagerWidget->variableTrackerBrowserWidget(),          &SeerVariableTrackerBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       _breakpointsBrowserWidget,                                      &SeerBreakpointsBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       _watchpointsBrowserWidget,                                      &SeerWatchpointsBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       _catchpointsBrowserWidget,                                      &SeerCatchpointsBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       _printpointsBrowserWidget,                                      &SeerPrintpointsBrowserWidget::handleStoppingPointReached);
    QObject::connect(this,                                                      &SeerGdbWidget::stoppingPointReached,                                                       _checkpointsBrowserWidget,                                      &SeerCheckpointsBrowserWidget::handleStoppingPointReached);

    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          editorManagerWidget,                                            &SeerEditorManagerWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          sourceLibraryManagerWidget->sourceBrowserWidget(),              &SeerSourceBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          sourceLibraryManagerWidget->functionBrowserWidget(),            &SeerFunctionBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          sourceLibraryManagerWidget->functionBrowserWidget(),            &SeerFunctionBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          sourceLibraryManagerWidget->staticBrowserWidget(),              &SeerStaticBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          sourceLibraryManagerWidget->libraryBrowserWidget(),             &SeerLibraryBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          sourceLibraryManagerWidget->adaExceptionsBrowserWidget(),       &SeerAdaExceptionsBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          sourceLibraryManagerWidget->skipBrowserWidget(),                &SeerSkipBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          variableManagerWidget->variableLoggerBrowserWidget(),           &SeerVariableLoggerBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          variableManagerWidget->variableTrackerBrowserWidget(),          &SeerVariableTrackerBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          variableManagerWidget->registerValuesBrowserWidget(),           &SeerRegisterValuesBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          stackManagerWidget,                                             &SeerStackManagerWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          stackManagerWidget->stackFramesBrowserWidget(),                 &SeerStackFramesBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          stackManagerWidget->stackLocalsBrowserWidget(),                 &SeerStackLocalsBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          stackManagerWidget->stackArgumentsBrowserWidget(),              &SeerStackArgumentsBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          stackManagerWidget->stackDumpBrowserWidget(),                   &SeerStackDumpBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          threadManagerWidget->threadFramesBrowserWidget(),               &SeerThreadFramesBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          threadManagerWidget->threadIdsBrowserWidget(),                  &SeerThreadIdsBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          threadManagerWidget->threadGroupsBrowserWidget(),               &SeerThreadGroupsBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          threadManagerWidget->adaTasksBrowserWidget(),                   &SeerAdaTasksBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          _breakpointsBrowserWidget,                                      &SeerBreakpointsBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          _watchpointsBrowserWidget,                                      &SeerWatchpointsBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          _catchpointsBrowserWidget,                                      &SeerCatchpointsBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          _printpointsBrowserWidget,                                      &SeerPrintpointsBrowserWidget::handleSessionTerminated);
    QObject::connect(this,                                                      &SeerGdbWidget::sessionTerminated,                                                          _checkpointsBrowserWidget,                                      &SeerCheckpointsBrowserWidget::handleSessionTerminated);

    QObject::connect(leftCenterRightSplitter,                                   &QSplitter::splitterMoved,                                                                  this,                                                           &SeerGdbWidget::handleSplitterMoved);
    QObject::connect(sourceLibraryVariableManagerSplitter,                      &QSplitter::splitterMoved,                                                                  this,                                                           &SeerGdbWidget::handleSplitterMoved);
    QObject::connect(codeManagerLogTabsSplitter,                                &QSplitter::splitterMoved,                                                                  this,                                                           &SeerGdbWidget::handleSplitterMoved);
    QObject::connect(stackThreadManagerSplitter,                                &QSplitter::splitterMoved,                                                                  this,                                                           &SeerGdbWidget::handleSplitterMoved);
    QObject::connect(manualCommandComboBox,                                     QOverload<int>::of(&QComboBox::activated),                                                  this,                                                           &SeerGdbWidget::handleManualCommandChanged);
    QObject::connect(_gdbOutputLog,                                             &SeerLogWidget::logEnabledChanged,                                                          this,                                                           &SeerGdbWidget::handleLogOuputChanged);
    QObject::connect(_gdbOutputLog,                                             &SeerGdbLogWidget::refreshBreakpointsList,                                                  this,                                                           &SeerGdbWidget::handleGdbGenericpointList);
    QObject::connect(_seerOutputLog,                                            &SeerLogWidget::logEnabledChanged,                                                          this,                                                           &SeerGdbWidget::handleLogOuputChanged);
    QObject::connect(_seerOutputLog,                                            &SeerLogWidget::logTimeStampChanged,                                                        this,                                                           &SeerGdbWidget::handleLogOuputChanged);

    QObject::connect(breakpointsLoadToolButton,                                 &QToolButton::clicked,                                                                      this,                                                           &SeerGdbWidget::handleGdbLoadBreakpoints);
    QObject::connect(breakpointsSaveToolButton,                                 &QToolButton::clicked,                                                                      this,                                                           &SeerGdbWidget::handleGdbSaveBreakpoints);
    QObject::connect(helpToolButton,                                            &QToolButton::clicked,                                                                      this,                                                           &SeerGdbWidget::handleHelpToolButtonClicked);

    // openocd: if OpenOCD failed to start because the port is already in use, run handleOpenOCDStartFailed
    QObject::connect(openocdWidget,                                             &SeerOpenOCDWidget::openocdStartFailed,                                                     this,                                                           &SeerGdbWidget::handleOpenOCDStartFailed);
#if ENABLE_GDB_LOGOUT == 1
    // For debuging
    QObject::connect(this,                                                      &SeerGdbWidget::allTextOutput,                                                              _gdbOutputLog,                                                  &SeerGdbLogWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::allTextOutput,                                                                 _gdbOutputLog,                                                  &SeerGdbLogWidget::handleText);
#endif
    // For debug on init
    QObject::connect(this,                                                      &SeerGdbWidget::requestRefreshSource,                                                       this,                                                           &SeerGdbWidget::handleGdbExecutableSources);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               this,                                                           &SeerGdbWidget::handleText);
    // For handling tracing functions, variables and types
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::seekIdentifierForward,                                            this,                                                           &SeerGdbWidget::handleSeekIdentifier);

    // Restore window settings.
    readSettings();
}

SeerGdbWidget::~SeerGdbWidget () {

    deleteConsole();

    if (_gdbMonitor) {
        delete _gdbMonitor;
    }

    if (_gdbProcess) {
        _gdbProcess->kill();
        _gdbProcess->waitForFinished();
        delete _gdbProcess;
    }
}

GdbMonitor* SeerGdbWidget::gdbMonitor () {
    return _gdbMonitor;
}

QProcess* SeerGdbWidget::gdbProcess () {
    return _gdbProcess;
}

void SeerGdbWidget::setExecutableName (const QString& executableName) {

    _executableName = executableName;

    setNewExecutableFlag(true);
}

const QString& SeerGdbWidget::executableName () const {
    return _executableName;
}

void SeerGdbWidget::setExecutableSymbolName (const QString& executableSymbolName) {

    _executableSymbolName = executableSymbolName;

    setNewExecutableFlag(true);
}

const QString& SeerGdbWidget::executableSymbolName () const {
    return _executableSymbolName;
}

void SeerGdbWidget::setNewExecutableFlag (bool flag) {

    _newExecutableFlag = flag;
}

bool SeerGdbWidget::newExecutableFlag () const {
    return _newExecutableFlag;
}

void SeerGdbWidget::setExecutableArguments (const QString& executableArguments) {

    _executableArguments = executableArguments;
}

const QString& SeerGdbWidget::executableArguments () const {
    return _executableArguments;
}

void SeerGdbWidget::setExecutableWorkingDirectory (const QString& executableWorkingDirectory) {

    _executableWorkingDirectory = executableWorkingDirectory;
}

const QString& SeerGdbWidget::executableWorkingDirectory () const {
    return _executableWorkingDirectory;
}

void SeerGdbWidget::setExecutableBreakpointsFilename (const QString& breakpointsFilename) {
    _executableBreakpointsFilename = breakpointsFilename;
}

const QString& SeerGdbWidget::executableBreakpointsFilename () const {
    return _executableBreakpointsFilename;
}

void SeerGdbWidget::setExecutableBreakpointFunctionName (const QString& nameoraddress) {
    _executableBreakpointFunctionName = nameoraddress;
}

const QString& SeerGdbWidget::executableBreakpointFunctionName () const {
    return _executableBreakpointFunctionName;
}

void SeerGdbWidget::setExecutableBreakpointSourceName (const QString& sourceFilenameAndLineno) {
    _executableBreakpointSourceName = sourceFilenameAndLineno;
}

const QString& SeerGdbWidget::executableBreakpointSourceName () const {
    return _executableBreakpointSourceName;
}

void SeerGdbWidget::setExecutablePid (int pid) {
    _executablePid = pid;
}

int SeerGdbWidget::executablePid () const {
    return _executablePid;
}

void SeerGdbWidget::setExecutableConnectHostPort (const QString& connectHostPort) {
    _executableConnectHostPort = connectHostPort;
}

const QString& SeerGdbWidget::executableConnectHostPort () const {
    return _executableConnectHostPort;
}

void SeerGdbWidget::setExecutableRRTraceDirectory (const QString& rrTraceDirectory) {
    _executableRRTraceDirectory = rrTraceDirectory;
}

const QString& SeerGdbWidget::executableRRTraceDirectory () const {
    return _executableRRTraceDirectory;
}

void SeerGdbWidget::setExecutableCoreFilename (const QString& coreFilename) {
    _executableCoreFilename = coreFilename;
}

const QString& SeerGdbWidget::executableCoreFilename () const {
    return _executableCoreFilename;
}

void SeerGdbWidget::setExecutablePreGdbCommands (const QStringList& preGdbCommands) {
    _executablePreGdbCommands = preGdbCommands;
}

const QStringList& SeerGdbWidget::executablePreGdbCommands () const {
    return _executablePreGdbCommands;
}

void SeerGdbWidget::setExecutablePostGdbCommands (const QStringList& postGdbCommands) {
    _executablePostGdbCommands = postGdbCommands;
}

const QStringList& SeerGdbWidget::executablePostGdbCommands () const {
    return _executablePostGdbCommands;
}

void SeerGdbWidget::setExecutableLaunchMode (const QString& launchMode) {
    _executableLaunchMode = launchMode;
}

const QString& SeerGdbWidget::executableLaunchMode () const {
    return _executableLaunchMode;
}

const QString& SeerGdbWidget::executableBreakMode () const {
    return _executableBreakMode;
}

void SeerGdbWidget::saveLaunchMode() {
    _executableLaunchModeBackup = _executableLaunchMode;
    _executableBreakModeBackup  = _executableBreakMode;
}

void SeerGdbWidget::restoreLaunchMode() {
    _executableLaunchMode = _executableLaunchModeBackup;
    _executableBreakMode  = _executableBreakModeBackup;
}

bool SeerGdbWidget::hasBackupLaunchMode () const {
    if (_executableLaunchModeBackup == "") {
        return false;
    }

    return true;
}

void SeerGdbWidget::clearBackupLaunchMode () {
    _executableLaunchModeBackup = "";
    _executableBreakModeBackup  = "";
}

const QString& SeerGdbWidget::backupLaunchMode () const {
    return _executableLaunchModeBackup;
}

void SeerGdbWidget::setGdbProgram (const QString& program) {

    _gdbProgram = program;
}

QString SeerGdbWidget::gdbProgram () const {

    return _gdbProgram;
}

void SeerGdbWidget::setGdbArguments (const QString& arguments) {

    _gdbArguments = arguments;
}

QString SeerGdbWidget::gdbArguments () const {

    return _gdbArguments;
}

void SeerGdbWidget::setGdbProgramOverride (const QString& program) {

    _gdbProgramOverride = program;
}

QString SeerGdbWidget::gdbProgramOverride () const {

    return _gdbProgramOverride;
}

void SeerGdbWidget::setGdbArgumentsOverride (const QString& arguments) {

    _gdbArgumentsOverride = arguments;
}

QString SeerGdbWidget::gdbArgumentsOverride () const {

    return _gdbArgumentsOverride;
}

void SeerGdbWidget::setRRProgram (const QString& program) {

    _gdbRRProgram = program;
}

QString SeerGdbWidget::rrProgram () const {

    return _gdbRRProgram;
}

void SeerGdbWidget::setRRArguments (const QString& arguments) {

    _gdbRRArguments = arguments;
}

QString SeerGdbWidget::rrArguments () const {

    return _gdbRRArguments;
}

void SeerGdbWidget::setRRGdbArguments (const QString& arguments) {

    _gdbRRGdbArguments = arguments;
}

QString SeerGdbWidget::rrGdbArguments () const {

    return _gdbRRGdbArguments;
}

void SeerGdbWidget::setGdbAsyncMode (bool flag) {

    _gdbASyncMode = flag;
}

bool SeerGdbWidget::gdbAsyncMode () const {

    return _gdbASyncMode;
}

void SeerGdbWidget::setGdbNonStopMode (bool flag) {

    _gdbNonStopMode = flag;
}

bool SeerGdbWidget::gdbNonStopMode () const {

    return _gdbNonStopMode;
}

void SeerGdbWidget::setGdbServerDebug (bool flag) {

    _gdbServerDebug = flag;
}

bool SeerGdbWidget::gdbServerDebug () const {

    return _gdbServerDebug;
}

void SeerGdbWidget::setGdbHandleTerminatingException (bool flag) {

    _gdbHandleTerminatingException = flag;
}

bool SeerGdbWidget::gdbHandleTerminatingException () const {

    return _gdbHandleTerminatingException;
}

void SeerGdbWidget::setGdbRandomizeStartAddress (bool flag) {

    _gdbRandomizeStartAddress = flag;
}

bool SeerGdbWidget::gdbRandomizeStartAddress () const {

    return _gdbRandomizeStartAddress;
}

void SeerGdbWidget::setGdbEnablePrettyPrinting (bool flag) {

    _gdbEnablePrettyPrinting = flag;
}

bool SeerGdbWidget::gdbEnablePrettyPrinting () const {

    return _gdbEnablePrettyPrinting;
}

void SeerGdbWidget::setGdbRemoteTargetType (const QString& type) {

    _gdbRemoteTargetType = type;
}

QString SeerGdbWidget::gdbRemoteTargetType () const {

    return _gdbRemoteTargetType;
}

void SeerGdbWidget::setGdbRecordMode(const QString& mode) {

    if (mode == "auto") {
        if (gdbProgram().contains("udb")) {
            _gdbRecordMode = "udb";
        }else{
            _gdbRecordMode = "";
        }
    }else{
        _gdbRecordMode = mode;
    }

    if (_gdbRecordMode != "rr" && _gdbRecordMode != "udb" && _gdbRecordMode != "") {
        handleGdbCommand("record " + _gdbRecordMode);
    }

    emit recordSettingsChanged();
}

QString SeerGdbWidget::gdbRecordMode () const {

    return _gdbRecordMode;
}

void SeerGdbWidget::setGdbRecordDirection (const QString& direction) {

    _gdbRecordDirection = direction;

    emit recordSettingsChanged();
}

QString SeerGdbWidget::gdbRecordDirection () const {

    return _gdbRecordDirection;
}

SeerEditorManagerWidget* SeerGdbWidget::editorManager () {

    return editorManagerWidget;
}

const SeerEditorManagerWidget* SeerGdbWidget::editorManager () const {

    return editorManagerWidget;
}

void SeerGdbWidget::addMessage (const QString& message, QMessageBox::Icon messageType) {

    if (_messagesBrowserWidget == 0) {
        return;
    }

    _messagesBrowserWidget->addMessage(message, messageType);
}

void SeerGdbWidget::handleLogsTabMoved (int to, int from) {

    Q_UNUSED(from);
    Q_UNUSED(to);

    // Keep track of console tab if it moved.
    if (_consoleIndex == from) {
        _consoleIndex = to;
    }

    // Don't handle anything here if Seer is exiting.
    if (isQuitting()) {
        return;
    }

    writeLogsSettings();
}

void SeerGdbWidget::handleLogsTabChanged (int index) {

    Q_UNUSED(index);

    // Don't handle anything here if Seer is exiting.
    if (isQuitting()) {
        return;
    }

    writeLogsSettings();
}

void SeerGdbWidget::handleRaiseMessageTab () {

    int idx = logsTabWidget->indexOf(_messagesBrowserWidget);

    if (idx < 0) {
        return;
    }

    logsTabWidget->setCurrentIndex(idx);
}

void SeerGdbWidget::writeLogsSettings () {

    // Write tab order to settings.
    QStringList tabs;

    for (int i=0; i<logsTabWidget->tabBar()->count(); i++) {
        tabs.append(logsTabWidget->tabBar()->tabText(i));
    }

    QString current = logsTabWidget->tabBar()->tabText(logsTabWidget->tabBar()->currentIndex());

    //qDebug() << "Tabs"    << tabs;
    //qDebug() << "Current" << current;

    QSettings settings;

    settings.beginGroup("logsmanagerwindow"); {
        settings.setValue("taborder",   tabs.join(','));
        settings.setValue("tabcurrent", current);
    } settings.endGroup();
}

void SeerGdbWidget::readLogsSettings () {

    // Can't move things?
    if (logsTabWidget->tabBar()->isMovable() == false) {
        return;
    }

    // Read tab order from settings.
    QSettings   settings;
    QStringList tabs;
    QString     current;

    settings.beginGroup("logsmanagerwindow"); {
        tabs    = settings.value("taborder").toString().split(',');
        current = settings.value("tabcurrent").toString();
    } settings.endGroup();

    //qDebug() << "Tabs"    << tabs;
    //qDebug() << "Current" << current;

    // Move tabs to the requested order.
    for (int i=0; i<tabs.count(); i++) {

        QString tab = tabs[i];
        int     tb  = -1;

        for (int j=0; j<logsTabWidget->tabBar()->count(); j++) {
            if (logsTabWidget->tabBar()->tabText(j) == tab) {
                tb = j;
                break;
            }
        }

        if (tb != -1) {
            logsTabWidget->tabBar()->moveTab(tb, i);
        }
    }

    // Find the console tab index.
    _consoleIndex = -1;
    for (int i=0; i<logsTabWidget->tabBar()->count(); i++) {
        if (logsTabWidget->tabBar()->tabText(i) == "Console output") {
            _consoleIndex = i;
            break;
        }
    }

    if (_consoleIndex < 0) {
        qDebug() << "The console tab index is not in the settings.";
    }

    // Make a tab current.
    if (current != "") {
        for (int i=0; i<logsTabWidget->tabBar()->count(); i++) {
            if (logsTabWidget->tabBar()->tabText(i) == current) {
                logsTabWidget->setCurrentIndex(i);
                break;
            }
        }
    }else{
        logsTabWidget->setCurrentIndex(0);
    }
}

void SeerGdbWidget::handleText (const QString& text) {

    if (text.startsWith("*running")) {
        // If debugging on init is true, raise _debugOnInitOperationCv.notify to tell threadHandler that target is stopped
        if (isDebugOnInit() || isSeekIdentifier())                       // if OpenOCD is running debug on init or seeing identifier,
        {
            _debugOnInitRunningMutex.lock();
            _debugOnInitRunningCv.notify_one();
            _debugOnInitRunningMutex.unlock();
        }
        if (_gdbmultiarchPid > 0)               // if openoocd target running, set target is running
            setGdbMultiarchRunningState(true);
    }
    if (text.startsWith("*running,thread-id=\"all\"")) {
    // Probably a better way to handle all these types of stops.
    } else if (text.startsWith("*stopped")) {
        if (isSeekIdentifier())                     // if OpenOCD is running seeing identifier, release mutex
        {
            _traceIdentiferStopMutex.lock();
            _traceIdentiferStopCv.notify_one();
            _traceIdentiferStopMutex.unlock();
        }
        if (isDebugOnInit())  // if OpenOCD is running debug on init, release mutex
        {
            QString reason_text = Seer::parseFirst(text, "reason=", '"', '"', false);
            QString signal_name = Seer::parseFirst(text, "signal-name=", '"', '"', false);
            QString func        = Seer::parseFirst(text, "func=", '"', '"', false);
            QString fullname    = Seer::parseFirst(text, "fullname=", '"', '"', false);
            if (reason_text == "signal-received" && signal_name == "SIGINT" && _sigINTDebugOnInitFlag)  // if _sigINTDebugOnInitFlag is raised
            {
                _debugOnInitStopMutex.lock();
                _sigINTDebugOnInitFlag = false;
                _debugOnInitStopCv.notify_one();
                _debugOnInitStopMutex.unlock();
            }
            else if (reason_text == "breakpoint-hit")
            {
                if (_debugOnInitFindLoadModuleFile)
                {
                    if (func == "load_module" )   // only release muxtex if breakpoint hits "load_module" func
                    {
                        _loadModuleFile = fullname;
                        _debugOnInitFindLoadModuleFile = false;
                        _debugOnInitStopMutex.lock();
                        _debugOnInitStopCv.notify_one();
                        _debugOnInitStopMutex.unlock();   
                    }
                }
            }
        }
        setGdbMultiarchRunningState(false);     // target stopped
        if (isNewHardwareBreakpointFlag() == true)
        {
            setNewHardwareBreakpointFlag(false);
        }
        emit stoppingPointReached();            // fix recursion first breakpoint hit

    }else if (text.startsWith("=breakpoint-created,")) {

        handleGdbGenericpointList();

    }else if (text.startsWith("=thread-group-started,")) {
        // =thread-group-started,id="i1",pid="30916"

        QString pid_text = Seer::parseFirst(text, "pid=", '"', '"', false);

        //qDebug() << "Inferior pid = " << pid_text;

        setExecutablePid(pid_text.toLong());

    }else if (text.startsWith("=thread-group-exited,")) {

        handleGdbTerminateExecutable(false);

    } else if (text.startsWith("^done"))            // For handling debug on init
    {
        if (!(isDebugOnInit() || isSeekIdentifier()))                       // if OpenOCD is running debug on init or seeing identifier,
            return;
        if (text == "^done")                        // When bp is disabled or enabled or when kernel module source is loaded
        {
            _debugOnInitHandleBpMutex.lock();
            _debugOnInitHandleBpCv.notify_one();
            _debugOnInitHandleBpMutex.unlock();             // release mutex and cond variable when bp is handled
            _debugOnInitOperationMutex.lock();
            _debugOnInitOperationCv.notify_one();
            _debugOnInitOperationMutex.unlock();            // release mutex and cond variable when kernel module source is loaded
            
        }
        else if (text.startsWith("^done,BreakpointTable"))  // request for current breakpoint on target
        {
            // If this function is already run
            if (_debugOnInitBpReadFlag == true)
                return;
            _debugOnInitListBpMutex.lock();
            // Copied from SeerBreakpointsBrowserWidget::handleText
            QString newtext = Seer::filterEscapes(text);
            QString body_text = Seer::parseFirst(text, "body=", '[', ']', false);
            if (body_text != "") {
                QStringList bkpt_list = Seer::parse(newtext, "bkpt=", '{', '}', false);
                for (const auto& bkpt_text : bkpt_list) {
                    QStringList items = Seer::parseCommaList(bkpt_text, '[', ']');
                    QMap<QString,QString> keyValueMap = Seer::createKeyValueMap(items, '=');
                    QString number_text            = Seer::filterBookends(keyValueMap["number"],            '"', '"');
                    QString enabled_text           = Seer::filterBookends(keyValueMap["enabled"],           '"', '"');
                    _mapListBpStatus.insert(number_text, enabled_text);
                }
            }
            _debugOnInitBpReadFlag = false;
            _debugOnInitListBpCv.notify_one();
            _debugOnInitListBpMutex.unlock();
        }
        else if (text.startsWith("^done,bkpt"))             // request for add temp breakpoint when performing debug on init
        {
            if (_debugOnInitTempBpFlag == true)
                return;
            _debugOnInitHandleBpMutex.lock();
            _debugOnInitTempBpFlag = false;
            _debugOnInitHandleBpCv.notify_one();
            _debugOnInitHandleBpMutex.unlock();             // release mutex and cond variable
        }
        else if (text.startsWith("^done,value"))             // request for reading kernel module allocation address
        {
            _debugOnInitOperationMutex.lock();
            // format: ^done,value={{battr = {attr = {name = 0xffffff800e5d7380 \".note.gnu.build-id\", mode = 256}, 
            // size = 19, private = 0x0, f_mapping = 0x0, read = 0xffffffc0800ba9fc <module_sect_read>, write = 0x0, 
            // mmap = 0x0}, address = 18446743800871268352},
            // What we need is name and address value
            _mapKernelModuleAddress.clear();
            int pos = 0;
            while (true) {
                int namePos = text.indexOf("name =", pos);
                if (namePos == -1) break;

                int quoteStart = text.indexOf('\"', namePos);
                int quoteEnd = text.indexOf('\"', quoteStart + 1);
                if (quoteStart == -1 || quoteEnd == -1) break;

                QString sectionName = text.mid(quoteStart + 1, quoteEnd - quoteStart - 1);
                while (1) {                 // remove redundant backslash
                    if (!sectionName.isEmpty() && sectionName.back() == '\\') {
                        sectionName.chop(1); // Removes the last character
                    } else {
                        break;
                    }
                }
                int addrPos = text.indexOf("address =", quoteEnd);
                if (addrPos == -1) break;

                // Find next comma or closing brace to end the address number
                int commaPos = text.indexOf(',', addrPos);
                int bracePos = text.indexOf('}', addrPos);
                int addrEnd;

                if (commaPos == -1 && bracePos == -1) {
                    addrEnd = text.length();
                } else if (commaPos == -1) {
                    addrEnd = bracePos;
                } else if (bracePos == -1) {
                    addrEnd = commaPos;
                } else {
                    addrEnd = qMin(commaPos, bracePos);
                }

                QString addrStr = text.mid(addrPos + 9, addrEnd - (addrPos + 9)).trimmed();
                bool ok;
                quint64 addrVal = addrStr.toULongLong(&ok);
                if (ok) {
                    // Zero-pad to 16 hex digits and lowercase
                    QString addrHex = QString("0x%1").arg(addrVal, 16, 16, QChar('0')).toLower();
                    _mapKernelModuleAddress[sectionName] = addrHex;
                }
                pos = addrEnd;
            }
            _debugOnInitOperationCv.notify_one();
            _debugOnInitOperationMutex.unlock();             // release mutex and cond variable
        }
        else if (text.startsWith("^done,files"))            // request for refresh source code
        { 
            _debugOnInitRefreshSourceMutex.lock();
            _debugOnInitRefreshSourceCv.notify_one();
            _debugOnInitRefreshSourceMutex.unlock();
        }
        else if (text.startsWith("^done,symbols"))          // seeking for function, variable and type identifiers
        {
            //^done,symbols={debug=[{filename=" ",fullname=" ",
            // symbols=[{line=" ",name="uwTick",type="volatile uint32_t",description="volatile uint32_t uwTick;"},}]}
            _seekIdentifierMutex.lock();

            QString debug_text = Seer::parseFirst(text, "debug=", '[', ']', false);
            QStringList filenames_list = Seer::parse(debug_text, "", '{', '}', false);

            for (const auto& filename_entry : filenames_list) {

                QString filename_text = Seer::parseFirst(filename_entry, "filename=", '"', '"', false);
                QString fullname_text = Seer::parseFirst(filename_entry, "fullname=", '"', '"', false);

                // If that file is not in source browser, skip it
                if (sourceLibraryManagerWidget->sourceBrowserWidget()->findFileWithRegrex(fullname_text).isEmpty())
                    continue;

                QString symbols_text = Seer::parseFirst(filename_entry, "symbols=", '[', ']', false);
                QStringList symbols_list = Seer::parse(symbols_text, "", '{', '}', false);

                for (const auto& symbol_entry : symbols_list) {

                    QString line_text = Seer::parseFirst(symbol_entry, "line=", '"', '"', false);
                    QString name_text = Seer::parseFirst(symbol_entry, "name=", '"', '"', false);
                    // name_text may be st like: function_name(params...) , so only extract function_name part
                    name_text = name_text.section('(', 0, 0).trimmed();
                    if (name_text == _Identifier)           // you found it! signal to open file
                    {
                        if (!_debugOnInitFindLoadModuleFile)                // if not handling debug on init
                        {
                            editorManagerWidget->setEnableOpenFile(true);       // raise this flag to allow opening file
                            editorManagerWidget->handleOpenFile(filename_text, fullname_text, line_text.toInt());
                        }
                        else
                        {
                            _debugOnInitFindLoadModuleFile = false;
                            _loadModuleFile = fullname_text;
                        }
                    }
                }
            }

            _seekIdentifierCv.notify_one();
            _seekIdentifierMutex.unlock();

        }
        else if (text.startsWith("^done,result"))          // seeking for function, variable and type identifiers
        {
            if (_lsmodDebugOnInitFlag)                      // if lsmod to read kernel module info
            {
                // ^done,result="done",modules="[{name="helloworld"}]"
                _lsmodMutex.lock();
                QString moduleName = "";
                QString nameKey = "name=";
                int startIndex = text.indexOf(nameKey) + nameKey.length() + 2;

                if (startIndex != -1) {
                    moduleName = text.mid(startIndex, _moduleName.length());    // extract module name
                }
                if (moduleName == _moduleName)
                    _isModuleIsLoaded = true;
                else
                    _isModuleIsLoaded = false;

                _lsmodDebugOnInitFlag = false;              // reset flag
                _lsmodCv.notify_one();
                _lsmodMutex.unlock();
            }
        }
    }
    else{
        // All other text is ignored by this widget.
    }
}

void SeerGdbWidget::handleManualCommandExecute () {

    // Get new command.
    QString command = manualCommandComboBox->currentText();

    // Do nothing if it is blank.
    if (command == "") {
        return;
    }

    // Remove the second to last line, if it is blank.
    if (manualCommandComboBox->count() >= 2) {

        QString lastCommand = manualCommandComboBox->itemText(manualCommandComboBox->count()-2);

        if (lastCommand == "") {
            manualCommandComboBox->removeItem(manualCommandComboBox->count()-2);
        }
    }

    // Remove the last line, if it is blank.
    if (manualCommandComboBox->count() >= 1) {

        QString lastCommand = manualCommandComboBox->itemText(manualCommandComboBox->count()-1);

        if (lastCommand == "") {
            manualCommandComboBox->removeItem(manualCommandComboBox->count()-1);
        }
    }

    // Add entered command to the end of the list as long as it's not
    // already there.
    if (manualCommandComboBox->count() > 0) {

        QString lastCommand = manualCommandComboBox->itemText(manualCommandComboBox->count()-1);

        if (lastCommand != command) {
            manualCommandComboBox->addItem(command);
        }
    }

    // Add a blank entry. It will be removed when the next manual command is entered.
    manualCommandComboBox->addItem("");

    // Point to last one.
    manualCommandComboBox->setCurrentIndex(manualCommandComboBox->count()-1);

    // Execute it.
    handleGdbCommand(command);
}

void SeerGdbWidget::handleGdbCommand (const QString& command) {

    qCDebug(LC) << "Command=" << command;

    if (_gdbProcess->state() == QProcess::NotRunning) {
        QMessageBox::warning(this, "Seer",
                                   QString("The executable has not been started yet or has already exited.\n\n") +
                                   "(" + command + ")",
                                   QMessageBox::Ok);
        return;
    }

    QString str = command + "\n";    // Ensure there's a trailing RETURN.

    QByteArray bytes = str.toUtf8(); // 8-bit Unicode Transformation Format

#if ENABLE_GDB_LOGOUT == 1
    // Broadcast this log. For debugging
    emit allTextOutput("From Widget:" + str);
#endif

    _gdbProcess->write(bytes);       // Send the data into the stdin stream of the bash child process
}

void SeerGdbWidget::handleGdbExit () {

    handleGdbCommand("-gdb-exit");
}

void SeerGdbWidget::handleGdbRunExecutable (const QString& breakMode, bool loadSessionBreakpoints) {

    qCDebug(LC) << "Starting 'gdb run/start':" << breakMode;

    QApplication::setOverrideCursor(Qt::BusyCursor);

    while (1) {
        // Has a executable name been provided?
        if (executableName() == "") {

            QMessageBox::warning(this, "Seer",
                    QString("The executable name has not been provided.\n\nUse File->Debug..."),
                    QMessageBox::Ok);
            break;
        }

        _executableBreakMode = breakMode;

        // Always say a new executable.
        // This causes a new gdb each time. The same console, though.
        setNewExecutableFlag(true);

        // Delete the old gdb if there is a new executable.
        if (newExecutableFlag() == true) {
            killGdb();
        }

        // If gdb isn't running, start it.
        if (isGdbRuning() == false) {

            // Connect the terminal to the console.
            console()->resetTerminal();
            console()->connectTerminal();

            // Start gdb.
            bool f = startGdb();
            if (f == false) {
                QMessageBox::critical(this, tr("Error"), tr("Can't start gdb."));
                break;
            }

            if (gdbAsyncMode()) {
                handleGdbCommand("-gdb-set mi-async on"); // Turn on async mode so the 'interrupt' can happen.
            }

            if (gdbNonStopMode()) {
                handleGdbCommand("-gdb-set pagination off");
                handleGdbCommand("-gdb-set non-stop on");
            }else{
                handleGdbCommand("-gdb-set non-stop off");
            }

            handleGdbLoadMICommands();
            handleGdbSourceScripts();
        }

        // Set the program's tty device for stdin and stdout.
        handleGdbTerminalDeviceName();

        setExecutableLaunchMode("run");
        saveLaunchMode();
        setGdbRecordMode("auto");
        setExecutablePid(0);

        // Load the executable, if needed.
        if (newExecutableFlag() == true) {
            handleGdbExecutablePreCommands();       // Run any 'pre' commands before program is loaded.
            handleGdbExecutableName();              // Load the program into the gdb process.
            handleGdbExecutableSources();           // Load the program source files.
            handleGdbExecutableLoadBreakpoints();   // Set the program's breakpoints (if any) before running.

            if (loadSessionBreakpoints) {
                handleGdbSessionLoadBreakpoints();
            }

            setNewExecutableFlag(false);
        }

        // Set or reset some things.
        handleGdbExecutableWorkingDirectory();  // Set the program's working directory before running.
        handleGdbAssemblyDisassemblyFlavor();   // Set the disassembly flavor to use.
        handleGdbAssemblySymbolDemangling();    // Set the symbol demangling.

        if (assemblyShowAssemblyTabOnStartup()) {
            editorManager()->showAssembly();
        }

        if (gdbHandleTerminatingException()) {
            handleGdbCommand("-gdb-set unwind-on-terminating-exception on"); // Turn on terminating exceptions when gdb calls the program's functions.
        }else{
            handleGdbCommand("-gdb-set unwind-on-terminating-exception off");
        }

        if (gdbRandomizeStartAddress()) {
            handleGdbCommand("-gdb-set disable-randomization off"); // Turn on randomization of starting address for process.
        }

        if (gdbEnablePrettyPrinting()) {
            handleGdbCommand("-enable-pretty-printing"); // Turn on pretty-printing. Can not be turned off.
        }

        // Set the program's arguments before running.
        handleGdbExecutableArguments();

        // Set a breakpoint for start up if "infunction" or "insource".
        if (_executableBreakMode == "infunction" && executableBreakpointFunctionName() != "") {

            if (executableBreakpointFunctionName().contains("^0[xX][0-9a-fA-F]+")) {
                handleGdbBreakpointInsert("*" + executableBreakpointFunctionName());
            }else{
                handleGdbBreakpointInsert("-f --function " + executableBreakpointFunctionName());
            }
        }

        if (_executableBreakMode == "insource" && executableBreakpointSourceName() != "") {
            handleGdbBreakpointInsert(executableBreakpointSourceName());
        }

        // Run any 'post' commands after program is loaded.
        handleGdbExecutablePostCommands();

        // Run the executable.
        if (_executableBreakMode == "inmain") {
            handleGdbCommand("-exec-run --all --start"); // Stop in main
        }else{
            handleGdbCommand("-exec-run --all"); // Do not stop in main. But honor other breakpoints that may have been previously set.
        }

        // Set window titles with name of program.
        emit changeWindowTitle(QString("%1 (pid=%2)").arg(executableName()).arg(QGuiApplication::applicationPid()));

        // Notify the state of the GdbWidget has changed.
        emit stateChanged();

        break;
    }

    QApplication::restoreOverrideCursor();

    qCDebug(LC) << "Finishing 'gdb run/start'.";
}

void SeerGdbWidget::handleGdbAttachExecutable (bool loadSessionBreakpoints) {

    qCDebug(LC) << "Starting 'gdb attach'.";

    QApplication::setOverrideCursor(Qt::BusyCursor);

    while (1) {

        // Has a executable name been provided?
        if (executableName() == "") {

            QMessageBox::warning(this, "Seer",
                    QString("The executable name has not been provided.\n\nUse File->Debug..."),
                    QMessageBox::Ok);
            break;
        }

        _executableBreakMode = "";

        // Always say a new executable.
        // This causes a new gdb each time. The same console, though.
        setNewExecutableFlag(true);

        // Delete the old gdb if there is a new executable.
        if (newExecutableFlag() == true) {
            killGdb();
        }

        // If gdb isn't running, start it.
        // No need to connect to the console in this mode.
        if (isGdbRuning() == false) {

            // Connect the terminal to the console.
            console()->resetTerminal();
            console()->connectTerminal();

            // Start gdb.
            bool f = startGdb();
            if (f == false) {
                QMessageBox::critical(this, tr("Error"), tr("Can't start gdb."));
                break;
            }

            if (gdbAsyncMode()) {
                handleGdbCommand("-gdb-set mi-async on"); // Turn on async mode so the 'interrupt' can happen.
            }

            handleGdbLoadMICommands();
            handleGdbSourceScripts();
        }

        // Set the program's tty device for stdin and stdout.
        // Not really needed for 'attach' mode, but do it anyway.
        handleGdbTerminalDeviceName();

        // No console for 'attach' mode but make sure it's reattached.
        setExecutableLaunchMode("attach");
        saveLaunchMode();
        setGdbRecordMode("auto");
        reattachConsole();

        // Load the executable, if needed.
        if (newExecutableFlag() == true) {
            handleGdbExecutablePreCommands();       // Run any 'pre' commands before program is loaded.
            handleGdbExecutableName();              // Load the program into the gdb process.
            handleGdbExecutableSources();           // Load the program source files.
            handleGdbExecutableLoadBreakpoints();   // Set the program's breakpoints (if any) before running.

            if (loadSessionBreakpoints) {
                handleGdbSessionLoadBreakpoints();
            }

            setNewExecutableFlag(false);
        }

        // Set or reset some things.
        handleGdbExecutableWorkingDirectory();  // Set the program's working directory before running.
        handleGdbAssemblyDisassemblyFlavor();   // Set the disassembly flavor to use.
        handleGdbAssemblySymbolDemangling();    // Set the symbol demangling.

        if (assemblyShowAssemblyTabOnStartup()) {
            editorManager()->showAssembly();
        }

        if (gdbHandleTerminatingException()) {
            handleGdbCommand("-gdb-set unwind-on-terminating-exception on"); // Turn on terminating exceptions when gdb calls the program's functions.
        }else{
            handleGdbCommand("-gdb-set unwind-on-terminating-exception off");
        }

        // Attach to the executable's pid.
        handleGdbCommand(QString("-target-attach %1").arg(executablePid()));

        // Run any 'post' commands after program is loaded.
        handleGdbExecutablePostCommands();

        // Set window titles with name of program.
        emit changeWindowTitle(QString("%1 (pid=%2)").arg(executableName()).arg(QGuiApplication::applicationPid()));

        // Notify the state of the GdbWidget has changed.
        emit stateChanged();

        break;
    }

    QApplication::restoreOverrideCursor();

    qCDebug(LC) << "Finishing 'gdb attach'.";
}

void SeerGdbWidget::handleGdbConnectExecutable (bool loadSessionBreakpoints) {

    qCDebug(LC) << "Starting 'gdb connect'.";

    QApplication::setOverrideCursor(Qt::BusyCursor);

    while (1) {

        _executableBreakMode = "";

        // Always say a new executable.
        // This causes a new gdb each time. The same console, though.
        setNewExecutableFlag(true);

        // Disconnect from the terminal and delete the old gdb if there is a new executable.
        if (newExecutableFlag() == true) {
            console()->deleteTerminal();
            killGdb();
        }

        // If gdb isn't running, start it.
        if (isGdbRuning() == false) {

            // Connect the terminal to the console.
            console()->resetTerminal();
            console()->connectTerminal();

            // Start gdb.
            bool f = startGdb();
            if (f == false) {
                QMessageBox::critical(this, tr("Error"), tr("Can't start gdb."));
                break;
            }

            handleGdbLoadMICommands();
            handleGdbSourceScripts();
        }

        // Set the program's tty device for stdin and stdout.
        // Not really needed for 'connect' mode, but do it anyway.
        handleGdbTerminalDeviceName();

        // No console for 'connect' mode but make sure it's reattached.
        setExecutableLaunchMode("connect");
        saveLaunchMode();
        setGdbRecordMode("auto");
        setExecutablePid(0);
        reattachConsole();

        // Load any 'pre' commands.
        if (newExecutableFlag() == true) {
            if (gdbServerDebug()) {
                handleGdbCommand("-gdb-set debug remote 1"); // Turn on gdbserver debug
            }else{
                handleGdbCommand("-gdb-set debug remote 0");
            }
            handleGdbExecutablePreCommands(); // Run any 'pre' commands before program is loaded.
        }

        // Connect to the remote gdbserver using the proper remote type.
        handleGdbCommand(QString("-target-select %1 %2").arg(gdbRemoteTargetType()).arg(executableConnectHostPort()));

        // Load the executable, if needed.
        if (newExecutableFlag() == true) {
            handleGdbExecutableName();              // Load the program into the gdb process.
            handleGdbExecutableSources();           // Load the program source files.
            handleGdbExecutableLoadBreakpoints();   // Set the program's breakpoints (if any) before running.

            if (loadSessionBreakpoints) {
                handleGdbSessionLoadBreakpoints();
            }

            setNewExecutableFlag(false);
        }

        // Set or reset some things.
        handleGdbExecutableWorkingDirectory();  // Set the program's working directory before running.
        handleGdbAssemblyDisassemblyFlavor();   // Set the disassembly flavor to use.
        handleGdbAssemblySymbolDemangling();    // Set the symbol demangling.

        if (assemblyShowAssemblyTabOnStartup()) {
            editorManager()->showAssembly();
        }

        if (gdbHandleTerminatingException()) {
            handleGdbCommand("-gdb-set unwind-on-terminating-exception on"); // Turn on terminating exceptions when gdb calls the program's functions.
        }else{
            handleGdbCommand("-gdb-set unwind-on-terminating-exception off");
        }

        // Run any 'post' commands after program is loaded.
        handleGdbExecutablePostCommands();

        // Set window titles with name of program.
        emit changeWindowTitle(QString("%1 (pid=%2)").arg(executableConnectHostPort()).arg(QGuiApplication::applicationPid()));

        // Notify the state of the GdbWidget has changed.
        emit stateChanged();

        break;
    }

    QApplication::restoreOverrideCursor();

    qCDebug(LC) << "Finishing 'gdb connect'.";
}

void SeerGdbWidget::handleGdbRRExecutable (bool loadSessionBreakpoints) {

    qCDebug(LC) << "Starting 'gdb direct rr'.";

    QApplication::setOverrideCursor(Qt::BusyCursor);

    while (1) {

        // Has a executable name been provided?
        if (executableName() != "") {
            QMessageBox::warning(this, "Seer",
                    QString("The executable name can't be provided for 'rr' mode."),
                    QMessageBox::Ok);
            break;
        }

        _executableBreakMode = "";

        // Always say a new executable.
        // This causes a new gdb each time. The same console, though.
        setNewExecutableFlag(true);

        // Disconnect from the console and delete the old gdb, then reconnect.
        if (newExecutableFlag() == true) {
            killGdb();
        }

        // If gdb isn't running, start it.
        if (isGdbRuning() == false) {

            // Connect the terminal to the console.
            console()->resetTerminal();
            console()->connectTerminal();

            // Start gdb.
            bool f = startGdbRR();
            if (f == false) {
                QMessageBox::critical(this, tr("Error"), tr("Can't start gdb."));
                break;
            }

            handleGdbLoadMICommands();
            handleGdbSourceScripts();
        }

        // Set the program's tty device for stdin and stdout.
        handleGdbTerminalDeviceName();

        // Set the launch mode.
        setExecutableLaunchMode("rr");
        saveLaunchMode();
        setGdbRecordMode("rr");
        setGdbRecordDirection("");
        setExecutablePid(0);

        // Load the executable, if needed.
        // For RR, this will start it.
        if (newExecutableFlag() == true) {
            handleGdbExecutablePreCommands();       // Run any 'pre' commands before program is loaded.
            handleGdbExecutableName();              // Load the program into the gdb process.
            handleGdbExecutableSources();           // Load the program source files.
            handleGdbExecutableLoadBreakpoints();   // Set the program's breakpoints (if any) before running.

            if (loadSessionBreakpoints) {
                handleGdbSessionLoadBreakpoints();
            }

            setNewExecutableFlag(false);
        }

        // Set or reset some things.
        handleGdbExecutableWorkingDirectory();  // Set the program's working directory before running.
        handleGdbAssemblyDisassemblyFlavor();   // Set the disassembly flavor to use.
        handleGdbAssemblySymbolDemangling();    // Set the symbol demangling.

        if (assemblyShowAssemblyTabOnStartup()) {
            editorManager()->showAssembly();
        }

        if (gdbHandleTerminatingException()) {
            handleGdbCommand("-gdb-set unwind-on-terminating-exception on"); // Turn on terminating exceptions when gdb calls the program's functions.
        }else{
            handleGdbCommand("-gdb-set unwind-on-terminating-exception off");
        }

        if (gdbEnablePrettyPrinting()) {
            handleGdbCommand("-enable-pretty-printing"); // Turn on pretty-printing. Can not be turned off.
        }

        // Run any 'post' commands after program is loaded.
        handleGdbExecutablePostCommands();

        // Set window titles with name of program.
        emit changeWindowTitle(QString("%1 (pid=%2)").arg(executableRRTraceDirectory()).arg(QGuiApplication::applicationPid()));

        // Notify the state of the GdbWidget has changed.
        emit stateChanged();

        break;
    }

    QApplication::restoreOverrideCursor();

    qCDebug(LC) << "Finishing 'gdb rr'.";
}

void SeerGdbWidget::handleGdbCoreFileExecutable () {

    qCDebug(LC) << "Starting 'gdb corefile'.";

    QApplication::setOverrideCursor(Qt::BusyCursor);

    while (1) {

        // Has a executable name been provided?
        if (executableName() == "") {

            QMessageBox::warning(this, "Seer",
                    QString("The executable name has not been provided.\n\nUse File->Debug..."),
                    QMessageBox::Ok);
            break;
        }

        _executableBreakMode = "";

        // Always say a new executable.
        // This causes a new gdb each time. The same console, though.
        setNewExecutableFlag(true);

        // Disconnect from the console and delete the old gdb. No need to reconnect.
        if (newExecutableFlag() == true) {
            killGdb();
        }

        // If gdb isn't running, start it.
        // No need to connect to the console in this mode.
        if (isGdbRuning() == false) {

            // Connect the terminal to the console.
            console()->resetTerminal();
            console()->connectTerminal();

            // Start gdb.
            bool f = startGdb();
            if (f == false) {
                QMessageBox::critical(this, tr("Error"), tr("Can't start gdb."));
                break;
            }

            if (gdbAsyncMode()) {
                handleGdbCommand("-gdb-set mi-async on"); // Turn on async mode so the 'interrupt' can happen.
            }

            handleGdbLoadMICommands();
            handleGdbSourceScripts();
        }

        // Set the program's tty device for stdin and stdout.
        // Not really needed for 'core' mode, but do it anyway.
        handleGdbTerminalDeviceName();

        // No console for 'core' mode but make sure it's reattached.
        setExecutableLaunchMode("corefile");
        saveLaunchMode();
        setGdbRecordMode("auto");
        setExecutablePid(0);
        reattachConsole();

        if (newExecutableFlag() == true) {
            handleGdbExecutablePreCommands();       // Run any 'pre' commands before program is loaded.
            handleGdbExecutableName();              // Load the program into the gdb process.
            handleGdbExecutableSources();           // Load the program source files.
            handleGdbAssemblyDisassemblyFlavor();   // Set the disassembly flavor to use.
            handleGdbAssemblySymbolDemangling();    // Set the symbol demangling.

            if (assemblyShowAssemblyTabOnStartup()) {
                editorManager()->showAssembly();
            }
        }

        setNewExecutableFlag(false);

        // Load the executable's core file.
        handleGdbCommand(QString("-target-select core %1").arg(executableCoreFilename()));

        // Run any 'post' commands after program is loaded.
        handleGdbExecutablePostCommands();

        // This is needed for 'core' mode to refresh the stack frame, for some reason.
        handleGdbStackListFrames();

        // Set window titles with name of program.
        emit changeWindowTitle(QString("%1 (pid=%2)").arg(executableName()).arg(QGuiApplication::applicationPid()));

        // Notify the state of the GdbWidget has changed.
        emit stateChanged();

        break;
    }

    QApplication::restoreOverrideCursor();

    qCDebug(LC) << "Finishing 'gdb corefile'.";
}

void SeerGdbWidget::handleGdbTerminateExecutable (bool confirm) {

    while (1) {

        // Do you really want to restart?
        if (isGdbRuning() == true) {

            if (confirm) {
                int result = QMessageBox::warning(this, "Seer",
                        QString("Terminate current session?"),
                        QMessageBox::Ok|QMessageBox::Cancel, QMessageBox::Cancel);

                if (result == QMessageBox::Cancel) {
                    break;
                }
            }

            handleGdbCommand(QString("save breakpoints /tmp/breakpoints.seer.%1").arg(QCoreApplication::applicationPid()));
            delay(2);

            // Give the gdb and 'exit' command.
            // This should handle detaching from an attached pid.
            handleGdbCommand("-exec-kill");

            // Kill the gdb.
            killGdb();

            // If openOCD is running then kill it
            if (openocdProcess())
            {
                openocdWidget->killOpenOCD();
            }
            if (openocdWidget->openocdConsole())
            {
                openocdWidget->killConsole();
            }

            editorManagerWidget->clearFilesStack();
            // Print a message.
            addMessage("Program terminated.", QMessageBox::Warning);

            // Alert listeners the session has been terminated.
            emit sessionTerminated();

            // Notify the state of the GdbWidget has changed.
            emit stateChanged();
        }

        break;
    }
}

void SeerGdbWidget::handleGdbShutdown () {

    // Remove session breakpoint file, if any.
    QFile::remove(QString("/tmp/breakpoints.seer.%1").arg(QCoreApplication::applicationPid()));

    // Do nothing if there's not gdb running.
    if (isGdbRuning() == false) {
        return;
    }

    // We are in no mode now.
    setExecutableLaunchMode("");
    setGdbRecordMode("");

    // Give the gdb and 'exit' command.
    // This should handle detaching from an attached pid.
    handleGdbExit();

    // Kill the gdb.
    killGdb();
}

void SeerGdbWidget::handleGdbRunToLine (QString fullname, int lineno) {

    if (executableLaunchMode() == "") {
        return;
    }

    QString command = "-exec-until " + fullname + ":" + QString::number(lineno);

    handleGdbCommand(command);
}

void SeerGdbWidget::handleGdbRunToAddress (QString address) {

    if (executableLaunchMode() == "") {
        return;
    }

    QString command = "-exec-until *" +address;

    handleGdbCommand(command);
}

void SeerGdbWidget::handleGdbNext () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-exec-next %1").arg(gdbRecordDirection()));
}

void SeerGdbWidget::handleGdbNexti () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-exec-next-instruction %1").arg(gdbRecordDirection()));
}

void SeerGdbWidget::handleGdbStep () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-exec-step %1").arg(gdbRecordDirection()));
}

void SeerGdbWidget::handleGdbStepi () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-exec-step-instruction %1").arg(gdbRecordDirection()));
}

void SeerGdbWidget::handleGdbFinish () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-exec-finish %1").arg(gdbRecordDirection()));
}

void SeerGdbWidget::handleGdbContinue () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-exec-continue %1").arg(gdbRecordDirection()));
}

void SeerGdbWidget::handleGdbRecordStart () {

    if (executableLaunchMode() == "") {
        return;
    }

    if (executableLaunchMode() == "rr" || executableLaunchMode() == "udb") {
        QMessageBox::warning(this, "Seer", QString("Record 'Start' not available in RR or UDB mode."), QMessageBox::Ok);
        return;
    }

    setGdbRecordMode("full");
    setGdbRecordDirection("");
}

void SeerGdbWidget::handleGdbRecordStop () {

    if (executableLaunchMode() == "") {
        return;
    }

    if (executableLaunchMode() == "rr" || executableLaunchMode() == "udb") {
        QMessageBox::warning(this, "Seer", QString("Record 'Stop' not available in RR or UDB mode."), QMessageBox::Ok);
        return;
    }

    setGdbRecordMode("stop");
    setGdbRecordDirection("");
}

void SeerGdbWidget::handleGdbRecordForward () {

    setGdbRecordDirection("");
}

void SeerGdbWidget::handleGdbRecordReverse () {

    setGdbRecordDirection("--reverse");
}

void SeerGdbWidget::handleGdbRecordStartStopToggle () {

    if (gdbRecordMode() == "stop" || gdbRecordMode() == "") {

        setGdbRecordMode("full");
        setGdbRecordDirection("");

    }else if (gdbRecordMode() == "full") {

        setGdbRecordMode("stop");
        setGdbRecordDirection("");

    }else if (gdbRecordMode() == "rr") {

        // Don't do anthing.

    }else if (gdbRecordMode() == "udb") {

        // Don't do anthing.

    }else{

        setGdbRecordMode("stop");
        setGdbRecordDirection("");
    }
}

void SeerGdbWidget::handleGdbRecordDirectionToggle () {

    if (gdbRecordDirection() == "") {
        setGdbRecordDirection("--reverse");
    }else if (gdbRecordDirection() == "--reverse") {
        setGdbRecordDirection("");
    }else{
        setGdbRecordDirection("");
    }
}

void SeerGdbWidget::handleGdbInterrupt () {

    if (openocdWidget->isOpenocdRunning() == true && gdbProgram() == gdbMultiarchExePath() && \
        _gdbProcess->state() == QProcess::Running && _gdbmultiarchPid > 0)
        {
            handleGdbInterruptSIGINT();
            QApplication::setOverrideCursor(Qt::ArrowCursor);
        }
    else
        sendGdbInterrupt(-1);
}

void SeerGdbWidget::handleGdbInterruptSIGINT () {

    sendGdbInterrupt(SIGINT);
}

void SeerGdbWidget::handleGdbInterruptSIGKILL () {

    sendGdbInterrupt(SIGKILL);
}

void SeerGdbWidget::handleGdbInterruptSIGFPE () {

    sendGdbInterrupt(SIGFPE);
}

void SeerGdbWidget::handleGdbInterruptSIGSEGV () {

    sendGdbInterrupt(SIGSEGV);
}

void SeerGdbWidget::handleGdbInterruptSIGUSR1 () {

    sendGdbInterrupt(SIGUSR1);
}

void SeerGdbWidget::handleGdbInterruptSIGUSR2 () {

    sendGdbInterrupt(SIGUSR2);
}

void SeerGdbWidget::handleGdbRunThreadGroup (QString threadGroup) {

    if (executableLaunchMode() == "") {
        return;
    }

    if (threadGroup == "") {
        return;
    }

    handleGdbCommand(QString("-exec-run --thread-group %1").arg(threadGroup));
}

void SeerGdbWidget::handleGdbStartThreadGroup (QString threadGroup) {

    if (executableLaunchMode() == "") {
        return;
    }

    if (threadGroup == "") {
        return;
    }

    handleGdbCommand(QString("-exec-run --thread-group %1 --start").arg(threadGroup));
}

void SeerGdbWidget::handleGdbContinueThreadGroup (QString threadGroup) {

    if (executableLaunchMode() == "") {
        return;
    }

    if (threadGroup == "") {
        return;
    }

    handleGdbCommand(QString("-exec-continue %1 --thread-group %2").arg(gdbRecordDirection()).arg(threadGroup));
}

void SeerGdbWidget::handleGdbInterruptThreadGroup (QString threadGroup) {

    if (executableLaunchMode() == "") {
        return;
    }

    if (threadGroup == "") {
        return;
    }

    handleGdbCommand(QString("-exec-interrupt --thread-group %1").arg(threadGroup));
}

void SeerGdbWidget::handleGdbNextThreadId (int threadid) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-exec-next --thread %1 %2").arg(threadid).arg(gdbRecordDirection()));
}

void SeerGdbWidget::handleGdbStepThreadId (int threadid) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-exec-step %1 --thread %2").arg(gdbRecordDirection()).arg(threadid));
}

void SeerGdbWidget::handleGdbFinishThreadId (int threadid) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-exec-finish %1 --thread %2").arg(gdbRecordDirection()).arg(threadid));
}

void SeerGdbWidget::handleGdbContinueThreadId (int threadid) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-exec-continue %1 --thread %2").arg(gdbRecordDirection()).arg(threadid));
}

void SeerGdbWidget::handleGdbInterruptThreadId (int threadid) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-exec-interrupt --thread %1").arg(threadid));
}

void SeerGdbWidget::handleGdbExecutableSources () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-file-list-exec-source-files");
}

void SeerGdbWidget::handleGdbExecutableFunctions (int id, const QString& functionRegex) {

    if (executableLaunchMode() == "") {
        return;
    }

    if (id <= 0) {
        return;
    }

    if (functionRegex == "") {
        return;
    }

    //qDebug() << id << functionRegex;

    QApplication::setOverrideCursor(Qt::BusyCursor);
    handleGdbCommand(QString("%1-symbol-info-functions --include-nondebug --name %2").arg(id).arg(functionRegex));
    QApplication::restoreOverrideCursor();
}

void SeerGdbWidget::handleGdbExecutableTypes (int id, const QString& typeRegex) {

    if (executableLaunchMode() == "") {
        return;
    }

    if (id <= 0) {
        return;
    }

    if (typeRegex == "") {
        return;
    }

    //qDebug() << id << typeRegex;

    handleGdbCommand(QString("%1-symbol-info-types --name %2").arg(id).arg(typeRegex));
}

void SeerGdbWidget::handleGdbExecutableVariables (int id, const QString& variableNameRegex, const QString& variableTypeRegex) {

    if (executableLaunchMode() == "") {
        return;
    }

    if (id <= 0) {
        return;
    }

    if (variableNameRegex == "" && variableTypeRegex == "") {
        return;
    }

    QString command = QString("%1-symbol-info-variables").arg(id);

    if (variableNameRegex != "") {
        command += QString(" --name %1").arg(variableNameRegex);
    }

    if (variableTypeRegex != "") {
        command += QString(" --type %1").arg(variableTypeRegex);
    }

    handleGdbCommand(command);
}

void SeerGdbWidget::handleGdbExecutableLibraries () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-file-list-shared-libraries");
}

void SeerGdbWidget::handleGdbExecutableName () {

    if (executableLaunchMode() == "") {
        return;
    }

    //qDebug() << executableName();
    //qDebug() << executableSymbolName();

    // executableName() is expected to be non-blank.

    // An executable and no symbol file? Symbols are expected in the executable.
    if (executableName() != "" && executableSymbolName() == "") {

        handleGdbCommand(QString("-file-exec-and-symbols \"") + executableName() + "\"");

    // An executable and a symbol file?  Open the executable and symbol files separately.
    }else if (executableName() != "" && executableSymbolName() != "") {

        handleGdbCommand(QString("-file-exec-file \"")   + executableName() + "\"");
        handleGdbCommand(QString("-file-symbol-file \"") + executableSymbolName() + "\"");

    // No executable and a symbol file?  Open the symbol files only.
    }else if (executableName() == "" && executableSymbolName() != "") {

        handleGdbCommand(QString("-file-symbol-file \"") + executableSymbolName() + "\"");
    }
}

void SeerGdbWidget::handleGdbExecutableArguments () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-exec-arguments ") + executableArguments());
}

void SeerGdbWidget::handleGdbExecutableWorkingDirectory () {

    if (executableLaunchMode() == "") {
        return;
    }

    if (executableWorkingDirectory() != "") {
        handleGdbCommand(QString("-environment-cd \"") + executableWorkingDirectory() + "\"");
    }
}

void SeerGdbWidget::handleGdbExecutableLoadBreakpoints () {

    if (executableBreakpointsFilename() == "") {
        return;
    }

    handleGdbCommand("source -v " + executableBreakpointsFilename());
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbExecutablePreCommands () {

    for (const auto& i : _executablePreGdbCommands) {
        handleGdbCommand(i);
    }
}

void SeerGdbWidget::handleGdbExecutablePostCommands () {

    for (const auto& i : _executablePostGdbCommands) {
        handleGdbCommand(i);
    }
}

void SeerGdbWidget::handleGdbSessionLoadBreakpoints () {

    handleGdbCommand(QString("source -v /tmp/breakpoints.seer.%1").arg(QCoreApplication::applicationPid()));
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbSessionSaveBreakpoints () {

    handleGdbCommand(QString("source -v /tmp/breakpoints.seer.%1").arg(QCoreApplication::applicationPid()));
}

void SeerGdbWidget::handleGdbTerminalDeviceName () {

    if (_consoleWidget->terminalDeviceName() != "") {

        handleGdbCommand(QString("-inferior-tty-set ") + _consoleWidget->terminalDeviceName());

    }else{
        qWarning() << "Can't set TTY name because the name is blank.";
    }
}

void SeerGdbWidget::handleGdbStackListFrames () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-stack-list-frames 0 100");         // on u-boot, stack frame might last for hundred levels. Limit this to 100 levels to avoid gdb hang
}

void SeerGdbWidget::handleGdbStackSelectFrame (int frameno) {

    if (executableLaunchMode() == "") {
        return;
    }

    // This is not supported anymore with gdbmi.
    // handleGdbCommand(QString("-stack-select-frame %1").arg(frameno));

    // So resort to calling the non-gdbmi version.
    handleGdbCommand(QString("frame %1").arg(frameno));

    emit stoppingPointReached();
}

void SeerGdbWidget::handleGdbStackListLocals () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-stack-list-variables --all-values");    // on u-boot, stack frame might last for hundred levels. Limit this to 100 levels to avoid gdb hang
}

void SeerGdbWidget::handleGdbStackListArguments () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-stack-list-arguments --all-values 0 100");    // on u-boot, stack frame might last for hundred levels. Limit this to 100 levels to avoid gdb hang
}

void SeerGdbWidget::handleGdbGenericpointList () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-list");
}

void SeerGdbWidget::handleGdbBreakpointDelete (QString breakpoints) {

    if (executableLaunchMode() == "") {
        return;
    }
    // if OpenOCD is running well, halt target then delete breakpoint, then resume since on
    // embedded system, we cannot delete hbreakpoint on runtime with gdb MI so this is workaround
    if (openocdWidget->isOpenocdRunning() == true && gdbProgram() == gdbMultiarchExePath() && \
        _gdbProcess->state() == QProcess::Running && _gdbmultiarchPid > 0)
    {
        // if target is running
        if (gdbMultiarchRunningState() == true)
        {
            setNewHardwareBreakpointFlag(true);
            _gdbMonitor->setNewHardBreakpointFlag();
            editorManagerWidget->setEnableOpenFile(false);                  // when add bp at runtime, seer display source code when receives SIGINT, so this will fix it
            handleGdbInterruptSIGINT();
            handleGdbCommand("-break-delete " + breakpoints);
            handleGdbGenericpointList();
            handleGdbContinue();
            editorManagerWidget->setEnableOpenFile(true);                   // re-enable open file
        }
        else    // simply delete bp
        {
            handleGdbCommand("-break-delete " + breakpoints);
            handleGdbGenericpointList();
        }
    }
    else
    {
        handleGdbCommand("-break-delete " + breakpoints);
        handleGdbGenericpointList();
    }
}

void SeerGdbWidget::handleGdbBreakpointEnable (QString breakpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    if (openocdWidget->isOpenocdRunning() == true && gdbProgram() == gdbMultiarchExePath() && \
        _gdbProcess->state() == QProcess::Running && _gdbmultiarchPid > 0)
    {
        // if target is running
        if (gdbMultiarchRunningState() == true)
        {
            setNewHardwareBreakpointFlag(true);
            _gdbMonitor->setNewHardBreakpointFlag();
            editorManagerWidget->setEnableOpenFile(false);
            handleGdbInterruptSIGINT();
            handleGdbCommand("-break-enable " + breakpoints);
            handleGdbGenericpointList();
            handleGdbContinue();
            editorManagerWidget->setEnableOpenFile(true);                   // re-enable open file
        }
        else    // simply enable bp
        {
            handleGdbCommand("-break-enable " + breakpoints);
            handleGdbGenericpointList();
        }
    }
    else
    {
        handleGdbCommand("-break-enable " + breakpoints);
        handleGdbGenericpointList();
    }
}

void SeerGdbWidget::handleGdbBreakpointDisable (QString breakpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    if (openocdWidget->isOpenocdRunning() == true && gdbProgram() == gdbMultiarchExePath() && \
        _gdbProcess->state() == QProcess::Running && _gdbmultiarchPid > 0)
    {
        // if target is running
        if (gdbMultiarchRunningState() == true)
        {
            setNewHardwareBreakpointFlag(true);
            _gdbMonitor->setNewHardBreakpointFlag();
            editorManagerWidget->setEnableOpenFile(false);
            handleGdbInterruptSIGINT();
            handleGdbCommand("-break-disable " + breakpoints);
            handleGdbGenericpointList();
            handleGdbContinue();
            editorManagerWidget->setEnableOpenFile(true);                   // re-enable open file
        }
        else    // simply disable bp
        {
            handleGdbCommand("-break-disable " + breakpoints);
            handleGdbGenericpointList();
        }
    }
    else
    {
        handleGdbCommand("-break-disable " + breakpoints);
        handleGdbGenericpointList();
    }
}

void SeerGdbWidget::handleGdbBreakpointInfo (int breakpointid, QString breakpoint) {

    if (executableLaunchMode() == "") {
        return;
    }

    QString str = QString("%1-break-info %2").arg(breakpointid).arg(breakpoint);

    handleGdbCommand(str);
}

void SeerGdbWidget::handleGdbBreakpointInsert (QString breakpoint) {

    if (executableLaunchMode() == "") {
        return;
    }
    // First check if breakpoint exists there
    // "-f --source \"/home/quangnm/Documents/GitHub/seer/stm32f1_blink/src/main.c\" --line 52"

    // if OpenOCD is running well, halt target then add breakpoint, then resume since on
    // embedded system, we cannot add hbreakpoint on runtime with gdb MI so this is workaround
    if (openocdWidget->isOpenocdRunning() == true && gdbProgram() == gdbMultiarchExePath() && \
        _gdbProcess->state() == QProcess::Running && _gdbmultiarchPid > 0)
    {
        // if target is running
        if (gdbMultiarchRunningState() == true)
        {
            setNewHardwareBreakpointFlag(true);
            _gdbMonitor->setNewHardBreakpointFlag();
            editorManagerWidget->setEnableOpenFile(false);                  // when add bp at runtime, seer display source code when receives SIGINT, so this will fix it
            handleGdbInterruptSIGINT();
            handleGdbCommand("-break-insert -h " + breakpoint);
            handleGdbGenericpointList();
            handleGdbContinue();
            editorManagerWidget->setEnableOpenFile(true);                   // re-enable open file
        }
        else    // simply put -h breakpoint
        {
            handleGdbCommand("-break-insert -h " + breakpoint);
            handleGdbGenericpointList();
        }
    }
    else
    {
        handleGdbCommand("-break-insert " + breakpoint);
        handleGdbGenericpointList();
    }
}

void SeerGdbWidget::handleGdbBreakpointCondition (QString breakpoint, QString condition) {

    if (executableLaunchMode() == "") {
        return;
    }

    QString str = condition.replace('"', "\\\""); // Quote " characters.

    handleGdbCommand("-break-condition " + breakpoint + " \"" + condition + "\"");
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbBreakpointIgnore (QString breakpoint, QString count) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-after " + breakpoint + " " + count);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbBreakpointCommand (QString breakpoint, QString command) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-commands " + breakpoint + " \"" + command + "\"");
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbBreakpointCommands (QString breakpoint, QStringList commands) {

    if (executableLaunchMode() == "") {
        return;
    }

    QString commandstext;

    for (int i=0; i<commands.count(); i++) {
        if (commands[i].trimmed().isEmpty()) {
            continue;
        }

        commandstext += QString(" \"") + commands[i] + "\"";
    }

    handleGdbCommand("-break-commands " + breakpoint + commandstext);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbBreakpointReload (QStringList breakpointsText) {

      for (int i=0; i<breakpointsText.size(); i++) {
           handleGdbBreakpointInsert(breakpointsText[i]);
      }
}

void SeerGdbWidget::handleGdbWatchpointReload (QStringList watchpointsText) {

      for (int i=0; i<watchpointsText.size(); i++) {
           handleGdbWatchpointInsert(watchpointsText[i]);
      }
}

void SeerGdbWidget::handleGdbCatchpointReload (QStringList catchpointsText) {

      for (int i=0; i<catchpointsText.size(); i++) {
           handleGdbCatchpointInsert(catchpointsText[i]);
      }
}

void SeerGdbWidget::handleGdbWatchpointDelete (QString watchpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-delete " + watchpoints);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbWatchpointEnable (QString watchpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-enable " + watchpoints);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbWatchpointDisable (QString watchpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-disable " + watchpoints);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbWatchpointInsert (QString watchpoint) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-watch " + watchpoint);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbCatchpointDelete (QString catchpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-delete " + catchpoints);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbCatchpointEnable (QString catchpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-enable " + catchpoints);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbCatchpointDisable (QString catchpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-disable " + catchpoints);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbCatchpointInsert (QString catchpoint) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-catch-" + catchpoint); // A little bit different than break insert or watch insert.
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbPrintpointDelete (QString printpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-delete " + printpoints);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbPrintpointEnable (QString printpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-enable " + printpoints);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbPrintpointDisable (QString printpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-disable " + printpoints);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbPrintpointInsert (QString type, QString function, QString channel, QString parameters) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-gdb-set dprintf-style "    + type);
    handleGdbCommand("-gdb-set dprintf-function " + function);
    handleGdbCommand("-gdb-set dprintf-channel "  + channel);

    handleGdbCommand("-dprintf-insert " + parameters);
    handleGdbGenericpointList();
}


void SeerGdbWidget::handleGdbThreadListFrames () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-thread-info");
}

void SeerGdbWidget::handleGdbThreadListIds () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-thread-list-ids");
}

void SeerGdbWidget::handleGdbThreadListGroups () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-list-thread-groups");
}

void SeerGdbWidget::handleGdbThreadSelectId (int threadid) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-thread-select %1").arg(threadid));

    emit stoppingPointReached();
}

void SeerGdbWidget::handleGdbAdaListTasks () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-ada-task-info");
}

void SeerGdbWidget::handleGdbAdaListExceptions () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-info-ada-exceptions");
}

void SeerGdbWidget::handleGdbSkipList () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-skip-list");
}

void SeerGdbWidget::handleGdbSkipAdd (QString skipmode, QString skipparameters) {

    if (executableLaunchMode() == "") {
        return;
    }

    if (skipmode == "file") {
        handleGdbCommand("-skip-create-file " + skipparameters);
    }else if (skipmode == "gfile") {
        handleGdbCommand("-skip-create-gfile " + skipparameters);
    }else if (skipmode == "function") {
        handleGdbCommand("-skip-create-function " + skipparameters);
    }else if (skipmode == "rfunction") {
        handleGdbCommand("-skip-create-rfunction " + skipparameters);
    }else{
        return;
    }

    handleGdbSkipList();
}

void SeerGdbWidget::handleGdbSkipDelete (QString skipids) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-skip-delete " + skipids);

    handleGdbSkipList();
}

void SeerGdbWidget::handleGdbSkipEnable (QString skipids) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-skip-enable " + skipids);

    handleGdbSkipList();
}

void SeerGdbWidget::handleGdbSkipDisable (QString skipids) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-skip-disable " + skipids);

    handleGdbSkipList();
}

void SeerGdbWidget::handleGdbCheckpointList () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-checkpoint-list");
}

void SeerGdbWidget::handleGdbCheckpointInsert () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-checkpoint-create");
    handleGdbCommand("-checkpoint-list");
}

void SeerGdbWidget::handleGdbCheckpointSelect (QString id) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-checkpoint-select " + id);

    emit stoppingPointReached();
}

void SeerGdbWidget::handleGdbCheckpointDelete (QString ids) {

    if (executableLaunchMode() == "") {
        return;
    }

    QStringList list = ids.split(" ");

    for (auto id : list) {
        handleGdbCommand("-checkpoint-delete " + id);
    }

    handleGdbCommand("-checkpoint-list");
}

void SeerGdbWidget::handleGdbRegisterListNames () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-data-list-register-names");
}

void SeerGdbWidget::handleGdbRegisterListValues (QString fmt) {

    if (executableLaunchMode() == "") {
        return;
    }

    if (fmt == "") {
        fmt = "N";
    }

    // XXX Not sure what --skip-unavailable does.
    // XXX Perhaps skips registers that can't get value for.
    // XXX handleGdbCommand("-data-list-register-values --skip-unavailable " + fmt);

    handleGdbCommand("-data-list-register-values " + fmt);
}

void SeerGdbWidget::handleGdbRegisterSetValue (QString fmt, QString name, QString value) {

    if (executableLaunchMode() == "") {
        return;
    }

    // Set the register value.
    handleGdbCommand("-gdb-set $" + name + "=" + value);

    // Refresh whoever is listening.
    handleGdbRegisterListValues(fmt);
}


void SeerGdbWidget::handleGdbDataEvaluateExpression (int expressionid, QString expression) {

    if (executableLaunchMode() == "") {
        return;
    }

    // Check if there's a ObjectiveC pretext.
    QString token("(objc)");

    if (expression.startsWith(token)) {
        handleGdbCommand(QString::number(expressionid) + "-objc-evaluate-expression \"" + expression.mid(token.length()) + "\"");

    // Otherwise handle normally.
    }else{
    handleGdbCommand(QString::number(expressionid) + "-data-evaluate-expression \"" + expression + "\"");
    }
}

void SeerGdbWidget::handleGdbVarObjCreate (int expressionid, QString expression) {

    if (executableLaunchMode() == "") {
        return;
    }

    QString str = QString("%1-var-create seer%1 \"*\" \"%2\"").arg(expressionid).arg(expression);

    handleGdbCommand(str);
}

void SeerGdbWidget::handleGdbVarObjListChildren (int expressionid, QString objname) {

    if (executableLaunchMode() == "") {
        return;
    }

    QString str = QString("%1-var-list-children --all-values \"%2\"").arg(expressionid).arg(objname);

    handleGdbCommand(str);
}

void SeerGdbWidget::handleGdbVarObjUpdate (int expressionid, QString objname) {

    if (executableLaunchMode() == "") {
        return;
    }

    QString str = QString("%1-var-update --all-values \"%2\"").arg(expressionid).arg(objname);

    handleGdbCommand(str);
}

void SeerGdbWidget::handleGdbVarObjAssign (int expressionid, QString objname, QString value) {

    if (executableLaunchMode() == "") {
        return;
    }

    QString str = QString("%1-var-assign \"%2\" %3").arg(expressionid).arg(objname).arg(value);

    handleGdbCommand(str);
}

void SeerGdbWidget::handleGdbVarObjDelete (int expressionid, QString objname) {

    if (executableLaunchMode() == "") {
        return;
    }

    QString str = QString("%1-var-delete \"%2\"").arg(expressionid).arg(objname);

    handleGdbCommand(str);
}

void SeerGdbWidget::handleGdbVarObjAttributes (int objid, QString objname) {

    if (executableLaunchMode() == "") {
        return;
    }

    QString str = QString("%1-var-show-attributes \"%2\"").arg(objid).arg(objname);

    handleGdbCommand(str);
}

void SeerGdbWidget::handleGdbDataListValues () {

    if (executableLaunchMode() == "") {
        return;
    }

    for (int i=0; i<_dataExpressionId.size(); i++) {
        handleGdbDataEvaluateExpression(_dataExpressionId[i], _dataExpressionName[i]);
    }
}

void SeerGdbWidget::handleGdbDataListExpressions () {

    if (executableLaunchMode() == "") {
        return;
    }

    // ^done,DataExpressionTable={
    //     entry={id="2",expression="a"},
    //     entry={id="3",expression="i"}
    // }

    QString text;
    text += "^done,DataExpressionTable={";

    for (int i=0; i<_dataExpressionId.size(); i++) {
        if (i != 0) {
            text += ",";
        }
        text += "entry={id=\"" + QString::number(_dataExpressionId[i]) + "\",expression=\"" + _dataExpressionName[i] + "\"}";
    }

    text += "}";

    gdbMonitor()->handleTextOutput(text);
}

void SeerGdbWidget::handleGdbDataAddExpression (QString expression) {

    if (executableLaunchMode() == "") {
        return;
    }

    // Is this one already present?
    // ??? Emit an error?
    if (_dataExpressionName.indexOf(expression) >= 0) {
        return;
    }

    // Add it to our list of variables.
    _dataExpressionName.push_back(expression);
    _dataExpressionId.push_back(Seer::createID());

    // ^done,DataExpressionAdded={
    //     id="2",
    //     expression="a"
    // }

    QString text = QString("^done,DataExpressionAdded={id=\"%1\",expression=\"%2\"}").arg(_dataExpressionId.back()).arg(_dataExpressionName.back());

    //qDebug() << text;

    gdbMonitor()->handleTextOutput(text);
}

void SeerGdbWidget::handleGdbDataDeleteExpressions (QString expressionids) {

    if (executableLaunchMode() == "") {

        // Clear expression list.
        _dataExpressionId.clear();
        _dataExpressionName.clear();

        return;
    }

    // ^done,DataExpressionDeleted={
    //     entry={id="2", expression="a"},
    //     entry={id="3", expression="a"}
    // }

    QString text;
    text += "^done,DataExpressionDeleted={";

    if (expressionids == "*") {

        bool first = true;
        for (int i=0; i<_dataExpressionId.size(); i++) {

            if (first == false) {
                text += ",";
            }

            text += "entry={id=\"" + QString::number(_dataExpressionId[i]) + "\",expression=\"" + _dataExpressionName[i] + "\"}";
            first = false;
        }

        _dataExpressionId.clear();
        _dataExpressionName.clear();

    }else{

        QStringList ids = expressionids.split(' ', Qt::SkipEmptyParts);

        bool first = true;
        for (int i=0; i<ids.size(); i++) {

            if (ids[i] == "") {
                continue;
            }

            int index = _dataExpressionId.indexOf(ids[i].toInt());

            if (index < 0) {
                continue;
            }

            if (first == false) {
                text += ",";
            }

            text += "entry={id=\"" + QString::number(_dataExpressionId[index]) + "\",expression=\"" + _dataExpressionName[index] + "\"}";
            first = false;

            _dataExpressionId.remove(index);
            _dataExpressionName.remove(index);
        }
    }

    text += "}";

    gdbMonitor()->handleTextOutput(text);
}

void SeerGdbWidget::handleGdbMemoryAddExpression (QString expression) {

    if (executableLaunchMode() == "") {
        return;
    }

    SeerMemoryVisualizerWidget* w = new SeerMemoryVisualizerWidget(0);
    w->show();

    // Connect things.
    QObject::connect(_gdbMonitor,  &GdbMonitor::astrixTextOutput,                           w,    &SeerMemoryVisualizerWidget::handleText);
    QObject::connect(_gdbMonitor,  &GdbMonitor::caretTextOutput,                            w,    &SeerMemoryVisualizerWidget::handleText);
    QObject::connect(w,            &SeerMemoryVisualizerWidget::evaluateVariableExpression, this, &SeerGdbWidget::handleGdbDataEvaluateExpression);
    QObject::connect(w,            &SeerMemoryVisualizerWidget::evaluateMemoryExpression,   this, QOverload<int,QString,int>::of(&SeerGdbWidget::handleGdbMemoryEvaluateExpression));
    QObject::connect(w,            &SeerMemoryVisualizerWidget::evaluateAsmExpression,      this, &SeerGdbWidget::handleGdbAsmEvaluateExpression);

    // Tell the visualizer what variable to use.
    w->setVariableName(expression);
}

void SeerGdbWidget::handleGdbArrayAddExpression (QString expression) {

    if (executableLaunchMode() == "") {
        return;
    }

    SeerArrayVisualizerWidget* w = new SeerArrayVisualizerWidget(0);
    w->show();

    // Connect things.
    QObject::connect(_gdbMonitor,  &GdbMonitor::astrixTextOutput,                           w,    &SeerArrayVisualizerWidget::handleText);
    QObject::connect(_gdbMonitor,  &GdbMonitor::caretTextOutput,                            w,    &SeerArrayVisualizerWidget::handleText);
    QObject::connect(w,            &SeerArrayVisualizerWidget::evaluateVariableExpression,  this, &SeerGdbWidget::handleGdbDataEvaluateExpression);
    QObject::connect(w,            &SeerArrayVisualizerWidget::evaluateMemoryExpression,    this, &SeerGdbWidget::handleGdbArrayEvaluateExpression);

    // Tell the visualizer what variable to use.
    w->setAVariableName(expression);
}

void SeerGdbWidget::handleGdbMatrixAddExpression (QString expression) {

    if (executableLaunchMode() == "") {
        return;
    }

    SeerMatrixVisualizerWidget* w = new SeerMatrixVisualizerWidget(0);
    w->show();

    // Connect things.
    QObject::connect(_gdbMonitor,  &GdbMonitor::astrixTextOutput,                            w,    &SeerMatrixVisualizerWidget::handleText);
    QObject::connect(_gdbMonitor,  &GdbMonitor::caretTextOutput,                             w,    &SeerMatrixVisualizerWidget::handleText);
    QObject::connect(w,            &SeerMatrixVisualizerWidget::evaluateVariableExpression,  this, &SeerGdbWidget::handleGdbDataEvaluateExpression);
    QObject::connect(w,            &SeerMatrixVisualizerWidget::evaluateMemoryExpression,    this, &SeerGdbWidget::handleGdbArrayEvaluateExpression);

    // Tell the visualizer what variable to use.
    w->setVariableName(expression);
}

void SeerGdbWidget::handleGdbStructAddExpression (QString expression) {

    if (executableLaunchMode() == "") {
        return;
    }

    SeerStructVisualizerWidget* w = new SeerStructVisualizerWidget(0);
    w->show();

    // Connect things.
    QObject::connect(_gdbMonitor,  &GdbMonitor::astrixTextOutput,                            w,    &SeerStructVisualizerWidget::handleText);
    QObject::connect(_gdbMonitor,  &GdbMonitor::caretTextOutput,                             w,    &SeerStructVisualizerWidget::handleText);
    QObject::connect(w,            &SeerStructVisualizerWidget::evaluateVariableExpression,  this, &SeerGdbWidget::handleGdbDataEvaluateExpression);
    QObject::connect(w,            &SeerStructVisualizerWidget::addMemoryVisualizer,         this, &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(w,            &SeerStructVisualizerWidget::addArrayVisualizer,          this, &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(w,            &SeerStructVisualizerWidget::addMatrixVisualizer,         this, &SeerGdbWidget::handleGdbMatrixAddExpression);
    QObject::connect(w,            &SeerStructVisualizerWidget::addStructVisualizer,         this, &SeerGdbWidget::handleGdbStructAddExpression);

    // Tell the visualizer what variable to use.
    w->setVariableName(expression);
}

void SeerGdbWidget::handleGdbVarAddExpression (QString expression) {

    if (executableLaunchMode() == "") {
        return;
    }

    SeerVarVisualizerWidget* w = new SeerVarVisualizerWidget(0);
    w->show();

    // Connect things.
    QObject::connect(_gdbMonitor,  &GdbMonitor::astrixTextOutput,                            w,    &SeerVarVisualizerWidget::handleText);
    QObject::connect(_gdbMonitor,  &GdbMonitor::caretTextOutput,                             w,    &SeerVarVisualizerWidget::handleText);
    QObject::connect(w,            &SeerVarVisualizerWidget::varObjCreate,                   this, &SeerGdbWidget::handleGdbVarObjCreate);
    QObject::connect(w,            &SeerVarVisualizerWidget::varObjListChildren,             this, &SeerGdbWidget::handleGdbVarObjListChildren);
    QObject::connect(w,            &SeerVarVisualizerWidget::varObjUpdate,                   this, &SeerGdbWidget::handleGdbVarObjUpdate);
    QObject::connect(w,            &SeerVarVisualizerWidget::varObjAssign,                   this, &SeerGdbWidget::handleGdbVarObjAssign);
    QObject::connect(w,            &SeerVarVisualizerWidget::varObjDelete,                   this, &SeerGdbWidget::handleGdbVarObjDelete);
    QObject::connect(w,            &SeerVarVisualizerWidget::varObjAttributes,               this, &SeerGdbWidget::handleGdbVarObjAttributes);
    QObject::connect(w,            &SeerVarVisualizerWidget::addMemoryVisualizer,            this, &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(w,            &SeerVarVisualizerWidget::addArrayVisualizer,             this, &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(w,            &SeerVarVisualizerWidget::addMatrixVisualizer,            this, &SeerGdbWidget::handleGdbMatrixAddExpression);
    QObject::connect(w,            &SeerVarVisualizerWidget::addVarVisualizer,               this, &SeerGdbWidget::handleGdbVarAddExpression);

    // Tell the visualizer what variable to use.
    w->setVariableName(expression);
}

void SeerGdbWidget::handleGdbImageAddExpression (QString expression) {

    if (executableLaunchMode() == "") {
        return;
    }

    SeerImageVisualizerWidget* w = new SeerImageVisualizerWidget(0);
    w->show();

    // Connect things.
    QObject::connect(_gdbMonitor,  &GdbMonitor::astrixTextOutput,                           w,    &SeerImageVisualizerWidget::handleText);
    QObject::connect(_gdbMonitor,  &GdbMonitor::caretTextOutput,                            w,    &SeerImageVisualizerWidget::handleText);
    QObject::connect(w,            &SeerImageVisualizerWidget::evaluateVariableExpression,  this, &SeerGdbWidget::handleGdbDataEvaluateExpression);
    QObject::connect(w,            &SeerImageVisualizerWidget::evaluateMemoryExpression,    this, QOverload<int,QString,int>::of(&SeerGdbWidget::handleGdbMemoryEvaluateExpression));

    // Tell the visualizer what variable to use.
    w->setVariableName(expression);
}

void SeerGdbWidget::handleGdbMemoryEvaluateExpression (int expressionid, QString address, int count) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString::number(expressionid) + "-data-read-memory-bytes " + address + " " + QString::number(count));
}

void SeerGdbWidget::handleGdbMemoryEvaluateExpression (int expressionid, QString address, int offset, int count) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString::number(expressionid) + "-data-read-memory-bytes -o " + QString::number(offset) + " " + address + " " + QString::number(count));
}

void SeerGdbWidget::handleGdbAsmEvaluateExpression (int expressionid, QString address, int count, int mode) {

    // -data-disassemble -s $pc -e "$pc + 96" -- 2

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("%1-data-disassemble -s \"%2 - %3\" -e \"%4 + %5\" -- %6").arg(expressionid).arg(address).arg(0).arg(address).arg(count).arg(mode));
}

void SeerGdbWidget::handleGdbArrayEvaluateExpression (int expressionid, QString address, int count) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString::number(expressionid) + "-data-read-memory-bytes " + address + " " + QString::number(count));
}

void SeerGdbWidget::handleGdbGetAssembly (QString address) {

    if (executableLaunchMode() == "") {
        return;
    }

    //qDebug() << "Getting assembly for address" << address << "for mode" << assemblyDisassemblyMode();

    QString command;

    if (assemblyDisassemblyMode() == "length") {
        command = QString("-data-disassemble -s \"%1\" -e \"%1 + %2\" -- 2").arg(address).arg(assemblyDisassemblyBytes());
    }else if (assemblyDisassemblyMode() == "function") {
        command = QString("-data-disassemble -a \"%1\" -- 2").arg(address);
    }else{
        command = QString("-data-disassemble -a \"%1\" -- 2").arg(address);
    }

    //qDebug() << command;

    handleGdbCommand(command);
}

void SeerGdbWidget::handleGdbGetSourceAndAssembly (QString address) {

    if (executableLaunchMode() == "") {
        return;
    }

    //qDebug() << "Getting source and assembly for address" << address << "for mode" << assemblyDisassemblyMode();

    QString command;

    if (assemblyDisassemblyMode() == "length") {
        command = QString("-data-disassemble -s \"%1\" -e \"%1 + %2\" -- 5").arg(address).arg(assemblyDisassemblyBytes());
    }else if (assemblyDisassemblyMode() == "function") {
        command = QString("-data-disassemble -a \"%1\" -- 5").arg(address);
    }else{
        command = QString("-data-disassemble -a \"%1\" -- 5").arg(address);
    }

    //qDebug() << command;

    handleGdbCommand(command);
}

void SeerGdbWidget::handleGdbMemoryVisualizer () {

    handleGdbMemoryAddExpression("");
}

void SeerGdbWidget::handleGdbArrayVisualizer () {

    handleGdbArrayAddExpression("");
}

void SeerGdbWidget::handleGdbMatrixVisualizer () {

    handleGdbMatrixAddExpression("");
}

void SeerGdbWidget::handleGdbStructVisualizer () {

    handleGdbStructAddExpression("");
}

void SeerGdbWidget::handleGdbVarVisualizer () {

    handleGdbVarAddExpression("");
}

void SeerGdbWidget::handleGdbImageVisualizer () {

    handleGdbImageAddExpression("");
}

void SeerGdbWidget::handleSplitterMoved (int pos, int index) {

    Q_UNUSED(pos);
    Q_UNUSED(index);

    //qDebug() << "Splitter moved to " << pos << index;

    writeSettings();
}

void SeerGdbWidget::handleManualCommandChanged () {

    //qDebug() << "Manual Command ComboBox changed";

    writeSettings();
}

void SeerGdbWidget::handleLogOuputChanged () {

    //qDebug() << "Log Output changed";

    writeSettings();
}

void SeerGdbWidget::handleGdbLoadBreakpoints () {

    QFileDialog dialog(this, "Seer - Load Breakpoints from a file.", "./", "Breakpoints (*.seer);;All files (*.*)");
    dialog.setOptions(QFileDialog::DontUseNativeDialog);
    dialog.setAcceptMode(QFileDialog::AcceptOpen);
    dialog.setFileMode(QFileDialog::AnyFile);
    dialog.setDefaultSuffix("seer");

    if (dialog.exec() != QDialog::Accepted) {
        return;
    }

    QStringList files = dialog.selectedFiles();

    if (files.size() == 0) {
        return;
    }

    if (files.size() > 1) {
        QMessageBox::critical(this, tr("Error"), tr("Select only 1 file."));
        return;
    }

    QString fname = files[0];

    handleGdbCommand("source -v " + fname);
    handleGdbGenericpointList();

    QMessageBox::information(this, "Seer", "Loaded.");
}

void SeerGdbWidget::handleGdbSaveBreakpoints () {

    if (_breakpointsBrowserWidget->isEmpty() &&
        _watchpointsBrowserWidget->isEmpty() &&
        _catchpointsBrowserWidget->isEmpty() &&
        _printpointsBrowserWidget->isEmpty()) {

        QMessageBox::information(this, "Seer", "No breakpoints of any kind to save.");

        return;
    }

    QFileDialog dialog(this, "Seer - Save Breakpoints to a file.", "./", "Breakpoints (*.seer);;All files (*.*)");
    dialog.setOptions(QFileDialog::DontUseNativeDialog);
    dialog.setAcceptMode(QFileDialog::AcceptSave);
    dialog.setFileMode(QFileDialog::AnyFile);
    dialog.setDefaultSuffix("seer");
    dialog.selectFile("breakpoints.seer");

    if (dialog.exec() != QDialog::Accepted) {
        return;
    }

    QStringList files = dialog.selectedFiles();

    if (files.size() == 0) {
        return;
    }

    if (files.size() > 1) {
        QMessageBox::critical(this, tr("Error"), tr("Select only 1 file."));
        return;
    }

    QString fname = files[0];

    handleGdbCommand("save breakpoints " + fname);

    QMessageBox::information(this, "Seer", "Saved.");
}

void SeerGdbWidget::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/BreakpointGdbSeerManager.md");
    help->setWindowFlags(help->windowFlags() | Qt::WindowStaysOnTopHint);
    help->show();
    help->raise();
}


void SeerGdbWidget::handleGdbAssemblyDisassemblyFlavor () {

    if (_assemblyDisassemblyFlavor != "") {
        handleGdbCommand(QString("-gdb-set disassembly-flavor ") + _assemblyDisassemblyFlavor);
    }
}

void SeerGdbWidget::handleGdbAssemblySymbolDemangling () {

    if (_assemblySymbolDemangling != "") {
        handleGdbCommand(QString("-gdb-set print asm-demangle ") + _assemblySymbolDemangling);
    }
}

void SeerGdbWidget::handleGdbProcessFinished (int exitCode, QProcess::ExitStatus exitStatus) {

    //qDebug() << "Gdb process finished. Exit code =" << exitCode << "Exit status =" << exitStatus;

    // Warn if gdb exits only if we are in some kind of run mode.
    if (executableLaunchMode() != "") {

        QMessageBox::warning(this, "Seer",
                QString("The GDB program exited unexpectedly.\n\n") +
                QString("Exit code=%1 Exit status=%2").arg(exitCode).arg(exitStatus) + "\n\n" +
                QString("'%1 %2'").arg(_gdbProcess->program()).arg(_gdbProcess->arguments().join(' ')) + "\n\n" +
                QString("Please restart Seer."),
                QMessageBox::Ok);
    }
}

void SeerGdbWidget::handleGdbProcessErrored (QProcess::ProcessError errorStatus) {

    //qDebug() << "Error launching gdb process. Error =" << errorStatus;

    if (errorStatus == QProcess::FailedToStart) {
        QMessageBox::warning(this, "Seer",
                                   QString("Unable to launch the GDB program.\n\n") +
                                   QString("'%1 %2'").arg(_gdbProcess->program()).arg(_gdbProcess->arguments().join(' ')) + "\n\n" +
                                   QString("Error status=%1)").arg(errorStatus),
                                   QMessageBox::Ok);
    }
}

void SeerGdbWidget::handleConsoleModeChanged () {

    if (_consoleIndex < 0) {
        return;
    }

    if (_consoleWidget == nullptr) {
        return;
    }

    if (_consoleMode == "detached") {
        logsTabWidget->detachTab(_consoleIndex, false);
        _consoleWidget->raise();
        _consoleWidget->resetSize();
    }else if (_consoleMode == "detachedminimized") {
        logsTabWidget->detachTab(_consoleIndex, true);
        _consoleWidget->resetSize();
    }else if (_consoleMode == "attached") {
        logsTabWidget->reattachTab(_consoleIndex);
    }else{
        logsTabWidget->reattachTab(_consoleIndex);
    }
}

void SeerGdbWidget::handleAboutToQuit () {

    // Detect if we're exiting Seer.
    setIsQuitting(true);
}

void SeerGdbWidget::writeSettings () {

    //qDebug() << "Write Settings";

    QSettings settings;

    settings.beginGroup("mainwindowsplitters"); {
        settings.setValue("leftCenterRightSplitter",              leftCenterRightSplitter->saveState());
        settings.setValue("codeManagerLogTabsSplitter",           codeManagerLogTabsSplitter->saveState());
        settings.setValue("sourceLibraryVariableManagerSplitter", sourceLibraryVariableManagerSplitter->saveState());
        settings.setValue("stackThreadManagerSplitter",           stackThreadManagerSplitter->saveState());
    } settings.endGroup();

    settings.beginGroup("consolewindow"); {
        settings.setValue("mode", consoleMode());
        settings.setValue("scrolllines", consoleScrollLines());
    }settings.endGroup();

    settings.beginWriteArray("manualgdbcommandshistory"); {
        QStringList commands = manualCommands(rememberManualCommandCount());

        for (int i = 0; i < commands.size(); ++i) {
            settings.setArrayIndex(i);
            settings.setValue("command", commands[i]);
        }
    } settings.endArray();

    settings.beginWriteArray("sourcealternatedirectories"); {
        QStringList directories = sourceAlternateDirectories();

        for (int i = 0; i < directories.size(); ++i) {
            settings.setArrayIndex(i);
            settings.setValue("directory", directories[i]);
        }
    } settings.endArray();

    settings.beginWriteArray("sourceignorefilepatters"); {
        QStringList patterns = sourceIgnoreFilePatterns();

        for (int i = 0; i < patterns.size(); ++i) {
            settings.setArrayIndex(i);
            settings.setValue("pattern", patterns[i]);
        }
    } settings.endArray();

    settings.beginWriteArray("sourcemiscfilepatterns"); {
        QStringList patterns = sourceMiscFilePatterns();

        for (int i = 0; i < patterns.size(); ++i) {
            settings.setArrayIndex(i);
            settings.setValue("pattern", patterns[i]);
        }
    } settings.endArray();

    settings.beginWriteArray("sourcesourcefilepatterns"); {
        QStringList patterns = sourceSourceFilePatterns();

        for (int i = 0; i < patterns.size(); ++i) {
            settings.setArrayIndex(i);
            settings.setValue("pattern", patterns[i]);
        }
    } settings.endArray();

    settings.beginWriteArray("sourceheaderfilepatterns"); {
        QStringList patterns = sourceHeaderFilePatterns();

        for (int i = 0; i < patterns.size(); ++i) {
            settings.setArrayIndex(i);
            settings.setValue("pattern", patterns[i]);
        }
    } settings.endArray();

    settings.beginGroup("assembly"); {
        settings.setValue("showassemblytabonstartup",    assemblyShowAssemblyTabOnStartup());
        settings.setValue("keepassemblytabontop",        assemblyKeepAssemblyTabOnTop());
        settings.setValue("assemblydisassemblyflavor",   assemblyDisassemblyFlavor());
        settings.setValue("assemblysymboldemagling",     assemblySymbolDemagling());
        settings.setValue("assemblyregisterformat",      assemblyRegisterFormat());
        settings.setValue("assemblyshowaddresscolumn",   assemblyShowAddressColumn());
        settings.setValue("assemblyshowoffsetcolumn",    assemblyShowOffsetColumn());
        settings.setValue("assemblyshowopcodecolumn",    assemblyShowOpcodeColumn());
        settings.setValue("assemblyshowsourcelines",     assemblyShowSourceLines());
        settings.setValue("assemblydisassemblymode",     assemblyDisassemblyMode());
        settings.setValue("assemblydisassemblybytes",    assemblyDisassemblyBytes());
    } settings.endGroup();

    settings.beginGroup("gdboutputlog"); {
        settings.setValue("enabled", isGdbOutputLogEnabled());
        settings.setValue("timestamp", isGdbOutputLogTimeStampEnabled());
    } settings.endGroup();

    settings.beginGroup("seeroutputlog"); {
        settings.setValue("enabled", isSeerOutputLogEnabled());
        settings.setValue("timestamp", isSeerOutputLogTimeStampEnabled());
    } settings.endGroup();
}

void SeerGdbWidget::readSettings () {

    QSettings settings;

    settings.beginGroup("mainwindowsplitters"); {
        leftCenterRightSplitter->restoreState(settings.value("leftCenterRightSplitter").toByteArray());
        codeManagerLogTabsSplitter->restoreState(settings.value("codeManagerLogTabsSplitter").toByteArray());
        sourceLibraryVariableManagerSplitter->restoreState(settings.value("sourceLibraryVariableManagerSplitter").toByteArray());
        stackThreadManagerSplitter->restoreState(settings.value("stackThreadManagerSplitter").toByteArray());
    } settings.endGroup();

    settings.beginGroup("consolewindow"); {
        setConsoleMode(settings.value("mode", "attached").toString());
        setConsoleScrollLines(settings.value("scrolllines", 1000).toInt());
    } settings.endGroup();

    int size = settings.beginReadArray("manualgdbcommandshistory"); {
        QStringList commands;

        for (int i = 0; i < size; ++i) {
            settings.setArrayIndex(i);

            commands << settings.value("command").toString();
        }

        setManualCommands(commands);
    } settings.endArray();

    size = settings.beginReadArray("sourcealternatedirectories"); {
        QStringList directories;

        for (int i = 0; i < size; ++i) {
            settings.setArrayIndex(i);

            directories << settings.value("directory").toString();
        }

        setSourceAlternateDirectories(directories);
    } settings.endArray();

    size = settings.beginReadArray("sourceignorefilepatters"); {
        QStringList patterns;

        for (int i = 0; i < size; ++i) {
            settings.setArrayIndex(i);

            patterns << settings.value("pattern").toString();
        }

        setSourceIgnoreFilePatterns(patterns);
    } settings.endArray();

    if (settings.childGroups().contains("sourcemiscfilepatterns")) {
        size = settings.beginReadArray("sourcemiscfilepatterns"); {
            QStringList patterns;

            for (int i = 0; i < size; ++i) {
                settings.setArrayIndex(i);

                patterns << settings.value("pattern").toString();
            }

            setSourceMiscFilePatterns(patterns);
        } settings.endArray();
    }

    if (settings.childGroups().contains("sourcesourcefilepatterns")) {
        size = settings.beginReadArray("sourcesourcefilepatterns"); {
            QStringList patterns;

            for (int i = 0; i < size; ++i) {
                settings.setArrayIndex(i);

                patterns << settings.value("pattern").toString();
            }

            setSourceSourceFilePatterns(patterns);
        } settings.endArray();
    }

    if (settings.childGroups().contains("sourceheaderfilepatterns")) {
        size = settings.beginReadArray("sourceheaderfilepatterns"); {
            QStringList patterns;

            for (int i = 0; i < size; ++i) {
                settings.setArrayIndex(i);

                patterns << settings.value("pattern").toString();
            }

            setSourceHeaderFilePatterns(patterns);
        } settings.endArray();
    }

    settings.beginGroup("assembly"); {
        setAssemblyShowAssemblyTabOnStartup( settings.value("showassemblytabonstartup",  false).toBool());
        setAssemblyKeepAssemblyTabOnTop(     settings.value("keepassemblytabontop",      true).toBool());
        setAssemblyDisassemblyFlavor(        settings.value("assemblydisassemblyflavor", "att").toString());
        setAssemblySymbolDemagling(          settings.value("assemblysymboldemagling",   "on").toString());
        setAssemblyRegisterFormat(           settings.value("assemblyregisterformat",    "Natural").toString());
        setAssemblyShowAddressColumn(        settings.value("assemblyshowaddresscolumn", true).toBool());
        setAssemblyShowOffsetColumn(         settings.value("assemblyshowoffsetcolumn",  false).toBool());
        setAssemblyShowOpcodeColumn(         settings.value("assemblyshowopcodecolumn",  false).toBool());
        setAssemblyShowSourceLines(          settings.value("assemblyshowsourcelines",   false).toBool());
        setAssemblyDisassemblyMode(          settings.value("assemblydisassemblymode",   "function").toString(),    settings.value("assemblydisassemblybytes", "256").toInt());
    } settings.endGroup();

    settings.beginGroup("gdboutputlog"); {
        setGdbOutputLogEnabled(settings.value("enabled", true).toBool());
        setGdbOutputLogTimeStampEnabled(settings.value("timestamp", false).toBool());
    } settings.endGroup();

    settings.beginGroup("seeroutputlog"); {
        setSeerOutputLogEnabled(settings.value("enabled", false).toBool());
        setSeerOutputLogTimeStampEnabled(settings.value("timestamp", false).toBool());
    } settings.endGroup();
}

bool SeerGdbWidget::isQuitting () const {
    return _isQuitting;
}

void SeerGdbWidget::setIsQuitting (bool f) {
    _isQuitting = f;
}

bool SeerGdbWidget::isGdbRuning () const {

    if (_gdbProcess->state() == QProcess::NotRunning) {
        return false;
    }

    return true;
}

bool SeerGdbWidget::startGdb () {

    // Don't do anything, if already running.
    if (isGdbRuning()) {
        qWarning() << "Already running";
        return false;
    }

    // Set the gdb program name to use.
    bool ok;

    QString rawcommand = gdbProgramOverride();

    if (rawcommand == "") {
        rawcommand = gdbProgram();
    }

    QString expandedcommand = Seer::expandEnv(rawcommand, &ok);

    //qDebug() << "Raw command     : " << rawcommand;
    //qDebug() << "Expanded command: " << expandedcommand;

    if (ok == false) {

        QMessageBox::critical(this, "Error", QString("Can't resolve all environment variables in command to launch gdb:\n'%1'").arg(rawcommand));

        return false;
    }

    // Build the gdb argument list.
    QString rawarguments = gdbArgumentsOverride();

    if (rawarguments == "") {
        rawarguments = gdbArguments();
    }

    QString expandedarguments = Seer::expandEnv(rawarguments, &ok);

    //qDebug() << "Raw arguments     : " << rawarguments;
    //qDebug() << "Expanded arguments: " << expandedarguments;

    if (ok == false) {

        QMessageBox::critical(this, "Error", QString("Can't resolve all environment variables in arguments to launch gdb:\n'%1'").arg(rawarguments));

        return false;
    }

    // Split string into words, handling "double quoted" words.
    QStringList args = Seer::split(expandedarguments);

    //qDebug() << args;

    // Give the gdb process the program and the argument list.
    _gdbProcess->setProgram(expandedcommand);
    _gdbProcess->setArguments(args);

    // We need to set the C language, otherwise the MI interface is translated and our message
    // filters will not work.
    QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
    env.insert("LANG", "C");
    _gdbProcess->setProcessEnvironment(env);

    // Start the gdb process.
    _gdbProcess->start();

    //qDebug() << _gdbProcess->state();

    return true;
}

bool SeerGdbWidget::startGdbRR () {

    // Don't do anything, if already running.
    if (isGdbRuning()) {
        qWarning() << "Already running";
        return false;
    }

    // Does the RR trace directory exist?
    if (QFile::exists(executableRRTraceDirectory()) == false) {
        QMessageBox::critical(this, "Seer",
                                    QString("The RR trace-directory '" + executableRRTraceDirectory() + "' doesn't exist."),
                                    QMessageBox::Ok);
        return false;
    }

    // Set the gdb program name to use.
    QString command   = rrProgram();
    QString arguments = rrArguments() + " --tty " + _consoleWidget->terminalDeviceName() + " " + executableRRTraceDirectory();

    if (rrGdbArguments() != "") {
        arguments += " -- " + rrGdbArguments();
    }

    // Split string into words, handling "double quoted" words.
    QStringList args = Seer::split(arguments);

    //qDebug() << "Expanded command: "   << command;
    //qDebug() << "Expanded arguments: " << arguments;

    // Give the gdb process the program and the argument list.
    _gdbProcess->setProgram(command);
    _gdbProcess->setArguments(args);

    // We need to set the C language, otherwise the MI interface is translated and our message
    // filters will not work.
    QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
    env.insert("LANG", "C");
    _gdbProcess->setProcessEnvironment(env);

    // Start the gdb process.
    _gdbProcess->start();

    //qDebug() << _gdbProcess->state();

    return true;
}

void SeerGdbWidget::killGdb () {

    // Don't do anything, if isn't running.
    if (isGdbRuning() == false) {
        return;
    }

    // Clear the launch mode.
    setExecutableLaunchMode("");
    setGdbRecordMode("");

    // Kill the process.
    _gdbProcess->kill();

    // Wait for it to end.
    _gdbProcess->waitForFinished();

    // Sanity check.
    if (isGdbRuning()) {
        qWarning() << "Is running but shouldn't be.";
    }
}

void SeerGdbWidget::createConsole () {

    if (_consoleWidget == 0) {
        _consoleWidget = new SeerConsoleWidget(0);

        // Connect window title changes.
        QObject::connect(this, &SeerGdbWidget::changeWindowTitle, _consoleWidget, &SeerConsoleWidget::handleChangeWindowTitle);

        // The console needs to know when it's detached or reattached.
        QObject::connect(logsTabWidget, qOverload<QWidget*>(&QDetachTabWidget::tabDetached),   _consoleWidget, &SeerConsoleWidget::handleTabDetached);
        QObject::connect(logsTabWidget, qOverload<QWidget*>(&QDetachTabWidget::tabReattached), _consoleWidget, &SeerConsoleWidget::handleTabReattached);

        _consoleIndex = logsTabWidget->addTab(_consoleWidget, "Console output");

        QObject::connect(_consoleWidget, &SeerConsoleWidget::newTextAdded,  this, &SeerGdbWidget::handleConsoleNewTextAdded);
        QObject::connect(_consoleWidget, &SeerConsoleWidget::newTextViewed, this, &SeerGdbWidget::handleConsoleNewTextViewed);

        setConsoleMode(consoleMode());
        setConsoleScrollLines(consoleScrollLines());
    }
}

void SeerGdbWidget::handleConsoleNewTextAdded () {

    if (_consoleIndex >= 0) {
        logsTabWidget->setTabIcon(_consoleIndex, QIcon(":/seer/resources/RelaxLightIcons/data-information.svg"));
    }
}

void SeerGdbWidget::handleConsoleNewTextViewed () {

    if (_consoleIndex >= 0) {
        logsTabWidget->setTabIcon(_consoleIndex, QIcon());
    }
}

SeerConsoleWidget* SeerGdbWidget::console () {

    return _consoleWidget;
}

void SeerGdbWidget::deleteConsole () {

    if (_consoleWidget) {

        if (_consoleIndex >= 0) {
            logsTabWidget->removeTab(_consoleIndex);
        }

        delete _consoleWidget;
        _consoleWidget = 0;
        _consoleIndex  = -1;
    }
}

void SeerGdbWidget::reattachConsole () {

     if (_consoleIndex < 0) {
        return;
    }

    if (_consoleWidget == nullptr) {
        return;
    }

    _consoleMode = "attached";

    logsTabWidget->reattachTab(_consoleIndex);
}

void SeerGdbWidget::setConsoleMode (const QString& mode) {

    _consoleMode = mode;

    handleConsoleModeChanged();
}

QString SeerGdbWidget::consoleMode () const {

    return _consoleMode;
}

void SeerGdbWidget::setConsoleScrollLines (int count) {

    _consoleScrollLines = count;

    if (_consoleWidget) {
        _consoleWidget->setScrollLines(_consoleScrollLines);
    }
}

int SeerGdbWidget::consoleScrollLines () const {

    return _consoleScrollLines;
}

void SeerGdbWidget::setManualCommands (const QStringList& commands) {

    manualCommandComboBox->clear();
    manualCommandComboBox->addItems(commands);

    // Point to last one.
    if (manualCommandComboBox->count() > 0) {
        manualCommandComboBox->setCurrentIndex(manualCommandComboBox->count()-1);
    }
}

QStringList SeerGdbWidget::manualCommands(int count) const {

    //qDebug() << "Count =" << count;

    // Select all if a zero.
    if (count == 0) {
        count = manualCommandComboBox->count();
    }

    // No more than count.
    if (count > manualCommandComboBox->count()) {
        count = manualCommandComboBox->count();
    }

    // Calculate starting position in list.
    int index = manualCommandComboBox->count() - count;

    // Get the list.
    QStringList list;

    for (; index<count; index++) {
        list << manualCommandComboBox->itemText(index);
    }

    return list;
}

void SeerGdbWidget::setRememberManualCommandCount (int count) {

    _rememberManualCommandCount = count;
}

int SeerGdbWidget::rememberManualCommandCount () const {

    return _rememberManualCommandCount;
}

void SeerGdbWidget::clearManualCommandHistory () {

    // Zap the entries in the combobox.
    manualCommandComboBox->clear();

    // Write the settings.
    writeSettings();
}

const QStringList& SeerGdbWidget::sourceAlternateDirectories() const {

    return editorManager()->editorAlternateDirectories();
}

void SeerGdbWidget::setSourceAlternateDirectories (const QStringList& alternateDirectories) {

    editorManager()->setEditorAlternateDirectories(alternateDirectories);
}

void SeerGdbWidget::setSourceMiscFilePatterns (const QStringList& filePatterns) {

    sourceLibraryManagerWidget->sourceBrowserWidget()->setMiscFilePatterns(filePatterns);
}

const QStringList& SeerGdbWidget::sourceMiscFilePatterns () const {

    return sourceLibraryManagerWidget->sourceBrowserWidget()->miscFilePatterns();
}

void SeerGdbWidget::setSourceSourceFilePatterns (const QStringList& filePatterns) {

    sourceLibraryManagerWidget->sourceBrowserWidget()->setSourceFilePatterns(filePatterns);
}

const QStringList& SeerGdbWidget::sourceSourceFilePatterns () const {

    return sourceLibraryManagerWidget->sourceBrowserWidget()->sourceFilePatterns();
}

void SeerGdbWidget::setSourceHeaderFilePatterns (const QStringList& filePatterns) {

    sourceLibraryManagerWidget->sourceBrowserWidget()->setHeaderFilePatterns(filePatterns);
}

const QStringList& SeerGdbWidget::sourceHeaderFilePatterns () const {

    return sourceLibraryManagerWidget->sourceBrowserWidget()->headerFilePatterns();
}

void SeerGdbWidget::setSourceIgnoreFilePatterns (const QStringList& filePatterns) {

    _ignoreFilePatterns = filePatterns;

    sourceLibraryManagerWidget->sourceBrowserWidget()->setIgnoreFilePatterns(sourceIgnoreFilePatterns());
    editorManager()->setEditorIgnoreDirectories(sourceIgnoreFilePatterns());
}

const QStringList& SeerGdbWidget::sourceIgnoreFilePatterns () const {

    return _ignoreFilePatterns;
}

void SeerGdbWidget::setAssemblyShowAssemblyTabOnStartup (bool flag) {

    _assemblyShowAssemblyTabOnStartup = flag;
}

bool SeerGdbWidget::assemblyShowAssemblyTabOnStartup () const {

    return _assemblyShowAssemblyTabOnStartup;
}

void SeerGdbWidget::setAssemblyKeepAssemblyTabOnTop (bool flag) {

    editorManager()->setKeepAssemblyTabOnTop(flag);
}

bool SeerGdbWidget::assemblyKeepAssemblyTabOnTop () const {

    return editorManager()->keepAssemblyTabOnTop();
}

void SeerGdbWidget::setAssemblyDisassemblyFlavor (const QString& flavor) {

    _assemblyDisassemblyFlavor = flavor;

    if (isGdbRuning()) {
        handleGdbAssemblyDisassemblyFlavor();

        emit assemblyConfigChanged();
    }
}

QString SeerGdbWidget::assemblyDisassemblyFlavor () const {

    return _assemblyDisassemblyFlavor;
}

void SeerGdbWidget::setAssemblySymbolDemagling (const QString& onoff) {

    _assemblySymbolDemangling = onoff;

    if (isGdbRuning()) {
        handleGdbAssemblySymbolDemangling();

        emit assemblyConfigChanged();
    }
}

void SeerGdbWidget::handleGdbSchedulerLockingMode (QString mode) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-gdb-set scheduler-locking %1").arg(mode));
}

void SeerGdbWidget::handleGdbScheduleMultipleMode (QString mode) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand(QString("-gdb-set schedule-multiple %1").arg(mode));
}

void SeerGdbWidget::handleGdbForkFollowMode (QString mode) {

    if (executableLaunchMode() == "") {
        return;
    }

    if (mode == "parent") {
        handleGdbCommand("-gdb-set follow-fork-mode parent");
        handleGdbCommand("-gdb-set detach-on-fork   on");
    }else if (mode == "child") {
        handleGdbCommand("-gdb-set follow-fork-mode child");
        handleGdbCommand("-gdb-set detach-on-fork   on");
    }else if (mode == "both") {
        handleGdbCommand("-gdb-set follow-fork-mode parent");
        handleGdbCommand("-gdb-set detach-on-fork   off");
    }else{
        qWarning() << "Invalid 'ForkFollowMode' of '" << mode << "'";
    }
}

void SeerGdbWidget::handleGdbLoadMICommands () {

    // Don't do anything, if isn't running.
    if (isGdbRuning() == false) {
        return;
    }

    // Path the MI files in the resources.
    QString miPath = ":/seer/resources/mi-python/"; // Resource file path

    QDir miDirectory(miPath);

    if (!miDirectory.exists()) {
        qDebug() << "Directory does not exist:" << miPath;
        return;
    }

    // Get list of MI files.
    QFileInfoList miList = miDirectory.entryInfoList(QDir::NoDotAndDotDot | QDir::AllEntries);

    // Print the list.
    qDebug() << "Sourcing scripts from:" << miPath;
    foreach (QFileInfo miInfo, miList) {

        // Open the source file from resources.
        QFile miFile(miInfo.absoluteFilePath());
        if (!miFile.exists()) {
            qDebug().nospace().noquote() << "Resource file '" << miInfo << "' does not exist!";
            continue;
        }

        // Destination file path in /tmp.
        QString destinationPath = "/tmp/" + miInfo.fileName();

        // Delete possible old temp version, if it exists.
        if (QFile::exists(destinationPath)) {
            bool f = QFile::remove(destinationPath);
            if (f == false) {
                qDebug().nospace().noquote() << "Old temp Resource file '" << destinationPath << "' can not be deleted!";
                continue;
            }
        }

        // Copy to temp.
        bool f = miFile.copy(destinationPath);
        if (f == false) {
            qDebug().nospace().noquote() << "Resource file '" << miInfo << "' can not be copied to '" << destinationPath << "'!";
            continue;
        }

        // Source it.
        if (QFile::exists(destinationPath) == false) {
            qDebug().nospace().noquote() << "Temp Resource file '" << destinationPath << "' does not exist!";
            continue;
        }

        qDebug() << "source " + miInfo.absoluteFilePath();

        QString command = "source " + destinationPath;

        handleGdbCommand(command);
    }
    qDebug() << "Done.";
}

void SeerGdbWidget::handleGdbSourceScripts () {

    // Don't do anything, if isn't running.
    if (isGdbRuning() == false) {
        return;
    }

    // Get setting's config path.
    QSettings settings;

    QString configFile = settings.fileName();

    if (configFile == "") {
        return;
    }

    QFileInfo configFileInfo(configFile);

    QString configDirectory = configFileInfo.absolutePath();

    // Get a list of files in the "scripts" folder.
    QString scriptsDirectory = configDirectory + "/scripts";

    QDir dir(scriptsDirectory);
    QStringList scripts = dir.entryList(QDir::Files, QDir::Name);

    // Source each one.
    qDebug() << "Sourcing scripts from:" << scriptsDirectory;
    for (const QString& script : scripts) {
        QString command = "source " + scriptsDirectory + "/" + script;

        qDebug() << command;

        handleGdbCommand(command);
    }
    qDebug() << "Done.";
}

QString SeerGdbWidget::assemblySymbolDemagling () const {

    return _assemblySymbolDemangling;
}

void SeerGdbWidget::setAssemblyShowAddressColumn (bool flag) {

    editorManager()->setAssemblyShowAddressColumn(flag);
}

bool SeerGdbWidget::assemblyShowAddressColumn () const {

    return editorManager()->assemblyShowAddressColumn();
}

void SeerGdbWidget::setAssemblyShowOffsetColumn (bool flag) {

    editorManager()->setAssemblyShowOffsetColumn(flag);
}

bool SeerGdbWidget::assemblyShowOffsetColumn () const {

    return editorManager()->assemblyShowOffsetColumn();
}

void SeerGdbWidget::setAssemblyShowOpcodeColumn (bool flag) {

    editorManager()->setAssemblyShowOpcodeColumn(flag);
}

bool SeerGdbWidget::assemblyShowOpcodeColumn () const {

    return editorManager()->assemblyShowOpcodeColumn();
}

void SeerGdbWidget::setAssemblyShowSourceLines (bool flag) {

    editorManager()->setAssemblyShowSourceLines(flag);
}

bool SeerGdbWidget::assemblyShowSourceLines () const {

    return editorManager()->assemblyShowSourceLines();
}

void SeerGdbWidget::setAssemblyRegisterFormat (const QString& format) {

    _assemblyRegisterFormat = format;

    variableManagerWidget->registerValuesBrowserWidget()->setRegisterFormat(_assemblyRegisterFormat);
}

QString SeerGdbWidget::assemblyRegisterFormat () const {

    return _assemblyRegisterFormat;
}

void SeerGdbWidget::setAssemblyDisassemblyMode (const QString& mode, int bytes) {

    _assemblyDisassemblyMode  = mode;
    _assemblyDisassemblyBytes = bytes;
}

QString SeerGdbWidget::assemblyDisassemblyMode () const {

    return _assemblyDisassemblyMode;
}

int SeerGdbWidget::assemblyDisassemblyBytes () const {

    return _assemblyDisassemblyBytes;
}

void SeerGdbWidget::setGdbOutputLogEnabled (bool flag) {

    _gdbOutputLog->setLogEnabled(flag);
}

bool SeerGdbWidget::isGdbOutputLogEnabled () const {

    return _gdbOutputLog->isLogEnabled();
}

void SeerGdbWidget::setGdbOutputLogTimeStampEnabled (bool flag) {

    _gdbOutputLog->setTimeStampEnabled(flag);
}

bool SeerGdbWidget::isGdbOutputLogTimeStampEnabled () const {

    return _gdbOutputLog->isTimeStampEnabled();
}

void SeerGdbWidget::setSeerOutputLogEnabled (bool flag) {

    _seerOutputLog->setLogEnabled(flag);
}

bool SeerGdbWidget::isSeerOutputLogEnabled () const {

    return _seerOutputLog->isLogEnabled();
}

void SeerGdbWidget::setSeerOutputLogTimeStampEnabled (bool flag) {

    _seerOutputLog->setTimeStampEnabled(flag);
}

bool SeerGdbWidget::isSeerOutputLogTimeStampEnabled () const {

    return _seerOutputLog->isTimeStampEnabled();
}

void SeerGdbWidget::sendGdbInterrupt (int signal) {

    //qDebug() << "Sending an interrupt to the program. Signal =" << signal;

    if (executableLaunchMode() == "") {
        return;
    }

    if (executablePid() < 1) {
        QMessageBox::warning(this, "Seer",
                                   QString("No executable is running or I don't know the PID of the process."),
                                   QMessageBox::Ok);
        return;
    }

    // Use kill() to send the signal to the inferior.
    // -exec-interrupt does not work for -exec-until when the line
    // number is not in the current function. In this case, -exec-until
    // behaves like -exec-continue but -exec-interrupt has no effect. :^(
    // We do have the ability to use a different signal, though. :^)

    if (signal < 0) {
        handleGdbCommand("-exec-interrupt");

    }else{
        if (gdbProgram() == gdbMultiarchExePath() && _gdbProcess->state() == QProcess::Running && _gdbmultiarchPid > 0)
        {
            int stat = kill(_gdbmultiarchPid, signal);
            if (stat < 0) {
                QMessageBox::warning(this, "Seer OpenOCD",
                                        QString("Unable to send signal '%1' to pid %2.\nError = '%3'").arg(strsignal(signal)).arg(executablePid()).arg(strerror(errno)),
                                        QMessageBox::Ok);
            }
        }
        else
        {
            int stat = kill(executablePid(), signal);
            if (stat < 0) {
                QMessageBox::warning(this, "Seer",
                                        QString("Unable to send signal '%1' to pid %2.\nError = '%3'").arg(strsignal(signal)).arg(executablePid()).arg(strerror(errno)),
                                        QMessageBox::Ok);
            }
        }
        
    }
}

// Deleay N seconds by executing Qt's even loop a bunch of times.
// This gives time for certain gdb commands to finish. Like saving the breakpoints before exit.
void SeerGdbWidget::delay (int seconds) {

    QTime dieTime = QTime::currentTime().addSecs(seconds);

    while (QTime::currentTime() < dieTime) {
        QCoreApplication::processEvents(QEventLoop::AllEvents, 100);
    }
}

/***********************************************************************************************************************
 * OpenOCD related getters, setters and handlers                                                                       *
 **********************************************************************************************************************/
// getter and setter, mainly called from SeerMainWindow.cpp
// ::Main
const QString& SeerGdbWidget::openOCDExePath() {
    return _openOCDExePath;
}

void SeerGdbWidget::setOpenOCDExePath (const QString& path) {
    _openOCDExePath = path;
}

const QString& SeerGdbWidget::gdbPort() {
    return _GDBPort;
}

void SeerGdbWidget::setGdbPort (const QString& port){
    _GDBPort = port;
}

const QString& SeerGdbWidget::telnetPort() {
    return _TelnetPort;
}

void SeerGdbWidget::setTelnetPort (const QString& port){
    _TelnetPort = port;
}

const QString& SeerGdbWidget::openOCDCommand() {
    return _openOCDCommands;
}

void SeerGdbWidget::setOpenOCDCommand (const QString& command){
    _openOCDCommands = command;
}

// ::GDB Multiarch
const QString& SeerGdbWidget::gdbMultiarchExePath () {
    return _gdbMultiarchExePath;
}

void SeerGdbWidget::setGdbMultiarchExePath (const QString& path) {
    _gdbMultiarchExePath = path;
}

const QString& SeerGdbWidget::gdbMultiarchCommand () {
    return _gdbMultiarchCommands;
}

void SeerGdbWidget::setGdbMultiarchCommand (const QString& command) {
    _gdbMultiarchCommands = command;
}

const QString SeerGdbWidget::openOCDTarget ()
{
    return _openOCDTarget;
}

void SeerGdbWidget::setOpenOCDTarget (const QString& target)
{
    _openOCDTarget = target;
}

// :: Docker
bool SeerGdbWidget::isBuiltInDocker()
{
    return _isBuildInDocker;
}

void SeerGdbWidget::setBuiltInDocker(bool check)
{
    _isBuildInDocker = check;
}

const QString SeerGdbWidget::absoluteBuildFolderPath()
{
    return _absoluteBuildPath;
}

void SeerGdbWidget::setAbsoluteBuildFolderPath(const QString& path)
{
    _absoluteBuildPath = path;
    if (_absoluteBuildPath.endsWith('/'))       // If end with '/', chop it    
        _absoluteBuildPath.chop(1);
}

const QString SeerGdbWidget::dockerBuildFolderPath()
{
    return _dockerBuildPath;
}

void SeerGdbWidget::setDockerBuildFolderPath(const QString& path)
{
    _dockerBuildPath = path;
    if (_dockerBuildPath.endsWith('/'))         // If end with '/', chop it    
        _dockerBuildPath.chop(1);
}

// ::Symbol Files
const QMap<QString, std::tuple<QString, bool, QString>> SeerGdbWidget::symbolFiles(void)
{
    return _symbolFiles;
}

void SeerGdbWidget::setSymbolFiles(const QMap<QString, std::tuple<QString, bool, QString>>& symbolFiles)
{
    _symbolFiles.clear();
    for (auto it = symbolFiles.constBegin(); it != symbolFiles.constEnd(); ++it) {
        _symbolFiles[it.key()]=it.value();
    }
}

SeerOpenOCDWidget* SeerGdbWidget::openOCDWidgetInstance() {
    return openocdWidget;
}

QProcess* SeerGdbWidget::openocdProcess() {
    return openocdWidget->openocdProcess();
}

// start gdb-multiarch, return true if success, false otherwise
void SeerGdbWidget::setGdbMultiarchPid(int pid)
{
    if (gdbProgram() != gdbMultiarchExePath())
        return;
    _gdbmultiarchPid = pid;
    setExecutablePid(pid);
}

void SeerGdbWidget::setNewHardwareBreakpointFlag(bool flag)
{
    _newHBreakFlag = flag;
}

bool SeerGdbWidget::isNewHardwareBreakpointFlag()
{
    return _newHBreakFlag;
}

void SeerGdbWidget::setGdbMultiarchRunningState(bool flag)
{
    _isTargetRunning = flag;
}

bool SeerGdbWidget::gdbMultiarchRunningState()
{
    return _isTargetRunning;
}

void SeerGdbWidget::setDebugOnInitFlag(bool flag)
{
    _debugOnInitFlag = flag;
}

bool SeerGdbWidget::isDebugOnInit()
{
    return _debugOnInitFlag;
}

void SeerGdbWidget::setSeekIdentifierFlag(bool flag)
{
    _seekingIndentifierFlag = flag;
}

bool SeerGdbWidget::isSeekIdentifier()
{
    return _seekingIndentifierFlag;
}
/***********************************************************************************************************************
 * slot                                                                                                                *
 **********************************************************************************************************************/
// This is call when Launch in OpenOCD mode, just like function invoked in Run/Attach mode
void SeerGdbWidget::handleGdbMultiarchOpenOCDExecutable()
{
    // Create the OpenOCD console tab, add to the log tabs
    openocdWidget->newOpenOCDWidget();
    openocdWidget->createOpenOCDConsole(logsTabWidget);
    openocdWidget->setTelnetPort(telnetPort());
    openocdWidget->setOpenOCDTarget(_openOCDTarget);

    // Start OpenOCD with the given path and command
    bool foo = openocdWidget->startOpenOCD(openOCDExePath(), openOCDCommand());
    if (foo == false) {
        QMessageBox::warning(this, "Seer",
                                   QString("Unable to launch the OpenOCD program.\n\n") +
                                   QString("'%1 %2'").arg(SeerGdbWidget::openOCDExePath()).arg(SeerGdbWidget::openOCDCommand()) + "\n\n" +
                                   QString("Please check your OpenOCD configuration."),
                                   QMessageBox::Ok);
        return;
    }
    // Now, set _gdbProgram as gdb-multiarch, provided by openocd launch mode
    setGdbProgram(gdbMultiarchExePath());
    setGdbMultiarchRunningState(true);          // always assume that target is running
    // OpenOCD works in connect mode, so use code of handleGdbConnectExecutable()
    qCDebug(LC) << "Starting 'openocd gdb-multiarch connect'";

    QApplication::setOverrideCursor(Qt::BusyCursor);

    while (1) {

        _executableBreakMode = "";

        // Always say a new executable.
        // This causes a new gdb each time. The same console, though.
        setNewExecutableFlag(true);

        // Disconnect from the terminal and delete the old gdb if there is a new executable.
        // Is this really needed? -> Comment out
        // if (newExecutableFlag() == true) {
        //     console()->deleteTerminal();
        //     killGdb();
        // }

        // If gdb isn't running, start it.
        // No need to connect to the console in this mode.
        if (isGdbRuning() == false) {

            bool f = startGdb();
            if (f == false) {
                QMessageBox::critical(this, tr("Error"), tr("Can't start gdb."));
                break;
            }
            
            handleGdbCommand("-gdb-set non-stop off");
            handleGdbLoadMICommands();
            handleGdbSourceScripts();
        }

        // No console for 'connect' mode but make sure it's reattached.
        setExecutableLaunchMode("openocd");
        saveLaunchMode();
        setGdbRecordMode("auto");
        setGdbMultiarchPid(_gdbProcess->processId());
        reattachConsole();

        // Load any 'pre' commands.
        if (newExecutableFlag() == true) {
            if (gdbServerDebug()) {
                handleGdbCommand("-gdb-set debug remote 1"); // Turn on gdbserver debug
            }else{
                handleGdbCommand("-gdb-set debug remote 0");
            }
        }
        setGdbRemoteTargetType("extended-remote");

        // If symbol is built in docker, then _dockerBuildPath shall be replace by _absoluteBuildPath
        if (isBuiltInDocker())
        {
            QString manualCommand = "set substitute-path " + dockerBuildFolderPath() + " " + absoluteBuildFolderPath();
            handleGdbCommand(manualCommand);
        }
        
        // Handle additional gdb-multiarch command
        handleGdbCommand(QString("%1").arg(gdbMultiarchCommand()));
        // Load the executable, if needed.
        if (newExecutableFlag() == true) {
            for (auto it = _symbolFiles.constBegin(); it != _symbolFiles.constEnd(); ++it) {
                const auto &tuple = it.value();
                const bool enableLoadAddress = std::get<1>(tuple);
                const QString &loadAddress = std::get<2>(tuple);
                QString loadSymbolCmd = "add-symbol-file " + it.key();
                if (enableLoadAddress)
                {
                    loadSymbolCmd += " " + loadAddress;
                }
                handleGdbCommand(loadSymbolCmd);
            }
            
            handleGdbExecutableSources();           // Load the program source files. gdb-multiarch keeps
            handleGdbExecutableLoadBreakpoints();   // Set the program's breakpoints (if any) before running. gdb-multiarch keeps

            setNewExecutableFlag(false);
        }

        for (auto it = _symbolFiles.constBegin(); it != _symbolFiles.constEnd(); ++it) {
            const auto &tuple = it.value();
            const QString &sourcePath = std::get<0>(tuple);
            QString loadSourceCmd = "-environment-directory \"" + sourcePath + "\"";
            handleGdbCommand(loadSourceCmd);
        }
        // Set or reset some things.
        handleGdbAssemblyDisassemblyFlavor();   // Set the disassembly flavor to use.
        handleGdbAssemblySymbolDemangling();    // Set the symbol demangling.

        if (assemblyShowAssemblyTabOnStartup()) {
            editorManager()->showAssembly();
        }

        if (gdbHandleTerminatingException()) {
            handleGdbCommand("-gdb-set unwind-on-terminating-exception on"); // Turn on terminating exceptions when gdb calls the program's functions.
        }else{
            handleGdbCommand("-gdb-set unwind-on-terminating-exception off");
        }

        // Connect to the remote gdbserver using the proper remote type. Only do this when all symbol and source code is loaded
        handleGdbCommand(QString("-target-select %1 :%2").arg(gdbRemoteTargetType()).arg(gdbPort()));

        // Set window titles with name of program.
        emit changeWindowTitle(QString("OpenOCD - Gdb-multiarch Debugging session (GDB pid = %1)").arg(_gdbProcess->processId()));

        // Notify the state of the GdbWidget has changed.
        emit stateChanged();

        break;
    }

    QApplication::setOverrideCursor(Qt::ArrowCursor);

    qCDebug(LC) << "Finishing 'gdb-multiarch connect'.";

}

void SeerGdbWidget::handleOpenOCDMainHelpButtonClicked()
{
    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/OpenOCDHelp.md");
    help->setWindowFlags(help->windowFlags() | Qt::WindowStaysOnTopHint);
    help->exec();
}

void SeerGdbWidget::handleOpenOCDStartFailed()
{
    logsTabWidget->setCurrentIndex(9);          // Switch to openocd console tab
}

// Promtp a dialog, tell user to input kernel module that they want to debug
void SeerGdbWidget::handleDebugKernelModule()
{
    // 1. Read all breakpoints status, save it
    // 2. Disable all breakpoints
    // 3. Check lsmod to see if module is already loaded
    // 4. Add temp breakpoint to load_module and insmod the module
    // 5. Wait until breakpoint reached, read kernel module adress: *mod->sect_attrs->attrs@mod->sect_attrs->nsections
    // 6. Load kernel module to gdb-multiarch with provided address
    // 7. Let's put breakpoint at _init function and run to it
    // 8. Reload previous breakpoint. Let target run.
    SeerOpenOCDDebugOnInit dlg(this);
    int ret = dlg.exec();
    if (ret == 0)
        return;
    
    _moduleName                 = dlg.moduleName();
    _commandToTerm              = dlg.commandToTerm();
    _kernelModuleSymbolPath     = dlg.kernelModuleSymbolPath();
    _kernelModuleSourceCodePath = dlg.kernelModuleSourceCodePath();
    _serialPortPath             = dlg.serialPortPath();
    
    if (_kernelModuleSymbolPath == "")
    {
        QMessageBox::warning(nullptr, "Seer", QString("Path to kernel module symbol is empty. Abort!"), QMessageBox::Ok);
        return;
    }
    if (_kernelModuleSourceCodePath == "")
    {
        QMessageBox::warning(nullptr, "Seer", QString("Path to kernel module source code is empty. Abort!"), QMessageBox::Ok);
        return;
    }
    if ( Seer::isFileExistNotify(_kernelModuleSymbolPath) == false)
        return;
    if ( Seer::isDirExistNotify(_kernelModuleSourceCodePath) == false)
        return;
    if ( Seer::isFileExistNotify(_serialPortPath) == false)
        return;
    if (_moduleName == "" || _commandToTerm == "")
    {
        QMessageBox::warning(nullptr, "Seer", QString("Command to terminal is empty. Abort!"), QMessageBox::Ok);
        return;
    }

    // Set Flag to tell the others function that openocd is handling
    setDebugOnInitFlag(true);
    // Problem: If handle all 1->8 in 1 continuous function then race condition will certainly occur. This is because
    //          When handleGdbCommand is send, it take a while for gdb to respond. By that time, function would be over
    //          thus it couldn't receive any data from gdb
    // Solution: Multithread for task parallel execution, keeping mainwindow alive while child thread handle reading data
    //           Mutex and conditional variable for synchronization between SeerGdbWidget::handleText and child thread
    _workerThread = QThread::create([this]() {
        debugOnInitHandler();                    // Run your background logic here
    });
    QObject::connect(_workerThread, &QThread::finished, _workerThread, &QObject::deleteLater);
    _workerThread->start();
}

// A threading function for handling openocd debug on init, it's not a slot. This acts as consumer, wait until 
// producer SeerGdbWidget::handleText returns unlock
void SeerGdbWidget::debugOnInitHandler()
{
    // 1. Read all breakpoints status, save it
    _mapListBpStatus.clear();
    _debugOnInitBpReadFlag = false;
    _debugOnInitTempBpFlag = false;
    _debugOnInitJustReadModuleDir = false;
    _debugOnInitFindLoadModuleFile = false;
    _loadModuleFile.clear();
    handleSyncGdbInterruptSIGINT_DebugOnInit();                 // SIGINT      -> *stopped
    handleSyncGdbGenericpointList();                            // -break-list -> list bp ^done,BreakpointTable

    // 2. Disable all breakpoints
    for (auto it = _mapListBpStatus.begin(); it != _mapListBpStatus.end(); it ++)
    {
        handleSyncBreakDisable(it.key());
    }

    // 3. Check lsmod to see if module is already loaded
    // raise this flag, so that *stopped will stop and read data to find out which file contains load_module function
    _debugOnInitFindLoadModuleFile = true;

    _lsmodDebugOnInitFlag = true;
    _isModuleIsLoaded = false;
    handleSyncLsmod(_moduleName);                   // lsmod module name  -> return: ^done,result

    if (_isModuleIsLoaded)
    {
        // 8. Reload previous breakpoint. Let target run.
        for (auto it = _mapListBpStatus.begin(); it != _mapListBpStatus.end(); it ++)
        {
            if (it.value() == "y")
                handleSyncBreakEnable(it.key());
            else
                handleSyncBreakDisable(it.key());
        }
        handleSyncGdbContinue();
        emit requestWarning(QString("Module is already loaded. Please unload module first before debug on init."));
        setDebugOnInitFlag(false);                  // lower this flag, indicating debug on init thread ended
        return;
    }

    // 4. Add temp breakpoint to load_module and insmod the module
    QString tmpBp = "-t load_module";
    handleSyncBreakInsert(tmpBp);                   // Add temp breakpoint
    handleSyncGdbContinue();                        // -exec-continue -> *stopped,reason="breakpoint-hit"

    handleSyncSendToSerial(_serialPortPath, _commandToTerm);        // And insmod the module
    // Wait until breakpoint reached
    _debugOnInitStopMutex.lock();
    _debugOnInitStopCv.wait(&_debugOnInitStopMutex);
    _debugOnInitStopMutex.unlock();

    // Breakpoint hits!
    int lineNumber = 0;
    if (_loadModuleFile.isEmpty())
    {
        emit requestWarning(QString("Debug on Init fail!\n Cannot find kernel/module/main.c."));
        return;
    }
    else            // Now, let check which line has "return do_init_module(mod)"
    {
        if (!(_moduleInitLineNo > 0))
        {
            QFile file(_loadModuleFile);

            if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
                emit requestWarning(QString("Debug on Init fail!\n Cannot open file kernel/module/main.c."));
                return;
            }
            QTextStream in(&file);

            while (!in.atEnd()) {
                QString line = in.readLine();
                if (line.contains("return do_init_module")) {
                    _moduleInitLineNo = lineNumber + 1;
                    break;
                }
                ++lineNumber;
            }
        }
    }

    // add breakpoint at correct line so that we can read correct kernel module load address
    QString expression = "-t -f --source \"" + _loadModuleFile + "\" --line " + QString::number(_moduleInitLineNo);;
    handleSyncBreakInsert(expression);
    _debugOnInitFindLoadModuleFile = true;
    handleSyncGdbContinue();                        // -exec-continue -> *stopped,reason="breakpoint-hit"
    // Wait until breakpoint reached
    _debugOnInitStopMutex.lock();
    _debugOnInitStopCv.wait(&_debugOnInitStopMutex);
    _debugOnInitStopMutex.unlock();

    // 5. Read kernel module adress: *mod->sect_attrs->attrs@mod->sect_attrs->nsections
    QString gdbReadExpr = "-data-evaluate-expression \"*mod->sect_attrs->attrs@mod->sect_attrs->nsections\"";
    handleSyncManualGdbCommand(gdbReadExpr);
    
    // 6. Load kernel module to gdb-multiarch with provided address
    QString gdbLoadExpr = "add-symbol-file " + _kernelModuleSymbolPath + " ";
    for (auto it = _mapKernelModuleAddress.begin(); it != _mapKernelModuleAddress.end(); it ++)
    {
        gdbLoadExpr += "-s " + it.key() + " " + it.value() + " ";
    }
    handleSyncManualGdbCommand(gdbLoadExpr);

    // load source code
    QString loadCmd = "directory " + _kernelModuleSourceCodePath;
    handleSyncManualGdbCommand(loadCmd);            // -> ^done
    // now refresh source browser to load new kernel module source code
    handleSyncRefreshSource();

    // 7. Let's put breakpoint at _init function and run to it
    QString initFuncbpCmd = "thbreak *" + _mapKernelModuleAddress.value(".init.text");;
    handleSyncManualGdbCommand(initFuncbpCmd);
    
    // 8. Reload previous breakpoint. Let target run.
    for (auto it = _mapListBpStatus.begin(); it != _mapListBpStatus.end(); it ++)
    {
        if (it.value() == "y")
            handleSyncBreakEnable(it.key());
        else
            handleSyncBreakDisable(it.key());
    }

    handleSyncGdbContinue();
    emit requestWarning(QString("Debug on Init done."));
    setDebugOnInitFlag(false);                      // lower this flag, indicating debug on init thread ended
    return;
}

/***********************************************************************************************************************
 * Functions for handling debug on init                                                                                *
 **********************************************************************************************************************/
// These below functions will emit signal back to MainWindow, and MainWindow will again invoke handler
// from gdbWidget accordingly. This is to avoid invoking any Qprocess function inside QThread.
// If we don't do this, program might be disrupted. Action buttons may not work and we cannot send any
// command to openocd via gdb/mi command bar. This is just a temporary solution.
void SeerGdbWidget::handleSyncGdbInterruptSIGINT_DebugOnInit()
{
    _debugOnInitStopMutex.lock();
    _sigINTDebugOnInitFlag = true;
    handleGdbInterruptSIGINT();
    _debugOnInitStopCv.wait(&_debugOnInitStopMutex);
    _debugOnInitStopMutex.unlock();
}

void SeerGdbWidget::handleSyncGdbGenericpointList()
{
    _debugOnInitListBpMutex.lock();
    emit requestBreakList();
    _debugOnInitListBpCv.wait(&_debugOnInitListBpMutex);
    _debugOnInitListBpMutex.unlock();
}

void SeerGdbWidget::handleSyncGdbContinue()
{
    _debugOnInitRunningMutex.lock();
    emit requestContinue();
    _debugOnInitRunningCv.wait(&_debugOnInitRunningMutex);
    _debugOnInitRunningMutex.unlock();
}

void SeerGdbWidget::handleSyncBreakInsert (QString bp)
{
    _debugOnInitHandleBpMutex.lock();
    emit requestBreakInsert(bp);
    _debugOnInitHandleBpCv.wait(&_debugOnInitHandleBpMutex);
    _debugOnInitHandleBpMutex.unlock();
}

void SeerGdbWidget::handleSyncBreakEnable (QString bp)
{
    _debugOnInitHandleBpMutex.lock();
    emit requestBreakEnable(bp);
    _debugOnInitHandleBpCv.wait(&_debugOnInitHandleBpMutex);
    _debugOnInitHandleBpMutex.unlock();
}

void SeerGdbWidget::handleSyncBreakDisable (QString bp)
{
    _debugOnInitHandleBpMutex.lock();
    emit requestBreakDisable(bp);
    _debugOnInitHandleBpCv.wait(&_debugOnInitHandleBpMutex);
    _debugOnInitHandleBpMutex.unlock();
}

void SeerGdbWidget::handleSyncManualGdbCommand(QString expression)
{
    _debugOnInitOperationMutex.lock();
    emit requestGdbCommand(expression);
    _debugOnInitOperationCv.wait(&_debugOnInitOperationMutex);
    _debugOnInitOperationMutex.unlock();
}

void SeerGdbWidget::handleSyncGdbFindVariableIdentifier(const QString& identifier)
{
    _seekIdentifierMutex.lock();
    emit requestSeekVariableIdentifier(identifier);
    _seekIdentifierCv.wait(&_seekIdentifierMutex);
    _seekIdentifierMutex.unlock();
}

void SeerGdbWidget::handleSyncGdbFindFunctionIdentifier (const QString& identifier)
{
    _seekIdentifierMutex.lock();
    emit requestSeekFunctionIdentifier(identifier);
    _seekIdentifierCv.wait(&_seekIdentifierMutex);
    _seekIdentifierMutex.unlock();
}

void SeerGdbWidget::handleSyncGdbFindTypeIdentifier (const QString& identifier)
{
    _seekIdentifierMutex.lock();
    emit requestSeekTypeIdentifier(identifier);
    _seekIdentifierCv.wait(&_seekIdentifierMutex);
    _seekIdentifierMutex.unlock();
}

void SeerGdbWidget::handleSyncSeekVariableIdentifier (const QString& identifier)
{
    handleGdbCommand("-symbol-info-variables " + identifier);
}

void SeerGdbWidget::handleSyncSeekFunctionIdentifier (const QString& identifier)
{
    handleGdbCommand("-symbol-info-functions " + identifier);
}

void SeerGdbWidget::handleSyncSeekTypeIdentifier (const QString& identifier)
{
    handleGdbCommand("-symbol-info-types " + identifier);
}

void SeerGdbWidget::handleSyncSendToSerial(QString path, QString expression)
{
    _debugOnInitOperationMutex.lock();
    emit requestSendToSerial(path,expression);
    _debugOnInitOperationCv.wait(&_debugOnInitOperationMutex);
    _debugOnInitOperationMutex.unlock();
}

void SeerGdbWidget::handleSendToSerial(QString path, QString expression)
{
    _debugOnInitOperationMutex.lock();
    QProcess process;
    QString cmd = "echo \"" + expression + "\" > " + path;
    process.start("/bin/sh", QStringList() << "-c" << cmd);
    process.waitForFinished();
    QByteArray stdoutData = process.readAllStandardOutput();
    QByteArray stderrData = process.readAllStandardError();
    if (!stderrData.isEmpty()) {
        QMessageBox::warning(this, "Seer", QString(stderrData + "\n Please run insmod/rmmod command manually!"), QMessageBox::Ok, QMessageBox::Ok);
    }
    _debugOnInitOperationCv.notify_one();
    _debugOnInitOperationMutex.unlock();
}

void SeerGdbWidget::handleSyncRefreshSource()
{
    _debugOnInitRefreshSourceMutex.lock();
    emit requestRefreshSource();
    _debugOnInitRefreshSourceCv.wait(&_debugOnInitRefreshSourceMutex);
    _debugOnInitRefreshSourceMutex.unlock();
}

// Functions for handling python lsmod
void SeerGdbWidget::handleSyncLsmod(QString kernelModuleName)
{
    _lsmodMutex.lock();
    emit requestLsmod(kernelModuleName);
    _lsmodCv.wait(&_lsmodMutex);
    _lsmodMutex.unlock();
}

void SeerGdbWidget::handleGdbLsmod (const QString& kernelModuleName)
{
    handleGdbCommand("-lsmod " + kernelModuleName);
}

void SeerGdbWidget::handleSyncWarning (const QString& warningMsg)
{
    QMessageBox::warning(this, "Seer", QString(warningMsg), QMessageBox::Ok);
}
/***********************************************************************************************************************
 * Functions for handling tracing identifier                                                                           *
 **********************************************************************************************************************/
void SeerGdbWidget::handleSyncGdbInterruptSIGINT_TraceIdentifier()
{
    _traceIdentiferStopMutex.lock();
    handleGdbInterruptSIGINT();
    _traceIdentiferStopCv.wait(&_traceIdentiferStopMutex);
    _traceIdentiferStopMutex.unlock();
}

void SeerGdbWidget::handleSeekIdentifier(const QString& identifier)
{
    // Raise the flag
    setSeekIdentifierFlag(true);
    _Identifier = identifier;
    QApplication::setOverrideCursor(Qt::BusyCursor);
    // Create a thread handling this
    _workerThread = QThread::create([this, identifier]() {
        traceIdentifierHandler(identifier);                     // Run your background logic here
    });
    QObject::connect(_workerThread, &QThread::finished, _workerThread, &QObject::deleteLater);
    _workerThread->start();
}

// Handler for handling tracing identifier multithread
void SeerGdbWidget::traceIdentifierHandler(const QString& identifier)
{
    // If openocd is runinng, and target is running, temporarily interrupt the target and read symbol
    if (openocdWidget->isOpenocdRunning() == true && gdbProgram() == gdbMultiarchExePath() && \
        _gdbProcess->state() == QProcess::Running && _gdbmultiarchPid > 0)
    {
        // if target is running
        if (gdbMultiarchRunningState() == true)
        {
            setNewHardwareBreakpointFlag(true);
            editorManagerWidget->setEnableOpenFile(false);                  // precent open file when signal is sent
            handleSyncGdbInterruptSIGINT_TraceIdentifier();                 // SIGINT      -> *stopped
            handleSyncGdbFindVariableIdentifier(QString(" --name " + identifier));
            handleSyncGdbFindFunctionIdentifier(QString(" --name " + identifier));
            handleSyncGdbFindTypeIdentifier(QString(" --name " + identifier));
            handleSyncGdbContinue();                            // -exec-continue -> *stopped,reason="breakpoint-hit"
            editorManagerWidget->setEnableOpenFile(true);       // re-enable open file
        }
        else    // simply read symbol
        {
            handleSyncGdbFindVariableIdentifier(QString(" --name " + identifier));
            handleSyncGdbFindFunctionIdentifier(QString(" --name " + identifier));
            handleSyncGdbFindTypeIdentifier(QString(" --name " + identifier));
        }
    }
    else
    {   // For desktop application
        handleSyncGdbFindVariableIdentifier(QString(" --name " + identifier));
        handleSyncGdbFindFunctionIdentifier(QString(" --name " + identifier));
        handleSyncGdbFindTypeIdentifier(QString(" --name " + identifier));
    }
    setSeekIdentifierFlag(false);    // Lower the flag
    QApplication::setOverrideCursor(Qt::BusyCursor);
}

/***********************************************************************************************************************
 * Functions for handling exception level                                                                              *
 **********************************************************************************************************************/
void SeerGdbWidget::handleExceptionLevelChanged(const QString& exceptionLevel)
{
    // Check document: OpenOCD.pdf: section "16.6.6 ARMv8-A specific commands" for more information
    QMap<QString, QString> exceptionMap;
    exceptionMap["EL1H"]="sec_el1";
    exceptionMap["EL3H"]="sec_el3";
    exceptionMap["N-EL1H"]="nsec_el1";
    exceptionMap["N-EL2H"]="nsec_el2";
    exceptionMap["EL1H / EL3H"]="sec_el1 sec_el3";
    exceptionMap["N-EL1H / N-EL2H"]="nsec_el1 nsec_el2";
    exceptionMap["off"]="off";
    if (_openOCDTarget == "")
        return;
    // send cmd to telnet
    qint64 bytesWritten = openocdWidget->telnetExecuteCmd(QString(_openOCDTarget + " catch_exc " + exceptionMap[exceptionLevel]));
    if (bytesWritten == -1) {
        qWarning() << "Failed to write to process!";
    } else {
        qDebug() << "Successfully wrote" << bytesWritten << "bytes.";
    }
        
}


