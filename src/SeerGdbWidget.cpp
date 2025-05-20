#include "SeerGdbWidget.h"
#include "SeerLogWidget.h"
#include "SeerMemoryVisualizerWidget.h"
#include "SeerArrayVisualizerWidget.h"
#include "SeerStructVisualizerWidget.h"
#include "SeerVarVisualizerWidget.h"
#include "SeerImageVisualizerWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include "QHContainerWidget.h"
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

    _gdbOutputLog             = new SeerGdbLogWidget(this);
    _seerOutputLog            = new SeerSeerLogWidget(this);
    _gdbOutputLog->setPlaceholderText("[gdb output]");
    _seerOutputLog->setPlaceholderText("[seer output]");

    logsTabWidget->addTab(_messagesBrowserWidget,    "Messages");
    logsTabWidget->addTab(_breakpointsBrowserWidget, "Breakpoints");
    logsTabWidget->addTab(_watchpointsBrowserWidget, "Watchpoints");
    logsTabWidget->addTab(_catchpointsBrowserWidget, "Catchpoints");
    logsTabWidget->addTab(_printpointsBrowserWidget, "Printpoints");
    logsTabWidget->addTab(_gdbOutputLog,             "GDB output");
    logsTabWidget->addTab(_seerOutputLog,            "Seer output");
    logsTabWidget->setCurrentIndex(0);

    // Create the console.
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
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               variableManagerWidget->registerValuesBrowserWidget(),           &SeerRegisterValuesBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               variableManagerWidget->variableTrackerBrowserWidget(),          &SeerVariableTrackerBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::caretTextOutput,                                                               variableManagerWidget->variableLoggerBrowserWidget(),           &SeerVariableLoggerBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::astrixTextOutput,                                                              _watchpointsBrowserWidget,                                      &SeerWatchpointsBrowserWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::astrixTextOutput,                                                              this,                                                           &SeerGdbWidget::handleText);
    QObject::connect(_gdbMonitor,                                               &GdbMonitor::equalTextOutput,                                                               this,                                                           &SeerGdbWidget::handleText);

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
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::addMemoryVisualize,                                               this,                                                           &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::addArrayVisualize,                                                this,                                                           &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::addStructVisualize,                                               this,                                                           &SeerGdbWidget::handleGdbVarAddExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::evaluateVariableExpression,                                       this,                                                           &SeerGdbWidget::handleGdbDataEvaluateExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::evaluateVariableExpression,                                       variableManagerWidget->variableLoggerBrowserWidget(),           &SeerVariableLoggerBrowserWidget::handleEvaluateVariableExpression);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::requestAssembly,                                                  this,                                                           &SeerGdbWidget::handleGdbGetAssembly);
    QObject::connect(editorManagerWidget,                                       &SeerEditorManagerWidget::requestSourceAndAssembly,                                         this,                                                           &SeerGdbWidget::handleGdbGetSourceAndAssembly);

    QObject::connect(sourceLibraryManagerWidget->sourceBrowserWidget(),         &SeerSourceBrowserWidget::refreshSourceList,                                                this,                                                           &SeerGdbWidget::handleGdbExecutableSources);
    QObject::connect(sourceLibraryManagerWidget->sourceBrowserWidget(),         &SeerSourceBrowserWidget::selectedFile,                                                     editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);
    QObject::connect(sourceLibraryManagerWidget->functionBrowserWidget(),       &SeerFunctionBrowserWidget::refreshFunctionList,                                            this,                                                           &SeerGdbWidget::handleGdbExecutableFunctions);
    QObject::connect(sourceLibraryManagerWidget->functionBrowserWidget(),       &SeerFunctionBrowserWidget::selectedFile,                                                   editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);
    QObject::connect(sourceLibraryManagerWidget->typeBrowserWidget(),           &SeerTypeBrowserWidget::refreshTypeList,                                                    this,                                                           &SeerGdbWidget::handleGdbExecutableTypes);
    QObject::connect(sourceLibraryManagerWidget->typeBrowserWidget(),           &SeerTypeBrowserWidget::selectedFile,                                                       editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);
    QObject::connect(sourceLibraryManagerWidget->staticBrowserWidget(),         &SeerStaticBrowserWidget::refreshVariableList,                                              this,                                                           &SeerGdbWidget::handleGdbExecutableVariables);
    QObject::connect(sourceLibraryManagerWidget->staticBrowserWidget(),         &SeerStaticBrowserWidget::selectedFile,                                                     editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);
    QObject::connect(sourceLibraryManagerWidget->libraryBrowserWidget(),        &SeerLibraryBrowserWidget::refreshLibraryList,                                              this,                                                           &SeerGdbWidget::handleGdbExecutableLibraries);
    QObject::connect(sourceLibraryManagerWidget->adaExceptionsBrowserWidget(),  &SeerAdaExceptionsBrowserWidget::refreshAdaExceptions,                                      this,                                                           &SeerGdbWidget::handleGdbAdaListExceptions);
    QObject::connect(sourceLibraryManagerWidget->adaExceptionsBrowserWidget(),  &SeerAdaExceptionsBrowserWidget::insertCatchpoint,                                          this,                                                           &SeerGdbWidget::handleGdbCatchpointInsert);
    QObject::connect(sourceLibraryManagerWidget->skipBrowserWidget(),           &SeerSkipBrowserWidget::refreshSkipList,                                                    this,                                                           &SeerGdbWidget::handleGdbListSkips);
    QObject::connect(sourceLibraryManagerWidget->skipBrowserWidget(),           &SeerSkipBrowserWidget::addSkip,                                                            this,                                                           &SeerGdbWidget::handleGdbAddSkip);
    QObject::connect(sourceLibraryManagerWidget->skipBrowserWidget(),           &SeerSkipBrowserWidget::deleteSkips,                                                        this,                                                           &SeerGdbWidget::handleGdbDeleteSkips);
    QObject::connect(sourceLibraryManagerWidget->skipBrowserWidget(),           &SeerSkipBrowserWidget::enableSkips,                                                        this,                                                           &SeerGdbWidget::handleGdbEnableSkips);
    QObject::connect(sourceLibraryManagerWidget->skipBrowserWidget(),           &SeerSkipBrowserWidget::disableSkips,                                                       this,                                                           &SeerGdbWidget::handleGdbDisableSkips);

    QObject::connect(stackManagerWidget->stackFramesBrowserWidget(),            &SeerStackFramesBrowserWidget::refreshStackFrames,                                          this,                                                           &SeerGdbWidget::handleGdbStackListFrames);
    QObject::connect(stackManagerWidget->stackFramesBrowserWidget(),            &SeerStackFramesBrowserWidget::selectedFrame,                                               this,                                                           &SeerGdbWidget::handleGdbStackSelectFrame);
    QObject::connect(stackManagerWidget->stackFramesBrowserWidget(),            &SeerStackFramesBrowserWidget::selectedFile,                                                editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenFile);
    QObject::connect(stackManagerWidget->stackFramesBrowserWidget(),            &SeerStackFramesBrowserWidget::selectedAddress,                                             editorManagerWidget,                                            &SeerEditorManagerWidget::handleOpenAddress);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::refreshStackArguments,                                    this,                                                           &SeerGdbWidget::handleGdbStackListArguments);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::addVariableLoggerExpression,                              variableManagerWidget->variableLoggerBrowserWidget(),           &SeerVariableLoggerBrowserWidget::addVariableExpression);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::addVariableTrackerExpression,                             this,                                                           &SeerGdbWidget::handleGdbDataAddExpression);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::addMemoryVisualize,                                       this,                                                           &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::addArrayVisualize,                                        this,                                                           &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::addStructVisualize,                                       this,                                                           &SeerGdbWidget::handleGdbVarAddExpression);
    QObject::connect(stackManagerWidget->stackArgumentsBrowserWidget(),         &SeerStackArgumentsBrowserWidget::refreshVariableTrackerValues,                             this,                                                           &SeerGdbWidget::handleGdbDataListExpressions);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::refreshStackLocals,                                          this,                                                           &SeerGdbWidget::handleGdbStackListLocals);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::addVariableLoggerExpression,                                 variableManagerWidget->variableLoggerBrowserWidget(),           &SeerVariableLoggerBrowserWidget::addVariableExpression);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::addVariableTrackerExpression,                                this,                                                           &SeerGdbWidget::handleGdbDataAddExpression);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::addMemoryVisualize,                                          this,                                                           &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::addArrayVisualize,                                           this,                                                           &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::addStructVisualize,                                          this,                                                           &SeerGdbWidget::handleGdbVarAddExpression);
    QObject::connect(stackManagerWidget->stackLocalsBrowserWidget(),            &SeerStackLocalsBrowserWidget::refreshVariableTrackerValues,                                this,                                                           &SeerGdbWidget::handleGdbDataListExpressions);
    QObject::connect(stackManagerWidget->stackDumpBrowserWidget(),              &SeerStackDumpBrowserWidget::refreshStackPointer,                                           this,                                                           &SeerGdbWidget::handleGdbDataEvaluateExpression);
    QObject::connect(stackManagerWidget->stackDumpBrowserWidget(),              &SeerStackDumpBrowserWidget::refreshStackDump,                                              this,                                                           QOverload<int,QString,int,int>::of(&SeerGdbWidget::handleGdbMemoryEvaluateExpression));
    QObject::connect(stackManagerWidget->stackDumpBrowserWidget(),              &SeerStackDumpBrowserWidget::addMemoryVisualize,                                            this,                                                           &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(stackManagerWidget,                                        &SeerStackManagerWidget::refreshThreadFrames,                                               this,                                                           &SeerGdbWidget::handleGdbThreadListFrames);

    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::refreshVariableTrackerNames,                             this,                                                           &SeerGdbWidget::handleGdbDataListExpressions);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::refreshVariableTrackerValues,                            this,                                                           &SeerGdbWidget::handleGdbDataListValues);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::addVariableExpression,                                   this,                                                           &SeerGdbWidget::handleGdbDataAddExpression);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::deleteVariableExpressions,                               this,                                                           &SeerGdbWidget::handleGdbDataDeleteExpressions);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::addMemoryVisualize,                                      this,                                                           &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::addArrayVisualize,                                       this,                                                           &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(variableManagerWidget->variableTrackerBrowserWidget(),     &SeerVariableTrackerBrowserWidget::addStructVisualize,                                      this,                                                           &SeerGdbWidget::handleGdbVarAddExpression);
    QObject::connect(variableManagerWidget->variableLoggerBrowserWidget(),      &SeerVariableLoggerBrowserWidget::evaluateVariableExpression,                               this,                                                           &SeerGdbWidget::handleGdbDataEvaluateExpression);
    QObject::connect(variableManagerWidget->variableLoggerBrowserWidget(),      &SeerVariableLoggerBrowserWidget::addMemoryVisualize,                                       this,                                                           &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(variableManagerWidget->variableLoggerBrowserWidget(),      &SeerVariableLoggerBrowserWidget::addArrayVisualize,                                        this,                                                           &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(variableManagerWidget->variableLoggerBrowserWidget(),      &SeerVariableLoggerBrowserWidget::addStructVisualize,                                       this,                                                           &SeerGdbWidget::handleGdbVarAddExpression);
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

    _gdbRecordMode = mode;

    if (mode != "rr" && mode != "") {
        handleGdbCommand("record " + gdbRecordMode());
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

    if (text.startsWith("*running,thread-id=\"all\"")) {

    // Probably a better way to handle all these types of stops.
    }else if (text.startsWith("*stopped")) {

        emit stoppingPointReached();

    }else if (text.startsWith("=breakpoint-created,")) {

        handleGdbGenericpointList();

    }else if (text.startsWith("=thread-group-started,")) {
        // =thread-group-started,id="i1",pid="30916"

        QString pid_text = Seer::parseFirst(text, "pid=", '"', '"', false);

        //qDebug() << "Inferior pid = " << pid_text;

        setExecutablePid(pid_text.toLong());

    }else if (text.startsWith("=thread-group-exited,")) {

        handleGdbTerminateExecutable(false);

    }else{
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

        // Disconnect from the console and delete the old gdb if there is a new executable.
        if (newExecutableFlag() == true) {
            console()->deleteTerminal();
            killGdb();
        }

        // If gdb isn't running, start it.
        if (isGdbRuning() == false) {

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
        console()->createTerminal();
        console()->connectTerminal();
        handleGdbTerminalDeviceName();

        setExecutableLaunchMode("run");
        saveLaunchMode();
        setGdbRecordMode("");
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

        // Disconnect from the console and delete the old gdb if there is a new executable.
        if (newExecutableFlag() == true) {
            console()->deleteTerminal();
            killGdb();
        }

        // If gdb isn't running, start it.
        // No need to connect to the console in this mode.
        if (isGdbRuning() == false) {

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

        // No console for 'attach' mode but make sure it's reattached.
        setExecutableLaunchMode("attach");
        saveLaunchMode();
        setGdbRecordMode("");
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
        // No need to connect to the console in this mode.
        if (isGdbRuning() == false) {

            bool f = startGdb();
            if (f == false) {
                QMessageBox::critical(this, tr("Error"), tr("Can't start gdb."));
                break;
            }

            handleGdbLoadMICommands();
            handleGdbSourceScripts();
        }

        // No console for 'connect' mode but make sure it's reattached.
        setExecutableLaunchMode("connect");
        saveLaunchMode();
        setGdbRecordMode("");
        setExecutablePid(0);
        reattachConsole();

        // Load any 'pre' commands.
        if (newExecutableFlag() == true) {
            if (gdbServerDebug()) {
                handleGdbCommand("-gdb-set debug remote 1"); // Turn on gdbserver debug
            }else{
                handleGdbCommand("-gdb-set debug remote 0");
            }
            handleGdbExecutablePreCommands();       // Run any 'pre' commands before program is loaded.
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
            console()->deleteTerminal();
            killGdb();
        }

        // If gdb isn't running, start it.
        if (isGdbRuning() == false) {

            bool f = startGdbRR();
            if (f == false) {
                QMessageBox::critical(this, tr("Error"), tr("Can't start gdb."));
                break;
            }
        }

        // Set the program's tty device for stdin and stdout.
        console()->createTerminal();
        console()->connectTerminal();
        handleGdbTerminalDeviceName();

        // Set the launch mode.
        setExecutableLaunchMode("rr");
        saveLaunchMode();
        setGdbRecordMode("rr");
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

        // Restart the executable if it was already running.
        if (newExecutableFlag() == false) {
            if (_executableBreakMode == "inmain") {
                handleGdbCommand("-exec-run --all --start"); // Stop in main
            }else{
                handleGdbCommand("-exec-run --all"); // Do not stop in main. But honor other breakpoints that may have been previously set.
            }
        }

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
            console()->deleteTerminal();
            killGdb();
        }

        // If gdb isn't running, start it.
        // No need to connect to the console in this mode.
        if (isGdbRuning() == false) {

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

        // No console for 'core' mode but make sure it's reattached.
        setExecutableLaunchMode("corefile");
        saveLaunchMode();
        setGdbRecordMode("");
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

        // This is needed for code mode to refresh the stack frame, for some reason.
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
            delay(1);

            // Give the gdb and 'exit' command.
            // This should handle detaching from an attached pid.
            handleGdbCommand("-exec-kill");

            // Kill the gdb.
            killGdb();

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

    handleGdbCommand(QString("-exec-continue %1 --all").arg(gdbRecordDirection()));
}

void SeerGdbWidget::handleGdbRecordStart () {

    if (executableLaunchMode() == "") {
        return;
    }

    if (executableLaunchMode() == "rr") {
        QMessageBox::warning(this, "Seer", QString("Record 'Start' not available in RR mode."), QMessageBox::Ok);
        return;
    }

    setGdbRecordMode("full");
    setGdbRecordDirection("");
}

void SeerGdbWidget::handleGdbRecordStop () {

    if (executableLaunchMode() == "") {
        return;
    }

    if (executableLaunchMode() == "rr") {
        QMessageBox::warning(this, "Seer", QString("Record 'Stop' not available in RR mode."), QMessageBox::Ok);
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

    delay(1);
    QFile::remove(QString("/tmp/breakpoints.seer.%1").arg(QCoreApplication::applicationPid()));
}

void SeerGdbWidget::handleGdbSessionSaveBreakpoints () {

    handleGdbCommand(QString("source -v /tmp/breakpoints.seer.%1").arg(QCoreApplication::applicationPid()));
}

void SeerGdbWidget::handleGdbTerminalDeviceName () {

    if (_consoleWidget->terminalDeviceName() != "") {

        handleGdbCommand(QString("-inferior-tty-set  ") + _consoleWidget->terminalDeviceName());

    }else{
        qWarning() << "Can't set TTY name because the name is blank.";
    }
}

void SeerGdbWidget::handleGdbStackListFrames () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-stack-list-frames");
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

    handleGdbCommand("-stack-list-variables --all-values");
}

void SeerGdbWidget::handleGdbStackListArguments () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-stack-list-arguments --all-values");
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

    handleGdbCommand("-break-delete " + breakpoints);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbBreakpointEnable (QString breakpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-enable " + breakpoints);
    handleGdbGenericpointList();
}

void SeerGdbWidget::handleGdbBreakpointDisable (QString breakpoints) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-break-disable " + breakpoints);
    handleGdbGenericpointList();
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

    handleGdbCommand("-break-insert " + breakpoint);
    handleGdbGenericpointList();
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

void SeerGdbWidget::handleGdbListSkips () {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-skip-list");
}

void SeerGdbWidget::handleGdbAddSkip (QString skipmode, QString skipparameters) {

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

    handleGdbListSkips();
}

void SeerGdbWidget::handleGdbDeleteSkips (QString skipids) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-skip-delete " + skipids);

    handleGdbListSkips();
}

void SeerGdbWidget::handleGdbEnableSkips (QString skipids) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-skip-enable " + skipids);

    handleGdbListSkips();
}

void SeerGdbWidget::handleGdbDisableSkips (QString skipids) {

    if (executableLaunchMode() == "") {
        return;
    }

    handleGdbCommand("-skip-disable " + skipids);

    handleGdbListSkips();
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

    handleGdbCommand(QString::number(expressionid) + "-data-evaluate-expression \"" + expression + "\"");
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
        handleGdbCommand(QString::number(_dataExpressionId[i]) + "-data-evaluate-expression \"" + _dataExpressionName[i] + "\"");
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
    QObject::connect(w,            &SeerStructVisualizerWidget::addMemoryVisualize,          this, &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(w,            &SeerStructVisualizerWidget::addArrayVisualize,           this, &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(w,            &SeerStructVisualizerWidget::addStructVisualize,          this, &SeerGdbWidget::handleGdbStructAddExpression);

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
    QObject::connect(w,            &SeerVarVisualizerWidget::addMemoryVisualize,             this, &SeerGdbWidget::handleGdbMemoryAddExpression);
    QObject::connect(w,            &SeerVarVisualizerWidget::addArrayVisualize,              this, &SeerGdbWidget::handleGdbArrayAddExpression);
    QObject::connect(w,            &SeerVarVisualizerWidget::addVarVisualize,                this, &SeerGdbWidget::handleGdbVarAddExpression);

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
            qDebug() << "Resource file" << miInfo << "does not exist!";
            continue;
        }

        // Destination file path in /tmp.
        QString destinationPath = "/tmp/" + miInfo.fileName();

        // Copy to temp. Don't check return status. I don't think it works
        // if the source is in Resources.
        miFile.copy(destinationPath);

        // Source it.
        if (QFile::exists(destinationPath) == false) {
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
        handleGdbCommand("-exec-interrupt --all");

    }else{
        int stat = kill(executablePid(), signal);
        if (stat < 0) {
            QMessageBox::warning(this, "Seer",
                                       QString("Unable to send signal '%1' to pid %2.\nError = '%3'").arg(strsignal(signal)).arg(executablePid()).arg(strerror(errno)),
                                       QMessageBox::Ok);
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

