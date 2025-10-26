// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerMainWindow.h"
#include "SeerDebugDialog.h"
#include "SeerConfigDialog.h"
#include "SeerArgumentsDialog.h"
#include "SeerAboutDialog.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include "SeerOpenOCDWidget.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QMenu>
#include <QtWidgets/QStyleFactory>
#include <QtWidgets/QToolButton>
#include <QtWidgets/QToolTip>
#include <QtGui/QKeySequence>
#include <QtGui/QPalette>
#include <QtCore/QCoreApplication>
#include <QtCore/QTimer>
#include <QRegularExpression>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerMainWindow::SeerMainWindow(QWidget* parent) : QMainWindow(parent) {

    //
    // Set up UI.
    //
    setupUi(this);

    //
    // Set up other parts of the UI.
    //

    // Add status bar indicator.
    _runStatus = new SeerRunStatusIndicatorBox(this);
    _runStatus->setVisible(false);
    statusBar()->addPermanentWidget(_runStatus->indicatorBox());

    // Add progress spin widget.
    QWidget* spacerWidget = new QWidget(this);
    spacerWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    spacerWidget->setStyleSheet("background-color:transparent"); // Need this for QToolBar StyleSheets to work.
    toolBar->addWidget(spacerWidget);

    _progressIndicator = new SeerProgressIndicator(this);
    _progressIndicator->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Preferred);
    _progressIndicator->setFixedWidth(96);
    _progressIndicator->setColor(palette().color(QPalette::WindowText));

    toolBar->addWidget(_progressIndicator);

    // Add help button.
    QToolButton* helpToolButton = new QToolButton(this);
    helpToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpToolButton->setToolTip("Help on Seer main window.");

    toolBar->addWidget(helpToolButton);

    // Set up Styles menu.
    _styleMenuActionGroup = new QActionGroup(this);
    _styleMenuActionGroup->setExclusionPolicy(QActionGroup::ExclusionPolicy::Exclusive);
    _styleMenuActionGroup->setEnabled(true);
    _styleMenuActionGroup->setVisible(true);

    QAction* lightStyleAction = menuStyles->addAction("light");
    lightStyleAction->setCheckable(true);
    _styleMenuActionGroup->addAction(lightStyleAction);

    QAction* darkStyleAction = menuStyles->addAction("dark");
    darkStyleAction->setCheckable(true);
    _styleMenuActionGroup->addAction(darkStyleAction);

    QStringList styles = QStyleFactory::keys();

    for (int i = 0; i < styles.size(); i++) {

        QAction* styleAction = menuStyles->addAction(styles.at(i));
        styleAction->setCheckable(true);

        _styleMenuActionGroup->addAction(styleAction);
    }

    // Hide Nexti and Stepi. Enabled/disabled by SeerEditorManagerWidget.
    actionGdbNext->setVisible(true);
    actionGdbNexti->setVisible(false);
    actionGdbStep->setVisible(true);
    actionGdbStepi->setVisible(false);
    actionGdbFinish->setVisible(true);

    // Set up Interrupt menu.
    QMenu* menuInterrupt = new QMenu(this);
    _interruptAction = menuInterrupt->addAction("Interrupt");
    menuInterrupt->addSeparator();
    QAction* interruptActionSIGINT  = menuInterrupt->addAction("SIGINT");
    QAction* interruptActionSIGKILL = menuInterrupt->addAction("SIGKILL");
    QAction* interruptActionSIGFPE  = menuInterrupt->addAction("SIGFPE");
    QAction* interruptActionSIGSEGV = menuInterrupt->addAction("SIGSEGV");
    QAction* interruptActionSIGUSR1 = menuInterrupt->addAction("SIGUSR1");
    QAction* interruptActionSIGUSR2 = menuInterrupt->addAction("SIGUSR2");

    actionInterruptProcess->setMenu(menuInterrupt);

    // Set up Visualizer menu.
    QMenu* menuVisualizer = new QMenu(this);
    QAction* visualizerMemoryAction = menuVisualizer->addAction("Memory");
    menuVisualizer->addSeparator();
    QAction* visualizerArrayAction  = menuVisualizer->addAction("Array");
    QAction* visualizerMatrixAction = menuVisualizer->addAction("Matrix");
    QAction* visualizerVarAction    = menuVisualizer->addAction("Struct");
    QAction* visualizerStructAction = menuVisualizer->addAction("Basic Struct");
    QAction* visualizerImageAction  = menuVisualizer->addAction("Image");

    actionVisualizers->setMenu(menuVisualizer);

    // Make openocd menu invisible
    menuOpenOCD->menuAction()->setVisible(false);

    // Set up control menu for recording.
    QActionGroup* recordDirectionActionGroup = new QActionGroup(this);
    recordDirectionActionGroup->addAction(actionControlRecordForward);
    recordDirectionActionGroup->addAction(actionControlRecordReverse);

    // Set the inital key settings.
    setKeySettings(SeerKeySettings::populate());
    setProjectFilename("");

    //
    // Set up signals/slots.
    //
    QObject::connect(actionFileDebug,                   &QAction::triggered,                            this,           &SeerMainWindow::handleFileDebug);
    QObject::connect(actionFileArguments,               &QAction::triggered,                            this,           &SeerMainWindow::handleFileArguments);
    QObject::connect(actionFileQuit,                    &QAction::triggered,                            this,           &SeerMainWindow::handleFileQuit);
    QObject::connect(actionViewMemoryVisualizer,        &QAction::triggered,                            this,           &SeerMainWindow::handleViewMemoryVisualizer);
    QObject::connect(actionViewArrayVisualizer,         &QAction::triggered,                            this,           &SeerMainWindow::handleViewArrayVisualizer);
    QObject::connect(actionViewMatrixVisualizer,        &QAction::triggered,                            this,           &SeerMainWindow::handleViewMatrixVisualizer);
    QObject::connect(actionViewStructVisualizer,        &QAction::triggered,                            this,           &SeerMainWindow::handleViewVarVisualizer);
    QObject::connect(actionViewBasicStructVisualizer,   &QAction::triggered,                            this,           &SeerMainWindow::handleViewStructVisualizer);
    QObject::connect(actionViewImageVisualizer,         &QAction::triggered,                            this,           &SeerMainWindow::handleViewImageVisualizer);
    QObject::connect(actionViewAssembly,                &QAction::triggered,                            this,           &SeerMainWindow::handleViewAssembly);
    QObject::connect(actionConsoleAttached,             &QAction::triggered,                            this,           &SeerMainWindow::handleViewConsoleAttached);
    QObject::connect(actionConsoleDetached,             &QAction::triggered,                            this,           &SeerMainWindow::handleViewConsoleDetached);
    QObject::connect(actionConsoleDetachedMinimized,    &QAction::triggered,                            this,           &SeerMainWindow::handleViewConsoleDetachedMinimized);
    QObject::connect(actionHelpAbout,                   &QAction::triggered,                            this,           &SeerMainWindow::handleHelpAbout);

    QObject::connect(actionControlRestart,              &QAction::triggered,                            this,           &SeerMainWindow::handleRestartExecutable);
    QObject::connect(actionControlTerminate,            &QAction::triggered,                            this,           &SeerMainWindow::handleTerminateExecutable);
    QObject::connect(actionControlContinue,             &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbContinue);
    QObject::connect(actionControlNext,                 &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbNext);
    QObject::connect(actionControlStep,                 &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbStep);
    QObject::connect(actionControlNexti,                &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbNexti);
    QObject::connect(actionControlStepi,                &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbStepi);
    QObject::connect(actionControlFinish,               &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbFinish);
    QObject::connect(actionControlRecordStart,          &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbRecordStart);
    QObject::connect(actionControlRecordForward,        &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbRecordForward);
    QObject::connect(actionControlRecordReverse,        &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbRecordReverse);
    QObject::connect(actionControlRecordStop,           &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbRecordStop);
    QObject::connect(actionControlInterrupt,            &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbInterrupt);

    QObject::connect(actionSettingsConfiguration,       &QAction::triggered,                            this,           &SeerMainWindow::handleSettingsConfiguration);
    QObject::connect(actionSettingsSaveConfiguration,   &QAction::triggered,                            this,           &SeerMainWindow::handleSettingsSaveConfiguration);

    QObject::connect(actionGdbLaunch,                   &QAction::triggered,                            this,           &SeerMainWindow::handleFileDebug);
    QObject::connect(actionGdbTerminate,                &QAction::triggered,                            this,           &SeerMainWindow::handleTerminateExecutable);
    QObject::connect(actionGdbRestart,                  &QAction::triggered,                            this,           &SeerMainWindow::handleRestartExecutable);
    QObject::connect(_styleMenuActionGroup,             &QActionGroup::triggered,                       this,           &SeerMainWindow::handleStyleMenuChanged);
    QObject::connect(actionGdbContinue,                 &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbContinue);
    QObject::connect(actionGdbNext,                     &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbNext);
    QObject::connect(actionGdbStep,                     &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbStep);
    QObject::connect(actionGdbNexti,                    &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbNexti);
    QObject::connect(actionGdbStepi,                    &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbStepi);
    QObject::connect(actionGdbFinish,                   &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbFinish);
    QObject::connect(actionRecordProcess,               &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbRecordStartStopToggle);
    QObject::connect(actionRecordDirection,             &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbRecordDirectionToggle);

    QObject::connect(actionInterruptProcess,            &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbInterrupt);
    QObject::connect(_interruptAction,                  &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbInterrupt);
    QObject::connect(interruptActionSIGINT,             &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbInterruptSIGINT);
    QObject::connect(interruptActionSIGKILL,            &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbInterruptSIGKILL);
    QObject::connect(interruptActionSIGFPE,             &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbInterruptSIGFPE);
    QObject::connect(interruptActionSIGSEGV,            &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbInterruptSIGSEGV);
    QObject::connect(interruptActionSIGUSR1,            &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbInterruptSIGUSR1);
    QObject::connect(interruptActionSIGUSR2,            &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbInterruptSIGUSR2);

    QObject::connect(actionVisualizers,                 &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbMemoryVisualizer);
    QObject::connect(visualizerMemoryAction,            &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbMemoryVisualizer);
    QObject::connect(visualizerArrayAction,             &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbArrayVisualizer);
    QObject::connect(visualizerMatrixAction,            &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbMatrixVisualizer);
    QObject::connect(visualizerVarAction,               &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbVarVisualizer);
    QObject::connect(visualizerStructAction,            &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbStructVisualizer);
    QObject::connect(visualizerImageAction,             &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleGdbImageVisualizer);

    QObject::connect(gdbWidget->gdbMonitor(),           &GdbMonitor::astrixTextOutput,                  _runStatus,     &SeerRunStatusIndicatorBox::handleText);
    QObject::connect(gdbWidget->gdbMonitor(),           &GdbMonitor::equalTextOutput,                   _runStatus,     &SeerRunStatusIndicatorBox::handleText);
    QObject::connect(gdbWidget->gdbMonitor(),           &GdbMonitor::caretTextOutput,                   _runStatus,     &SeerRunStatusIndicatorBox::handleText);
    QObject::connect(gdbWidget->gdbMonitor(),           &GdbMonitor::astrixTextOutput,                  this,           &SeerMainWindow::handleText);
    QObject::connect(gdbWidget->gdbMonitor(),           &GdbMonitor::caretTextOutput,                   this,           &SeerMainWindow::handleText);
    QObject::connect(gdbWidget->gdbMonitor(),           &GdbMonitor::equalTextOutput,                   this,           &SeerMainWindow::handleText);
    QObject::connect(gdbWidget->editorManager(),        &SeerEditorManagerWidget::showMessage,          this,           &SeerMainWindow::handleShowMessage);
    QObject::connect(gdbWidget->editorManager(),        &SeerEditorManagerWidget::assemblyTabShown,     this,           &SeerMainWindow::handleViewAssemblyShown);

    QObject::connect(gdbWidget,                         &SeerGdbWidget::recordSettingsChanged,          this,           &SeerMainWindow::handleRecordSettingsChanged);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::changeWindowTitle,              this,           &SeerMainWindow::handleChangeWindowTitle);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::stateChanged,                   this,           &SeerMainWindow::handleGdbStateChanged);

    QObject::connect(_runStatus,                        &SeerRunStatusIndicatorBox::statusChanged,      this,           &SeerMainWindow::handleRunStatusChanged);
    QObject::connect(qApp,                              &QApplication::aboutToQuit,                     gdbWidget,      &SeerGdbWidget::handleGdbShutdown);

    QObject::connect(helpToolButton,                    &QToolButton::clicked,                          this,           &SeerMainWindow::handleHelpToolButtonClicked);
    // This handle button state when target state changes
    QObject::connect(gdbWidget->gdbMonitor(),           &GdbMonitor::astrixTextOutput,                  this,           &SeerMainWindow::handleStatusChanged);
    QObject::connect(gdbWidget->gdbMonitor(),           &GdbMonitor::caretTextOutput,                   this,           &SeerMainWindow::handleStatusChanged);
    // Openocd signal and slots
    // This handle when actionOpenOCDDebugModule is clicked
    QObject::connect(actionOpenOCDDebugModule,          &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleDebugKernelModule);
    QObject::connect(actionOpenOCDHelp,                 &QAction::triggered,                            gdbWidget,      &SeerGdbWidget::handleOpenOCDMainHelpButtonClicked);

    // Handle Debug on Init, avoid thread disruption
    QObject::connect(gdbWidget,                         &SeerGdbWidget::requestContinue,                gdbWidget,      &SeerGdbWidget::handleGdbContinue);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::requestBreakList,               gdbWidget,      &SeerGdbWidget::handleGdbGenericpointList);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::requestBreakInsert,             gdbWidget,      &SeerGdbWidget::handleGdbBreakpointInsert);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::requestBreakEnable,             gdbWidget,      &SeerGdbWidget::handleGdbBreakpointEnable);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::requestBreakDisable,            gdbWidget,      &SeerGdbWidget::handleGdbBreakpointDisable);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::requestGdbCommand,              gdbWidget,      &SeerGdbWidget::handleGdbCommand);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::requestSendToSerial,            gdbWidget,      &SeerGdbWidget::handleSendToSerial);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::requestSeekVariableIdentifier,  gdbWidget,      &SeerGdbWidget::handleSyncSeekVariableIdentifier);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::requestSeekFunctionIdentifier,  gdbWidget,      &SeerGdbWidget::handleSyncSeekFunctionIdentifier);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::requestSeekTypeIdentifier,      gdbWidget,      &SeerGdbWidget::handleSyncSeekTypeIdentifier);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::requestLsmod,                   gdbWidget,      &SeerGdbWidget::handleGdbLsmod);
    QObject::connect(gdbWidget,                         &SeerGdbWidget::requestWarning,                 gdbWidget,      &SeerGdbWidget::handleSyncWarning);

    handleRecordSettingsChanged();

    //
    // Initialize contents.
    //

    // Restore window settings.
    readSettings();

    // Restore configuration settings.
    readConfigSettings();

    // Show the main window.
    show();

    handleShowMessage("Welcome to Seer. The All Knowing...", 3000);
}

SeerMainWindow::~SeerMainWindow() {
    handleTerminateExecutable();                    // QuangNM13: see if this could fix segmantation fault on exit.
}

void SeerMainWindow::setExecutableName (const QString& executableName) {
    gdbWidget->setExecutableName(executableName);
}

const QString& SeerMainWindow::executableName () const {
    return gdbWidget->executableName();
}

void SeerMainWindow::setExecutableSymbolName (const QString& executableSymbolName) {
    gdbWidget->setExecutableSymbolName(executableSymbolName);
}

const QString& SeerMainWindow::executableSymbolName () const {
    return gdbWidget->executableSymbolName();
}

void SeerMainWindow::setExecutableArguments (const QString& executableArguments) {

    gdbWidget->setExecutableArguments(executableArguments);
}

void SeerMainWindow::setExecutableArguments (const QStringList& executableArguments) {

    //
    // Convert the list of arguments into a single argument string.
    // Be careful of arguments that contain a space. These need to be surrounded by
    // a "'" character to retain the argument grouping.
    //
    //  ie: myprog  42.0 "This is a multi-worded argument"
    //
    // Has 2 arguments. Not 6.
    //
    QString arguments;

    for (auto arg : executableArguments) {

        if (arg.contains(' ')) {
            arg = "'" + arg + "'";
        }

        if (arguments == "") {
            arguments = arg;
        }else{
            arguments += " " + arg;
        }
    }

    setExecutableArguments(arguments);
}

const QString& SeerMainWindow::executableArguments () const {
    return gdbWidget->executableArguments();
}

void SeerMainWindow::setExecutableWorkingDirectory (const QString& executableWorkingDirectory) {
    gdbWidget->setExecutableWorkingDirectory(executableWorkingDirectory);
}

const QString& SeerMainWindow::executableWorkingDirectory () const {
    return gdbWidget->executableWorkingDirectory();
}

void SeerMainWindow::setExecutableBreakpointsFilename (const QString& breakpointsFilename) {
    gdbWidget->setExecutableBreakpointsFilename(breakpointsFilename);
}

const QString& SeerMainWindow::executableBreakpointsFilename () const {
    return gdbWidget->executableBreakpointsFilename();
}

void  SeerMainWindow::setExecutableBreakpointFunctionName (const QString& nameoraddress) {
    gdbWidget->setExecutableBreakpointFunctionName(nameoraddress);
}

const QString& SeerMainWindow::executableBreakpointFunctionName () const {
    return gdbWidget->executableBreakpointFunctionName();
}

void  SeerMainWindow::setExecutableBreakpointSourceName (const QString& sourceFilenameAndLineno) {
    gdbWidget->setExecutableBreakpointSourceName(sourceFilenameAndLineno);
}

const QString& SeerMainWindow::executableBreakpointSourceName () const {
    return gdbWidget->executableBreakpointSourceName();
}

void SeerMainWindow::setExecutableShowAssemblyTab (bool flag) {
    gdbWidget->setAssemblyShowAssemblyTabOnStartup(flag);
}

bool SeerMainWindow::executableShowAssemblyTab () const {
    return gdbWidget->assemblyShowAssemblyTabOnStartup();
}

void SeerMainWindow::setExecutableRandomizeStartAddress (bool flag) {
    gdbWidget->setGdbRandomizeStartAddress(flag);
}

bool SeerMainWindow::executableRandomizeStartAddress () const {
    return gdbWidget->gdbRandomizeStartAddress();
}

void SeerMainWindow::setExecutableNonStopMode (bool flag) {
    gdbWidget->setGdbNonStopMode(flag);
}

bool SeerMainWindow::executableNonStopMode () const {
    return gdbWidget->gdbNonStopMode();
}

void SeerMainWindow::setExecutablePid (int pid) {
    gdbWidget->setExecutablePid(pid);
}

int SeerMainWindow::executablePid () const {
    return gdbWidget->executablePid();
}

void SeerMainWindow::setExecutableConnectHostPort (const QString& executableConnectHostPort) {
    gdbWidget->setExecutableConnectHostPort(executableConnectHostPort);
}

const QString& SeerMainWindow::executableConnectHostPort () const {
    return gdbWidget->executableConnectHostPort();
}

void SeerMainWindow::setExecutableConnectRemoteTargetType (const QString& type) {
    gdbWidget->setGdbRemoteTargetType(type);
}

QString SeerMainWindow::executableConnectRemoteTargetType () const {
    return gdbWidget->gdbRemoteTargetType();
}

void SeerMainWindow::setExecutableConnectGdbserverDebug (bool enable) {
    gdbWidget->setGdbServerDebug(enable);
}

bool SeerMainWindow::executableConnectGdbserverDebug () const {
    return gdbWidget->gdbServerDebug();
}

void SeerMainWindow::setExecutableRRTraceDirectory (const QString& executableRRTraceDirectory) {
    gdbWidget->setExecutableRRTraceDirectory(executableRRTraceDirectory);
}

const QString& SeerMainWindow::executableRRTraceDirectory () const {
    return gdbWidget->executableRRTraceDirectory();
}

void SeerMainWindow::setExecutableCoreFilename (const QString& executableCoreFilename) {
    gdbWidget->setExecutableCoreFilename(executableCoreFilename);
}

const QString& SeerMainWindow::executableCoreFilename () const {
    return gdbWidget->executableCoreFilename();
}

void SeerMainWindow::setExecutablePreGdbCommands (const QStringList& preGdbCommands) {
    gdbWidget->setExecutablePreGdbCommands(preGdbCommands);
}

const QStringList& SeerMainWindow::executablePreGdbCommands() const {
    return gdbWidget->executablePreGdbCommands();
}

void SeerMainWindow::setExecutablePostGdbCommands (const QStringList& postGdbCommands) {
    gdbWidget->setExecutablePostGdbCommands(postGdbCommands);
}

const QStringList& SeerMainWindow::executablePostGdbCommands() const {
    return gdbWidget->executablePostGdbCommands();
}

void SeerMainWindow::setProjectFilename (const QString& projectFilename) {
    _projectFile = projectFilename;
}

const QString& SeerMainWindow::projectFilename () const {
    return _projectFile;
}

void SeerMainWindow::setGdbProgramOverride (const QString& gdbProgram) {
    gdbWidget->setGdbProgramOverride(gdbProgram);
}

QString SeerMainWindow::gdbProgramOverride () const {
    return gdbWidget->gdbProgramOverride();
}

void SeerMainWindow::setGdbArgumentsOverride (const QString& gdbArguments) {
    gdbWidget->setGdbArgumentsOverride(gdbArguments);
}

QString SeerMainWindow::gdbArgumentsOverride () const {
    return gdbWidget->gdbArgumentsOverride();
}

void SeerMainWindow::launchExecutable (const QString& launchMode, const QString& breakMode) {

    // Show all buttons by default. Turn some off depending on debug mode.
    actionGdbContinue->setVisible(true);
    actionGdbNext->setVisible(true);
    actionGdbNexti->setVisible(true);
    actionGdbStep->setVisible(true);
    actionGdbStepi->setVisible(true);
    actionGdbFinish->setVisible(true);
    actionInterruptProcess->setVisible(true);
    actionRecordProcess->setVisible(true);
    actionRecordDirection->setVisible(true);

    actionGdbRestart->setVisible(false);
    actionGdbTerminate->setVisible(false);
    actionGdbLaunch->setVisible(false);

    actionControlRestart->setVisible(false);
    actionControlTerminate->setVisible(false);
    actionControlInterrupt->setVisible(true);

    // openocd
    actionOpenOCDAttach->setVisible(false);
    menuOpenOCD->menuAction()->setVisible(false);
    deleteExceptionLevelBar();

    // if OpenOCD tab exists, kill it along with any running OpenOCD process.
    if (gdbWidget->openOCDWidgetInstance()->openocdProcess())
    {
        if (gdbWidget->openOCDWidgetInstance()->isOpenocdRunning() == true) {
            gdbWidget->openOCDWidgetInstance()->killOpenOCD();
            gdbWidget->openOCDWidgetInstance()->killConsole();
            gdbWidget->setGdbProgram("/usr/bin/gdb");
        }
    }

    // As new session is started, kill any existing gdb process
    gdbWidget->handleGdbShutdown();

    if (launchMode == "run") {

        gdbWidget->handleGdbRunExecutable(breakMode, false);

    }else if (launchMode == "start") {

        gdbWidget->handleGdbRunExecutable(breakMode, false);

    }else if (launchMode == "attach") {

        gdbWidget->handleGdbAttachExecutable(false);

    }else if (launchMode == "connect") {

        gdbWidget->handleGdbConnectExecutable(false);

    }else if (launchMode == "rr") {

        gdbWidget->handleGdbRRExecutable(false);

    }else if (launchMode == "corefile") {

        actionGdbContinue->setVisible(false);
        actionGdbNext->setVisible(false);
        actionGdbNexti->setVisible(false);
        actionGdbStep->setVisible(false);
        actionGdbStepi->setVisible(false);
        actionGdbFinish->setVisible(false);

        actionInterruptProcess->setVisible(false);
        actionRecordProcess->setVisible(false);
        actionRecordDirection->setVisible(false);

        gdbWidget->handleGdbCoreFileExecutable();

    } else if (launchMode == "openocd") {
        actionRecordProcess->setVisible(false);
        actionRecordDirection->setVisible(false);
        actionGdbNexti->setVisible(false);
        actionGdbStepi->setVisible(false);
        actionControlNexti->setVisible(false);
        actionControlStepi->setVisible(false);
        // display attach button
        menuOpenOCD->menuAction()->setVisible(true);
        actionOpenOCDAttach->setVisible(true);
        // launch gdb-multiarch and openocd
        gdbWidget->handleGdbMultiarchOpenOCDExecutable();
        // add exception level bar
        createExceptionLevelBar();

    }else if (launchMode == "project") {

        actionGdbLaunch->setVisible(true);

        // If no mode, schedule the opening of the debug dialog.
        QTimer::singleShot(200, this, &SeerMainWindow::handleFileDebug);

    }else if (launchMode == "none") {

        actionGdbLaunch->setVisible(true);

        // If no mode, schedule the opening of the debug dialog.
        QTimer::singleShot(200, this, &SeerMainWindow::handleFileDebug);

    }else if (launchMode == "configdialog") {

        actionGdbLaunch->setVisible(true);

        // Launch the config dialog.
        QTimer::singleShot(200, this, &SeerMainWindow::handleSettingsConfiguration);

    }else{
        qDebug() << "UNKNOWN launch mode:" << launchMode;
    }
}

const QString& SeerMainWindow::executableLaunchMode () const {
    return gdbWidget->executableLaunchMode();
}

const QString& SeerMainWindow::executableBreakMode () const {
    return gdbWidget->executableBreakMode();
}

void SeerMainWindow::setStyleName (const QString& name) {

    // Check for Dark/Light style from Seer's resource tree.
    if (name == "dark" || name == "light") {

        QFile s(":qdarkstyle/" + name + "/" + name + "style.qss");
        if (s.exists() == false) {
            qDebug() << "Stylesheet '" + name + "' doesn't exist!";
            return;
        }

        s.open(QFile::ReadOnly | QFile::Text);
        QTextStream ts(&s);
        qApp->setStyleSheet(ts.readAll());

        _styleName = name;

    // Otherwise, a system installed one or Qt internal one.
    }else{

        QApplication::setStyle(name);

        _styleName = name;
    }
}

const QString& SeerMainWindow::styleName () {
    return _styleName;
}

void SeerMainWindow::handleFileDebug () {

    SeerDebugDialog dlg(this);

    dlg.setExecutableName(executableName());
    dlg.setExecutableSymbolName(executableSymbolName());
    dlg.setExecutableWorkingDirectory(executableWorkingDirectory());
    dlg.setExecutableArguments(executableArguments());
    dlg.setLaunchMode(executableLaunchMode());
    dlg.setBreakpointMode(executableBreakMode());
    dlg.setBreakpointsFilename(executableBreakpointsFilename());
    dlg.setBreakpointFunctionName(executableBreakpointFunctionName());
    dlg.setBreakpointSourceName(executableBreakpointSourceName());
    dlg.setShowAssemblyTab(executableShowAssemblyTab());
    dlg.setRandomizeStartAddress(executableRandomizeStartAddress());
    dlg.setNonStopMode(executableNonStopMode());
    dlg.setAttachPid(executablePid());
    dlg.setConnectHostPort(executableConnectHostPort());
    dlg.setConnectRemoteTargetType(executableConnectRemoteTargetType());
    dlg.setConnectGdbserverDebug(executableConnectGdbserverDebug());
    dlg.setRRTraceDirectory(executableRRTraceDirectory());
    dlg.setCoreFilename(executableCoreFilename());
    dlg.setPreGdbCommands(executablePreGdbCommands());
    dlg.setPostGdbCommands(executablePostGdbCommands());
    dlg.setProjectFilename(projectFilename());
    // OpenOcd
    dlg.setOpenOCDExePath(openOCDExePath());
    dlg.setOpenOCDCommand(openOCDCommand());
    dlg.setGdbMultiarchExePath(gdbMultiarchExePath());
    dlg.setGdbPort(gdbPort());
    dlg.setTelnetPort(telnetPort());
    dlg.setGdbMultiarchCommand(gdbMultiarchCommand());
    dlg.setBuiltInDocker(isBuiltInDocker());
    dlg.setAbsoluteBuildFolderPath(absoluteBuildFolderPath());
    dlg.setDockerBuildFolderPath(dockerBuildFolderPath());
    dlg.setGdbMultiarchStopAtTempFunc(isGdbMultiarchIsStopAtTempFunc());
    dlg.setGdbMultiarchStopAtFunc(gdbMultiarchStopAtFunc());
    dlg.setGdbMultiarchStopAtExeption(isGdbMultiarchStopAtException());
    dlg.setGdbMultiarchExeptionLevelToStop(gdbMultiarchExeptionLevelToStop());
    dlg.setOpenOCDTarget(openOCDTarget());
    dlg.setSymbolFiles(symbolFiles());

    setProjectFilename(""); // Clear project name here. No need to have it anymore.

    int ret = dlg.exec();

    if (ret == 0) {
        return;
    }

    QString launchMode = dlg.launchMode();
    QString breakMode  = dlg.breakpointMode();

    if (launchMode == "") {
        return;
    }

    setExecutableName(dlg.executableName());
    setExecutableSymbolName(dlg.executableSymbolName());
    setExecutableWorkingDirectory(dlg.executableWorkingDirectory());
    setExecutableArguments(dlg.executableArguments());
    setExecutableBreakpointsFilename(dlg.breakpointsFilename());
    setExecutableBreakpointFunctionName(dlg.breakpointFunctionName());
    setExecutableBreakpointSourceName(dlg.breakpointSourceName());
    setExecutableShowAssemblyTab(dlg.showAssemblyTab());
    setExecutableRandomizeStartAddress(dlg.randomizeStartAddress());
    setExecutableNonStopMode(dlg.nonStopMode());
    setExecutablePid(dlg.attachPid());
    setExecutableConnectHostPort(dlg.connectHostPort());
    setExecutableConnectRemoteTargetType(dlg.connectRemoteTargetType());
    setExecutableConnectGdbserverDebug(dlg.connectGdbserverDebug());
    setExecutableRRTraceDirectory(dlg.rrTraceDirectory());
    setExecutableCoreFilename(dlg.coreFilename());
    setExecutablePreGdbCommands(dlg.preGdbCommands());
    setExecutablePostGdbCommands(dlg.postGdbCommands());

    // read openocd variables
    setOpenOCDExePath(dlg.openOCDExePath());
    setOpenOCDCommand(dlg.openOCDCommand());
    setGdbMultiarchExePath(dlg.gdbMultiarchExePath());
    setGdbPort(dlg.gdbPort());
    setTelnetPort(dlg.telnetPort());
    setGdbMultiarchCommand(dlg.gdbMultiarchCommand());
    setBuiltInDocker(dlg.isBuiltInDocker());
    setAbsoluteBuildFolderPath(dlg.absoluteBuildFolderPath());
    setDockerBuildFolderPath(dlg.dockerBuildFolderPath());
    setGdbMultiarchStopAtTempFunc(dlg.isGdbMultiarchIsStopAtTempFunc());
    setGdbMultiarchStopAtFunc(dlg.gdbMultiarchStopAtFunc());
    setGdbMultiarchStopAtExeption(dlg.isGdbMultiarchStopAtException());
    setGdbMultiarchExeptionLevelToStop(dlg.gdbMultiarchExeptionLevelToStop());
    setOpenOCDTarget(dlg.openOCDTarget());
    setSymbolFiles(dlg.symbolWidgetManager()->symbolFiles());

    gdbWidget->setGdbMultiarchPid(0);           // clear current gdb-multiarch pid

    launchExecutable(launchMode, breakMode);
}

void SeerMainWindow::handleFileArguments () {

    SeerArgumentsDialog dlg(this);

    dlg.setExecutableArguments(executableArguments());

    int ret = dlg.exec();

    if (ret == 0) {
        return;
    }

    setExecutableArguments(dlg.executableArguments());
}

void SeerMainWindow::handleFileQuit () {

    gdbWidget->handleGdbShutdown();
    QCoreApplication::exit(0);
}

void SeerMainWindow::handleViewMemoryVisualizer () {

    gdbWidget->handleGdbMemoryVisualizer();
}

void SeerMainWindow::handleViewArrayVisualizer () {

    gdbWidget->handleGdbArrayVisualizer();
}

void SeerMainWindow::handleViewMatrixVisualizer () {

    gdbWidget->handleGdbMatrixVisualizer();
}

void SeerMainWindow::handleViewStructVisualizer () {

    gdbWidget->handleGdbStructVisualizer();
}

void SeerMainWindow::handleViewVarVisualizer () {

    gdbWidget->handleGdbVarVisualizer();
}

void SeerMainWindow::handleViewImageVisualizer () {

    gdbWidget->handleGdbImageVisualizer();
}

void SeerMainWindow::handleViewAssembly () {

    gdbWidget->editorManager()->showAssembly();
}

void SeerMainWindow::handleViewAssemblyShown (bool shown) {

    // Corefile always have them off.
    if (executableLaunchMode() == "corefile") {

        actionGdbNext->setVisible(false);
        actionGdbNexti->setVisible(false);
        actionGdbStep->setVisible(false);
        actionGdbStepi->setVisible(false);
        actionGdbFinish->setVisible(false);

    // Toggle regular and instruction buttons.
    }else{
        actionGdbNext->setVisible(!shown);
        actionGdbNexti->setVisible(shown);
        actionGdbStep->setVisible(!shown);
        actionGdbStepi->setVisible(shown);
        actionGdbFinish->setVisible(!shown);
    }
}

void SeerMainWindow::handleViewConsoleAttached () {

    gdbWidget->setConsoleMode("attached");
}

void SeerMainWindow::handleViewConsoleDetached () {

    gdbWidget->setConsoleMode("detached");
}

void SeerMainWindow::handleViewConsoleDetachedMinimized () {

    gdbWidget->setConsoleMode("detachedminimized");
}

void SeerMainWindow::handleSettingsConfiguration () {

    SeerConfigDialog dlg(this);

    dlg.setSeerConsoleMode(gdbWidget->consoleMode());
    dlg.setSeerConsoleScrollLines(gdbWidget->consoleScrollLines());
    dlg.setSeerRememberManualCommandCount(gdbWidget->rememberManualCommandCount());
    dlg.setGdbProgram(gdbWidget->gdbProgram());
    dlg.setGdbArguments(gdbWidget->gdbArguments());
    dlg.setGdbAsyncMode(gdbWidget->gdbAsyncMode());
    dlg.setGdbNonStopMode(gdbWidget->gdbNonStopMode());
    dlg.setGdbHandleTerminatingException(gdbWidget->gdbHandleTerminatingException());
    dlg.setGdbRandomizeStartAddress(gdbWidget->gdbRandomizeStartAddress());
    dlg.setGdbEnablePrettyPrinting(gdbWidget->gdbEnablePrettyPrinting());
    dlg.setGdbRemoteTargetType(gdbWidget->gdbRemoteTargetType());
    dlg.setEditorFont(gdbWidget->editorManager()->editorFont());
    dlg.setEditorTabSize(gdbWidget->editorManager()->editorTabSize());
    dlg.setEditorHighlighterSettings(gdbWidget->editorManager()->editorHighlighterSettings());
    dlg.setEditorHighlighterEnabled(gdbWidget->editorManager()->editorHighlighterEnabled());
    dlg.setEditorHighlighterEnabled(gdbWidget->editorManager()->editorHighlighterEnabled());
    dlg.setExternalEditorCommand(gdbWidget->editorManager()->editorExternalEditorCommand());
    dlg.setSourceAlternateDirectories(gdbWidget->sourceAlternateDirectories());
    dlg.setSourceIgnoreFilePatterns(gdbWidget->sourceIgnoreFilePatterns());
    dlg.setSourceMiscFilePatterns(gdbWidget->sourceMiscFilePatterns());
    dlg.setSourceSourceFilePatterns(gdbWidget->sourceSourceFilePatterns());
    dlg.setSourceHeaderFilePatterns(gdbWidget->sourceHeaderFilePatterns());
    dlg.setAssemblyShowAssemblyTabOnStartup(gdbWidget->assemblyShowAssemblyTabOnStartup());
    dlg.setAssemblyKeepAssemblyTabOnTop(gdbWidget->assemblyKeepAssemblyTabOnTop());
    dlg.setAssemblyDisassemblyFlavor(gdbWidget->assemblyDisassemblyFlavor());
    dlg.setAssemblySymbolDemagling(gdbWidget->assemblySymbolDemagling());
    dlg.setAssemblyShowAddressColumn(gdbWidget->assemblyShowAddressColumn());
    dlg.setAssemblyShowOffsetColumn(gdbWidget->assemblyShowOffsetColumn());
    dlg.setAssemblyShowOpcodeColumn(gdbWidget->assemblyShowOpcodeColumn());
    dlg.setAssemblyShowSourceLines(gdbWidget->assemblyShowSourceLines());
    dlg.setAssemblyRegisterFormat(gdbWidget->assemblyRegisterFormat());
    dlg.setAssemblyDisassemblyMode(gdbWidget->assemblyDisassemblyMode(), gdbWidget->assemblyDisassemblyBytes());
    dlg.setKeySettings(keySettings());
    dlg.setRRProgram(gdbWidget->rrProgram());
    dlg.setRRArguments(gdbWidget->rrArguments());
    dlg.setRRGdbArguments(gdbWidget->rrGdbArguments());

    int ret = dlg.exec();

    if (ret == 0) {
        return;
    }

    // Update the GdbWidget with the new settings.
    gdbWidget->setConsoleMode(dlg.seerConsoleMode());
    gdbWidget->setConsoleScrollLines(dlg.seerConsoleScrollLines());
    gdbWidget->setRememberManualCommandCount(dlg.seerRememberManualCommandCount());
    gdbWidget->setGdbProgram(dlg.gdbProgram());
    gdbWidget->setGdbArguments(dlg.gdbArguments());
    gdbWidget->setGdbAsyncMode(dlg.gdbAsyncMode());
    gdbWidget->setGdbNonStopMode(dlg.gdbNonStopMode());
    gdbWidget->setGdbHandleTerminatingException(dlg.gdbHandleTerminatingException());
    gdbWidget->setGdbRandomizeStartAddress(dlg.gdbRandomizeStartAddress());
    gdbWidget->setGdbEnablePrettyPrinting(dlg.gdbEnablePrettyPrinting());
    gdbWidget->setGdbRemoteTargetType(dlg.gdbRemoteTargetType());
    gdbWidget->editorManager()->setEditorFont(dlg.editorFont());
    gdbWidget->editorManager()->setEditorTabSize(dlg.editorTabSize());
    gdbWidget->editorManager()->setEditorHighlighterSettings(dlg.editorHighlighterSettings());
    gdbWidget->editorManager()->setEditorHighlighterEnabled(dlg.editorHighlighterEnabled());
    gdbWidget->editorManager()->setEditorExternalEditorCommand(dlg.externalEditorCommand());
    gdbWidget->setSourceAlternateDirectories(dlg.sourceAlternateDirectories());
    gdbWidget->setSourceIgnoreFilePatterns(dlg.sourceIgnoreFilePatterns());
    gdbWidget->setSourceMiscFilePatterns(dlg.sourceMiscFilePatterns());
    gdbWidget->setSourceSourceFilePatterns(dlg.sourceSourceFilePatterns());
    gdbWidget->setSourceHeaderFilePatterns(dlg.sourceHeaderFilePatterns());
    gdbWidget->setAssemblyShowAssemblyTabOnStartup(dlg.assemblyShowAssemblyTabOnStartup());
    gdbWidget->setAssemblyKeepAssemblyTabOnTop(dlg.assemblyKeepAssemblyTabOnTop());
    gdbWidget->setAssemblyDisassemblyFlavor(dlg.assemblyDisassemblyFlavor());
    gdbWidget->setAssemblySymbolDemagling(dlg.assemblySymbolDemagling());
    gdbWidget->setAssemblyShowAddressColumn(dlg.assemblyShowAddressColumn());
    gdbWidget->setAssemblyShowOffsetColumn(dlg.assemblyShowOffsetColumn());
    gdbWidget->setAssemblyShowOpcodeColumn(dlg.assemblyShowOpcodeColumn());
    gdbWidget->setAssemblyShowSourceLines(dlg.assemblyShowSourceLines());
    gdbWidget->setAssemblyRegisterFormat(dlg.assemblyRegisterFormat());
    gdbWidget->setAssemblyDisassemblyMode(dlg.assemblyDisassemblyMode(), dlg.assemblyDisassemblyBytes());
    gdbWidget->setRRProgram(dlg.rrProgram());
    gdbWidget->setRRArguments(dlg.rrArguments());
    gdbWidget->setRRGdbArguments(dlg.rrGdbArguments());

    // Clear history, if we need to.
    bool clearManualCommandHistory = dlg.seerClearManualCommandHistory();

    if (clearManualCommandHistory) {
        gdbWidget->clearManualCommandHistory();
    }

    // Set the key shortcuts.
    setKeySettings(dlg.keySettings());
}

void SeerMainWindow::handleSettingsSaveConfiguration () {

    int result = QMessageBox::warning(this, "Seer - Settings",
                                      QString("Write the configuration settings?"),
                                      QMessageBox::Ok|QMessageBox::Cancel, QMessageBox::Cancel);

    if (result == QMessageBox::Cancel) {
        return;
    }

    writeConfigSettings();
    gdbWidget->writeSettings();

    QMessageBox::information(this, "Seer", "Saved.");
}

void SeerMainWindow::handleHelpAbout () {

    SeerAboutDialog dlg(this);

    dlg.exec();
}

void SeerMainWindow::handleTerminateExecutable () {

    gdbWidget->handleGdbTerminateExecutable();
    gdbWidget->setExecutableLaunchMode("");
    handleGdbStateChanged();
    _runStatus->handleTerminate();
    menuOpenOCD->menuAction()->setVisible(false);
    actionOpenOCDAttach->setVisible(false);
    deleteExceptionLevelBar();
}

void SeerMainWindow::handleRestartExecutable () {

    if (gdbWidget->isGdbRuning() == false && gdbWidget->hasBackupLaunchMode()) {
        gdbWidget->restoreLaunchMode();
    }

    if (gdbWidget->executableLaunchMode() == "run" || gdbWidget->executableLaunchMode() == "start") {

        QString breakfunction = gdbWidget->executableBreakpointFunctionName();
        QString breaksource   = gdbWidget->executableBreakpointSourceName();

        // Stop in function?
        if (breakfunction != "") {

            gdbWidget->handleGdbRunExecutable("infunction", true);

        // Stop at source:line?
        }else if (breaksource != "") {

            gdbWidget->handleGdbRunExecutable("insource", true);

        // Otherwise, attempt to stop in "main".
        }else{
            gdbWidget->handleGdbRunExecutable("inmain", true);
        }

    }else if (gdbWidget->executableLaunchMode() == "attach") {

        gdbWidget->handleGdbAttachExecutable(true);

    }else if (gdbWidget->executableLaunchMode() == "connect") {

        gdbWidget->handleGdbConnectExecutable(true);

    }else if (gdbWidget->executableLaunchMode() == "rr") {

        gdbWidget->handleGdbRRExecutable(true);

    }else if (gdbWidget->executableLaunchMode() == "corefile") {

        gdbWidget->handleGdbCoreFileExecutable();

    }else if (gdbWidget->executableLaunchMode() == "openocd") {
        menuOpenOCD->menuAction()->setVisible(true);
        actionOpenOCDAttach->setVisible(true);
        gdbWidget->handleGdbMultiarchOpenOCDExecutable();
        // add exception level bar
        createExceptionLevelBar();

    }
    else{
        qDebug() << "UNKNOWN launch mode:" << gdbWidget->executableLaunchMode();
    }
}

void SeerMainWindow::handleStyleMenuChanged () {

    QAction* action = _styleMenuActionGroup->checkedAction();

    if (action == 0) {
        return;
    }

    setStyleName(action->text());
}

void SeerMainWindow::handleShowMessage (QString message, int time) {

    statusBar()->showMessage(message, time);
}

void SeerMainWindow::handleGdbStateChanged () {

    // qDebug() << "MODE:" << gdbWidget->executableLaunchMode() << "GDBRUNNING:" << gdbWidget->isGdbRuning() << "HASBACKUPLAUNCH:" << gdbWidget->hasBackupLaunchMode();

    //
    // We are currently debugging a program. Allow for Terminate/Detach/Disconnect...
    //
    // mode: "run" gdbrunning: true hasbackuplaunch: false
    // terminate: show  debug: hide  retart: hide
    //
    // mode: "run" gdbrunning: true hasbackuplaunch: true
    // terminate: show  debug: hide  retart: hide
    //
    if (gdbWidget->executableLaunchMode() != "" && gdbWidget->isGdbRuning() == true) {

        // Launch and Restart. Applies to all.
        actionGdbLaunch->setVisible(false);
        actionGdbRestart->setVisible(false);
        actionControlRestart->setVisible(false);

        // Run mode
        if (gdbWidget->executableLaunchMode() == "run" || gdbWidget->executableLaunchMode() == "start" ||
            gdbWidget->executableLaunchMode() == "rr"  || gdbWidget->executableLaunchMode() == "corefile") {
            // Terminate
            actionGdbTerminate->setVisible(true);
            actionGdbTerminate->setText("Terminate");
            actionGdbTerminate->setToolTip("Terminate the current debugging session.");
            actionControlTerminate->setVisible(true);
            actionControlTerminate->setText("Terminate");
            actionControlTerminate->setToolTip("Terminate the current debugging session.");

        // Attach mode
        }else if (gdbWidget->executableLaunchMode() == "attach") {
            // Detach
            actionGdbTerminate->setVisible(true);
            actionGdbTerminate->setText("Detach");
            actionGdbTerminate->setToolTip("Detach from the current debugging session.");
            actionControlTerminate->setVisible(true);
            actionControlTerminate->setText("Detach");
            actionControlTerminate->setToolTip("Detach from the current debugging session.");

        // Connect mode
        }else if (gdbWidget->executableLaunchMode() == "connect" || gdbWidget->executableLaunchMode() == "openocd") {
            // Disconnect
            actionGdbTerminate->setVisible(true);
            actionGdbTerminate->setText("Disconnect");
            actionGdbTerminate->setToolTip("Disconnect from the current debugging session.");
            actionControlTerminate->setVisible(true);
            actionControlTerminate->setText("Disconnect");
            actionControlTerminate->setToolTip("Disconnect from the current debugging session.");

        }else{
            qDebug() << "UNKNOWN launch mode:" << gdbWidget->executableLaunchMode();
        }

        return;
    }

    //
    // We are debugging a program but gdb has been killed. Allow for Launch and for Restart/Reattach/Reconnect...
    //
    // mode: "" gdbrunning: false hasbackuplaunch: true
    // terminate: hide  debug: show  retart: show
    //
    if (gdbWidget->executableLaunchMode() == "" && gdbWidget->isGdbRuning() == false && gdbWidget->hasBackupLaunchMode() == true) {

        // Allow a new debugging session.
        actionGdbLaunch->setVisible(true);
        actionGdbLaunch->setToolTip("Start a new debugging session.");

        // Hide terminate. Applies to all.
        actionGdbTerminate->setVisible(false);
        actionControlTerminate->setVisible(false);

        // Enable execution button
        actionControlContinue->setEnabled(true);
        actionControlNext->setEnabled(true);
        actionControlStep->setEnabled(true);
        actionControlFinish->setEnabled(true);
        actionControlInterrupt->setEnabled(true);
        actionControlNexti->setEnabled(true);
        actionControlStepi->setEnabled(true);
        actionGdbContinue->setEnabled(true);
        actionGdbNext->setEnabled(true);
        actionGdbStep->setEnabled(true);
        actionGdbFinish->setEnabled(true);
        actionGdbNexti->setEnabled(true);
        actionGdbStepi->setEnabled(true);
        actionInterruptProcess->setEnabled(true);

        if (gdbWidget->backupLaunchMode() == "run" || gdbWidget->backupLaunchMode() == "start" ||
            gdbWidget->backupLaunchMode() == "rr"  || gdbWidget->backupLaunchMode() == "corefile") {

            actionGdbRestart->setVisible(true);
            actionGdbRestart->setText("Restart");
            actionGdbRestart->setToolTip("Restart the current debugging session.");
            actionControlRestart->setVisible(true);
            actionControlRestart->setText("Restart");
            actionControlRestart->setToolTip("Restart the current debugging session.");

        }else if (gdbWidget->backupLaunchMode() == "attach") {
            actionGdbRestart->setVisible(true);
            actionGdbRestart->setText("Reattach");
            actionGdbRestart->setToolTip("Reattach the current debugging session.");
            actionControlRestart->setVisible(true);
            actionControlRestart->setText("Reattach");
            actionControlRestart->setToolTip("Reattach the current debugging session.");

        }else if (gdbWidget->backupLaunchMode() == "connect" || gdbWidget->backupLaunchMode() == "openocd") {
            actionGdbRestart->setVisible(true);
            actionGdbRestart->setText("Reconnect");
            actionGdbRestart->setToolTip("Reconnect the current debugging session.");
            actionControlRestart->setVisible(true);
            actionControlRestart->setText("Reconnect");
            actionControlRestart->setToolTip("Reconnect the current debugging session.");

        }else{
            qDebug() << "UNKNOWN launch mode:" << gdbWidget->backupLaunchMode();
        }

        return;
    }

    qDebug() << "BAD STATE!";
}

void SeerMainWindow::handleText (const QString& text) {

    if (text.startsWith("^error,msg=") || text.contains(QRegularExpression("^([0-9]+)\\^error,msg="))) {

        // ^error,msg="The program is not being run."
        // ^error,msg="ptrace: No such process."
        // 3^error,msg="Undefined MI command: symbol-info-variables",code="undefined-command"
        // 5^error,msg="No symbol "delta" in current context."
        // 5^error,msg="A syntax error in expression, near `'."

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        // Filter out less important errors.
        if (newtext.contains("^error,msg=\"No registers.\"")) {
            return;
        }

        if (newtext.contains("^error,msg=\"Selected thread is running.\"")) {
            return;
        }

        if (newtext.contains("^error,msg=\"Cannot inspect Ada tasks when program is not running\"")) {
            return;
        }

        if (newtext.contains("^error,msg=\"The current thread has terminated\"")) {
            return;
        }

        if (newtext.contains("^error,msg=\"A syntax error in expression, near ")) {
            return;
        }

        if (newtext.contains("^error,msg=\"No symbol \"")) {
            return;
        }

        // Display the error message.
        QString msg_text = Seer::parseFirst(text, "msg=", false);

        if (msg_text == "") {
            return;
        }

        // Show error on status bar.
        handleShowMessage(Seer::filterBookends(msg_text, '"', '"'), 3000);

        // Break early for certain errors.
        if (msg_text == "No symbol \"disassembly\" in current context.") {
            return;
        }

        if (msg_text == "\"-data-disassemble: No function contains specified address\"") {
            return;
        }

        if (msg_text == "\"No symbol \"disassembly\" in current context.\"") {
            return;
        }

        if (msg_text.startsWith("\"A syntax error in expression, near")) {
            return;
        }

        if (msg_text.startsWith("\"Invalid character ")) {
            return;
        }

        if (msg_text.startsWith("\"No symbol ")) {
            return;
        }

        if (msg_text.startsWith("\"Problem parsing arguments: data-evaluate-expression")) {
            return;
        }

        if (msg_text == "\"Attempt to use a type name as an expression\"") {
            return;
        }

        gdbWidget->addMessage(Seer::filterEscapes(msg_text), QMessageBox::Warning);

        return;

    }else if (text == "^running") {
        // Swallow this message.
        return;

    }else if (text == "^done") {
        return;

    }else if (text.startsWith("^done,files=[") && text.endsWith("]")) {
        return;

    }else if (text.startsWith("^done,shared-libraries=[") && text.endsWith("]")) {
        return;

    }else if (text.startsWith("^done,stack=[") && text.endsWith("]")) {
        return;

    }else if (text.startsWith("^done,variables=[") && text.endsWith("]")) {
        return;

    }else if (text.startsWith("^done,stack-args=[") && text.endsWith("]")) {
        return;

    }else if (text.startsWith("^done,BreakpointTable={") && text.endsWith("}")) {
        return;

    }else if (text.startsWith("^done,bkpt={") && text.endsWith("}")) {
        return;

    }else if (text.startsWith("^done,hw-awpt={")) {
        return;

    }else if (text.startsWith("^done,hw-rwpt={")) {
        return;

    }else if (text.startsWith("^done,wpt={")) {
        return;

    }else if (text.startsWith("^done,thread-ids={")) {
        return;

    }else if (text.startsWith("^done,new-thread-id=")) {
        return;

    }else if (text.startsWith("^done,threads=[")) {
        return;

    }else if (text.startsWith("^done,groups=[")) {
        return;

    }else if (text.startsWith("^done,register-names=[") && text.endsWith("]")) {
        return;

    }else if (text.startsWith("^done,register-values=[") && text.endsWith("]")) {
        return;

    }else if (text.startsWith("^done,DataExpressionAdded={") && text.endsWith("}")) {
        return;

    }else if (text.startsWith("^done,DataExpressionDeleted={") && text.endsWith("}")) {
        return;

    }else if (text.startsWith("^done,DataExpressionTable={") && text.endsWith("}")) {
        return;

    }else if (text.startsWith("^done,symbols={") && text.endsWith("}")) {
        return;

    }else if (text.startsWith("^done,asm_insns=[")) {
        return;

    }else if (text.startsWith("^done,ada-exceptions={") && text.endsWith("}")) {
        return;

    }else if (text.startsWith("^done,skips=[") && text.endsWith("]")) {
        return;

    }else if (text.startsWith("^done,checkpoints=[") && text.endsWith("]")) {
        return;

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done"))) {
        return;

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,value="))) {
        return;

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^error,msg="))) {
        return;

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,memory="))) {
        return;

    }else if (text == "^exit") {
        return;

    }else if (text.startsWith("*running,thread-id=\"")) {

        QString threadid_text = Seer::parseFirst(text, "thread-id=", '"', '"', false);

        handleShowMessage("Program started. Thread id: " + threadid_text, 3000);

        return;

    }else if (text == "^connected") {
        //^connected
        return;

    }else if (text.startsWith("^connected,frame=")) {
        //^connected,frame={level=\"0\",addr=\"0x00007f48351f80c1\",func=\"read\",args=[],from=\"/lib64/libc.so.6\",arch=\"i386:x86-64\"}"
        return;

    }else if (text.startsWith("*stopped")) {

        QString reason_text = Seer::parseFirst(text, "reason=", '"', '"', false);

        if (reason_text == "") {
            reason_text = "unknown";
        }

        handleShowMessage("Program stopped. Reason: " + reason_text, 3000);

        if (reason_text == "signal-received") {
            //*stopped,reason="signal-received",signal-name="SIGSEGV",signal-meaning="Segmentation fault", ...

            QString signalname_text = Seer::parseFirst(text, "signal-name=", '"', '"', false);

            gdbWidget->addMessage("Program encountered a '" + signalname_text + "' signal.", QMessageBox::Warning);

        }else if (reason_text == "breakpoint-hit") {

            QString bkptno_text = Seer::parseFirst(text, "bkptno=", '"', '"', false);
            QString disp_text   = Seer::parseFirst(text, "disp=",   '"', '"', false);

            if (disp_text == "del") {
                gdbWidget->addMessage("Program reached temporary breakpoint '" + bkptno_text + "'.", QMessageBox::Information);
            }else{
                gdbWidget->addMessage("Program reached breakpoint '" + bkptno_text + "'.", QMessageBox::Information);
            }
            // enable some button when target stop
            handleGdbTargetInterrupt();

        }else if (reason_text == "watchpoint-trigger") {
            //*stopped,reason="watchpoint-trigger",wpt={number="3",exp="i"},value={old="32767",new="42"},frame={addr="0x0000000000400d79",func="function1",args=[{name="text",value="\"Hello, World!\""}],file="function1.cpp",fullname="/home/erniep/Development/Peak/src/Seer/helloworld/function1.cpp",line="9",arch="i386:x86-64"},thread-id="1",stopped-threads="all",core="0"

            QString wpt_text    = Seer::parseFirst(text,       "wpt=",    '{', '}', false);
            QString number_text = Seer::parseFirst(wpt_text,   "number=", '"', '"', false);
            QString exp_text    = Seer::parseFirst(wpt_text,   "exp=",    '"', '"', false);
            QString value_text  = Seer::parseFirst(text,       "value=",  '{', '}', false);
            QString old_text    = Seer::parseFirst(value_text, "old=",    '"', '"', false);
            QString new_text    = Seer::parseFirst(value_text, "new=",    '"', '"', false);

            gdbWidget->addMessage(QString("Watchpoint triggered.\n\nNumber: %1\nExpression: %2\nOld value: %3\nNew value: %4").arg(number_text).arg(exp_text).arg(old_text).arg(new_text), QMessageBox::Information);

        }else if (reason_text == "read-watchpoint-trigger") {
            //*stopped,reason="read-watchpoint-trigger",hw-rwpt={number="5",exp="i"},value={value="42"},frame={addr="0x0000000000400d9a",func="function1",args=[{name="text",value="\"Hello, World!\""}],file="function1.cpp",fullname="/home/erniep/Development/Peak/src/Seer/helloworld/function1.cpp",line="11",arch="i386:x86-64"},thread-id="1",stopped-threads="all",core="4"

            QString hwwpt_text  = Seer::parseFirst(text,       "hw-rwpt=", '{', '}', false);
            QString number_text = Seer::parseFirst(hwwpt_text, "number=",  '"', '"', false);
            QString exp_text    = Seer::parseFirst(hwwpt_text, "exp=",     '"', '"', false);
            QString value_text  = Seer::parseFirst(text,       "value=",   '{', '}', false);
            QString value_text2 = Seer::parseFirst(value_text, "value=",   '"', '"', false);

            gdbWidget->addMessage(QString("Watchpoint triggered.\n\nNumber: %1\nExpression: %2\nValue: %3").arg(number_text).arg(exp_text).arg(value_text2), QMessageBox::Information);

        }else if (reason_text == "access-watchpoint-trigger") {
            //*stopped,reason="access-watchpoint-trigger",hw-awpt={number="3",exp="v"},value={old="1",new="11"},frame={addr="0x000000000040059a",func="bar",args=[{name="v",value="11"}],file="helloonefile.cpp",fullname="/home/erniep/Development/Peak/src/Seer/helloonefile/helloonefile.cpp",line="15",arch="i386:x86-64"},thread-id="1",stopped-threads="all",core="3"

            QString hwawpt_text = Seer::parseFirst(text,        "hw-awpt=", '{', '}', false);
            QString number_text = Seer::parseFirst(hwawpt_text, "number=",  '"', '"', false);
            QString exp_text    = Seer::parseFirst(hwawpt_text, "exp=",     '"', '"', false);
            QString value_text  = Seer::parseFirst(text,        "value=",   '{', '}', false);
            QString old_text    = Seer::parseFirst(value_text,  "old=",     '"', '"', false);
            QString new_text    = Seer::parseFirst(value_text,  "new=",     '"', '"', false);

            gdbWidget->addMessage(QString("Watchpoint triggered.\n\nNumber: %1\nExpression: %2\nOld value: %3\nNew value: %4").arg(number_text).arg(exp_text).arg(old_text).arg(new_text), QMessageBox::Information);

        }else if (reason_text == "watchpoint-scope") {
            //*stopped,reason="watchpoint-scope",wpnum="5", frame={func="callee3",args=[{name="strarg", value="0x11940 \"A string argument.\""}], file="../../../devo/gdb/testsuite/gdb.mi/basics.c", fullname="/home/foo/bar/devo/gdb/testsuite/gdb.mi/basics.c",line="18"}

            QString wpnum_text = Seer::parseFirst(text, "wpnum=", '"', '"', false);

            gdbWidget->addMessage(QString("Watchpoint went out of scope. Will be deleted.\n\nNumber: %1").arg(wpnum_text), QMessageBox::Information);

        }else if (reason_text == "exited-normally") {
            //*stopped,reason="exited-normally"

            gdbWidget->addMessage("Program exited normally.", QMessageBox::Information);

        }else if (reason_text == "exited") {
            //*stopped,reason="exited",exit-code="01"

            QString exitcode_text = Seer::parseFirst(text, "exit-code=", '"', '"', false);

            gdbWidget->addMessage("Program exited with code '" + exitcode_text +"'", QMessageBox::Information);

        }else if (reason_text == "exited-signalled") {
            //*stopped,reason="exited-signalled",signal-name="SIGSEGV",signal-meaning="Segmentation fault"

            QString signalname_text = Seer::parseFirst(text, "signal-name=", '"', '"', false);

            gdbWidget->addMessage("Program exited abnormally.\nIt encountered a '" + signalname_text + "' signal.", QMessageBox::Warning);

        }else if (reason_text == "unknown") {

            // Don't bother showing this.
            // Attaching to a pid will generate an unknown *stopped message that is useless.

            // qDebug() << "Text=" << text;
            // qDebug() << "Reason=" << reason_text;

            // gdbWidget->addMessage("Program encountered an unknown problem. See the Gdb output tab for messages.", QMessageBox::Warning);
        }

        return;

    }else if (text.startsWith("=thread-group-started,")) {
        // =thread-group-started,id="i1",pid="30916"

        QString pid_text = Seer::parseFirst(text, "pid=", '"', '"', false);

        //qDebug() << "Inferior pid = " << pid_text;

        gdbWidget->addMessage("Program started. (pid=" + pid_text +")", QMessageBox::Information);

        return;

    }else if (text.startsWith("=")) {
        // Suppress all other '=' messages.

        return;
    }

    // Leave in for stray error messages.
    qDebug() << text;
}

void SeerMainWindow::handleRunStatusChanged (SeerRunStatusIndicatorBox::RunStatus status) {

    if (status == SeerRunStatusIndicatorBox::RunStatus::Idle) {
        _progressIndicator->stop();

    }else if (status == SeerRunStatusIndicatorBox::RunStatus::Stopped) {
        _progressIndicator->stop();

    }else if (status == SeerRunStatusIndicatorBox::RunStatus::Running) {
        _progressIndicator->start();

    }else{
        _progressIndicator->stop();
    }
}

void SeerMainWindow::handleRecordSettingsChanged () {

    if (gdbWidget->gdbRecordMode() == "stop" || gdbWidget->gdbRecordMode() == "") {

        // Menu Control
        actionControlRecordStart->setEnabled(true);
        actionControlRecordStop->setEnabled(false);
        actionControlRecordForward->setEnabled(false);
        actionControlRecordReverse->setEnabled(false);
        actionControlRecordForward->setChecked(false);
        actionControlRecordReverse->setChecked(false);

        // Toolbar
        actionRecordProcess->setText("Record");
        actionRecordProcess->setToolTip("Toggle Record mode.");
        actionRecordProcess->setEnabled(true);
        actionRecordDirection->setEnabled(false);
        actionRecordDirection->setIcon(QIcon(":/seer/resources/RelaxLightIcons/go-next.svg"));

    }else if (gdbWidget->gdbRecordMode() == "full") {

        // Menu Control
        actionControlRecordStart->setEnabled(false);
        actionControlRecordStop->setEnabled(true);
        actionControlRecordForward->setEnabled(true);
        actionControlRecordReverse->setEnabled(true);

        if (gdbWidget->gdbRecordDirection() == "") {

            actionControlRecordForward->setChecked(true);
            actionRecordDirection->setIcon(QIcon(":/seer/resources/RelaxLightIcons/go-next.svg"));

        }else if (gdbWidget->gdbRecordDirection() == "--reverse") {

            actionControlRecordReverse->setChecked(true);
            actionRecordDirection->setIcon(QIcon(":/seer/resources/RelaxLightIcons/go-previous.svg"));

        }else{

            actionControlRecordForward->setChecked(false);
            actionControlRecordReverse->setChecked(false);
            actionRecordDirection->setIcon(QIcon(":/seer/resources/RelaxLightIcons/go-next.svg"));

            qDebug() << "Bad record direction of '" << gdbWidget->gdbRecordDirection() << "'";
        }

        // Toolbar
        actionRecordProcess->setText("Recording");
        actionRecordProcess->setToolTip("Toggle Record mode.");
        actionRecordProcess->setEnabled(true);
        actionRecordDirection->setEnabled(true);

    }else if (gdbWidget->gdbRecordMode() == "rr") {

        // Menu Control
        actionControlRecordStart->setEnabled(false);
        actionControlRecordStop->setEnabled(false);
        actionControlRecordForward->setEnabled(true);
        actionControlRecordReverse->setEnabled(true);

        if (gdbWidget->gdbRecordDirection() == "") {

            actionControlRecordForward->setChecked(true);
            actionRecordDirection->setIcon(QIcon(":/seer/resources/RelaxLightIcons/go-next.svg"));

        }else if (gdbWidget->gdbRecordDirection() == "--reverse") {

            actionControlRecordReverse->setChecked(true);
            actionRecordDirection->setIcon(QIcon(":/seer/resources/RelaxLightIcons/go-previous.svg"));

        }else{

            actionControlRecordForward->setChecked(false);
            actionControlRecordReverse->setChecked(false);
            actionRecordDirection->setIcon(QIcon(":/seer/resources/RelaxLightIcons/go-next.svg"));

            qDebug() << "Bad record direction of '" << gdbWidget->gdbRecordDirection() << "'";
        }

        // Toolbar
        actionRecordProcess->setText("RR");
        actionRecordProcess->setToolTip("Using RR debugger.");
        actionRecordProcess->setEnabled(true);
        actionRecordDirection->setEnabled(true);

    }else if (gdbWidget->gdbRecordMode() == "udb") {

        // Menu Control
        actionControlRecordStart->setEnabled(false);
        actionControlRecordStop->setEnabled(false);
        actionControlRecordForward->setEnabled(true);
        actionControlRecordReverse->setEnabled(true);

        if (gdbWidget->gdbRecordDirection() == "") {

            actionControlRecordForward->setChecked(true);
            actionRecordDirection->setIcon(QIcon(":/seer/resources/RelaxLightIcons/go-next.svg"));

        }else if (gdbWidget->gdbRecordDirection() == "--reverse") {

            actionControlRecordReverse->setChecked(true);
            actionRecordDirection->setIcon(QIcon(":/seer/resources/RelaxLightIcons/go-previous.svg"));

        }else{

            actionControlRecordForward->setChecked(false);
            actionControlRecordReverse->setChecked(false);
            actionRecordDirection->setIcon(QIcon(":/seer/resources/RelaxLightIcons/go-next.svg"));

            qDebug() << "Bad record direction of '" << gdbWidget->gdbRecordDirection() << "'";
        }

        // Toolbar
        actionRecordProcess->setText("UDB");
        actionRecordProcess->setToolTip("Using UDB debugger.");
        actionRecordProcess->setEnabled(true);
        actionRecordDirection->setEnabled(true);

    }else{
        qDebug() << "Bad record mode of:" << gdbWidget->gdbRecordMode();
    }
}

void SeerMainWindow::handleChangeWindowTitle (QString title) {

    if (title == "") {
        setWindowTitle("Seer Debugger");
    }else{
        setWindowTitle("Seer Debugger - '" + title + "'");
    }
}

void SeerMainWindow::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/MainWindow.md");
    help->setWindowFlags(help->windowFlags() | Qt::WindowStaysOnTopHint);
    help->show();
    help->raise();
}


void SeerMainWindow::writeSettings() {

    QSettings settings;

    settings.beginGroup("mainwindow"); {
        settings.setValue("size", size());
    } settings.endGroup();
}

void SeerMainWindow::readSettings() {

    QSettings settings;

    settings.beginGroup("mainwindow"); {
        resize(settings.value("size", QSize(1250, 1000)).toSize());
    } settings.endGroup();
}

void SeerMainWindow::writeConfigSettings () {

    QSettings settings;

    settings.beginGroup("mainwindow"); {
        settings.setValue("qtstyle", styleName());
    } settings.endGroup();

    settings.beginGroup("gdb"); {
        settings.setValue("program",                    gdbWidget->gdbProgram());
        settings.setValue("arguments",                  gdbWidget->gdbArguments());
        settings.setValue("asyncmode",                  gdbWidget->gdbAsyncMode());
        settings.setValue("nonstopmode",                gdbWidget->gdbNonStopMode());
        settings.setValue("handleterminatingexception", gdbWidget->gdbHandleTerminatingException());
        settings.setValue("randomizestartaddress",      gdbWidget->gdbRandomizeStartAddress());
        settings.setValue("enableprettyprinting",       gdbWidget->gdbEnablePrettyPrinting());
        settings.setValue("remotetargettype",           gdbWidget->gdbRemoteTargetType());
    } settings.endGroup();

    settings.beginGroup("rr"); {
        settings.setValue("program",                    gdbWidget->rrProgram());
        settings.setValue("arguments",                  gdbWidget->rrArguments());
        settings.setValue("gdbarguments",               gdbWidget->rrGdbArguments());
    } settings.endGroup();

    settings.beginGroup("editor"); {

        settings.setValue("font",    gdbWidget->editorManager()->editorFont().toString());
        settings.setValue("tabsize", gdbWidget->editorManager()->editorTabSize());
        settings.setValue("externaleditorcommand", gdbWidget->editorManager()->editorExternalEditorCommand());

        settings.beginGroup("highlighter"); {

            settings.setValue("enabled", gdbWidget->editorManager()->editorHighlighterEnabled());

            SeerHighlighterSettings highlighter = gdbWidget->editorManager()->editorHighlighterSettings();
            QStringList keys = highlighter.keys();

            for (int i=0; i<keys.size(); i++) {
                settings.beginGroup(keys[i]); {
                    QTextCharFormat f = highlighter.get(keys[i]);
                    settings.setValue("fontweight",      f.fontWeight());
                    settings.setValue("fontitalic",      f.fontItalic());
                    settings.setValue("foregroundcolor", f.foreground().color());
                    settings.setValue("backgroundcolor", f.background().color());
                } settings.endGroup();
            }

            settings.setValue("cppsuffixes",  highlighter.cppSourceSuffixes());
            settings.setValue("odinsuffixes", highlighter.odinSourceSuffixes());
            settings.setValue("rustsuffixes", highlighter.rustSourceSuffixes());
        } settings.endGroup();

    } settings.endGroup();

    settings.beginGroup("manualgdbcommands"); {
        settings.setValue("remembercount",   gdbWidget->rememberManualCommandCount());
    } settings.endGroup();

    settings.beginWriteArray("shortcuts"); {

        SeerKeySettings keysettings = keySettings();
        QStringList keys = keysettings.keys();

        for (int i=0; i<keys.size(); i++) {

            SeerKeySetting keysetting = keysettings.get(keys[i]);

            settings.setArrayIndex(i);
            settings.setValue("action", keysetting._action);
            settings.setValue("key",  keysetting._sequence.toString());
            settings.setValue("help", keysetting._description);
        }

    } settings.endArray();
}

void SeerMainWindow::readConfigSettings () {

    QSettings settings;

    settings.beginGroup("mainwindow"); {
        if (settings.contains("qtstyle")) {
            setStyleName(settings.value("qtstyle").toString());
        }
    } settings.endGroup();

    settings.beginGroup("gdb"); {
        gdbWidget->setGdbProgram(settings.value("program", "/usr/bin/gdb").toString());
        gdbWidget->setGdbArguments(settings.value("arguments", "--interpreter=mi").toString());
        gdbWidget->setGdbAsyncMode(settings.value("asyncmode", true).toBool());
        gdbWidget->setGdbNonStopMode(settings.value("nonstopmode", false).toBool());
        gdbWidget->setGdbHandleTerminatingException(settings.value("handleterminatingexception", true).toBool());
        gdbWidget->setGdbRandomizeStartAddress(settings.value("randomizestartaddress", false).toBool());
        gdbWidget->setGdbEnablePrettyPrinting(settings.value("enableprettyprinting", true).toBool());
        gdbWidget->setGdbRemoteTargetType(settings.value("remotetargettype", "extended-remote").toString());
    } settings.endGroup();

    settings.beginGroup("rr"); {
        gdbWidget->setRRProgram(settings.value("program", "/usr/bin/rr").toString());
        gdbWidget->setRRArguments(settings.value("arguments", "replay --interpreter=mi").toString());
        gdbWidget->setRRGdbArguments(settings.value("gdbarguments", "").toString());
    } settings.endGroup();

    settings.beginGroup("editor"); {

        QFont f;
        if (settings.contains("font")) {
            f.fromString(settings.value("font").toString());
        }else{
            f = QFont("monospace", 10);
        }
        gdbWidget->editorManager()->setEditorFont(f);

        gdbWidget->editorManager()->setEditorTabSize(settings.value("tabsize", 4).toInt());
        gdbWidget->editorManager()->setEditorExternalEditorCommand(settings.value("externaleditorcommand").toString());

        settings.beginGroup("highlighter"); {

            gdbWidget->editorManager()->setEditorHighlighterEnabled(settings.value("enabled",true).toBool());

            SeerHighlighterSettings highlighter = gdbWidget->editorManager()->editorHighlighterSettings();
            QStringList keys = highlighter.keys();

            for (int i=0; i<keys.size(); i++) {
                settings.beginGroup(keys[i]); {
                    QTextCharFormat f = highlighter.get(keys[i]);

                    if (settings.contains("fontweight")) {
                        f.setFontWeight(settings.value("fontweight").toInt());
                    }

                    if (settings.contains("fontitalic")) {
                        f.setFontItalic(settings.value("fontitalic").toBool());
                    }

                    if (settings.contains("foregroundcolor")) {
                        f.setForeground(settings.value("foregroundcolor").value<QColor>());
                    }

                    if (settings.contains("backgroundcolor")) {
                        f.setBackground(settings.value("backgroundcolor").value<QColor>());
                    }

                    highlighter.add(keys[i], f);

                } settings.endGroup();
            }

            if (settings.contains("cppsuffixes"))  highlighter.setCppSourceSuffixes(settings.value("cppsuffixes").toString());
            if (settings.contains("odinsuffixes")) highlighter.setOdinSourceSuffixes(settings.value("odinsuffixes").toString());
            if (settings.contains("rustsuffixes")) highlighter.setRustSourceSuffixes(settings.value("rustsuffixes").toString());

            gdbWidget->editorManager()->setEditorHighlighterSettings(highlighter);

        } settings.endGroup();
    } settings.endGroup();

    settings.beginGroup("manualgdbcommands"); {
        gdbWidget->setRememberManualCommandCount(settings.value("remembercount", 10).toInt());
    } settings.endGroup();

    int size = settings.beginReadArray("shortcuts"); {

        SeerKeySettings keysettings = keySettings(); // Start with defaults. The add() will overwrite.

        for (int i = 0; i < size; ++i) {

            settings.setArrayIndex(i);

            QString      name = settings.value("action").toString();
            QKeySequence key  = QKeySequence::fromString(settings.value("key").toString());
            QString      help = settings.value("help").toString();

            keysettings.add(name, SeerKeySetting(name, key, help));
        }

        setKeySettings(keysettings);

    } settings.endArray();
}

void SeerMainWindow::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QMainWindow::resizeEvent(event);
}

void SeerMainWindow::closeEvent (QCloseEvent* event) {

    event->accept();

    QCoreApplication::exit(0);
}

void SeerMainWindow::setKeySettings (const SeerKeySettings& settings) {

    _keySettings = settings;

    refreshShortCuts();
}

const SeerKeySettings SeerMainWindow::keySettings () const {

    return _keySettings;
}

void SeerMainWindow::refreshShortCuts () {

    // Dynamically change tool tip for 'Restart' depending on debug mode.
    if (_keySettings.has("Restart")) {

        SeerKeySetting setting = _keySettings.get("Restart");

        actionGdbRestart->setToolTip(setting._description);
        actionGdbRestart->setText(setting._action + " (" + setting._sequence.toString() + ")");
        actionControlRestart->setShortcut(setting._sequence);
    }

    if (_keySettings.has("Next")) {

        SeerKeySetting setting = _keySettings.get("Next");

        actionGdbNext->setToolTip(setting._description);
        actionGdbNext->setText(setting._action + " (" + setting._sequence.toString() + ")");
        actionControlNext->setShortcut(setting._sequence);
    }

    if (_keySettings.has("Nexti")) {

        SeerKeySetting setting = _keySettings.get("Nexti");

        actionGdbNexti->setToolTip(setting._description);
        actionGdbNexti->setText(setting._action + " (" + setting._sequence.toString() + ")");
        actionControlNexti->setShortcut(setting._sequence);
    }

    if (_keySettings.has("Step")) {

        SeerKeySetting setting = _keySettings.get("Step");

        actionGdbStep->setToolTip(setting._description);
        actionGdbStep->setText(setting._action + " (" + setting._sequence.toString() + ")");
        actionControlStep->setShortcut(setting._sequence);
    }

    if (_keySettings.has("Stepi")) {

        SeerKeySetting setting = _keySettings.get("Stepi");

        actionGdbStepi->setToolTip(setting._description);
        actionGdbStepi->setText(setting._action + " (" + setting._sequence.toString() + ")");
        actionControlStepi->setShortcut(setting._sequence);
    }

    if (_keySettings.has("Finish")) {

        SeerKeySetting setting = _keySettings.get("Finish");

        actionGdbFinish->setToolTip(setting._description);
        actionGdbFinish->setText(setting._action + " (" + setting._sequence.toString() + ")");
        actionControlFinish->setShortcut(setting._sequence);
    }

    if (_keySettings.has("Continue")) {

        SeerKeySetting setting = _keySettings.get("Continue");

        actionGdbContinue->setToolTip(setting._description);
        actionGdbContinue->setText(setting._action + " (" + setting._sequence.toString() + ")");
        actionControlContinue->setShortcut(setting._sequence);
    }

    if (_keySettings.has("Interrupt")) {

        SeerKeySetting setting = _keySettings.get("Interrupt");

        _interruptAction->setText(setting._action + " (" + setting._sequence.toString() + ")");
        actionControlInterrupt->setShortcut(setting._sequence);
    }

    if (_keySettings.has("Debug")) {

        SeerKeySetting setting = _keySettings.get("Debug");

        actionFileDebug->setShortcut(setting._sequence);
    }

    if (_keySettings.has("Arguments")) {

        SeerKeySetting setting = _keySettings.get("Arguments");

        actionFileArguments->setShortcut(setting._sequence);
    }

    if (_keySettings.has("Quit")) {

        SeerKeySetting setting = _keySettings.get("Quit");

        actionFileQuit->setShortcut(setting._sequence);
    }

    gdbWidget->editorManager()->setEditorKeySettings(keySettings());
}

// openocd get and set functions
const QString& SeerMainWindow::openOCDExePath() {
    return gdbWidget->openOCDExePath();
}

void SeerMainWindow::setOpenOCDExePath (const QString& path) {
    gdbWidget->setOpenOCDExePath(path);
}

const QString& SeerMainWindow::openOCDCommand() {
    return gdbWidget->openOCDCommand();
}

void SeerMainWindow::setOpenOCDCommand (const QString& command){
    gdbWidget->setOpenOCDCommand(command);
}

// ::GDB Multiarch
const QString& SeerMainWindow::gdbMultiarchExePath () {
    return gdbWidget->gdbMultiarchExePath();
}

void SeerMainWindow::setGdbMultiarchExePath (const QString& path) {
    gdbWidget->setGdbMultiarchExePath(path);
}

const QString& SeerMainWindow::gdbPort() {
    return gdbWidget->gdbPort();
}

void SeerMainWindow::setGdbPort (const QString& port){
    gdbWidget->setGdbPort(port);
}

const QString& SeerMainWindow::telnetPort() {
    return gdbWidget->telnetPort();
}

void SeerMainWindow::setTelnetPort (const QString& port){
    gdbWidget->setTelnetPort(port);
}

const QString& SeerMainWindow::gdbMultiarchCommand () {
    return gdbWidget->gdbMultiarchCommand();
}

void SeerMainWindow::setGdbMultiarchCommand (const QString& command) {
    gdbWidget->setGdbMultiarchCommand(command);
}

bool SeerMainWindow::isGdbMultiarchIsStopAtTempFunc () {
    return gdbWidget->isGdbMultiarchIsStopAtTempFunc();
}

void SeerMainWindow::setGdbMultiarchStopAtTempFunc (bool check) {
    gdbWidget->setGdbMultiarchStopAtTempFunc(check);
}

const QString SeerMainWindow::gdbMultiarchStopAtFunc () {
    return gdbWidget->gdbMultiarchStopAtFunc();
}

void SeerMainWindow::setGdbMultiarchStopAtFunc (const QString& func) {
    gdbWidget->setGdbMultiarchStopAtFunc(func);
}

bool SeerMainWindow::isGdbMultiarchStopAtException () {
    return gdbWidget->isGdbMultiarchStopAtException();
}

void SeerMainWindow::setGdbMultiarchStopAtExeption (bool check) {
    gdbWidget->setGdbMultiarchStopAtExeption(check);
}

const QString SeerMainWindow::gdbMultiarchExeptionLevelToStop() {
    return gdbWidget->gdbMultiarchExeptionLevelToStop();
}

void SeerMainWindow::setGdbMultiarchExeptionLevelToStop (const QString& level) {
    gdbWidget->setGdbMultiarchExeptionLevelToStop(level);
}

/// ::Docker
bool SeerMainWindow::isBuiltInDocker()
{
    return gdbWidget->isBuiltInDocker();
}

void SeerMainWindow::setBuiltInDocker(bool check)
{
    gdbWidget->setBuiltInDocker(check);
}

const QString SeerMainWindow::absoluteBuildFolderPath()
{
    return gdbWidget->absoluteBuildFolderPath();
}

void SeerMainWindow::setAbsoluteBuildFolderPath(const QString& path)
{
    gdbWidget->setAbsoluteBuildFolderPath(path);
}

const QString SeerMainWindow::dockerBuildFolderPath()
{
    return gdbWidget->dockerBuildFolderPath();
}

void SeerMainWindow::setDockerBuildFolderPath(const QString& path)
{
    return gdbWidget->setDockerBuildFolderPath(path);
}

const QString SeerMainWindow::openOCDTarget ()
{
    return gdbWidget->openOCDTarget();
}

void SeerMainWindow::setOpenOCDTarget (const QString& target)
{
    gdbWidget->setOpenOCDTarget(target);
}

// ::Symbol Files
const QMap<QString, std::tuple<QString, bool, QString>> SeerMainWindow::symbolFiles (void)
{
    return gdbWidget->symbolFiles();
}

void SeerMainWindow::setSymbolFiles (const QMap<QString, std::tuple<QString, bool, QString>>& _symbolFiles)
{
    gdbWidget->setSymbolFiles(_symbolFiles);
}

// Disable some button while target is running
void SeerMainWindow::handleGdbTargetRunning()
{
    actionControlContinue->setEnabled(false);
    actionControlNext->setEnabled(false);
    actionControlStep->setEnabled(false);
    actionControlFinish->setEnabled(false);
    actionControlInterrupt->setEnabled(true);
    actionControlNexti->setEnabled(false);
    actionControlStepi->setEnabled(false);
    actionGdbContinue->setEnabled(false);
    actionGdbNext->setEnabled(false);
    actionGdbStep->setEnabled(false);
    actionGdbFinish->setEnabled(false);
    actionGdbNexti->setEnabled(false);
    actionGdbStepi->setEnabled(false);
    actionInterruptProcess->setEnabled(true);
}
// Enable some button while target is interrupted
void SeerMainWindow::handleGdbTargetInterrupt()
{
    actionControlContinue->setEnabled(true);
    actionControlNext->setEnabled(true);
    actionControlStep->setEnabled(true);
    actionControlFinish->setEnabled(true);
    actionControlInterrupt->setEnabled(false);
    actionControlNexti->setEnabled(true);
    actionControlStepi->setEnabled(true);
    actionGdbContinue->setEnabled(true);
    actionGdbNext->setEnabled(true);
    actionGdbStep->setEnabled(true);
    actionGdbFinish->setEnabled(true);
    actionGdbNexti->setEnabled(true);
    actionGdbStepi->setEnabled(true);
    actionInterruptProcess->setEnabled(false);
}

void SeerMainWindow::handleStatusChanged(QString message) {
    // target halt
    if (message.startsWith("*stopped"))
    {
        handleGdbTargetInterrupt();
    }
    else if (message.startsWith("^done,stack"))   // target is halted, typically when finish button is clicked
    {
        handleGdbTargetInterrupt();
    }
    else if (message.startsWith("*running"))      // target is running
    {
        handleGdbTargetRunning();
    }
    
}
/***********************************************************************************************************************
 * OpenOCD Exception Level on Menubar                                                                                  *
 **********************************************************************************************************************/
void SeerMainWindow::createExceptionLevelBar()
{
    _groupExeptionLevel = new QWidget(this);
    QHBoxLayout* groupLayout = new QHBoxLayout(_groupExeptionLevel);
    groupLayout->setContentsMargins(0, 0, 0, 0); // remove extra space
    groupLayout->setSpacing(5); // spacing between checkbox and combo

    _exceptionButton = new QPushButton("Enable Exeption Level", _groupExeptionLevel);
    _exceptionComboBox = new QComboBox(_groupExeptionLevel);
    _exceptionComboBox->addItems({"EL1H", "EL3H", "N-EL1H", "N-EL2H", "EL1H / EL3H", "N-EL1H / N-EL2H", "off"});

    _exceptionComboBox->setEnabled(true);
    _exceptionComboBox->setCurrentText("off");
    // Add widgets to layout
    groupLayout->addWidget(_exceptionButton);
    groupLayout->addWidget(_exceptionComboBox);
    toolBar->addWidget(_groupExeptionLevel);

    QObject::connect(_exceptionButton,      &QPushButton::clicked,              this,      &SeerMainWindow::handleExceptionButtonClicked);
    QObject::connect(_exceptionComboBox,    &QComboBox::currentTextChanged,     gdbWidget, &SeerGdbWidget::handleExceptionLevelChanged);
}

void SeerMainWindow::deleteExceptionLevelBar()
{
    if (_groupExeptionLevel)
        delete _groupExeptionLevel;
    _groupExeptionLevel = nullptr;
}

void SeerMainWindow::handleExceptionButtonClicked ()
{
    gdbWidget->handleExceptionLevelChanged(_exceptionComboBox->currentText());
}