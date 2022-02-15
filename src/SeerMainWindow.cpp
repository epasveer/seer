#include "SeerMainWindow.h"
#include "SeerDebugDialog.h"
#include "SeerConfigDialog.h"
#include "SeerArgumentsDialog.h"
#include "SeerAboutDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QShortcut>
#include <QtWidgets/QMenu>
#include <QtGui/QKeySequence>
#include <QtCore/QCoreApplication>
#include <QtCore/QTimer>
#include <QtCore/QRegExp>
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
    SeerRunStatusIndicator* runStatus = new SeerRunStatusIndicator(this);

    statusBar()->addPermanentWidget(runStatus);

    // Add progress spin widget.
    QWidget* spacerWidget = new QWidget(this);
    spacerWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    toolBar->addWidget(spacerWidget);

    _progressIndicator = new QProgressIndicator(this);
    _progressIndicator->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Preferred);
    _progressIndicator->setFixedWidth(96);
    _progressIndicator->setType(QProgressIndicator::ball_rotate);

    toolBar->addWidget(_progressIndicator);

    //
    // Set up shortcut keys.
    //
    QShortcut* nextKeyF5     = new QShortcut(QKeySequence(Qt::Key_F5), this);
    QShortcut* stepKeyF6     = new QShortcut(QKeySequence(Qt::Key_F6), this);
    QShortcut* finishKeyF7   = new QShortcut(QKeySequence(Qt::Key_F7), this);
    QShortcut* continueKeyF8 = new QShortcut(QKeySequence(Qt::Key_F8), this);

    //
    // Set up Interrupt menu.
    //
    QMenu* interruptMenu = new QMenu(this);
    QAction* interruptAction = interruptMenu->addAction("GDB Interrupt");
    interruptMenu->addSeparator();
    QAction* interruptActionSIGINT  = interruptMenu->addAction("SIGINT");
    QAction* interruptActionSIGKILL = interruptMenu->addAction("SIGKILL");
    QAction* interruptActionSIGFPE  = interruptMenu->addAction("SIGFPE");
    QAction* interruptActionSIGSEGV = interruptMenu->addAction("SIGSEGV");
    QAction* interruptActionSIGUSR1 = interruptMenu->addAction("SIGUSR1");
    QAction* interruptActionSIGUSR2 = interruptMenu->addAction("SIGUSR2");

    actionInterruptProcess->setMenu(interruptMenu);

    //
    // Set up signals/slots.
    //
    QObject::connect(actionFileDebug,                   &QAction::triggered,                    this,           &SeerMainWindow::handleFileDebug);
    QObject::connect(actionFileArguments,               &QAction::triggered,                    this,           &SeerMainWindow::handleFileArguments);
    QObject::connect(actionFileQuit,                    &QAction::triggered,                    this,           &SeerMainWindow::handleFileQuit);
    QObject::connect(actionViewMemoryVisualizer,        &QAction::triggered,                    this,           &SeerMainWindow::handleViewMemoryVisualizer);
    QObject::connect(actionViewArrayVisualizer,         &QAction::triggered,                    this,           &SeerMainWindow::handleViewArrayVisualizer);
    QObject::connect(actionConsoleNormal,               &QAction::triggered,                    this,           &SeerMainWindow::handleViewConsoleNormal);
    QObject::connect(actionConsoleHidden,               &QAction::triggered,                    this,           &SeerMainWindow::handleViewConsoleHidden);
    QObject::connect(actionConsoleMinimized,            &QAction::triggered,                    this,           &SeerMainWindow::handleViewConsoleMinimized);
    QObject::connect(actionHelpAbout,                   &QAction::triggered,                    this,           &SeerMainWindow::handleHelpAbout);

    QObject::connect(actionControlRun,                  &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbRunExecutable);
    QObject::connect(actionControlStart,                &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbStartExecutable);
    QObject::connect(actionControlContinue,             &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbContinue);
    QObject::connect(actionControlNext,                 &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbNext);
    QObject::connect(actionControlStep,                 &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbStep);
    QObject::connect(actionControlFinish,               &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbFinish);
    QObject::connect(actionControlInterrupt,            &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbInterrupt);

    QObject::connect(actionSettingsConfiguration,       &QAction::triggered,                    this,           &SeerMainWindow::handleSettingsConfiguration);
    QObject::connect(actionSettingsSaveConfiguration,   &QAction::triggered,                    this,           &SeerMainWindow::handleSettingsSaveConfiguration);

    QObject::connect(actionGdbRun,                      &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbRunExecutable);
    QObject::connect(actionGdbStart,                    &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbStartExecutable);
    QObject::connect(actionGdbContinue,                 &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbContinue);
    QObject::connect(actionGdbNext,                     &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbNext);
    QObject::connect(actionGdbStep,                     &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbStep);
    QObject::connect(actionGdbFinish,                   &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbFinish);
    QObject::connect(actionInterruptProcess,            &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbInterrupt);
    QObject::connect(actionMemoryVisualizer,            &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbMemoryVisualizer);
    QObject::connect(actionArrayVisualizer,             &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbArrayVisualizer);
    QObject::connect(interruptAction,                   &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbInterrupt);
    QObject::connect(interruptActionSIGINT,             &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbInterruptSIGINT);
    QObject::connect(interruptActionSIGKILL,            &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbInterruptSIGKILL);
    QObject::connect(interruptActionSIGFPE,             &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbInterruptSIGFPE);
    QObject::connect(interruptActionSIGSEGV,            &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbInterruptSIGSEGV);
    QObject::connect(interruptActionSIGUSR1,            &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbInterruptSIGUSR1);
    QObject::connect(interruptActionSIGUSR2,            &QAction::triggered,                    gdbWidget,      &SeerGdbWidget::handleGdbInterruptSIGUSR2);

    QObject::connect(nextKeyF5,                         &QShortcut::activated,                  gdbWidget,      &SeerGdbWidget::handleGdbNext);
    QObject::connect(stepKeyF6,                         &QShortcut::activated,                  gdbWidget,      &SeerGdbWidget::handleGdbStep);
    QObject::connect(finishKeyF7,                       &QShortcut::activated,                  gdbWidget,      &SeerGdbWidget::handleGdbFinish);
    QObject::connect(continueKeyF8,                     &QShortcut::activated,                  gdbWidget,      &SeerGdbWidget::handleGdbContinue);

    QObject::connect(gdbWidget->gdbMonitor(),           &GdbMonitor::astrixTextOutput,          runStatus,      &SeerRunStatusIndicator::handleText);
    QObject::connect(gdbWidget->gdbMonitor(),           &GdbMonitor::astrixTextOutput,          this,           &SeerMainWindow::handleText);
    QObject::connect(gdbWidget->gdbMonitor(),           &GdbMonitor::caretTextOutput,           this,           &SeerMainWindow::handleText);

    QObject::connect(runStatus,                         &SeerRunStatusIndicator::statusChanged, this,           &SeerMainWindow::handleRunStatusChanged);

    // Restore window settings.
    readSettings();

    // Restore configuration settings.
    readConfigSettings();

    //
    // Initialize contents.
    //
    statusBar()->showMessage(tr("Welcome to Seer. The All Knowing..."), 3000);
}

SeerMainWindow::~SeerMainWindow() {
}

void SeerMainWindow::setExecutableName (const QString& executableName) {
    gdbWidget->setExecutableName(executableName);
}

const QString& SeerMainWindow::executableName () const {
    return gdbWidget->executableName();
}

void SeerMainWindow::setExecutableArguments (const QString& executableArguments) {
    gdbWidget->setExecutableArguments(executableArguments);
}

void SeerMainWindow::setExecutableArguments (const QStringList& executableArguments) {

    //
    // Convert the list of arguments into a single argument string.
    // Becareful of arguments that contain a space. These need to be surrounded by
    // a ' character to retain the argument grouping.
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

void SeerMainWindow::setExecutablePid (int pid) {
    gdbWidget->setExecutablePid(pid);
}

int SeerMainWindow::executablePid () const {
    return gdbWidget->executablePid();
}

void SeerMainWindow::setExecutableHostPort (const QString& executableHostPort) {
    gdbWidget->setExecutableHostPort(executableHostPort);
}

const QString& SeerMainWindow::executableHostPort () const {
    return gdbWidget->executableHostPort();
}

void SeerMainWindow::setExecutableCoreFilename (const QString& executableCoreFilename) {
    gdbWidget->setExecutableCoreFilename(executableCoreFilename);
}

const QString& SeerMainWindow::executableCoreFilename () const {
    return gdbWidget->executableCoreFilename();
}

void SeerMainWindow::launchExecutable (const QString& launchMode) {

    if (launchMode == "start") {
        gdbWidget->handleGdbStartExecutable();

    }else if (launchMode == "run") {
        gdbWidget->handleGdbRunExecutable();

    }else if (launchMode == "attach") {
        gdbWidget->handleGdbAttachExecutable();

    }else if (launchMode == "connect") {
        gdbWidget->handleGdbConnectExecutable();

    }else if (launchMode == "corefile") {
        gdbWidget->handleGdbCoreFileExecutable();

    }else if (launchMode == "none") {

        // If no mode, schedule the opening of the debug dialog.
        QTimer::singleShot(200, this, &SeerMainWindow::handleFileDebug);

    }else{
        qWarning() << "SeerMainWindow::launchMode(): bad launchMode:" << launchMode;
    }
}

const QString& SeerMainWindow::executableLaunchMode () const {
    return gdbWidget->executableLaunchMode();
}

void SeerMainWindow::handleFileDebug () {

    SeerDebugDialog dlg(this);

    dlg.setExecutableName(executableName());
    dlg.setExecutableWorkingDirectory(executableWorkingDirectory());
    dlg.setExecutableArguments(executableArguments());
    dlg.setAttachPid(executablePid());
    dlg.setConnectHostPort(executableHostPort());
    dlg.setCoreFilename(executableCoreFilename());
    dlg.setLaunchMode(executableLaunchMode());

    int ret = dlg.exec();

    if (ret == 0) {
        return;
    }

    QString launchMode = dlg.launchMode();

    if (launchMode == "") {
        return;
    }

    setExecutableName(dlg.executableName());
    setExecutableWorkingDirectory(dlg.executableWorkingDirectory());
    setExecutableArguments(dlg.executableArguments());
    setExecutablePid(dlg.attachPid());
    setExecutableHostPort(dlg.connectHostPort());
    setExecutableCoreFilename(dlg.coreFilename());

    launchExecutable(launchMode);
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

    QCoreApplication::exit(0);
}

void SeerMainWindow::handleViewMemoryVisualizer () {

    gdbWidget->handleGdbMemoryVisualizer();
}

void SeerMainWindow::handleViewArrayVisualizer () {

    gdbWidget->handleGdbArrayVisualizer();
}

void SeerMainWindow::handleViewConsoleNormal () {

    gdbWidget->setConsoleMode("normal");
}

void SeerMainWindow::handleViewConsoleHidden () {

    gdbWidget->setConsoleMode("hidden");
}

void SeerMainWindow::handleViewConsoleMinimized () {

    gdbWidget->setConsoleMode("minimized");
}

void SeerMainWindow::handleSettingsConfiguration () {

    SeerConfigDialog dlg(this);

    dlg.setGdbProgram(gdbWidget->gdbProgram());
    dlg.setGdbArguments(gdbWidget->gdbArguments());
    dlg.setGdbAsyncMode(gdbWidget->gdbAsyncMode());
    dlg.setEditorFont(gdbWidget->editorManager()->editorFont());
    dlg.setEditorHighlighterSettings(gdbWidget->editorManager()->editorHighlighterSettings());
    dlg.setEditorHighlighterEnabled(gdbWidget->editorManager()->editorHighlighterEnabled());
    dlg.setSourceAlternateDirectories(gdbWidget->sourceAlternateDirectories());
    dlg.setSeerConsoleMode(gdbWidget->consoleMode());
    dlg.setSeerRememberManualCommandCount(gdbWidget->rememberManualCommandCount());

    int ret = dlg.exec();

    if (ret == 0) {
        return;
    }

    // Update the GdbWidget with the new settings.
    gdbWidget->setGdbProgram(dlg.gdbProgram());
    gdbWidget->setGdbArguments(dlg.gdbArguments());
    gdbWidget->setGdbAsyncMode(dlg.gdbAsyncMode());
    gdbWidget->editorManager()->setEditorFont(dlg.editorFont());
    gdbWidget->editorManager()->setEditorHighlighterSettings(dlg.editorHighlighterSettings());
    gdbWidget->editorManager()->setEditorHighlighterEnabled(dlg.editorHighlighterEnabled());
    gdbWidget->setSourceAlternateDirectories(dlg.sourceAlternateDirectories());
    gdbWidget->setConsoleMode(dlg.seerConsoleMode());
    gdbWidget->setRememberManualCommandCount(dlg.seerRememberManualCommandCount());

    // Clear history, if we need to.
    bool clearManualCommandHistory = dlg.seerClearManualCommandHistory();

    if (clearManualCommandHistory) {
        gdbWidget->clearManualCommandHistory();
    }
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

void SeerMainWindow::handleText (const QString& text) {

    if (text.startsWith("^error,msg=")) {

        // ^error,msg="The program is not being run."
        // ^error,msg="ptrace: No such process."

        //qDebug() << text;

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        // Filter out less important errors.
        if (newtext == "^error,msg=\"No registers.\"") {
            return;
        }

        if (newtext == "^error,msg=\"Selected thread is running.\"") {
            return;
        }

        // Display the error message.
        QString msg_text = Seer::parseFirst(text, "msg=", false);

        if (msg_text != "") {

            statusBar()->showMessage(Seer::filterBookEnds(msg_text, '"', '"'), 3000);

            QMessageBox::warning(this, "Error.", Seer::filterEscapes(msg_text));

            return;
        }

    }else if (text == "^running") {
        // Swallow this message.
        // statusBar()->showMessage(text.mid(1), 3000);
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

    }else if (text == "^exit") {
        return;

    }else if (text.contains(QRegExp("^([0-9]+)\\^done"))) {
        return;

    }else if (text.contains(QRegExp("^([0-9]+)\\^done,value="))) {
        return;

    }else if (text.contains(QRegExp("^([0-9]+)\\^error,msg="))) {
        return;

    }else if (text.contains(QRegExp("^([0-9]+)\\^done,memory="))) {
        return;

    }else if (text.startsWith("*running,thread-id=\"")) {

        QString threadid_text = Seer::parseFirst(text, "thread-id=", '"', '"', false);

        statusBar()->showMessage("Program started. Thread id: " + threadid_text, 3000);

        return;

    }else if (text.startsWith("^connected,frame=")) {
        //^connected,frame={level=\"0\",addr=\"0x00007f48351f80c1\",func=\"read\",args=[],from=\"/lib64/libc.so.6\",arch=\"i386:x86-64\"}"
        return;

    }else if (text.startsWith("*stopped,")) {

        QString reason_text = Seer::parseFirst(text, "reason=", '"', '"', false);

        statusBar()->showMessage("Program stopped. Reason: " + reason_text, 3000);

        if (reason_text == "signal-received") {
            //*stopped,reason="signal-received",signal-name="SIGSEGV",signal-meaning="Segmentation fault", ...

            QString signalname_text = Seer::parseFirst(text, "signal-name=", '"', '"', false);

            QMessageBox::warning(this, "Warning.", "Program encountered a '" + signalname_text + "' signal.");

        }else if (reason_text == "watchpoint-trigger") {
            //*stopped,reason="watchpoint-trigger",wpt={number="3",exp="i"},value={old="32767",new="42"},frame={addr="0x0000000000400d79",func="function1",args=[{name="text",value="\"Hello, World!\""}],file="function1.cpp",fullname="/home/erniep/Development/Peak/src/Seer/helloworld/function1.cpp",line="9",arch="i386:x86-64"},thread-id="1",stopped-threads="all",core="0"

            QString wpt_text    = Seer::parseFirst(text,       "wpt=",    '{', '}', false);
            QString number_text = Seer::parseFirst(wpt_text,   "number=", '"', '"', false);
            QString exp_text    = Seer::parseFirst(wpt_text,   "exp=",    '"', '"', false);
            QString value_text  = Seer::parseFirst(text,       "value=",  '{', '}', false);
            QString old_text    = Seer::parseFirst(value_text, "old=",    '"', '"', false);
            QString new_text    = Seer::parseFirst(value_text, "new=",    '"', '"', false);

            QMessageBox::information(this, "Note.", QString("Watchpoint triggered.\n\nNumber: %1\nExpression: %2\nOld value: %3\nNew value: %4").arg(number_text).arg(exp_text).arg(old_text).arg(new_text) );

        }else if (reason_text == "read-watchpoint-trigger") {
            //*stopped,reason="read-watchpoint-trigger",hw-rwpt={number="5",exp="i"},value={value="42"},frame={addr="0x0000000000400d9a",func="function1",args=[{name="text",value="\"Hello, World!\""}],file="function1.cpp",fullname="/home/erniep/Development/Peak/src/Seer/helloworld/function1.cpp",line="11",arch="i386:x86-64"},thread-id="1",stopped-threads="all",core="4"

            QString hwwpt_text  = Seer::parseFirst(text,       "hw-rwpt=", '{', '}', false);
            QString number_text = Seer::parseFirst(hwwpt_text, "number=",  '"', '"', false);
            QString exp_text    = Seer::parseFirst(hwwpt_text, "exp=",     '"', '"', false);
            QString value_text  = Seer::parseFirst(text,       "value=",   '{', '}', false);
            QString value_text2 = Seer::parseFirst(value_text, "value=",   '"', '"', false);

            QMessageBox::information(this, "Note.", QString("Watchpoint triggered.\n\nNumber: %1\nExpression: %2\nValue: %3").arg(number_text).arg(exp_text).arg(value_text2) );

        }else if (reason_text == "access-watchpoint-trigger") {
            //*stopped,reason="access-watchpoint-trigger",hw-awpt={number="3",exp="v"},value={old="1",new="11"},frame={addr="0x000000000040059a",func="bar",args=[{name="v",value="11"}],file="helloonefile.cpp",fullname="/home/erniep/Development/Peak/src/Seer/helloonefile/helloonefile.cpp",line="15",arch="i386:x86-64"},thread-id="1",stopped-threads="all",core="3"

            QString hwawpt_text = Seer::parseFirst(text,        "hw-awpt=", '{', '}', false);
            QString number_text = Seer::parseFirst(hwawpt_text, "number=",  '"', '"', false);
            QString exp_text    = Seer::parseFirst(hwawpt_text, "exp=",     '"', '"', false);
            QString value_text  = Seer::parseFirst(text,        "value=",   '{', '}', false);
            QString old_text    = Seer::parseFirst(value_text,  "old=",     '"', '"', false);
            QString new_text    = Seer::parseFirst(value_text,  "new=",     '"', '"', false);

            QMessageBox::information(this, "Note.", QString("Watchpoint triggered.\n\nNumber: %1\nExpression: %2\nOld value: %3\nNew value: %4").arg(number_text).arg(exp_text).arg(old_text).arg(new_text) );

        }else if (reason_text == "watchpoint-scope") {
            //*stopped,reason="watchpoint-scope",wpnum="5", frame={func="callee3",args=[{name="strarg", value="0x11940 \"A string argument.\""}], file="../../../devo/gdb/testsuite/gdb.mi/basics.c", fullname="/home/foo/bar/devo/gdb/testsuite/gdb.mi/basics.c",line="18"}

            QString wpnum_text = Seer::parseFirst(text, "wpnum=", '"', '"', false);

            QMessageBox::information(this, "Note.", QString("Watchpoint went out of scope. Will be deleted.\n\nNumber: %1").arg(wpnum_text) );

        }else if (reason_text == "exited-normally") {
            //*stopped,reason="exited-normally"

            QMessageBox::information(this, "Note.", "Program exited normally.");

        }else if (reason_text == "exited") {
            //*stopped,reason="exited",exit-code="01"

            QString exitcode_text = Seer::parseFirst(text, "exit-code=", '"', '"', false);

            QMessageBox::information(this, "Note.", "Program exited with code '" + exitcode_text +"'");

        }else if (reason_text == "exited-signalled") {
            //*stopped,reason="exited-signalled",signal-name="SIGSEGV",signal-meaning="Segmentation fault"

            QString signalname_text = Seer::parseFirst(text, "signal-name=", '"', '"', false);

            QMessageBox::warning(this, "Error.", "Program exited abnormally.\n\nIt encountered a '" + signalname_text + "' signal.");
        }

        return;
    }

    qDebug() << text;
}

void SeerMainWindow::handleRunStatusChanged (SeerRunStatusIndicator::RunStatus status) {

    if (status == SeerRunStatusIndicator::RunStatus::Idle) {
        _progressIndicator->stop();

    }else if (status == SeerRunStatusIndicator::RunStatus::Stopped) {
        _progressIndicator->stop();

    }else if (status == SeerRunStatusIndicator::RunStatus::Running) {
        _progressIndicator->start();

    }else{
        _progressIndicator->stop();
    }

}

void SeerMainWindow::closeEvent (QCloseEvent* event) {

    event->accept();

    QCoreApplication::exit(0);
}

void SeerMainWindow::writeSettings() {

    QSettings settings;

    settings.beginGroup("mainwindow"); {
        settings.setValue("size", size());
    } settings.endGroup();

    //qDebug() << size();
}

void SeerMainWindow::readSettings() {

    QSettings settings;

    settings.beginGroup("mainwindow"); {
        resize(settings.value("size", QSize(1250, 1000)).toSize());
    } settings.endGroup();

    //qDebug() << size();
}

void SeerMainWindow::writeConfigSettings () {

    QSettings settings;

    settings.beginGroup("gdb"); {
        settings.setValue("program",   gdbWidget->gdbProgram());
        settings.setValue("arguments", gdbWidget->gdbArguments());
    } settings.endGroup();

    settings.beginGroup("editor"); {

        settings.setValue("font", gdbWidget->editorManager()->editorFont().toString());

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

        } settings.endGroup();
    } settings.endGroup();

    settings.beginGroup("manualgdbcommands"); {
        settings.setValue("remembercount",   gdbWidget->rememberManualCommandCount());
    } settings.endGroup();
}

void SeerMainWindow::readConfigSettings () {

    QSettings settings;

    settings.beginGroup("gdb"); {
        gdbWidget->setGdbProgram(settings.value("program", "/usr/bin/gdb").toString());
        gdbWidget->setGdbArguments(settings.value("arguments", "--interpreter=mi").toString());
    } settings.endGroup();

    settings.beginGroup("editor"); {

        QFont f;
        if (settings.contains("font")) {
            f.fromString(settings.value("font").toString());
        }else{
            f = QFont("Source Code Pro", 10);
        }
        gdbWidget->editorManager()->setEditorFont(f);

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

            gdbWidget->editorManager()->setEditorHighlighterSettings(highlighter);

        } settings.endGroup();
    } settings.endGroup();

    settings.beginGroup("manualgdbcommands"); {
        gdbWidget->setRememberManualCommandCount(settings.value("remembercount", 10).toInt());
    } settings.endGroup();
}

void SeerMainWindow::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QMainWindow::resizeEvent(event);
}

