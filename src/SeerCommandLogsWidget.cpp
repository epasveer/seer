// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerCommandLogsWidget.h"
#include "SeerMacroToolButton.h"
#include "SeerLogWidget.h"
#include "SeerHelpPageDialog.h"
#include <QtGui/QFont>
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMenu>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

#include <QWidgetAction>

SeerCommandLogsWidget::SeerCommandLogsWidget (QWidget* parent) : QWidget(parent) {

    _consoleWidget              = 0;
    _consoleIndex               = -1;
    _consoleScrollLines         = 1000;
    _rememberManualCommandCount = 10;

    setupUi(this);

    // Setup the widgets
    QFont font;
    font.setFamily("monospace [Consolas]");
    font.setFixedPitch(true);
    font.setStyleHint(QFont::TypeWriter);

    m1ToolButton->setMacroName("M1");
    m2ToolButton->setMacroName("M2");
    m3ToolButton->setMacroName("M3");
    m4ToolButton->setMacroName("M4");
    m5ToolButton->setMacroName("M5");
    m6ToolButton->setMacroName("M6");
    m7ToolButton->setMacroName("M7");
    m8ToolButton->setMacroName("M8");
    m9ToolButton->setMacroName("M9");
    m0ToolButton->setMacroName("M0");

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

    logsTabWidget->clear();
    logsTabWidget->addTab(_messagesBrowserWidget,    "Messages");
    logsTabWidget->addTab(_breakpointsBrowserWidget, "Breakpoints");
    logsTabWidget->addTab(_watchpointsBrowserWidget, "Watchpoints");
    logsTabWidget->addTab(_catchpointsBrowserWidget, "Catchpoints");
    logsTabWidget->addTab(_printpointsBrowserWidget, "Printpoints");
    logsTabWidget->addTab(_checkpointsBrowserWidget, "Checkpoints");
    logsTabWidget->addTab(_gdbOutputLog,             "GDB output");
    logsTabWidget->addTab(_seerOutputLog,            "Seer output");
    logsTabWidget->setCurrentIndex(0);

    // Create the console tab.
    // Each RUN method will create and connect to the console's terminal.
    createConsole();

    // Set manual command settings.
    manualCommandComboBox->setFont(font);
    manualCommandComboBox->setEditable(true);
    manualCommandComboBox->lineEdit()->setPlaceholderText("Manually enter a gdb/mi command...");
    manualCommandComboBox->lineEdit()->setToolTip("Manually enter a gdb/mi command.");
    manualCommandComboBox->lineEdit()->setClearButtonEnabled(true);

    // Connect things.
    QObject::connect(logsTabWidget->tabBar(),           &QTabBar::tabMoved,                             this,   &SeerCommandLogsWidget::handleLogsTabMoved);
    QObject::connect(logsTabWidget->tabBar(),           &QTabBar::currentChanged,                       this,   &SeerCommandLogsWidget::handleLogsTabChanged);
    QObject::connect(manualCommandComboBox->lineEdit(), &QLineEdit::returnPressed,                      this,   &SeerCommandLogsWidget::handleManualCommandExecute);
    QObject::connect(manualCommandComboBox,             QOverload<int>::of(&QComboBox::activated),      this,   &SeerCommandLogsWidget::handleManualCommandChanged);
    QObject::connect(_gdbOutputLog,                     &SeerLogWidget::logEnabledChanged,              this,   &SeerCommandLogsWidget::handleLogOutputChanged);
    QObject::connect(_gdbOutputLog,                     &SeerLogWidget::logTimeStampChanged,            this,   &SeerCommandLogsWidget::handleLogOutputChanged);
    QObject::connect(_seerOutputLog,                    &SeerLogWidget::logEnabledChanged,              this,   &SeerCommandLogsWidget::handleLogOutputChanged);
    QObject::connect(_seerOutputLog,                    &SeerLogWidget::logTimeStampChanged,            this,   &SeerCommandLogsWidget::handleLogOutputChanged);
    QObject::connect(breakpointsLoadToolButton,         &QToolButton::clicked,                          this,   &SeerCommandLogsWidget::handleGdbLoadBreakpoints);
    QObject::connect(breakpointsSaveToolButton,         &QToolButton::clicked,                          this,   &SeerCommandLogsWidget::handleGdbSaveBreakpoints);
    QObject::connect(helpToolButton,                    &QToolButton::clicked,                          this,   &SeerCommandLogsWidget::handleHelpToolButtonClicked);
    QObject::connect(macroButtonGroup,                  &QButtonGroup::buttonClicked,                   this,   &SeerCommandLogsWidget::handleMacroToolButtonClicked);

    // Restore tab ordering and manual command history.
    readSettings();
    readHistorySettings();
}

SeerCommandLogsWidget::~SeerCommandLogsWidget () {

    deleteConsole();
}

SeerMessagesBrowserWidget* SeerCommandLogsWidget::messagesBrowser () {

    return _messagesBrowserWidget;
}

SeerBreakpointsBrowserWidget* SeerCommandLogsWidget::breakpointsBrowser () {

    return _breakpointsBrowserWidget;
}

SeerWatchpointsBrowserWidget* SeerCommandLogsWidget::watchpointsBrowser () {

    return _watchpointsBrowserWidget;
}

SeerCatchpointsBrowserWidget* SeerCommandLogsWidget::catchpointsBrowser () {

    return _catchpointsBrowserWidget;
}

SeerPrintpointsBrowserWidget* SeerCommandLogsWidget::printpointsBrowser () {

    return _printpointsBrowserWidget;
}

SeerCheckpointsBrowserWidget* SeerCommandLogsWidget::checkpointsBrowser () {

    return _checkpointsBrowserWidget;
}

SeerGdbLogWidget* SeerCommandLogsWidget::gdbOutputLog () {

    return _gdbOutputLog;
}

SeerSeerLogWidget* SeerCommandLogsWidget::seerOutputLog () {

    return _seerOutputLog;
}

SeerConsoleWidget* SeerCommandLogsWidget::console () {

    return _consoleWidget;
}

void SeerCommandLogsWidget::createConsole () {

    if (_consoleWidget == 0) {

        _consoleWidget = new SeerConsoleWidget(0);

        // Connect window title changes.
        // XXX QObject::connect(this, &SeerGdbWidget::changeWindowTitle, _consoleWidget, &SeerConsoleWidget::handleChangeWindowTitle);

        // The console needs to know when it's detached or reattached.
        QObject::connect(logsTabWidget, qOverload<QWidget*>(&QDetachTabWidget::tabDetached),   _consoleWidget, &SeerConsoleWidget::handleTabDetached);
        QObject::connect(logsTabWidget, qOverload<QWidget*>(&QDetachTabWidget::tabReattached), _consoleWidget, &SeerConsoleWidget::handleTabReattached);

        _consoleIndex = logsTabWidget->addTab(_consoleWidget, "Console output");

        QObject::connect(_consoleWidget, &SeerConsoleWidget::newTextAdded,  this, &SeerCommandLogsWidget::handleConsoleNewTextAdded);
        QObject::connect(_consoleWidget, &SeerConsoleWidget::newTextViewed, this, &SeerCommandLogsWidget::handleConsoleNewTextViewed);

        setConsoleMode(consoleMode());
        setConsoleScrollLines(consoleScrollLines());
    }
}

void SeerCommandLogsWidget::deleteConsole () {

    if (_consoleWidget) {

        if (_consoleIndex >= 0) {
            logsTabWidget->removeTab(_consoleIndex);
        }

        delete _consoleWidget;
        _consoleWidget = 0;
        _consoleIndex  = -1;
    }
}

void SeerCommandLogsWidget::reattachConsole () {

     if (_consoleIndex < 0) {
        return;
    }

    if (_consoleWidget == nullptr) {
        return;
    }

    _consoleMode = "attached";

    logsTabWidget->reattachTab(_consoleIndex);
}

void SeerCommandLogsWidget::setConsoleMode (const QString& mode) {

    _consoleMode = mode;

    handleConsoleModeChanged();
}

QString SeerCommandLogsWidget::consoleMode () const {

    return _consoleMode;
}

void SeerCommandLogsWidget::setConsoleScrollLines (int count) {

    _consoleScrollLines = count;

    if (_consoleWidget) {
        _consoleWidget->setScrollLines(_consoleScrollLines);
    }
}

int SeerCommandLogsWidget::consoleScrollLines () const {

    return _consoleScrollLines;
}

void SeerCommandLogsWidget::setManualCommands (const QStringList& commands) {

    manualCommandComboBox->clear();
    manualCommandComboBox->addItems(commands);

    // Point to last one.
    if (manualCommandComboBox->count() > 0) {
        manualCommandComboBox->setCurrentIndex(manualCommandComboBox->count()-1);
    }
}

QStringList SeerCommandLogsWidget::manualCommands(int count) const {

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

void SeerCommandLogsWidget::setRememberManualCommandCount (int count) {

    _rememberManualCommandCount = count;
}

int SeerCommandLogsWidget::rememberManualCommandCount () const {

    return _rememberManualCommandCount;
}

void SeerCommandLogsWidget::clearManualCommandHistory () {

    // Zap the entries in the combobox.
    manualCommandComboBox->clear();

    // Write the history settings.
    writeHistorySettings();
}

void SeerCommandLogsWidget::handleLogsTabMoved (int to, int from) {

    Q_UNUSED(from);
    Q_UNUSED(to);

    // Keep track of console tab if it moved.
    if (_consoleIndex == from) {
        _consoleIndex = to;
    }

    // Don't handle anything here if Seer is exiting.
    /* XXX
    if (isQuitting()) {
        return;
    }
    */

    writeSettings();
}

void SeerCommandLogsWidget::handleLogsTabChanged (int index) {

    Q_UNUSED(index);

    // Don't handle anything here if Seer is exiting.
    /* XXX
    if (isQuitting()) {
        return;
    }
    */

    writeSettings();
}

void SeerCommandLogsWidget::handleRaiseMessageTab () {

    int idx = logsTabWidget->indexOf(_messagesBrowserWidget);

    if (idx < 0) {
        return;
    }

    logsTabWidget->setCurrentIndex(idx);
}

void SeerCommandLogsWidget::handleConsoleModeChanged () {

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

void SeerCommandLogsWidget::handleConsoleNewTextAdded () {

    if (_consoleIndex >= 0) {
        logsTabWidget->setTabIcon(_consoleIndex, QIcon(":/seer/resources/RelaxLightIcons/data-information.svg"));
    }
}

void SeerCommandLogsWidget::handleConsoleNewTextViewed () {

    if (_consoleIndex >= 0) {
        logsTabWidget->setTabIcon(_consoleIndex, QIcon());
    }
}

void SeerCommandLogsWidget::handleManualCommandExecute () {

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
    emit executeGdbCommand(command);

    writeHistorySettings();
}

void SeerCommandLogsWidget::handleManualCommandChanged () {

    writeHistorySettings();
}


void SeerCommandLogsWidget::handleLogOutputChanged () {

    writeSettings();
}

void SeerCommandLogsWidget::handleGdbLoadBreakpoints () {

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

    emit executeGdbCommand("source -v " + fname);
    emit refreshBreakpointsList();

    QMessageBox::information(this, "Seer", "Loaded.");
}

void SeerCommandLogsWidget::handleGdbSaveBreakpoints () {

    if (breakpointsBrowser()->isEmpty() &&
        watchpointsBrowser()->isEmpty() &&
        catchpointsBrowser()->isEmpty() &&
        printpointsBrowser()->isEmpty()) {

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

    emit executeGdbCommand("save breakpoints " + fname);

    QMessageBox::information(this, "Seer", "Saved.");
}

void SeerCommandLogsWidget::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/BreakpointGdbSeerManager.md");
    help->show();
    help->raise();
}

void SeerCommandLogsWidget::handleMacroToolButtonClicked (QAbstractButton* button) {

    if (button == nullptr) {
        return;
    }

    SeerMacroToolButton* tb = qobject_cast<SeerMacroToolButton*>(button);

    QStringList lines = tb->commands();

    if (lines.size() == 0) {
        return;
    }

    emit executeGdbCommands(lines);
}

void SeerCommandLogsWidget::writeSettings () {

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

    // Write log settings.
    settings.beginGroup("gdboutputlog"); {
        settings.setValue("enabled",   _gdbOutputLog->isLogEnabled());
        settings.setValue("timestamp", _gdbOutputLog->isTimeStampEnabled());
    } settings.endGroup();

    settings.beginGroup("seeroutputlog"); {
        settings.setValue("enabled",   _seerOutputLog->isLogEnabled());
        settings.setValue("timestamp", _seerOutputLog->isTimeStampEnabled());
    } settings.endGroup();
}

void SeerCommandLogsWidget::readSettings () {

    QSettings settings;

    // Read tab order from settings.
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

    // Read log settings.
    settings.beginGroup("gdboutputlog"); {
        _gdbOutputLog->setLogEnabled(settings.value("enabled", true).toBool());
        _gdbOutputLog->setTimeStampEnabled(settings.value("timestamp", false).toBool());
    } settings.endGroup();

    settings.beginGroup("seeroutputlog"); {
        _seerOutputLog->setLogEnabled(settings.value("enabled", true).toBool());
        _seerOutputLog->setTimeStampEnabled(settings.value("timestamp", false).toBool());
    } settings.endGroup();
}

void SeerCommandLogsWidget::writeHistorySettings () {

    QSettings settings;

    // Write the manual command history.
    settings.beginWriteArray("manualgdbcommandshistory"); {
        QStringList commands = manualCommands(rememberManualCommandCount());
        if (commands.size() > 0) {
            for (int i = 0; i < commands.size(); ++i) {
                settings.setArrayIndex(i);
                settings.setValue("command", commands[i]);
            }
        }
    } settings.endArray();
}

void SeerCommandLogsWidget::readHistorySettings () {

    QSettings settings;

    // Read the manual command history.
    int size = settings.beginReadArray("manualgdbcommandshistory"); {
        QStringList commands;
        for (int i = 0; i < size; ++i) {
            settings.setArrayIndex(i);
            commands << settings.value("command").toString();
        }
        setManualCommands(commands);
    } settings.endArray();
}

