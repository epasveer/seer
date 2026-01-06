// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerCommandLogsWidget.h"
#include "SeerLogWidget.h"
#include "SeerMemoryVisualizerWidget.h"
#include "SeerArrayVisualizerWidget.h"
#include "SeerMatrixVisualizerWidget.h"
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

SeerCommandLogsWidget::SeerCommandLogsWidget (QWidget* parent) : QWidget(parent) {

    _consoleWidget                          = 0;
    _consoleIndex                           = -1;
    _consoleScrollLines                     = 1000;
    _rememberManualCommandCount             = 10;

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

    // Connect things.
    QObject::connect(logsTabWidget->tabBar(),    &QTabBar::tabMoved,          this,   &SeerCommandLogsWidget::handleLogsTabMoved);
    QObject::connect(logsTabWidget->tabBar(),    &QTabBar::currentChanged,    this,   &SeerCommandLogsWidget::handleLogsTabChanged);

    // Restore tab ordering.
    readSettings();
}

SeerCommandLogsWidget::~SeerCommandLogsWidget () {

    deleteConsole();
}

QLineEdit* SeerCommandLogsWidget::commandLineEdit () {

    return manualCommandComboBox->lineEdit();
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
}

void SeerCommandLogsWidget::readSettings () {

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

