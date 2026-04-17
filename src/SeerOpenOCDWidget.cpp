#include "SeerOpenOCDWidget.h"
#include <QtCore/QtCore>
#include <QtCore/QProcess>
#include <QMessageBox>
#include <QStringLiteral>
/***********************************************************************************************************************
 * Constructor & Destructor                                                                                            *
 **********************************************************************************************************************/
SeerOpenOCDWidget::SeerOpenOCDWidget (QWidget* parent) : SeerLogWidget(parent) {
    Q_UNUSED(parent);
    _openocdProcess = new QProcess(this);
    _openocdLogsTabWidget = nullptr;
}

SeerOpenOCDWidget::~SeerOpenOCDWidget (){
    terminate();
}

void SeerOpenOCDWidget::newOpenOCDWidget (){
    if (!_openocdProcess)
        _openocdProcess = new QProcess(this);
}

QProcess* SeerOpenOCDWidget::openocdProcess()
{
    return _openocdProcess;
}

/***********************************************************************************************************************
 * OpenOCD process                                                                                                     *
 **********************************************************************************************************************/
bool SeerOpenOCDWidget::startOpenOCD (const QString &openocdExe, const QString &command)
{
    if (_openocdProcess->state() == QProcess::Running) {
        QMessageBox::warning(nullptr, QObject::tr("Seer"), QObject::tr("OpenOCD is already running."));
        return false;
    }
    if (command == nullptr) {
        QMessageBox::warning(nullptr, QObject::tr("Seer"), QObject::tr("No OpenOCD command is specified."));
        return false;
    }
    if (_openocdProcess->state() == QProcess::NotRunning) {
        QStringList _command = QProcess::splitCommand(command);
        _openocdProcess->start(openocdExe, _command);
        return true;
    }
    return false;
}

void SeerOpenOCDWidget::terminate ()
{
    if (_openocdProcess)
    {
        _openocdProcess->terminate();                       // Try graceful termination first
        if (!_openocdProcess->waitForFinished(1000)) {      // Wait up to 1 second
            _openocdProcess->kill();                        // Force kill as a last resort
            _openocdProcess->waitForFinished(500);          // Brief wait to ensure kill completes
        }
    }
}

bool SeerOpenOCDWidget::isOpenocdRunning ()
{
    if (_openocdProcess)
        if (_openocdProcess->state() == QProcess::Running) {
            return true;
        }
    return false;
}
/***********************************************************************************************************************
 * Create a new console display for displaying openOCD log                                                             *
 **********************************************************************************************************************/
void SeerOpenOCDWidget::createOpenOCDConsole (QDetachTabWidget* parent)
{
    if (_openocdLogsTabWidget != nullptr) {
        return;
    }
    _openocdLogsTabWidget = new SeerLogWidget();
    parent->addTab(_openocdLogsTabWidget, "OpenOCD output");
    _openocdLogsTabWidget->setPlaceholderText("[OpenOCD output]");
    _openocdLogsTabWidget->setLogEnabled(true);
    connect(_openocdProcess, &QProcess::readyReadStandardOutput, this, &SeerOpenOCDWidget::handleReadOutput);
    connect(_openocdProcess, &QProcess::readyReadStandardError, this, &SeerOpenOCDWidget::handleReadError);
}

SeerLogWidget* SeerOpenOCDWidget::openocdConsole()
{
    return _openocdLogsTabWidget;
}

void SeerOpenOCDWidget::killConsole ()
{
    delete _openocdLogsTabWidget;
    _openocdLogsTabWidget = nullptr;
}

void SeerOpenOCDWidget::setConsoleVisible (bool flag)
{
    _openocdLogsTabWidget->setVisible(flag);
}

/***********************************************************************************************************************
 * Slot                                                                                                                *
 **********************************************************************************************************************/
void SeerOpenOCDWidget::handleReadOutput ()
{
    QString Text = QString::fromLocal8Bit(_openocdProcess->readAllStandardOutput());
    _openocdLogsTabWidget->handleText(Text);
}

void SeerOpenOCDWidget::handleReadError ()
{
    QString Text = QString::fromLocal8Bit(_openocdProcess->readAllStandardError());
    // If OpenOCD fails to start because the port is already in use, emit openocdTerminate signal
    if (Text.contains("Error: couldn't bind tcl to socket on port") || \
        Text.contains("Error: JTAG scan chain interrogation failed: all zeroes") || \
        Text.contains("Error: Invalid ACK (0) in DAP response") || \
        Text.contains("Error: Invalid ACK (7) in DAP response") || \
        Text.contains("Error: Receiving data from device timed out") || \
        Text.contains("Error: attempted 'gdb' connection rejected"))
    {
        if (_isFailed == true)
            return;
        _isFailed = true;
        // killOpenOCD();
        emit openocdStartFailed();
        QMessageBox::warning(this, "Seer", "OpenOCD failed to start. \nCheck openOCD output for details.", QMessageBox::Ok);
    }
    _openocdLogsTabWidget->handleText(Text);
}
