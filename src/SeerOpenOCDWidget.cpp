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
    _telnetProcess = new QProcess(this);
    _openocdlogsTabWidget = nullptr;
    _isTelnetReady = false;
    _exceptionLevel = "";
}

SeerOpenOCDWidget::~SeerOpenOCDWidget (){
    _isTelnetReady = false;
    _exceptionLevel = "";
    killOpenOCD();
    killTelnet();
}

void SeerOpenOCDWidget::newOpenOCDWidget (){
    if (!_openocdProcess)
        _openocdProcess = new QProcess(this);
    if (!_telnetProcess)
        _telnetProcess = new QProcess(this);
    _openocdlogsTabWidget = nullptr;
}

QProcess* SeerOpenOCDWidget::openocdProcess()
{
    return _openocdProcess;
}

QProcess* SeerOpenOCDWidget::telnetProcess()
{
    return _telnetProcess;
}

void SeerOpenOCDWidget::setTelnetPort (const QString& port)
{
    _telnetPort = port;
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

void SeerOpenOCDWidget::killOpenOCD ()
{
    if (_openocdProcess)
    {
        if (_openocdProcess->state() == QProcess::Running) {
            _openocdProcess->kill();
             _openocdProcess->waitForFinished();
            delete _openocdProcess;
            _openocdProcess = nullptr;
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
 * Telnet process                                                                                                      *
 **********************************************************************************************************************/
bool SeerOpenOCDWidget::startTelnet (const QString &port)
{
    if (_telnetProcess->state() == QProcess::NotRunning) {
        _telnetProcess->start("telnet", {"localhost", port});
        QByteArray stdoutData = _telnetProcess->readAllStandardOutput();
        QByteArray stderrData = _telnetProcess->readAllStandardError();
        return true;
    }
    return false;
}

void SeerOpenOCDWidget::killTelnet ()
{
    if (_telnetProcess)
    {
        if (_telnetProcess->state() == QProcess::Running) {
            _telnetProcess->kill();
             _telnetProcess->waitForFinished();
            delete _telnetProcess;
            _telnetProcess = nullptr;
        }
    }
}

bool SeerOpenOCDWidget::isTelnetRunning ()
{
    if (_telnetProcess)
        if (_telnetProcess->state() == QProcess::Running) {
            return true;
        }
    return false;
}

qint64 SeerOpenOCDWidget::telnetExecuteCmd(const QString& cmd)
{
    qint64 bytesWritten = _telnetProcess->write((cmd + "\n").toUtf8());
    if (!_telnetProcess->waitForBytesWritten(1000)) {
        QMessageBox::warning(this, "Seer", QString("Failed to send telnet command."), QMessageBox::Ok, QMessageBox::Ok);
        return 0;
    }
    QByteArray stdoutData = _telnetProcess->readAllStandardOutput();
    QByteArray stderrData = _telnetProcess->readAllStandardError();
    if (_firstTelnetCmd == true)
    {
        _firstTelnetCmd = false;
        telnetExecuteCmd(cmd);          // call it again
        return bytesWritten;
    }
    QMessageBox::information(this, QString("Seer Telnet Port " + _telnetPort), QString("Telnet output: " + stdoutData + stderrData));
    return bytesWritten;
}
/***********************************************************************************************************************
 * Create a new console display for displaying openOCD log                                                             *
 **********************************************************************************************************************/
void SeerOpenOCDWidget::createOpenOCDConsole (QDetachTabWidget* parent)
{
    if (_openocdlogsTabWidget != nullptr) {
        return;
    }
    _openocdlogsTabWidget = new SeerLogWidget();
    parent->addTab(_openocdlogsTabWidget, "OpenOCD output");
    _openocdlogsTabWidget->setPlaceholderText("[OpenOCD output]");
    _openocdlogsTabWidget->setLogEnabled(true);
    connect(_openocdProcess, &QProcess::readyReadStandardOutput, this, &SeerOpenOCDWidget::handleReadOutput);
    connect(_openocdProcess, &QProcess::readyReadStandardError, this, &SeerOpenOCDWidget::handleReadError);
}

SeerLogWidget* SeerOpenOCDWidget::openocdConsole()
{
    return _openocdlogsTabWidget;
}

void SeerOpenOCDWidget::killConsole ()
{
    delete _openocdlogsTabWidget;
    _openocdlogsTabWidget = nullptr;
}

void SeerOpenOCDWidget::setConsoleVisible (bool flag)
{
    _openocdlogsTabWidget->setVisible(flag);
}

bool SeerOpenOCDWidget::isTelnetPortReady ()
{
    return _isTelnetReady;
}

void SeerOpenOCDWidget::setStopAtException(const QString& exceptionLevel)
{
    _exceptionLevel = exceptionLevel;
}

void SeerOpenOCDWidget::setOpenOCDTarget(const QString& target)
{
    _openOCDTarget = target;
}
/***********************************************************************************************************************
 * Slot                                                                                                                *
 **********************************************************************************************************************/
void SeerOpenOCDWidget::handleReadOutput ()
{
    QString Text = QString::fromLocal8Bit(_openocdProcess->readAllStandardOutput());
    _openocdlogsTabWidget->handleText(Text);
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
    if (Text.contains("Listening on port") && Text.contains("for telnet connections"))
    {
        _isTelnetReady = true;
        startTelnet(_telnetPort);
        if (_exceptionLevel != "")
        {
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
            telnetExecuteCmd(QString(_openOCDTarget + " catch_exc " + exceptionMap[_exceptionLevel]));
        }
    }
    _openocdlogsTabWidget->handleText(Text);
}
