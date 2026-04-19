#include "SeerOpenOCDWidget.h"
#include <QtCore/QtCore>
#include <QtCore/QProcess>
#include <QMessageBox>
#include <QStringLiteral>
#include <iostream>
using namespace std;
/***********************************************************************************************************************
 * Constructor & Destructor                                                                                            *
 **********************************************************************************************************************/
SeerOpenOCDWidget::SeerOpenOCDWidget (QWidget* parent) : SeerLogWidget(parent) {
    Q_UNUSED(parent);
    _openocdProcess         = nullptr;
    _openocdLogsTabWidget   = nullptr;
    _telnetSocket           = nullptr;
}

SeerOpenOCDWidget::~SeerOpenOCDWidget (){
    terminate();
}
/***********************************************************************************************************************
 * OpenOCD process                                                                                                     *
 **********************************************************************************************************************/
bool SeerOpenOCDWidget::startOpenOCD (const QString &openocdExe, const QString &command)
{
    if (!_openocdProcess) {
        _openocdProcess = new QProcess(this);
    }
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
 * Telnet process                                                                                                      *
 **********************************************************************************************************************/
bool SeerOpenOCDWidget::startTelnet(const QString &port)
{
    if (_telnetSocket) {
        return false;
    }

    _telnetSocket = new QTcpSocket(this);
    _telnetPort = port;

    _telnetSocket->connectToHost("127.0.0.1", port.toUInt());

    if (!_telnetSocket->waitForConnected(2000)) {
        qWarning() << "Failed to connect to OpenOCD telnet port:"
                   << _telnetSocket->errorString();
        delete _telnetSocket;
        _telnetSocket = nullptr;
        return false;
    }
    return true;
}

void SeerOpenOCDWidget::terminateTelnet()
{
    if (_telnetSocket) {
        _telnetSocket->disconnectFromHost();

        if (_telnetSocket->state() != QAbstractSocket::UnconnectedState) {
            _telnetSocket->waitForDisconnected(1000);
        }

        delete _telnetSocket;
        _telnetSocket = nullptr;
    }
}

qint64 SeerOpenOCDWidget::telnetRunCmd(const QString& cmd)
{
    if (!_telnetSocket) {
        QMessageBox::warning(this, "Seer", "Socket not connected.");
        return 0;
    }

    // 1. Clear any old data
    _telnetSocket->readAll();

    // 2. Send command
    QByteArray data = (cmd + "\n").toUtf8();
    qint64 bytesWritten = _telnetSocket->write(data);

    if (!_telnetSocket->waitForBytesWritten(1000)) {
        QMessageBox::warning(this, "Seer", "Write failed.");
        return 0;
    }

    // 3. Wait until OpenOCD finishes and returns prompt ">"
    QByteArray response;
    int timeout = 2000;

    while (timeout > 0) {
        if (_telnetSocket->waitForReadyRead(200)) {
            response += _telnetSocket->readAll();

            if (response.contains("\n>") || response.endsWith("> ")) {
                break;
            }
        }
        timeout -= 200;
    }

    qDebug() << "Response:" << response;

    return bytesWritten;
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
        emit openocdStartFailed();
        QMessageBox::warning(this, "Seer", "OpenOCD failed to start. \nCheck openOCD output for details.", QMessageBox::Ok);
    }
    _openocdLogsTabWidget->handleText(Text);
}
