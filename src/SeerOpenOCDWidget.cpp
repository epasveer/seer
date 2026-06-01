#include "SeerOpenOCDWidget.h"
#include <QtCore/QtCore>
#include <QtCore/QProcess>
#include <QMessageBox>
#include <QStringLiteral>
#include "SeerUtl.h"
/***********************************************************************************************************************
 * Constructor & Destructor                                                                                            *
 **********************************************************************************************************************/
SeerOpenOCDWidget::SeerOpenOCDWidget (QWidget* parent) : SeerLogWidget(parent) {
    Q_UNUSED(parent);
    _openocdProcess         = nullptr;
    _gdbLiveWatchProcess    = nullptr;
    _openocdLogsTabWidget   = nullptr;
    _telnetSocket           = nullptr;
    _liveWatchTimer         = new QTimer(this);
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
        connect(_openocdProcess, &QProcess::readyReadStandardOutput, this, &SeerOpenOCDWidget::handleReadOutput);
        connect(_openocdProcess, &QProcess::readyReadStandardError,  this, &SeerOpenOCDWidget::handleReadError);
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
        QFile resource(":/seer/resources/openocd-helpers.tcl");
        if (!resource.open(QIODevice::ReadOnly))
            return false;

        if (!_tempDir.isValid())
            return false;

        QString scriptPath = _tempDir.filePath("openocd-helpers.tcl");
        QFile out(scriptPath);
        if (!out.open(QIODevice::WriteOnly))
            return false;
        out.write(resource.readAll());
        out.close();


        QString final_cmd = command + QString(" -f %1").arg(scriptPath) + QString(" -c CDLiveWatchSetup");
        QStringList args = QProcess::splitCommand(final_cmd);
        _openocdProcess->start(openocdExe, args);
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
    if (_openocdLogsTabWidget) {
        delete _openocdLogsTabWidget;
        _openocdLogsTabWidget = nullptr;
    }
    terminateGdbLiveWatch();
    disconnect(this,    &SeerOpenOCDWidget::toEditorManagerWidget,  nullptr,    nullptr);
    disconnect(this,    &SeerOpenOCDWidget::toTracker,              nullptr,    nullptr);
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
 * Openocd GDB process, for Live watch                                                                                 *
 **********************************************************************************************************************/
bool SeerOpenOCDWidget::startGdbLiveWatch (const QString &gdbExe)
{
    if (!_gdbLiveWatchProcess) {
        _gdbLiveWatchProcess = new QProcess(this);
        _gdbLiveWatchProcess->start(gdbExe, QStringList() << "-q" << "--interpreter=mi");
    }
    connect(_liveWatchTimer, &QTimer::timeout, [this](){
        gdbLiveWatchRunCommand("-var-update-live-watch");
    });
    _liveWatchTimer->start(500);       // Hard code Refresh rate = 2Hz
    connect(_gdbLiveWatchProcess, &QProcess::readyReadStandardOutput,   this, &SeerOpenOCDWidget::handleGdbOutput);
    connect(_gdbLiveWatchProcess, &QProcess::readyReadStandardError,    this, &SeerOpenOCDWidget::handleGdbOutput);
    return true;
}

void SeerOpenOCDWidget::gdbLiveWatchRunCommand(const QString &cmd)
{
    if (_gdbLiveWatchProcess) {
        QString str = cmd + "\n";
        QByteArray bytes = str.toUtf8();
        _gdbLiveWatchProcess->write(bytes);
    }
}

void SeerOpenOCDWidget::terminateGdbLiveWatch()
{
    if (_gdbLiveWatchProcess) {
        _gdbLiveWatchProcess->terminate();
        _gdbLiveWatchProcess->waitForFinished();
        delete _gdbLiveWatchProcess;
        _gdbLiveWatchProcess = nullptr;
        _liveWatchTimer->stop();
    }
}

void SeerOpenOCDWidget::handleGdbOutput()
{
    QString output = _gdbLiveWatchProcess->readAllStandardOutput() + _gdbLiveWatchProcess->readAllStandardError();
    if (output.contains(QRegularExpression("^([0-9]+)\\^done,value="))) {
        // Format: 16^done,value="435"
        //       18^done,value="870"
        for (const QString& line : output.split('\n', Qt::SkipEmptyParts))
            if (line.contains("^done,value="))
                    emit toTracker(line);
    } else if (output.contains(QRegularExpression("^([0-9]+)\\^done,symbols="))) {
        // Throw to SeerEditorManagerWidget
        emit toEditorManagerWidget(output);
    }
}

void SeerOpenOCDWidget::handleText(const QString& text)
{
    if (text.startsWith("^done,DataExpressionAdded={") && text.endsWith("}"))
    {
        // Handle add variable to live watch
        // Format: ^done,DataExpressionAdded={id="18",expression="i1"}
        QString frame_text      = Seer::parseFirst(text,       "DataExpressionAdded=", '{', '}', false);
        QString id_text         = Seer::parseFirst(frame_text, "id=",                  '"', '"', false);
        QString expression_text = Seer::parseFirst(frame_text, "expression=",          '"', '"', false);
        gdbLiveWatchRunCommand("-var-create-live-watch " + expression_text + " " + id_text);
    }
    else if (text.startsWith("^done,DataExpressionDeleted={") && text.endsWith("}"))
    {
        // Handle delete variable to live watch
        // Format: ^done,DataExpressionDeleted={entry={id="20",expression="i2"}}
        QString frame_text      = Seer::parseFirst(text,       "DataExpressionDeleted=", '{', '}', false);
        QString id_text         = Seer::parseFirst(frame_text, "id=",                  '"', '"', false);
        QString expression_text = Seer::parseFirst(frame_text, "expression=",          '"', '"', false);
        gdbLiveWatchRunCommand("-var-delete-live-watch " + id_text);
    }
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
}

SeerLogWidget* SeerOpenOCDWidget::openocdConsole()
{
    return _openocdLogsTabWidget;
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
