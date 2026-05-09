#pragma once
#include <QtCore/QString>
#include <QtCore/QtCore>
#include <QtCore/QProcess>
#include <QtWidgets/QWidget>
#include <QTcpSocket>
#include <QTimer>
#include "QDetachTabWidget.h"
#include "SeerLogWidget.h"
/***********************************************************************************************************************
 * 
 **********************************************************************************************************************/
class SeerOpenOCDWidget: public SeerLogWidget{
    Q_OBJECT
    public:
        explicit SeerOpenOCDWidget          (QWidget* parent = 0);
        ~SeerOpenOCDWidget                  ();
        // Start & kill OpenOCD process 
        bool                                startOpenOCD                    (const QString &openocdExe, const QString &command);
        void                                terminate                       ();
        bool                                isOpenocdRunning                ();
        // Start & kill GDB process for live watch
        bool                                startGdbLiveWatch               (const QString &gdbExe);
        void                                gdbLiveWatchRunCommand          (const QString &cmd);
        void                                terminateGdbLiveWatch           ();
        // Start & kill Telnet process
        bool                                startTelnet                     (const QString &port);
        void                                terminateTelnet                 ();
        qint64                              telnetRunCmd                    (const QString &command);
        // Create & kill Console displaying OpenOCD process logs
        void                                createOpenOCDConsole            (QDetachTabWidget* parent);
        void                                setConsoleVisible               (bool flag);
        // Getters & Setters
        SeerLogWidget*                      openocdConsole                  ();

    signals:
        void                                openocdStartFailed              ();
        void                                toTracker                       (const QString& text);

    private slots:
        void                                handleReadOutput                ();
        void                                handleReadError                 ();
        void                                handleGdbOutput                 ();
    
    public slots:
        void                                handleText                      (const QString& text);

    private:
        QProcess*                           _openocdProcess;
        QProcess*                           _gdbLiveWatchProcess;
        QTcpSocket*                         _telnetSocket;
        SeerLogWidget*                      _openocdLogsTabWidget;
        QString                             _telnetPort;
        QTemporaryDir                       _tempDir;
        QTimer*                             _liveWatchTimer;
};