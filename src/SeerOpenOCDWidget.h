#pragma once
#include <QtCore/QString>
#include <QtCore/QtCore>
#include <QtCore/QProcess>
#include <QtWidgets/QWidget>
#include <QTcpSocket>
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
        // Start & kill Telnet process
        bool                                startTelnet                     (const QString &port);
        void                                terminateTelnet                 ();
        qint64                              telnetRunCmd                    (const QString &command);
        // Create & kill Console displaying OpenOCD process logs
        void                                createOpenOCDConsole            (QDetachTabWidget* parent);
        void                                killConsole                     ();
        void                                setConsoleVisible               (bool flag);
        // Getters & Setters
        SeerLogWidget*                      openocdConsole                  ();

    signals:
        void                                openocdStartFailed              ();

    private slots:
        void                                handleReadOutput                ();
        void                                handleReadError                 ();

    private:
        QProcess*                           _openocdProcess;
        QTcpSocket*                         _telnetSocket;
        SeerLogWidget*                      _openocdLogsTabWidget;
        QString                             _telnetPort;
        
};