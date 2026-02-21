#pragma once
#include <QtCore/QString>
#include <QtCore/QtCore>
#include <QtCore/QProcess>
#include <QtWidgets/QWidget>
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
        void                                newOpenOCDWidget                ();
        void                                setTelnetPort                   (const QString& port);
        // Start & kill OpenOCD process 
        bool                                startOpenOCD                    (const QString &openocdExe, const QString &command);
        void                                killOpenOCD                     ();
        bool                                isOpenocdRunning                ();
        // Start & kill Telnet process  
        bool                                startTelnet                     (const QString &port);
        void                                killTelnet                      ();
        bool                                isTelnetRunning                 ();
        bool                                isTelnetPortReady               ();
        // Create & kill Console displaying OpenOCD process logs    
        void                                createOpenOCDConsole            (QDetachTabWidget* parent);
        void                                killConsole                     ();
        void                                setConsoleVisible               (bool flag);
        // Getters & Setters
        SeerLogWidget*                      openocdConsole                  ();
        SeerOpenOCDWidget*                  getOpenOCDWidget                ();
        QProcess*                           openocdProcess                  ();
        QProcess*                           telnetProcess                   ();
        // Exception level create
        qint64                              telnetExecuteCmd                (const QString& cmd);

    signals:
        void                                openocdDisconnect               ();
        void                                openocdStartFailed              ();

    private slots:
        void                                handleReadOutput                ();
        void                                handleReadError                 ();

    private:
        QProcess*                           _openocdProcess;
        QProcess*                           _telnetProcess;
        SeerLogWidget*                      _openocdlogsTabWidget;
        bool                                _isTelnetReady;
        QString                             _telnetPort;
        QString                             _openOCDTarget;
        bool                                _isFailed                       = false;
        bool                                _firstTelnetCmd                 = true;
};