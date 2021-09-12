#pragma once

#include "SeerConsoleWidget.h"
#include "SeerTildeEqualAmpersandLogWidget.h"
#include "SeerCaretAsteriskLogWidget.h"
#include "SeerBreakpointsBrowserWidget.h"
#include "SeerWatchpointsBrowserWidget.h"
#include "GdbMonitor.h"
#include <QtCore/QProcess>
#include <QtCore/QVector>
#include <QtWidgets/QWidget>

#include "ui_SeerGdbWidget.h"

class SeerGdbWidget : public QWidget, protected Ui::SeerGdbWidgetForm {

    Q_OBJECT

    public:
        explicit SeerGdbWidget (QWidget* parent = 0);
       ~SeerGdbWidget ();

        GdbMonitor*                         gdbMonitor                          ();
        QProcess*                           gdbProcess                          ();

        void                                setExecutableName                   (const QString& executableName);
        const QString&                      executableName                      () const;

        void                                setNewExecutableFlag                (bool flag);
        bool                                newExecutableFlag                   () const;

        void                                setExecutableArguments              (const QString& executableArguments);
        const QString&                      executableArguments                 () const;

        void                                setExecutableWorkingDirectory       (const QString& executableWorkingDirectory);
        const QString&                      executableWorkingDirectory          () const;

        void                                setExecutablePid                    (int pid);
        int                                 executablePid                       () const;

        void                                setExecutableHostPort               (const QString& hostPort);
        const QString&                      executableHostPort                  () const;

        void                                setExecutableCoreFilename           (const QString& coreFilename);
        const QString&                      executableCoreFilename              () const;

        void                                setExecutableLaunchMode             (const QString& launchMode);
        const QString&                      executableLaunchMode                () const;

    public slots:
        void                                handleText                          (const QString& text);
        void                                handleExecute                       ();
        void                                handleGdbCommand                    (const QString& command);
        void                                handleGdbExit                       ();
        void                                handleGdbRunExecutable              ();
        void                                handleGdbStartExecutable            ();
        void                                handleGdbAttachExecutable           ();
        void                                handleGdbConnectExecutable          ();
        void                                handleGdbCoreFileExecutable         ();
        void                                handleGdbRunToLine                  (QString fullname, int lineno);
        void                                handleGdbNext                       ();
        void                                handleGdbStep                       ();
        void                                handleGdbFinish                     ();
        void                                handleGdbContinue                   ();
        void                                handleGdbInterrupt                  ();
        void                                handleGdbExecutableSources          ();
        void                                handleGdbExecutableSharedLibraries  ();
        void                                handleGdbExecutableName             ();
        void                                handleGdbExecutableArguments        ();
        void                                handleGdbExecutableWorkingDirectory ();
        void                                handleGdbTtyDeviceName              ();
        void                                handleGdbStackListFrames            ();
        void                                handleGdbStackSelectFrame           (int frameno);
        void                                handleGdbStackListLocals            ();
        void                                handleGdbStackListArguments         ();
        void                                handleGdbBreakpointWatchpointList   ();
        void                                handleGdbBreakpointDelete           (QString breakpoints);
        void                                handleGdbBreakpointEnable           (QString breakpoints);
        void                                handleGdbBreakpointDisable          (QString breakpoints);
        void                                handleGdbBreakpointInsert           (QString breakpoint);
        void                                handleGdbBreakpointReload           (QStringList breakpointsText);
        void                                handleGdbWatchpointDelete           (QString watchpoints);
        void                                handleGdbWatchpointEnable           (QString watchpoints);
        void                                handleGdbWatchpointDisable          (QString watchpoints);
        void                                handleGdbWatchpointInsert           (QString watchpoint);
        void                                handleGdbThreadListIds              ();
        void                                handleGdbThreadListFrames           ();
        void                                handleGdbThreadSelectId             (int threadid);
        void                                handleGdbRegisterListNames          ();
        void                                handleGdbRegisterListValues         ();
        void                                handleGdbDataEvaluateExpression     (int expressionid, QString expression);
        void                                handleGdbDataListValues             ();
        void                                handleGdbDataListExpressions        ();
        void                                handleGdbDataAddExpression          (QString expression);
        void                                handleGdbDataDeleteExpressions      (QString expressionids);
        void                                handleGdbMemoryAddExpression        (QString expression);
        void                                handleGdbMemoryEvaluateExpression   (int expressionid, QString address, int count);

        void                                handleFinished                      (int exitCode, QProcess::ExitStatus exitStatus);

    signals:
        void                                stoppingPointReached                ();

    private:
        bool                                isGdbRuning                         () const;
        void                                startGdb                            ();
        void                                killGdb                             ();
        void                                createConsole                       ();
        void                                deleteConsole                       ();


        QString                             _executableName;
        QString                             _executableArguments;
        QString                             _executableWorkingDirectory;
        int                                 _executablePid;
        QString                             _executableHostPort;
        QString                             _executableCoreFilename;
        QString                             _executableLaunchMode;
        bool                                _newExecutableFlag;

        SeerConsoleWidget*                  _consoleWidget;
        SeerBreakpointsBrowserWidget*       _breakpointsBrowserWidget;
        SeerWatchpointsBrowserWidget*       _watchpointsBrowserWidget;
        SeerTildeEqualAmpersandLogWidget*   _gdbOutputLog;
        SeerCaretAsteriskLogWidget*         _seerOutputLog;

        GdbMonitor*                         _gdbMonitor;
        QProcess*                           _gdbProcess;

        QVector<int>                        _dataExpressionId;
        QVector<QString>                    _dataExpressionName;
};

