#pragma once

#include "SeerConsoleWidget.h"
#include "SeerTildeEqualAmpersandLogWidget.h"
#include "SeerCaretAsteriskLogWidget.h"
#include "SeerBreakpointsBrowserWidget.h"
#include "SeerWatchpointsBrowserWidget.h"
#include "SeerCatchpointsBrowserWidget.h"
#include "SeerPrintpointsBrowserWidget.h"
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

        // Gdb settings.
        void                                setGdbProgram                       (const QString& program);
        QString                             gdbProgram                          () const;

        void                                setGdbArguments                     (const QString& arguments);
        QString                             gdbArguments                        () const;

        void                                setGdbAsyncMode                     (bool flag);
        bool                                gdbAsyncMode                        () const;

        void                                setConsoleMode                      (const QString& mode);
        QString                             consoleMode                         () const;

        void                                setManualCommands                   (const QStringList& commands);
        QStringList                         manualCommands                      (int count) const;

        void                                setRememberManualCommandCount       (int count);
        int                                 rememberManualCommandCount          () const;
        void                                clearManualCommandHistory           ();

        void                                setSourceAlternateDirectories       (const QStringList& alternateDirectories);
        const QStringList&                  sourceAlternateDirectories          () const;

        void                                setGdbOutputLogEnabled              (bool flag);
        bool                                isGdbOutputLogEnabled               () const;

        void                                setSeerOutputLogEnabled             (bool flag);
        bool                                isSeerOutputLogEnabled              () const;

        // Editor manager.
        SeerEditorManagerWidget*            editorManager                       ();
        const SeerEditorManagerWidget*      editorManager                       () const;

        void                                writeSettings                       ();
        void                                readSettings                        ();

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
        void                                handleGdbInterruptSIGINT            ();
        void                                handleGdbInterruptSIGKILL           ();
        void                                handleGdbInterruptSIGFPE            ();
        void                                handleGdbInterruptSIGSEGV           ();
        void                                handleGdbInterruptSIGUSR1           ();
        void                                handleGdbInterruptSIGUSR2           ();
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
        void                                handleGdbGenericpointList           ();
        void                                handleGdbBreakpointDelete           (QString breakpoints);
        void                                handleGdbBreakpointEnable           (QString breakpoints);
        void                                handleGdbBreakpointDisable          (QString breakpoints);
        void                                handleGdbBreakpointInsert           (QString breakpoint);
        void                                handleGdbBreakpointReload           (QStringList breakpointsText);
        void                                handleGdbWatchpointReload           (QStringList watchpointsText);
        void                                handleGdbCatchpointReload           (QStringList catchpointsText);
        void                                handleGdbWatchpointDelete           (QString watchpoints);
        void                                handleGdbWatchpointEnable           (QString watchpoints);
        void                                handleGdbWatchpointDisable          (QString watchpoints);
        void                                handleGdbWatchpointInsert           (QString watchpoint);
        void                                handleGdbCatchpointDelete           (QString catchpoints);
        void                                handleGdbCatchpointEnable           (QString catchpoints);
        void                                handleGdbCatchpointDisable          (QString catchpoints);
        void                                handleGdbCatchpointInsert           (QString catchpoint);
        void                                handleGdbPrintpointDelete           (QString breakpoints);
        void                                handleGdbPrintpointEnable           (QString breakpoints);
        void                                handleGdbPrintpointDisable          (QString breakpoints);
        void                                handleGdbPrintpointInsert           (QString printpoint);
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
        void                                handleGdbMemoryVisualizer           ();
        void                                handleGdbArrayAddExpression         (QString expression);
        void                                handleGdbArrayVisualizer            ();
        void                                handleSplitterMoved                 (int pos, int index);
        void                                handleManualCommandChanged          ();
        void                                handleLogOuputChanged               ();

        void                                handleGdbProcessFinished            (int exitCode, QProcess::ExitStatus exitStatus);
        void                                handleGdbProcessErrored             (QProcess::ProcessError errorStatus);

    signals:
        void                                stoppingPointReached                ();

    protected:

    private:
        bool                                isGdbRuning                         () const;
        void                                startGdb                            ();
        void                                killGdb                             ();
        void                                createConsole                       ();
        void                                deleteConsole                       ();
        void                                sendGdbInterrupt                    (int signal);

        QString                             _gdbProgram;
        QString                             _gdbArguments;
        bool                                _gdbASyncMode;
        QString                             _executableName;
        QString                             _executableArguments;
        QString                             _executableWorkingDirectory;
        int                                 _executablePid;
        QString                             _executableHostPort;
        QString                             _executableCoreFilename;
        QString                             _executableLaunchMode;
        bool                                _newExecutableFlag;
        int                                 _currentFrame;

        SeerConsoleWidget*                  _consoleWidget;
        QString                             _consoleMode;
        int                                 _rememberManualCommandCount;
        SeerBreakpointsBrowserWidget*       _breakpointsBrowserWidget;
        SeerWatchpointsBrowserWidget*       _watchpointsBrowserWidget;
        SeerCatchpointsBrowserWidget*       _catchpointsBrowserWidget;
        SeerPrintpointsBrowserWidget*       _printpointsBrowserWidget;
        SeerTildeEqualAmpersandLogWidget*   _gdbOutputLog;
        SeerCaretAsteriskLogWidget*         _seerOutputLog;

        GdbMonitor*                         _gdbMonitor;
        QProcess*                           _gdbProcess;

        QVector<int>                        _dataExpressionId;
        QVector<QString>                    _dataExpressionName;
};

