#pragma once

#include "SeerConsoleWidget.h"
#include "SeerEditorWidgetSource.h"
#include "SeerGdbLogWidget.h"
#include "SeerSeerLogWidget.h"
#include "SeerMessagesBrowserWidget.h"
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

        void                                setExecutableSymbolName             (const QString& executableSymbolName);
        const QString&                      executableSymbolName                () const;

        void                                setNewExecutableFlag                (bool flag);
        bool                                newExecutableFlag                   () const;

        void                                setExecutableArguments              (const QString& executableArguments);
        const QString&                      executableArguments                 () const;

        void                                setExecutableWorkingDirectory       (const QString& executableWorkingDirectory);
        const QString&                      executableWorkingDirectory          () const;

        void                                setExecutableBreakpointsFilename    (const QString& breakpointsFilename);
        const QString&                      executableBreakpointsFilename       () const;

        void                                setExecutableBreakpointFunctionName (const QString& nameoraddress);
        const QString&                      executableBreakpointFunctionName    () const;

        void                                setExecutableBreakpointSourceName   (const QString& sourceFilenameAndLineno);
        const QString&                      executableBreakpointSourceName      () const;

        void                                setExecutablePid                    (int pid);
        int                                 executablePid                       () const;

        void                                setExecutableConnectHostPort        (const QString& connectHostPort);
        const QString&                      executableConnectHostPort           () const;

        void                                setExecutableRRTraceDirectory       (const QString& rrTraceDirectory);
        const QString&                      executableRRTraceDirectory          () const;

        void                                setExecutableCoreFilename           (const QString& coreFilename);
        const QString&                      executableCoreFilename              () const;

        void                                setExecutablePreGdbCommands         (const QStringList& preGdbCommands);
        const QStringList&                  executablePreGdbCommands            () const;

        void                                setExecutablePostGdbCommands        (const QStringList& postGdbCommands);
        const QStringList&                  executablePostGdbCommands           () const;

        void                                setExecutableLaunchMode             (const QString& launchMode);
        const QString&                      executableLaunchMode                () const;
        const QString&                      executableBreakMode                 () const;

        // Gdb settings.
        void                                setGdbProgram                       (const QString& program);
        QString                             gdbProgram                          () const;

        void                                setGdbArguments                     (const QString& arguments);
        QString                             gdbArguments                        () const;

        void                                setGdbProgramOverride               (const QString& program);
        QString                             gdbProgramOverride                  () const;

        void                                setGdbArgumentsOverride             (const QString& arguments);
        QString                             gdbArgumentsOverride                () const;

        void                                setGdbAsyncMode                     (bool flag);
        bool                                gdbAsyncMode                        () const;

        void                                setGdbNonStopMode                   (bool flag);
        bool                                gdbNonStopMode                      () const;

        void                                setGdbServerDebug                   (bool flag);
        bool                                gdbServerDebug                      () const;

        void                                setGdbHandleTerminatingException    (bool flag);
        bool                                gdbHandleTerminatingException       () const;

        void                                setGdbRandomizeStartAddress         (bool flag);
        bool                                gdbRandomizeStartAddress            () const;

        void                                setGdbEnablePrettyPrinting          (bool flag);
        bool                                gdbEnablePrettyPrinting             () const;

        void                                setGdbRemoteTargetType              (const QString& type);
        QString                             gdbRemoteTargetType                 () const;

        void                                setGdbRecordMode                    (const QString& mode);
        QString                             gdbRecordMode                       () const;

        void                                setGdbRecordDirection               (const QString& direction);
        QString                             gdbRecordDirection                  () const;

        void                                setConsoleMode                      (const QString& mode);
        QString                             consoleMode                         () const;

        void                                setConsoleScrollLines               (int count);
        int                                 consoleScrollLines                  () const;

        void                                setManualCommands                   (const QStringList& commands);
        QStringList                         manualCommands                      (int count) const;

        void                                setRememberManualCommandCount       (int count);
        int                                 rememberManualCommandCount          () const;
        void                                clearManualCommandHistory           ();

        void                                setSourceAlternateDirectories       (const QStringList& alternateDirectories);
        const QStringList&                  sourceAlternateDirectories          () const;

        void                                setSourceMiscFilePatterns           (const QStringList& filePatterns);
        const QStringList&                  sourceMiscFilePatterns              () const;

        void                                setSourceSourceFilePatterns         (const QStringList& filePatterns);
        const QStringList&                  sourceSourceFilePatterns            () const;

        void                                setSourceHeaderFilePatterns         (const QStringList& filePatterns);
        const QStringList&                  sourceHeaderFilePatterns            () const;

        void                                setSourceIgnoreFilePatterns         (const QStringList& filePatterns);
        const QStringList&                  sourceIgnoreFilePatterns            () const;

        void                                setAssemblyShowAssemblyTabOnStartup (bool flag);
        bool                                assemblyShowAssemblyTabOnStartup    () const;

        void                                setAssemblyKeepAssemblyTabOnTop     (bool flag);
        bool                                assemblyKeepAssemblyTabOnTop        () const;

        void                                setAssemblyDisassemblyFlavor        (const QString& flavor);
        QString                             assemblyDisassemblyFlavor           () const;

        void                                setAssemblySymbolDemagling          (const QString& onoff);
        QString                             assemblySymbolDemagling             () const;

        void                                setAssemblyShowAddressColumn        (bool flag);
        bool                                assemblyShowAddressColumn           () const;

        void                                setAssemblyShowOffsetColumn         (bool flag);
        bool                                assemblyShowOffsetColumn            () const;

        void                                setAssemblyShowOpcodeColumn         (bool flag);
        bool                                assemblyShowOpcodeColumn            () const;

        void                                setAssemblyShowSourceLines          (bool flag);
        bool                                assemblyShowSourceLines             () const;

        void                                setAssemblyRegisterFormat           (const QString& format);
        QString                             assemblyRegisterFormat              () const;

        void                                setAssemblyDisassemblyMode          (const QString& mode, int bytes);
        QString                             assemblyDisassemblyMode             () const;
        int                                 assemblyDisassemblyBytes            () const;

        void                                setGdbOutputLogEnabled              (bool flag);
        bool                                isGdbOutputLogEnabled               () const;

        void                                setGdbOutputLogTimeStampEnabled     (bool flag);
        bool                                isGdbOutputLogTimeStampEnabled      () const;

        void                                setSeerOutputLogEnabled             (bool flag);
        bool                                isSeerOutputLogEnabled              () const;

        void                                setSeerOutputLogTimeStampEnabled    (bool flag);
        bool                                isSeerOutputLogTimeStampEnabled     () const;

        // RR settings.
        void                                setRRProgram                        (const QString& program);
        QString                             rrProgram                           () const;

        void                                setRRArguments                      (const QString& arguments);
        QString                             rrArguments                         () const;

        void                                setRRGdbArguments                   (const QString& arguments);
        QString                             rrGdbArguments                      () const;

        // Editor manager.
        SeerEditorManagerWidget*            editorManager                       ();
        const SeerEditorManagerWidget*      editorManager                       () const;

        // Messages
        void                                addMessage                          (const QString& message, QMessageBox::Icon messageType);

        // Settings
        void                                writeSettings                       ();
        void                                readSettings                        ();

    public slots:
        void                                handleLogsTabMoved                  (int to, int from);
        void                                handleLogsTabChanged                (int index);
        void                                handleRaiseMessageTab               ();

        void                                handleText                          (const QString& text);
        void                                handleManualCommandExecute          ();
        void                                handleGdbCommand                    (const QString& command);
        void                                handleGdbExit                       ();
        void                                handleGdbRunExecutable              (const QString& breakMode);
        void                                handleGdbAttachExecutable           ();
        void                                handleGdbConnectExecutable          ();
        void                                handleGdbRRExecutable               ();
        void                                handleGdbCoreFileExecutable         ();
        void                                handleGdbShutdown                   ();
        void                                handleGdbRunToLine                  (QString fullname, int lineno);
        void                                handleGdbRunToAddress               (QString address);
        void                                handleGdbNext                       ();
        void                                handleGdbNexti                      ();
        void                                handleGdbStep                       ();
        void                                handleGdbStepi                      ();
        void                                handleGdbFinish                     ();
        void                                handleGdbContinue                   ();
        void                                handleGdbRecordStart                ();
        void                                handleGdbRecordStop                 ();
        void                                handleGdbRecordForward              ();
        void                                handleGdbRecordReverse              ();
        void                                handleGdbRecordStartStopToggle      ();
        void                                handleGdbRecordDirectionToggle      ();
        void                                handleGdbInterrupt                  ();
        void                                handleGdbInterruptSIGINT            ();
        void                                handleGdbInterruptSIGKILL           ();
        void                                handleGdbInterruptSIGFPE            ();
        void                                handleGdbInterruptSIGSEGV           ();
        void                                handleGdbInterruptSIGUSR1           ();
        void                                handleGdbInterruptSIGUSR2           ();

        void                                handleGdbRunThreadGroup             (QString threadGroup);
        void                                handleGdbStartThreadGroup           (QString threadGroup);
        void                                handleGdbContinueThreadGroup        (QString threadGroup);
        void                                handleGdbInterruptThreadGroup       (QString threadGroup);
        void                                handleGdbNextThreadId               (int threadid);
        void                                handleGdbStepThreadId               (int threadid);
        void                                handleGdbFinishThreadId             (int threadid);
        void                                handleGdbContinueThreadId           (int threadid);
        void                                handleGdbInterruptThreadId          (int threadid);

        void                                handleGdbExecutableSources          ();
        void                                handleGdbExecutableFunctions        (int id, const QString& functionRegex);
        void                                handleGdbExecutableTypes            (int id, const QString& typeRegex);
        void                                handleGdbExecutableVariables        (int id, const QString& variableNameRegex, const QString& variableTypeRegex);
        void                                handleGdbExecutableLibraries        ();
        void                                handleGdbExecutableName             ();
        void                                handleGdbExecutableArguments        ();
        void                                handleGdbExecutableWorkingDirectory ();
        void                                handleGdbExecutableLoadBreakpoints  ();
        void                                handleGdbExecutablePreCommands      ();
        void                                handleGdbExecutablePostCommands     ();
        void                                handleGdbTtyDeviceName              ();
        void                                handleGdbStackListFrames            ();
        void                                handleGdbStackSelectFrame           (int frameno);
        void                                handleGdbStackListLocals            ();
        void                                handleGdbStackListArguments         ();
        void                                handleGdbGenericpointList           ();
        void                                handleGdbBreakpointDelete           (QString breakpoints);
        void                                handleGdbBreakpointEnable           (QString breakpoints);
        void                                handleGdbBreakpointDisable          (QString breakpoints);
        void                                handleGdbBreakpointInfo             (int breakpointid, QString breakpoint);
        void                                handleGdbBreakpointInsert           (QString breakpoint);
        void                                handleGdbBreakpointCondition        (QString breakpoint, QString condition);
        void                                handleGdbBreakpointIgnore           (QString breakpoint, QString count);
        void                                handleGdbBreakpointCommand          (QString breakpoint, QString commands);
        void                                handleGdbBreakpointCommands         (QString breakpoint, QStringList commands);
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
        void                                handleGdbPrintpointInsert           (QString type, QString function, QString channel, QString parameters);
        void                                handleGdbThreadListFrames           ();
        void                                handleGdbThreadListIds              ();
        void                                handleGdbThreadListGroups           ();
        void                                handleGdbThreadSelectId             (int threadid);
        void                                handleGdbAdaListTasks               ();
        void                                handleGdbAdaListExceptions          ();
        void                                handleGdbListSkips                  ();
        void                                handleGdbDeleteSkips                (QString skipids);
        void                                handleGdbEnableSkips                (QString skipids);
        void                                handleGdbDisableSkips               (QString skipids);
        void                                handleGdbRegisterListNames          ();
        void                                handleGdbRegisterListValues         (QString fmt);
        void                                handleGdbRegisterSetValue           (QString fmt, QString name, QString value);
        void                                handleGdbDataEvaluateExpression     (int expressionid, QString expression);
        void                                handleGdbVarObjCreate               (int expressionid, QString expression);
        void                                handleGdbVarObjListChildren         (int expressionid, QString objname);
        void                                handleGdbVarObjUpdate               (int expressionid, QString objname);
        void                                handleGdbVarObjAssign               (int expressionid, QString objname, QString value);
        void                                handleGdbVarObjDelete               (int expressionid, QString objname);
        void                                handleGdbVarObjAttributes           (int objid,        QString objname);
        void                                handleGdbDataListValues             ();
        void                                handleGdbDataListExpressions        ();
        void                                handleGdbDataAddExpression          (QString expression);
        void                                handleGdbDataDeleteExpressions      (QString expressionids);
        void                                handleGdbMemoryAddExpression        (QString expression);
        void                                handleGdbArrayAddExpression         (QString expression);
        void                                handleGdbStructAddExpression        (QString expression);
        void                                handleGdbVarAddExpression           (QString expression);
        void                                handleGdbImageAddExpression         (QString expression);
        void                                handleGdbMemoryEvaluateExpression   (int expressionid, QString address, int count);
        void                                handleGdbMemoryEvaluateExpression   (int expressionid, QString address, int offset, int count);
        void                                handleGdbAsmEvaluateExpression      (int expressionid, QString address, int count, int mode);
        void                                handleGdbArrayEvaluateExpression    (int expressionid, QString address, int count);
        void                                handleGdbGetAssembly                (QString address);
        void                                handleGdbGetSourceAndAssembly       (QString address);
        void                                handleGdbMemoryVisualizer           ();
        void                                handleGdbArrayVisualizer            ();
        void                                handleGdbStructVisualizer           ();
        void                                handleGdbVarVisualizer              ();
        void                                handleGdbImageVisualizer            ();
        void                                handleSplitterMoved                 (int pos, int index);
        void                                handleManualCommandChanged          ();
        void                                handleLogOuputChanged               ();
        void                                handleGdbLoadBreakpoints            ();
        void                                handleGdbSaveBreakpoints            ();
        void                                handleHelpToolButtonClicked         ();
        void                                handleGdbAssemblyDisassemblyFlavor  ();
        void                                handleGdbAssemblySymbolDemangling   ();
        void                                handleGdbSchedulerLockingMode       (QString mode);
        void                                handleGdbScheduleMultipleMode       (QString mode);
        void                                handleGdbForkFollowMode             (QString mode);
        void                                handleGdbSourceScripts              ();

        void                                handleGdbProcessFinished            (int exitCode, QProcess::ExitStatus exitStatus);
        void                                handleGdbProcessErrored             (QProcess::ProcessError errorStatus);

        void                                handleConsoleModeChanged            ();
        void                                handleAboutToQuit                   ();

    signals:
        void                                stoppingPointReached                ();
        void                                changeWindowTitle                   (QString title);
        void                                assemblyConfigChanged               ();
        void                                recordSettingsChanged               ();

    protected:
        void                                writeLogsSettings                   ();
        void                                readLogsSettings                    ();

    private:
        bool                                isQuitting                          () const;
        void                                setIsQuitting                       (bool f);

        bool                                isGdbRuning                         () const;
        bool                                startGdb                            ();
        bool                                startGdbRR                          ();
        void                                killGdb                             ();
        void                                createConsole                       ();
        void                                deleteConsole                       ();
        void                                connectConsole                      ();
        void                                disconnectConsole                   ();
        void                                reattachConsole                     ();
        SeerConsoleWidget*                  console                             ();
        void                                sendGdbInterrupt                    (int signal);

        bool                                _isQuitting;
        QString                             _gdbProgram;
        QString                             _gdbArguments;
        QString                             _gdbProgramOverride;
        QString                             _gdbArgumentsOverride;
        QString                             _gdbRRProgram;
        QString                             _gdbRRArguments;
        QString                             _gdbRRGdbArguments;
        bool                                _gdbASyncMode;
        bool                                _gdbNonStopMode;
        bool                                _gdbServerDebug;
        bool                                _gdbHandleTerminatingException;
        bool                                _gdbRandomizeStartAddress;
        bool                                _gdbEnablePrettyPrinting;
        QString                             _gdbRecordMode;
        QString                             _gdbRecordDirection;
        QString                             _gdbRemoteTargetType;
        bool                                _assemblyShowAssemblyTabOnStartup;
        QString                             _assemblyDisassemblyFlavor;
        QString                             _assemblySymbolDemangling;
        QString                             _assemblyRegisterFormat;
        QString                             _assemblyDisassemblyMode;
        int                                 _assemblyDisassemblyBytes;

        QString                             _executableName;
        QString                             _executableSymbolName;
        QString                             _executableArguments;
        QString                             _executableWorkingDirectory;
        QString                             _executableBreakpointsFilename;
        QString                             _executableBreakpointFunctionName;
        QString                             _executableBreakpointSourceName;
        int                                 _executablePid;
        QString                             _executableConnectHostPort;
        QString                             _executableRRTraceDirectory;
        QString                             _executableCoreFilename;
        QString                             _executableLaunchMode;
        QString                             _executableBreakMode;
        QStringList                         _executablePreGdbCommands;
        QStringList                         _executablePostGdbCommands;
        bool                                _newExecutableFlag;
        int                                 _currentFrame;

        SeerConsoleWidget*                  _consoleWidget;
        int                                 _consoleIndex;
        QString                             _consoleMode;
        int                                 _consoleScrollLines;
        int                                 _rememberManualCommandCount;
        SeerMessagesBrowserWidget*          _messagesBrowserWidget;
        SeerBreakpointsBrowserWidget*       _breakpointsBrowserWidget;
        SeerWatchpointsBrowserWidget*       _watchpointsBrowserWidget;
        SeerCatchpointsBrowserWidget*       _catchpointsBrowserWidget;
        SeerPrintpointsBrowserWidget*       _printpointsBrowserWidget;
        SeerGdbLogWidget*                   _gdbOutputLog;
        SeerSeerLogWidget*                  _seerOutputLog;

        GdbMonitor*                         _gdbMonitor;
        QProcess*                           _gdbProcess;

        QVector<int>                        _dataExpressionId;
        QVector<QString>                    _dataExpressionName;

        QStringList                         _ignoreFilePatterns;
};

