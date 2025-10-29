// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

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
#include "SeerCheckpointsBrowserWidget.h"
#include "GdbMonitor.h"
#include <QtCore/QProcess>
#include <QtCore/QVector>
#include <QtWidgets/QWidget>
#include <QThread>
#include <QMutex>
#include <QWaitCondition>
#include <QMap>
#include <tuple>

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

        // Gdb
        bool                                isGdbRuning                         () const;
        void                                restoreLaunchMode                   ();
        void                                saveLaunchMode                      ();
        bool                                hasBackupLaunchMode                 () const;
        void                                clearBackupLaunchMode               ();
        const QString&                      backupLaunchMode                    () const;

        // OpenOCD
        SeerOpenOCDWidget*                  openOCDWidgetInstance               ();
        // ::Main
        const QString&                      openOCDExePath                      ();
        void                                setOpenOCDExePath                   (const QString& path);
        const QString&                      openOCDCommand                      ();
        void                                setOpenOCDCommand                   (const QString& command);
        // ::GDB Multiarch
        const QString&                      gdbMultiarchExePath                 ();
        void                                setGdbMultiarchExePath              (const QString& path);
        const QString&                      gdbPort                             ();
        void                                setGdbPort                          (const QString& port);
        const QString&                      telnetPort                          ();
        void                                setTelnetPort                       (const QString& port);
        const QString&                      gdbMultiarchCommand                 ();
        void                                setGdbMultiarchCommand              (const QString& command);
        bool                                isGdbMultiarchIsStopAtTempFunc        ();
        void                                setGdbMultiarchStopAtTempFunc     (bool check);
        const QString                       gdbMultiarchStopAtFunc              ();
        void                                setGdbMultiarchStopAtFunc           (const QString& func);
        bool                                isGdbMultiarchStopAtException       ();
        void                                setGdbMultiarchStopAtExeption  (bool check);
        const QString                       gdbMultiarchExeptionLevelToStop     ();
        void                                setGdbMultiarchExeptionLevelToStop  (const QString& level);
        const QString                       openOCDTarget                       ();
        void                                setOpenOCDTarget                    (const QString& target);
        // ::Docker
        bool                                isBuiltInDocker                     ();
        void                                setBuiltInDocker                    (bool check);
        const QString                       absoluteBuildFolderPath             ();
        void                                setAbsoluteBuildFolderPath          (const QString& path);
        const QString                       dockerBuildFolderPath               ();
        void                                setDockerBuildFolderPath            (const QString& path);

        // ::Symbol Files
        void                                setSymbolFiles                      (const QMap<QString, std::tuple<QString, bool, QString>>& symbolFiles);
        const QMap<QString, std::tuple<QString, bool, QString>>     symbolFiles (void);
        void                                setSeekIdentifierFlag               (bool flag);
        bool                                isSeekIdentifier                    ();

        void                                setGdbMultiarchPid                  (int pid);
        void                                setNewHardwareBreakpointFlag        (bool flag);
        bool                                isNewHardwareBreakpointFlag         ();
        void                                setDebugOnInitFlag                  (bool flag);
        bool                                isDebugOnInit                       ();
        void                                setGdbMultiarchRunningState         (bool flag);
        bool                                gdbMultiarchRunningState            ();
        QProcess*                           openocdProcess                      ();
        // handle multithread, for openocd debug on init feature
        void                                debugOnInitHandler                  ();
        void                                traceIdentifierHandler              (const QString& identifier);
        // Sync function, only for debug on init
        void                                handleSyncGdbInterruptSIGINT_DebugOnInit        ();
        void                                handleSyncGdbGenericpointList       ();
        void                                handleSyncGdbContinue               ();
        void                                handleSyncBreakInsert               (QString bp);
        void                                handleSyncBreakEnable               (QString bp);
        void                                handleSyncBreakDisable              (QString bp);
        void                                handleSyncManualGdbCommand          (QString expression);
        void                                handleSyncSendToSerial              (QString path, QString expression);
        void                                handleSyncRefreshSource             ();
        void                                handleSyncLsmod                     (QString kernelModuleName);
        void                                handleGdbLsmod                      (const QString& kernelModuleName);
        void                                handleSyncWarning                   (const QString& warningMsg);
        // Handler for Sync function
        void                                handleSyncGdbFindVariableIdentifier (const QString& identifier);
        void                                handleSyncGdbFindFunctionIdentifier (const QString& identifier);
        void                                handleSyncGdbFindTypeIdentifier     (const QString& identifier);
        void                                handleSendToSerial                  (QString path, QString expression);
        void                                handleSeekIdentifier                (const QString& identifier);
        void                                handleSyncGdbInterruptSIGINT_TraceIdentifier();

    public slots:
        void                                handleLogsTabMoved                  (int to, int from);
        void                                handleLogsTabChanged                (int index);
        void                                handleRaiseMessageTab               ();

        void                                handleText                          (const QString& text);
        void                                handleManualCommandExecute          ();
        void                                handleGdbCommand                    (const QString& command);
        void                                handleGdbExit                       ();
        void                                handleGdbRunExecutable              (const QString& breakMode, bool loadSessionBreakpoints);
        void                                handleGdbAttachExecutable           (bool loadSessionBreakpoints);
        void                                handleGdbConnectExecutable          (bool loadSessionBreakpoints);
        void                                handleGdbRRExecutable               (bool loadSessionBreakpoints);
        void                                handleGdbCoreFileExecutable         ();
        // openocd gdb-multiarch support
        void                                handleGdbMultiarchOpenOCDExecutable ();
        void                                handleOpenOCDMainHelpButtonClicked  ();
        void                                handleDebugKernelModule             ();
        void                                handleGdbTerminateExecutable        (bool confirm=true);
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
        void                                handleGdbSessionLoadBreakpoints     ();
        void                                handleGdbSessionSaveBreakpoints     ();
        void                                handleGdbTerminalDeviceName         ();
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
        void                                handleGdbSkipList                   ();
        void                                handleGdbSkipAdd                    (QString skipmode, QString skipparameters);
        void                                handleGdbSkipDelete                 (QString skipids);
        void                                handleGdbSkipEnable                 (QString skipids);
        void                                handleGdbSkipDisable                (QString skipids);
        void                                handleGdbCheckpointList             ();
        void                                handleGdbCheckpointInsert           ();
        void                                handleGdbCheckpointSelect           (QString id);
        void                                handleGdbCheckpointDelete           (QString ids);
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
        void                                handleGdbMatrixAddExpression        (QString expression);
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
        void                                handleGdbMatrixVisualizer           ();
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
        void                                handleGdbLoadMICommands             ();
        void                                handleGdbSourceScripts              ();

        void                                handleGdbProcessFinished            (int exitCode, QProcess::ExitStatus exitStatus);
        void                                handleGdbProcessErrored             (QProcess::ProcessError errorStatus);

        void                                handleConsoleModeChanged            ();
        void                                handleConsoleNewTextAdded           ();
        void                                handleConsoleNewTextViewed          ();
        void                                handleAboutToQuit                   ();
        void                                handleOpenOCDStartFailed            ();

        // For handling tracing functions, variables and types
        void                                handleSyncSeekVariableIdentifier    (const QString& identifier);
        void                                handleSyncSeekFunctionIdentifier    (const QString& identifier);
        void                                handleSyncSeekTypeIdentifier        (const QString& identifier);

        // Handling exception level changed
        void                                handleExceptionLevelChanged         (const QString& exceptionLevel);

    signals:
        void                                stoppingPointReached                ();
        void                                sessionTerminated                   ();
        void                                changeWindowTitle                   (QString title);
        void                                assemblyConfigChanged               ();
        void                                recordSettingsChanged               ();
        void                                stateChanged                        ();
        //openocd, for debugging gdb log
        void                                allTextOutput                       (const QString& text);
        // For Debug on Init
        void                                debugOnInitContinue                 ();
        void                                requestContinue                     ();
        void                                requestBreakList                    ();
        void                                requestBreakInsert                  (QString bp);
        void                                requestBreakEnable                  (QString bp);
        void                                requestBreakDisable                 (QString bp);
        void                                requestGdbCommand                   (QString expression);
        void                                requestSendToSerial                 (QString path, QString expression);
        void                                requestRefreshSource                ();
        void                                requestSeekVariableIdentifier       (const QString& expression);
        void                                requestSeekFunctionIdentifier       (const QString& expression);
        void                                requestSeekTypeIdentifier           (const QString& expression);
        void                                requestLsmod                        (const QString& kernelModuleName);
        void                                requestWarning                      (const QString& warningMsg);

    protected:
        void                                writeLogsSettings                   ();
        void                                readLogsSettings                    ();

    private:
        bool                                isQuitting                          () const;
        void                                setIsQuitting                       (bool f);

        bool                                startGdb                            ();
        bool                                startGdbRR                          ();

        // For openOCD gdb-multiarch support
        void                                killGdb                             ();
        void                                createConsole                       ();
        void                                deleteConsole                       ();
        void                                reattachConsole                     ();
        SeerConsoleWidget*                  console                             ();
        void                                sendGdbInterrupt                    (int signal);
        void                                delay                               (int seconds);

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
        int                                 _gdbmultiarchPid;               // specifically for gdbmultiarch
        QString                             _executableConnectHostPort;
        QString                             _executableRRTraceDirectory;
        QString                             _executableCoreFilename;
        QString                             _executableLaunchMode;
        QString                             _executableBreakMode;
        QString                             _executableLaunchModeBackup;
        QString                             _executableBreakModeBackup;
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
        SeerCheckpointsBrowserWidget*       _checkpointsBrowserWidget;
        SeerGdbLogWidget*                   _gdbOutputLog;
        SeerSeerLogWidget*                  _seerOutputLog;
        

        GdbMonitor*                         _gdbMonitor;
        QProcess*                           _gdbProcess;

        QVector<int>                        _dataExpressionId;
        QVector<QString>                    _dataExpressionName;

        QStringList                         _ignoreFilePatterns;

        // openOCD variables
        // OpenOCD
        QString                             _openOCDExePath;
        QString                             _openOCDCommands;
        // GDB Multiarch
        QString                             _gdbMultiarchExePath;
        QString                             _GDBPort;
        QString                             _TelnetPort;
        QString                             _gdbMultiarchCommands;
        QString                             _gdbMultiarchProgram;
        QString                             _gdbMultiarchArguments;
        // Docker
        bool                                _isBuildInDocker;
        QString                             _absoluteBuildPath;
        QString                             _dockerBuildPath;
        // Symbol Files
        QMap<QString, std::tuple<QString, bool, QString>>              _symbolFiles;
        // gdb multiarch variables
        bool                                _newHBreakFlag;
        bool                                _isTargetRunning;               // hold target state: running / halted
        bool                                _debugOnInitFlag;               // flag handling openocd debug on init
        bool                                _seekingIndentifierFlag;        // flag for handling seek identifier
        bool                                _isStopAtTempFunc;
        QString                             _stopAtFunc;
        bool                                _isStopAtException;
        QString                             _exceptionLevelToStop;
        QString                             _openOCDTarget;        
        // Kernel module
        QString                             _moduleName;
        QString                             _commandToTerm;
        QString                             _kernelModuleSymbolPath;
        QString                             _kernelModuleSourceCodePath;
        QString                             _serialPortPath;
        // Thread, Muxtex and condition variable, for Debug on Init synchronization. There are 3 types of Muxtex and condition
        // variable: for sigint, continue and normal command
        // _debugOnInitOperation for sync normal operation.
        // _debugOnInitStop for sigint operation
        // _debugOnInitRunning for continue operation
        // _debugOnInitListBp For handling -break-list
        // _debugOnInitHandleBp for handling breakpoint operation like delete, insert, enable, disable
        QMutex                              _debugOnInitOperationMutex;
        QWaitCondition                      _debugOnInitOperationCv;
        QMutex                              _debugOnInitStopMutex;
        QWaitCondition                      _debugOnInitStopCv;
        QMutex                              _debugOnInitRunningMutex;
        QWaitCondition                      _debugOnInitRunningCv;
        QMutex                              _debugOnInitListBpMutex;
        QWaitCondition                      _debugOnInitListBpCv;
        QMutex                              _debugOnInitHandleBpMutex;
        QWaitCondition                      _debugOnInitHandleBpCv;
        QMutex                              _debugOnInitRefreshSourceMutex;
        QWaitCondition                      _debugOnInitRefreshSourceCv;
        QThread*                            _workerThread;
        bool                                _debugOnInitBpReadFlag;
        bool                                _debugOnInitFindLoadModuleFile;
        bool                                _debugOnInitTempBpFlag;
        bool                                _debugOnInitJustReadModuleDir;
        QString                             _loadModuleFile;
        int                                 _loadModuleLineNo;
        QMutex                              _seekIdentifierMutex;
        QWaitCondition                      _seekIdentifierCv;
        QString                             _Identifier;
        bool                                _sigINTDebugOnInitFlag          = false;
        bool                                _lsmodDebugOnInitFlag           = false;
        bool                                _isModuleIsLoaded               = false;
        // Mutex and Cond variable for tracing identifier
        QMutex                              _traceIdentiferStopMutex;
        QWaitCondition                      _traceIdentiferStopCv;
        // Mutex and Cond variable for lsmod
        QMutex                              _lsmodMutex;
        QWaitCondition                      _lsmodCv;
        // List of breakpoint previous status, used in Debug on Init
        QMap<QString,QString>               _mapListBpStatus;
        // List of kernel module address
        QMap<QString, QString>              _mapKernelModuleAddress;
        int                                 _moduleInitLineNo = 0;
};
