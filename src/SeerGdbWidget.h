#pragma once

#include "SeerConsoleWidget.h"
#include "SeerEditorWidgetSource.h"
#include "SeerGdbLogWidget.h"
#include "SeerSeerLogWidget.h"
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

        void                                setExecutablePid                    (int pid);
        int                                 executablePid                       () const;

        void                                setExecutableHostPort               (const QString& hostPort);
        const QString&                      executableHostPort                  () const;

        void                                setExecutableSerialBaud             (int executableBaudRate);
        int                                 executableSerialBaud                () const;

        void                                setExecutableSerialParity           (const QString& executableParity);
        const QString&                      executableSerialParity              () const;

        void                                setExecutableCoreFilename           (const QString& coreFilename);
        const QString&                      executableCoreFilename              () const;

        void                                setExecutableLaunchMode             (const QString& launchMode);
        const QString&                      executableLaunchMode                () const;
        const QString&                      executableBreakMode                 () const;

        // Gdb settings.
        void                                setGdbProgram                       (const QString& program);
        QString                             gdbProgram                          () const;

        void                                setGdbArguments                     (const QString& arguments);
        QString                             gdbArguments                        () const;

        void                                setGdbAsyncMode                     (bool flag);
        bool                                gdbAsyncMode                        () const;

        void                                setGdbNonStopMode                   (bool flag);
        bool                                gdbNonStopMode                      () const;

        void                                setGdbHandleTerminatingException    (bool flag);
        bool                                gdbHandleTerminatingException       () const;

        void                                setGdbRandomizeStartAddress         (bool flag);
        bool                                gdbRandomizeStartAddress            () const;

        void                                setGdbEnablePrettyPrinting          (bool flag);
        bool                                gdbEnablePrettyPrinting             () const;

        void                                setGdbRecordMode                    (const QString& mode);
        QString                             gdbRecordMode                       () const;

        void                                setGdbRecordDirection               (const QString& direction);
        QString                             gdbRecordDirection                  () const;

        void                                setDprintfStyle                     (const QString& style);
        QString                             dprintfStyle                        () const;

        void                                setDprintfFunction                  (const QString& function);
        QString                             dprintfFunction                     () const;

        void                                setDprintfChannel                   (const QString& channel);
        QString                             dprintfChannel                      () const;

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

        void                                setSourceIgnoreDirectories          (const QStringList& ignoreDirectories);
        const QStringList&                  sourceIgnoreDirectories             () const;

        void                                setSourceMiscFilePatterns           (const QStringList& filePatterns);
        const QStringList&                  sourceMiscFilePatterns              () const;

        void                                setSourceSourceFilePatterns         (const QStringList& filePatterns);
        const QStringList&                  sourceSourceFilePatterns            () const;

        void                                setSourceHeaderFilePatterns         (const QStringList& filePatterns);
        const QStringList&                  sourceHeaderFilePatterns            () const;

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

        void                                setGdbOutputLogEnabled              (bool flag);
        bool                                isGdbOutputLogEnabled               () const;

        void                                setSeerOutputLogEnabled             (bool flag);
        bool                                isSeerOutputLogEnabled              () const;

        // Editor manager.
        SeerEditorManagerWidget*            editorManager                       ();
        const SeerEditorManagerWidget*      editorManager                       () const;

        // Settings
        void                                writeSettings                       ();
        void                                readSettings                        ();

        // Printpoints
        void                                resetDprintf                        ();

    public slots:
        void                                handleText                          (const QString& text);
        void                                handleManualCommandExecute          ();
        void                                handleGdbCommand                    (const QString& command);
        void                                handleGdbExit                       ();
        void                                handleGdbRunExecutable              (const QString& breakMode);
        void                                handleGdbAttachExecutable           ();
        void                                handleGdbConnectExecutable          ();
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
        void                                handleGdbThreadListFrames           ();
        void                                handleGdbThreadListIds              ();
        void                                handleGdbThreadListGroups           ();
        void                                handleGdbThreadSelectId             (int threadid);
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

        void                                handleGdbProcessFinished            (int exitCode, QProcess::ExitStatus exitStatus);
        void                                handleGdbProcessErrored             (QProcess::ProcessError errorStatus);

    signals:
        void                                stoppingPointReached                ();
        void                                changeWindowTitle                   (QString title);
        void                                assemblyConfigChanged               ();
        void                                recordSettingsChanged               ();

    protected:

    private:
        bool                                isGdbRuning                         () const;
        void                                startGdb                            ();
        void                                killGdb                             ();
        void                                createConsole                       ();
        void                                deleteConsole                       ();
        void                                connectConsole                      ();
        void                                disconnectConsole                   ();
        void                                sendGdbInterrupt                    (int signal);

        QString                             _gdbProgram;
        QString                             _gdbArguments;
        bool                                _gdbASyncMode;
        bool                                _gdbNonStopMode;
        bool                                _gdbHandleTerminatingException;
        bool                                _gdbRandomizeStartAddress;
        bool                                _gdbEnablePrettyPrinting;
        QString                             _gdbRecordMode;
        QString                             _gdbRecordDirection;
        QString                             _dprintfStyle;
        QString                             _dprintfFunction;
        QString                             _dprintfChannel;
        bool                                _assemblyShowAssemblyTabOnStartup;
        QString                             _assemblyDisassemblyFlavor;
        QString                             _assemblySymbolDemangling;
        QString                             _assemblyRegisterFormat;

        QString                             _executableName;
        QString                             _executableSymbolName;
        QString                             _executableArguments;
        QString                             _executableWorkingDirectory;
        QString                             _executableBreakpointsFilename;
        QString                             _executableBreakpointFunctionName;
        int                                 _executablePid;
        QString                             _executableHostPort;
        int                                 _executableSerialBaud;
        QString                             _executableSerialParity;
        QString                             _executableCoreFilename;
        QString                             _executableLaunchMode;
        QString                             _executableBreakMode;
        bool                                _newExecutableFlag;
        int                                 _currentFrame;

        SeerConsoleWidget*                  _consoleWidget;
        QString                             _consoleMode;
        int                                 _consoleScrollLines;
        int                                 _rememberManualCommandCount;
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
};

