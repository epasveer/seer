// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "ui_SeerMainWindow.h"
#include "SeerRunStatusIndicatorBox.h"
#include "SeerKeySettings.h"
#include "SeerProgressIndicator.h"
#include <QtWidgets/QMainWindow>
#include <QShortcut>
#include <QActionGroup>
#include <QtGui/QCloseEvent>
#include <QtGui/QResizeEvent>
#include <QtCore/QString>
#include <QtCore/QStringList>

class SeerMainWindow : public QMainWindow, protected Ui::SeerMainWindowForm {

    Q_OBJECT

    public:
        SeerMainWindow (QWidget* parent = 0);
       ~SeerMainWindow ();

        void                        setExecutableName                       (const QString& executableName);
        const QString&              executableName                          () const;
        void                        setExecutableSymbolName                 (const QString& executableSymbolName);
        const QString&              executableSymbolName                    () const;
        void                        setExecutableArguments                  (const QString& executableArguments);
        void                        setExecutableArguments                  (const QStringList& executableArguments);
        const QString&              executableArguments                     () const;
        void                        setExecutableWorkingDirectory           (const QString& executableWorkingDirectory);
        const QString&              executableWorkingDirectory              () const;
        void                        setExecutableBreakpointsFilename        (const QString& breakpointsFilename);
        const QString&              executableBreakpointsFilename           () const;
        void                        setExecutableBreakpointFunctionName     (const QString& nameoraddress);
        const QString&              executableBreakpointFunctionName        () const;
        void                        setExecutableBreakpointSourceName       (const QString& sourceFilenameAndLineno);
        const QString&              executableBreakpointSourceName          () const;
        void                        setExecutableShowAssemblyTab            (bool flag);
        bool                        executableShowAssemblyTab               () const;
        void                        setExecutableRandomizeStartAddress      (bool flag);
        bool                        executableRandomizeStartAddress         () const;
        void                        setExecutableNonStopMode                (bool flag);
        bool                        executableNonStopMode                   () const;
        void                        setExecutablePid                        (int pid);
        int                         executablePid                           () const;
        void                        setExecutableConnectHostPort            (const QString& executableConnectHostPort);
        const QString&              executableConnectHostPort               () const;
        void                        setExecutableConnectRemoteTargetType    (const QString& type);
        QString                     executableConnectRemoteTargetType       () const;
        void                        setExecutableConnectGdbserverDebug      (bool enable);
        bool                        executableConnectGdbserverDebug         () const;
        void                        setExecutableRRTraceDirectory           (const QString& executableRRTraceDirectory);
        const QString&              executableRRTraceDirectory              () const;
        void                        setExecutableCoreFilename               (const QString& executableCoreFilename);
        const QString&              executableCoreFilename                  () const;
        void                        setExecutablePreGdbCommands             (const QStringList& preGdbCommands);
        const QStringList&          executablePreGdbCommands                () const;
        void                        setExecutablePostGdbCommands            (const QStringList& postGdbCommands);
        const QStringList&          executablePostGdbCommands               () const;
        void                        setProjectFilename                      (const QString& projectFilename);
        const QString&              projectFilename                         () const;
        void                        setGdbProgramOverride                   (const QString& gdbProgram);
        QString                     gdbProgramOverride                      () const;
        void                        setGdbArgumentsOverride                 (const QString& gdbProgram);
        QString                     gdbArgumentsOverride                    () const;

        void                        launchExecutable                        (const QString& launchMode, const QString& breakMode);
        const QString&              executableLaunchMode                    () const;
        const QString&              executableBreakMode                     () const;

        void                        setStyleName                            (const QString& name);
        const QString&              styleName                               ();

        // openocd get and set functions
        // ::Main
        const QString&                      openOCDExePath                      ();
        void                                setOpenOCDExePath                   (const QString& path);
        const QString&                      gdbPort                             ();
        void                                setGdbPort                          (const QString& port);
        const QString&                      telnetPort                          ();
        void                                setTelnetPort                       (const QString& port);
        const QString&                      openOCDCommand                      ();
        void                                setOpenOCDCommand                   (const QString& command);
        // ::GDB Multiarch
        const QString&                      gdbMultiarchExePath                 ();
        void                                setGdbMultiarchExePath              (const QString& path);
        const QString&                      gdbMultiarchCommand                 ();
        void                                setGdbMultiarchCommand              (const QString& command);
        bool                                isGdbMultiarchIsStopAtTempFunc      ();
        void                                setGdbMultiarchStopAtTempFunc       (bool check);
        const QString                       gdbMultiarchStopAtFunc              ();
        void                                setGdbMultiarchStopAtFunc           (const QString& func);
        bool                                isGdbMultiarchStopAtException       ();
        void                                setGdbMultiarchStopAtExeption       (bool check);
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
        void                                setSymbolFiles                      (const QMap<QString, std::tuple<QString, bool, QString>>& _symbolFiles);
        const QMap<QString, std::tuple<QString, bool, QString>>     symbolFiles (void);

    private slots:
        void                        handleFileDebug                         ();
        void                        handleFileArguments                     ();
        void                        handleFileQuit                          ();
        void                        handleViewMemoryVisualizer              ();
        void                        handleViewArrayVisualizer               ();
        void                        handleViewMatrixVisualizer              ();
        void                        handleViewStructVisualizer              ();
        void                        handleViewVarVisualizer                 ();
        void                        handleViewImageVisualizer               ();
        void                        handleViewAssembly                      ();
        void                        handleViewAssemblyShown                 (bool shown);
        void                        handleViewConsoleAttached               ();
        void                        handleViewConsoleDetached               ();
        void                        handleViewConsoleDetachedMinimized      ();
        void                        handleSettingsConfiguration             ();
        void                        handleSettingsSaveConfiguration         ();
        void                        handleHelpAbout                         ();
        void                        handleText                              (const QString& text);
        void                        handleRunStatusChanged                  (SeerRunStatusIndicatorBox::RunStatus status);
        void                        handleRecordSettingsChanged             ();
        void                        handleChangeWindowTitle                 (QString title);
        void                        handleHelpToolButtonClicked             ();
        void                        handleTerminateExecutable               ();
        void                        handleRestartExecutable                 ();
        void                        handleStyleMenuChanged                  ();
        void                        handleShowMessage                       (QString message, int time);
        void                        handleGdbStateChanged                   ();
        void                        handleGdbTargetRunning                  ();
        void                        handleGdbTargetInterrupt                ();
        void                        handleStatusChanged                     (QString message);
        void                        handleExceptionButtonClicked            ();

    protected:
        void                        writeSettings                           ();
        void                        readSettings                            ();
        void                        writeConfigSettings                     ();
        void                        readConfigSettings                      ();
        void                        resizeEvent                             (QResizeEvent* event);
        void                        closeEvent                              (QCloseEvent* event);
        void                        setKeySettings                          (const SeerKeySettings& settings);
        const SeerKeySettings       keySettings                             () const;
        void                        refreshShortCuts                        ();

    private:
        void  createExceptionLevelBar();
        void  deleteExceptionLevelBar();
        QActionGroup*                       _styleMenuActionGroup;
        QString                             _styleName;
        QAction*                            _interruptAction;
        SeerProgressIndicator*              _progressIndicator;
        SeerKeySettings                     _keySettings;
        QString                             _projectFile;
        SeerRunStatusIndicatorBox*          _runStatus;
        QWidget*                            _groupExeptionLevel = nullptr;
        QPushButton*                        _exceptionButton;
        QComboBox*                          _exceptionComboBox;
};

