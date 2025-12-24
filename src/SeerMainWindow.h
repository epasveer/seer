// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "ui_SeerMainWindow.h"
#include "SeerRunStatusIndicator.h"
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
        void                        setExecutableShowAssemblyTabMode        (const QString& mode);
        QString                     executableShowAssemblyTabMode           () const;
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

    private slots:
        void                        handleFileDebugWithDefaultProject       ();
        void                        handleFileDebugWithOutDefaultProject    ();
        void                        handleFileDebug                         (bool loadDefaultProject);
        void                        handleFileArguments                     ();
        void                        handleFileQuit                          ();
        void                        handleViewMemoryVisualizer              ();
        void                        handleViewArrayVisualizer               ();
        void                        handleViewMatrixVisualizer              ();
        void                        handleViewStructVisualizer              ();
        void                        handleViewVarVisualizer                 ();
        void                        handleViewImageVisualizer               ();
        void                        handleViewParallelStacksVisualizer      ();
        void                        handleViewAssembly                      ();
        void                        handleViewAssemblyShown                 (bool shown);
        void                        handleViewConsoleAttached               ();
        void                        handleViewConsoleDetached               ();
        void                        handleViewConsoleDetachedMinimized      ();
        void                        handleSettingsConfiguration             ();
        void                        handleSettingsSaveConfiguration         ();
        void                        handleHelpAbout                         ();
        void                        handleText                              (const QString& text);
        void                        handleRunStatusChanged                  (SeerRunStatusIndicator::RunStatus status);
        void                        handleRecordSettingsChanged             ();
        void                        handleChangeWindowTitle                 (QString title);
        void                        handleHelpToolButtonClicked             ();
        void                        handleTerminateExecutable               ();
        void                        handleRestartExecutable                 ();
        void                        handleStyleMenuChanged                  ();
        void                        handleShowMessage                       (QString message, int time);
        void                        handleGdbStateChanged                   ();

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
        QActionGroup*               _styleMenuActionGroup;
        QString                     _styleName;
        QAction*                    _interruptAction;
        SeerProgressIndicator*      _progressIndicator;
        SeerKeySettings             _keySettings;
        QString                     _projectFile;
};

