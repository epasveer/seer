// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QDialog>
#include <QtWidgets/QButtonGroup>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QJsonDocument>
#include <tuple>
#include "ui_SeerDebugDialog.h"

class OpenOCDSymbolWidgetManager;
class SeerDebugDialog : public QDialog, protected Ui::SeerDebugDialogForm {

    Q_OBJECT

    public:
        explicit SeerDebugDialog (QWidget* parent = 0);
       ~SeerDebugDialog ();

        // Reset all
        void                    reset                                           ();

        // For any run mode.
        void                    setExecutableName                               (const QString& executableName);
        QString                 executableName                                  () const;

        void                    setExecutableSymbolName                         (const QString& executableSymbolName);
        QString                 executableSymbolName                            () const;

        void                    setExecutableWorkingDirectory                   (const QString& executableWorkingDirectory);
        QString                 executableWorkingDirectory                      () const;

        // Run or start executable. "run" or "start".
        void                    setExecutableArguments                          (const QString& executableArguments);
        QString                 executableArguments                             () const;
        void                    setBreakpointsFilename                          (const QString& breakpointsFilename);
        QString                 breakpointsFilename                             () const;
        void                    setBreakpointMode                               (const QString& mode);
        QString                 breakpointMode                                  () const;
        void                    setBreakpointFunctionName                       (const QString& nameoraddress);
        QString                 breakpointFunctionName                          () const;
        void                    setBreakpointSourceName                         (const QString& sourceFilenameAndLineno);
        QString                 breakpointSourceName                            () const;
        void                    setShowAssemblyTab                              (bool flag);
        bool                    showAssemblyTab                                 () const;
        void                    setRandomizeStartAddress                        (bool flag);
        bool                    randomizeStartAddress                           () const;
        void                    setNonStopMode                                  (bool flag);
        bool                    nonStopMode                                     () const;
        void                    setPreGdbCommands                               (const QStringList& preGdbCommands);
        QStringList             preGdbCommands                                  () const;
        void                    setPostGdbCommands                              (const QStringList& postGdbCommands);
        QStringList             postGdbCommands                                 () const;

        // Attach to a running process. "attach".
        void                    setAttachPid                                    (int pid);
        int                     attachPid                                       () const;

        // Connectect to a GDB server. "connect".
        void                    setConnectHostPort                              (const QString& connectHostPort);
        QString                 connectHostPort                                 () const;
        void                    setConnectRemoteTargetType                      (const QString& type);
        QString                 connectRemoteTargetType                         () const;
        void                    setConnectGdbserverDebug                        (bool enable);
        bool                    connectGdbserverDebug                           () const;


        // Connectect to a RR server. "rr".
        void                    setRRTraceDirectory                             (const QString& rrTraceDirectory);
        QString                 rrTraceDirectory                                () const;
        QString                 rrBreakpointsFilename                           () const;

        // Load a core file. "corefile".
        void                    setCoreFilename                                 (const QString& coreFilename);
        QString                 coreFilename                                    () const;

        // Get the launch mode.
        void                    setLaunchMode                                   (const QString& mode);
        QString                 launchMode                                      () const;

        // Get the project filename.
        void                    setProjectFilename                              (const QString& filename);
        QString                 projectFilename                                 () const;
        void                    loadProject                                     (const QString& filename, bool notify);
        void                    loadDefaultProjectSettings                      ();

        // Make a json document of the current debug dialog settings.
        QJsonDocument           makeJsonDoc                                     () const;
        bool                    loadJsonDoc                                     (const QJsonDocument& jsonDoc, const QString& filename);

        // openocd get and set functions
        // ::Main
        const QString                       openOCDExePath                      ();
        void                                setOpenOCDExePath                   (const QString& path);
        const QString                       gdbPort                             ();
        void                                setGdbPort                          (const QString& port);
        const QString                       openOCDCommand                      ();
        void                                setOpenOCDCommand                   (const QString& command);
        const QString                       telnetPort                          ();
        void                                setTelnetPort                       (const QString& port);
        // ::GDB Multiarch
        const QString                       gdbMultiarchExePath                 ();
        void                                setGdbMultiarchExePath              (const QString& path);
        const QString                       gdbMultiarchCommand                 ();
        void                                setGdbMultiarchCommand              (const QString& command);
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
        OpenOCDSymbolWidgetManager*         symbolWidgetManager                 ();
        void                                setSymbolFiles                      (const QMap<QString, std::tuple<QString, bool, QString>>& symbolFiles);

        public slots:
        void                    handleExecutableNameToolButton                  ();
        void                    handleExecutableSymbolNameToolButton            ();
        void                    handleExecutableWorkingDirectoryToolButton      ();
        void                    handleLoadBreakpointsFilenameToolButton         ();
        void                    handleBreakpointInFunctionLineEdit              ();
        void                    handleLoadRRTraceDirectoryToolButton            ();
        void                    handleLoadCoreFilenameToolButton                ();
        void                    handleProgramPidToolButton                      ();
        void                    handleLoadProjectToolButton                     ();
        void                    handleSaveProjectToolButton                     ();
        void                    handleRunModeChanged                            (int id);

        // OpenOCD button handler
        void                    handleOpenOCDDefaultButtonClicked               ();
        void                    handleOpenOCDTabChanged                         (int id);
        void                    handleExecutableOpenOCDButtonClicked            ();
        void                    handleOpenOCDBuildFolderPathButton              ();
        void                    handleLaunchButtonClicked                       ();
        void                    handleResetButtonClicked                        (QAbstractButton* button);

    private slots:
        void                    handleHelpModeToolButtonClicked                 ();
        void                    handleHelpRunToolButtonClicked                  ();
        void                    handleHelpAttachToolButtonClicked               ();
        void                    handleHelpConnectToolButtonClicked              ();
        void                    handleHelpRRToolButtonClicked                   ();
        void                    handleHelpCorefileToolButtonClicked             ();
        void                    handleOpenOCDDockerCheckboxClicked              ();
        void                    handleOpenOCDMainHelpButtonClicked              ();

    protected:
        void                    writeSettings                                   ();
        void                    readSettings                                    ();
        void                    writeDefaultProjectSettings                     (const QJsonDocument& document);
        void                    resizeEvent                                     (QResizeEvent* event);

    private:
        QString                         _projectFilename;
        OpenOCDSymbolWidgetManager*     _OpenOCDSymbolWidgetManager = nullptr;
        QMap<QString, QString>          _symbolFiles;
};

class OpenOCDSymbolFileWidget: public QWidget{
    Q_OBJECT

public:
    explicit OpenOCDSymbolFileWidget (QWidget* parent = nullptr);
    ~OpenOCDSymbolFileWidget ();
    const QString               symbolPath ();
    const QString               sourcePath ();
    bool                        isLoadAddressEnabled ();
    const QString               loadAddress ();
    void                        setSymbolPath (const QString& path);
    void                        setSourcePath (const QString& path);
    void                        setEnableLoadAddress (bool enable);
    void                        setLoadAddress (const QString& address);

private slots:
    void                    handleOpenOCDSymbolPathButtonClicked ();
    void                    handleOpenOCDDirPathButtonClicked ();
    void                    handleOpenOCDLoadAddressCheckBoxClicked ();

private:
    QString                 _symbolPath;
    QString                 _sourcePath;
    QLineEdit*              _symbolLineEdit;
    QLineEdit*              _sourceLineEdit;
    QPushButton*            _symbolToolButton;
    QPushButton*            _sourceToolButton;
    QCheckBox*              _loadAddressCheckBox;
    QLineEdit*              _loadAddressLineEdit;
    bool                    _isLoadAddressEnabled = false;
    QString                 _loadAddress = "";
};

class OpenOCDSymbolWidgetManager : public QWidget{
    Q_OBJECT

public:
    explicit OpenOCDSymbolWidgetManager (QWidget* parent = nullptr);
    ~OpenOCDSymbolWidgetManager ();

    const QMap<QString, std::tuple<QString, bool, QString>>  symbolFiles ();
    int                                     countSymbolFiles ();
    void                                    addGroupBox(const QMap<QString, std::tuple<QString, bool, QString>> &box);

public slots:
    void                                    addEmptyGroupBox();
    void                                    deleteGroupBox();

private:
    int                                     _countSymbolFiles = 0;
    QWidget *                               _scrollWidget;
    QVBoxLayout *                           _scrollLayout;
    QList<OpenOCDSymbolFileWidget *>        _groupBoxes;
    QMap<QString, std::tuple<QString, bool, QString>> _symbolFiles;
};