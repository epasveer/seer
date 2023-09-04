#pragma once

#include <QtWidgets/QDialog>
#include <QtWidgets/QButtonGroup>
#include <QtCore/QString>
#include <QtCore/QStringList>

#include "ui_SeerDebugDialog.h"

class SeerDebugDialog : public QDialog, protected Ui::SeerDebugDialogForm {

    Q_OBJECT

    public:
        explicit SeerDebugDialog (QWidget* parent = 0);
       ~SeerDebugDialog ();

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

        // Connectect to a RR server. "rr".
        void                    setRRTraceDirectory                             (const QString& rrTraceDirectory);
        QString                 rrTraceDirectory                                () const;

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

    protected slots:
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

    private slots:
        void                    handleHelpModeToolButtonClicked                 ();
        void                    handleHelpRunToolButtonClicked                  ();
        void                    handleHelpAttachToolButtonClicked               ();
        void                    handleHelpConnectToolButtonClicked              ();
        void                    handleHelpRRToolButtonClicked                   ();
        void                    handleHelpCorefileToolButtonClicked             ();

    protected:
        void                    writeSettings                                   ();
        void                    readSettings                                    ();
        void                    resizeEvent                                     (QResizeEvent* event);

    private:
        QString                 _projectFilename;
};

