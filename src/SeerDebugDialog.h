#pragma once

#include <QtWidgets/QDialog>
#include <QtWidgets/QButtonGroup>
#include <QtCore/QString>

#include "ui_SeerDebugDialog.h"

class SeerDebugDialog : public QDialog, protected Ui::SeerDebugDialogForm {

    Q_OBJECT

    public:
        explicit SeerDebugDialog (QWidget* parent = 0);
       ~SeerDebugDialog ();

        // For any run mode.
        void                    setExecutableName                               (const QString& executableName);
        QString                 executableName                                  () const;

        void                    setExecutableWorkingDirectory                   (const QString& executableWorkingDirectory);
        QString                 executableWorkingDirectory                      () const;

        // Run or start executable. "run" or "start".
        void                    setExecutableArguments                          (const QString& executableArguments);
        QString                 executableArguments                             () const;
        void                    setBreakpointsFilename                          (const QString& breakpointsFilename);
        QString                 breakpointsFilename                             () const;
        void                    setBreakInMain                                  (bool flag);
        bool                    breakInMain                                     () const;

        // Attach to a running process. "attach".
        void                    setAttachPid                                    (int pid);
        int                     attachPid                                       () const;

        // Connectect to a GDB server. "connect".
        void                    setConnectHostPort                              (const QString& connectHostPort);
        QString                 connectHostPort                                 () const;
        void                    setConnectSerialBaud                            (int connectBaudRate);
        int                     connectSerialBaud                               () const;
        void                    setConnectSerialParity                          (const QString& connectParity);
        QString                 connectSerialParity                             () const;

        // Load a core file. "corefile".
        void                    setCoreFilename                                 (const QString& coreFilename);
        QString                 coreFilename                                    () const;

        // Get the launch mode.
        void                    setLaunchMode                                   (const QString& mode);
        QString                 launchMode                                      ();

    protected slots:
        void                    handleExecutableNameToolButton                  ();
        void                    handleExecutableWorkingDirectoryToolButton      ();
        void                    handleLoadBreakpointsFilenameToolButton         ();
        void                    handleLoadCoreFilenameToolButton                ();
        void                    handleProgramPidToolButton                      ();
        void                    handleRunModeChanged                            (int id);

    private:
        QButtonGroup*           _runModeButtonGroup;
};

