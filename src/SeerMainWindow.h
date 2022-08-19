#pragma once

#include "ui_SeerMainWindow.h"
#include "SeerRunStatusIndicator.h"
#include "SeerKeySettings.h"
#include "QProgressIndicator.h"
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QShortcut>
#include <QtWidgets/QActionGroup>
#include <QtGui/QCloseEvent>
#include <QtGui/QResizeEvent>
#include <QtCore/QString>
#include <QtCore/QStringList>

class SeerMainWindow : public QMainWindow, protected Ui::SeerMainWindowForm {

    Q_OBJECT

    public:
        SeerMainWindow (QWidget* parent = 0);
       ~SeerMainWindow ();

        void                        setExecutableName                   (const QString& executableName);
        const QString&              executableName                      () const;
        void                        setExecutableArguments              (const QString& executableArguments);
        void                        setExecutableArguments              (const QStringList& executableArguments);
        const QString&              executableArguments                 () const;
        void                        setExecutableWorkingDirectory       (const QString& executableWorkingDirectory);
        const QString&              executableWorkingDirectory          () const;
        void                        setExecutableBreakpointsFilename    (const QString& breakpointsFilename);
        const QString&              executableBreakpointsFilename       () const;
        void                        setExecutableBreakpointFunctionName (const QString& nameoraddress);
        const QString&              executableBreakpointFunctionName    () const;
        void                        setExecutableShowAssemblyTab        (bool flag);
        bool                        executableShowAssemblyTab           () const;
        void                        setExecutableRandomizeStartAddress  (bool flag);
        bool                        executableRandomizeStartAddress     () const;
        void                        setExecutablePid                    (int pid);
        int                         executablePid                       () const;
        void                        setExecutableHostPort               (const QString& executableHostPort);
        const QString&              executableHostPort                  () const;
        void                        setExecutableSerialBaud             (int executableBaudRate);
        int                         executableSerialBaud                () const;
        void                        setExecutableSerialParity           (const QString& executableParity);
        const QString&              executableSerialParity              () const;
        void                        setExecutableCoreFilename           (const QString& executableCoreFilename);
        const QString&              executableCoreFilename              () const;

        void                        launchExecutable                    (const QString& launchMode, const QString& breakMode);
        const QString&              executableLaunchMode                () const;
        const QString&              executableBreakMode                 () const;

    private slots:
        void                        handleFileDebug                     ();
        void                        handleFileArguments                 ();
        void                        handleFileQuit                      ();
        void                        handleViewMemoryVisualizer          ();
        void                        handleViewArrayVisualizer           ();
        void                        handleViewStructVisualizer          ();
        void                        handleViewVarVisualizer             ();
        void                        handleViewAssembly                  ();
        void                        handleViewConsoleNormal             ();
        void                        handleViewConsoleHidden             ();
        void                        handleViewConsoleMinimized          ();
        void                        handleSettingsConfiguration         ();
        void                        handleSettingsSaveConfiguration     ();
        void                        handleHelpAbout                     ();
        void                        handleText                          (const QString& text);
        void                        handleRunStatusChanged              (SeerRunStatusIndicator::RunStatus status);
        void                        handleChangeWindowTitle             (QString title);
        void                        handleRunExecutable                 ();
        void                        handleStartExecutable               ();
        void                        handleStyleMenuChanged              ();

    protected:
        void                        writeSettings                       ();
        void                        readSettings                        ();
        void                        writeConfigSettings                 ();
        void                        readConfigSettings                  ();
        void                        resizeEvent                         (QResizeEvent* event);
        void                        closeEvent                          (QCloseEvent* event);
        void                        setKeySettings                      (const SeerKeySettings& settings);
        const SeerKeySettings       keySettings                         () const;
        void                        refreshShortCuts                    ();

    private:
        QActionGroup*               _styleMenuActionGroup;
        QProgressIndicator*         _progressIndicator;
        SeerKeySettings             _keySettings;
};

