#pragma once

#include "ui_SeerMainWindow.h"
#include <QtWidgets/QMainWindow>
#include <QtGui/QCloseEvent>
#include <QtCore/QString>
#include <QtCore/QStringList>

class SeerMainWindow : public QMainWindow, protected Ui::SeerMainWindowForm {

    Q_OBJECT

    public:
        SeerMainWindow (QWidget* parent = 0);
       ~SeerMainWindow ();

        void                setExecutableName               (const QString& executableName);
        const QString&      executableName                  () const;
        void                setExecutableArguments          (const QString& executableArguments);
        void                setExecutableArguments          (const QStringList& executableArguments);
        const QString&      executableArguments             () const;
        void                setExecutableWorkingDirectory   (const QString& executableWorkingDirectory);
        const QString&      executableWorkingDirectory      () const;
        void                setExecutablePid                (int pid);
        int                 executablePid                   () const;
        void                setExecutableHostPort           (const QString& executableHostPort);
        const QString&      executableHostPort              () const;
        void                setExecutableCoreFilename       (const QString& executableCoreFilename);
        const QString&      executableCoreFilename          () const;

        void                launchExecutable                (const QString& launchMode);
        const QString&      executableLaunchMode            () const;

    private slots:
        void                handleFileDebug                 ();
        void                handleFileArguments             ();
        void                handleFileQuit                  ();
        void                handleText                      (const QString& text);

    protected:
        void                closeEvent                      (QCloseEvent* event);

    private:
};

