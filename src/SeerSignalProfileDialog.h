// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>
#include <QtCore/QVector>

#include "ui_SeerSignalProfileDialog.h"

class SeerSignalProfileDialog : public QDialog, protected Ui::SeerSignalProfileDialogForm {

    Q_OBJECT

    public:
        explicit SeerSignalProfileDialog (QWidget* parent = 0);
       ~SeerSignalProfileDialog ();

        void                        setSignals                  (const QStringList& signalNames, const QVector<bool>& signalEnabled);
        QStringList                 signalNames                 () const;
        QVector<bool>               signalEnabled               () const;

        void                        setProfileName              (const QString& profileName);
        QString                     profileName                 () const;

    public slots:
         void                       accept                      ();

    private slots:
        void                        handleEnableSelected        ();
        void                        handleDisableSelected       ();
        void                        handleImportFile            ();
        void                        handleExportFile            ();

    protected:
        void                        writeSettings               ();
        void                        readSettings                ();
        void                        resizeEvent                 (QResizeEvent* event);

    private:
};

