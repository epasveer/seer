// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>
#include <QtCore/QVector>

#include "ui_SeerAssemblyPreferenceDialog.h"

class SeerAssemblyPreferenceDialog : public QDialog, protected Ui::SeerAssemblyPreferenceDialogForm {

    Q_OBJECT

    public:
        explicit SeerAssemblyPreferenceDialog (QWidget* parent = 0);
       ~SeerAssemblyPreferenceDialog ();

        void                        setRegiserNamePC            (const QString& name);
        QString                     regiserNamePC               () const;

        void                        setRegiserNameFLAGS         (const QString& name);
        QString                     regiserNameFLAGS            () const;

        void                        setRegiserNameSP            (const QString& name);
        QString                     regiserNameSP               () const;

        void                        setShowAssemblyAddress      (bool flag);
        bool                        showAssemblyAddress         () const;

        void                        setShowAssemblyOffset       (bool flag);
        bool                        showAssemblyOffset          () const;

        void                        setShowAssemblyOpcode       (bool flag);
        bool                        showAssemblyOpcode          () const;

        void                        setShowAssemblySource       (bool flag);
        bool                        showAssemblySource          () const;

    public slots:

    private slots:

    protected:
        void                        writeSettings               ();
        void                        readSettings                ();
        void                        resizeEvent                 (QResizeEvent* event);

    private:
};

