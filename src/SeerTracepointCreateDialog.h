// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>
#include <QtCore/QStringList>

#include "ui_SeerTracepointCreateDialog.h"

class SeerTracepointCreateDialog : public QDialog, protected Ui::SeerTracepointCreateDialogForm {

    Q_OBJECT

    public:
        explicit SeerTracepointCreateDialog (QWidget* parent = 0);
       ~SeerTracepointCreateDialog ();

        void            setFilename                 (const QString& text);
        void            setLineNumber               (const QString& text);

        QString         tracepointParameters        () const;       // Arguments for -break-insert. Empty if there's no location.
        QStringList     actions                     () const;       // Collect actions.
        int             passcount                   () const;       // 0 if there's no pass count.

    private slots:
        void            handleHelpToolButtonClicked ();
};
