// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>

#include "ui_SeerSignalEditValueDialog.h"

class SeerSignalEditValueDialog : public QDialog, protected Ui::SeerSignalEditValueDialogForm {

    Q_OBJECT

    public:
        explicit SeerSignalEditValueDialog (QWidget* parent = 0);
       ~SeerSignalEditValueDialog ();

        void            set                         (const QString& signame, const QString& stopvalue, const QString& printvalue, const QString& passvalue, const QString& description);

        QString         nameText                    () const;
        QString         stopText                    () const;
        QString         printText                   () const;
        QString         passText                    () const;
        QString         descriptionText             () const;

    public slots:
    private:
};

