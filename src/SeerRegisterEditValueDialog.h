// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>

#include "ui_SeerRegisterEditValueDialog.h"

class SeerRegisterEditValueDialog : public QDialog, protected Ui::SeerRegisterEditValueDialogForm {

    Q_OBJECT

    public:
        explicit SeerRegisterEditValueDialog (QWidget* parent = 0);
       ~SeerRegisterEditValueDialog ();

        void            set                         (const QString& regname, const QString& regvalue);

        QString         nameText                    () const;
        QString         valueText                   () const;

    public slots:
    private:
};

