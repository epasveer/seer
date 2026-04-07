// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>

#include "ui_SeerAboutDialog.h"

class SeerAboutDialog : public QDialog, protected Ui::SeerAboutDialogForm {

    Q_OBJECT

    public:
        explicit SeerAboutDialog (QWidget* parent = 0);
       ~SeerAboutDialog ();

    public slots:

    private:
};

