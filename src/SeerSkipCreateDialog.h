// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>

#include "ui_SeerSkipCreateDialog.h"

class SeerSkipCreateDialog : public QDialog, protected Ui::SeerSkipCreateDialogForm {

    Q_OBJECT

    public:
        explicit SeerSkipCreateDialog (QWidget* parent = 0);
       ~SeerSkipCreateDialog ();

        QString         skipMode                    () const;
        QString         skipParameters              () const;

    public slots:
    private slots:
        void            handleModeButtonGroup       ();
        void            handleHelpToolButton        ();

    private:
};

