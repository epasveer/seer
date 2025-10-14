// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>

#include "ui_SeerCatchpointCreateDialog.h"

class SeerCatchpointCreateDialog : public QDialog, protected Ui::SeerCatchpointCreateDialogForm {

    Q_OBJECT

    public:
        explicit SeerCatchpointCreateDialog (QWidget* parent = 0);
       ~SeerCatchpointCreateDialog ();

        void            setType                     (const QString& text);
        QString         typeText                    () const;

        void            setTemporaryEnabled         (bool flag);
        bool            temporaryEnabled            () const;

        void            setDisabledEnabled          (bool flag);
        bool            disabledEnabled             () const;

        void            setNameEnabled              (bool flag);
        bool            nameEnabled                 () const;

        void            setArguments                (const QString& text);
        QString         arguments                   () const;

        QString         catchpointText              () const;

    public slots:

    private:
};

