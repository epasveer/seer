// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerKeySettings.h"
#include <QtWidgets/QWidget>

#include "ui_SeerKeysConfigPage.h"

class SeerKeysConfigPage : public QWidget, protected Ui::SeerKeysConfigPage {

    Q_OBJECT

    public:
        explicit SeerKeysConfigPage (QWidget* parent = 0);
       ~SeerKeysConfigPage ();

        void                        setKeySettings              (const SeerKeySettings& settings);
        SeerKeySettings             keySettings                 () const;

        void                        reset                       ();

    protected slots:

    private:
};

