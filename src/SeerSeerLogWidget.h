// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerLogWidget.h"

class SeerSeerLogWidget : public SeerLogWidget {

    Q_OBJECT

    public:
        explicit SeerSeerLogWidget (QWidget* parent = 0);
       ~SeerSeerLogWidget ();

        void                processText                 (const QString& text);
};

