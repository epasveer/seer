// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerLogWidget.h"

class SeerTildeLogWidget : public SeerLogWidget {

    Q_OBJECT

    public:
        explicit SeerTildeLogWidget (QWidget* parent = 0);
       ~SeerTildeLogWidget ();

        void                processText                 (const QString& text);
};

