// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerLogWidget.h"

class SeerGdbLogWidget : public SeerLogWidget {

    Q_OBJECT

    public:
        explicit SeerGdbLogWidget (QWidget* parent = 0);
       ~SeerGdbLogWidget ();

        void                processText                 (const QString& text);

    signals:
        void                refreshBreakpointsList      ();
};

