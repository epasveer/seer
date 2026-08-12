// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerHighlighterSettings.h"
#include "SeerSourceHighlighter.h"

class SeerFortranSourceHighlighter : public SeerSourceHighlighter {

    Q_OBJECT

    public:
        SeerFortranSourceHighlighter(QTextDocument *parent = 0);

        virtual void        setHighlighterSettings          (const SeerHighlighterSettings &settings) override;
};
