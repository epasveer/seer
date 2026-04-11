// SPDX-FileCopyrightText: 2024 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerHighlighterSettings.h"
#include "SeerSourceHighlighter.h"

class SeerAdaSourceHighlighter : public SeerSourceHighlighter {

    Q_OBJECT

    public:
        SeerAdaSourceHighlighter(QTextDocument *parent = 0);

        virtual void        setHighlighterSettings          (const SeerHighlighterSettings &settings) override;
};
