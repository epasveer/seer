// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerHighlighterSettings.h"
#include "SeerSourceHighlighter.h"

class SeerRustSourceHighlighter : public SeerSourceHighlighter {

    Q_OBJECT

    public:
        SeerRustSourceHighlighter(QTextDocument *parent = 0);

        virtual void        setHighlighterSettings      (const SeerHighlighterSettings &settings) override;
};

