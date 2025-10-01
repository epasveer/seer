// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerHighlighterSettings.h"
#include "SeerSourceHighlighter.h"

class SeerOdinSourceHighlighter : public SeerSourceHighlighter {

  Q_OBJECT

public:
  SeerOdinSourceHighlighter(QTextDocument *parent = 0);

  virtual void
  setHighlighterSettings(SeerHighlighterSettings const &settings) override;
};
