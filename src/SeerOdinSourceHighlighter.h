// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerHighlighterSettings.h"
#include "SeerSourceHighlighter.h"

#include <QtCore/QRegularExpression>
#include <QtCore/QString>
#include <QtCore/QVector>
#include <QtGui/QSyntaxHighlighter>
#include <QtGui/QTextCharFormat>
#include <QtGui/QTextDocument>

class SeerOdinSourceHighlighter : public SeerSourceHighlighter {

  Q_OBJECT

public:
  SeerOdinSourceHighlighter(QTextDocument *parent = 0);

  virtual const SeerHighlighterSettings &highlighterSettings() override;
  virtual void
  setHighlighterSettings(const SeerHighlighterSettings &settings) override;
};
