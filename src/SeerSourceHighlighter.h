/*

## Syntax highlighting

To add syntax highlighting to a language. Subclass `SeerSourceHighlighter`
and then instatiate your highlighter by matching the filename in the `getSourceHighlighter` function.

*/

#pragma once

#include "SeerHighlighterSettings.h"
#include <QtCore/QRegularExpression>
#include <QtGui/QSyntaxHighlighter>


class SeerSourceHighlighter : public QSyntaxHighlighter {

  Q_OBJECT

public:
  SeerSourceHighlighter(QTextDocument *parent = 0);

  virtual const SeerHighlighterSettings &highlighterSettings() = 0;
  virtual void
  setHighlighterSettings(const SeerHighlighterSettings &settings) = 0;

protected:
  virtual void highlightBlock(const QString &text);

  SeerHighlighterSettings _highlighterSettings;

  QTextCharFormat _singleLineCommentFormat;
  QTextCharFormat _multiLineCommentFormat;
  QRegularExpression _commentStartExpression;
  QRegularExpression _commentEndExpression;

  QTextCharFormat _keywordFormat;
  QTextCharFormat _classFormat;
  QTextCharFormat _quotationFormat;
  QTextCharFormat _functionFormat;

  struct HighlightingRule {
    QRegularExpression pattern;
    QTextCharFormat format;
  };

  QVector<HighlightingRule> _highlightingRules{};
};

SeerSourceHighlighter *getSourceHighlighter(QString const& file, SeerHighlighterSettings settings);
