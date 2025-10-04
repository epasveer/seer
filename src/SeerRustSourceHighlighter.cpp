// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerRustSourceHighlighter.h"

SeerRustSourceHighlighter::SeerRustSourceHighlighter(QTextDocument *parent)
    : SeerSourceHighlighter(parent) {

  // Set to default formats.
  setHighlighterSettings(SeerHighlighterSettings::populate(""));
}

void SeerRustSourceHighlighter::setHighlighterSettings(
    const SeerHighlighterSettings &settings) {
  _highlighterSettings = settings;

  _classFormat = _highlighterSettings.get("Class");
  _quotationFormat = _highlighterSettings.get("Quotation");
  _functionFormat = _highlighterSettings.get("Function");
  _singleLineCommentFormat = _highlighterSettings.get("Comment");
  _multiLineCommentFormat = _highlighterSettings.get("Multiline Comment");
  _keywordFormat = _highlighterSettings.get("Keyword");

  const QString keywordPatterns[] = {
      QStringLiteral("\\bas\\b"),       QStringLiteral("\\bbreak\\b"),
      QStringLiteral("\\bconst\\b"),    QStringLiteral("\\bcontinue\\b"),
      QStringLiteral("\\bcrate\\b"),    QStringLiteral("\\belse\\b"),
      QStringLiteral("\\benum\\b"),     QStringLiteral("\\bextern\\b"),
      QStringLiteral("\\bfalse\\b"),    QStringLiteral("\\bfn\\b"),
      QStringLiteral("\\bfor\\b"),      QStringLiteral("\\bif\\b"),
      QStringLiteral("\\bimpl\\b"),     QStringLiteral("\\bin\\b"),
      QStringLiteral("\\blet\\b"),      QStringLiteral("\\bloop\\b"),
      QStringLiteral("\\bmatch\\b"),    QStringLiteral("\\bmod\\b"),
      QStringLiteral("\\bmove\\b"),     QStringLiteral("\\bmut\\b"),
      QStringLiteral("\\bpub\\b"),      QStringLiteral("\\bref\\b"),
      QStringLiteral("\\breturn\\b"),   QStringLiteral("\\bself\\b"),
      QStringLiteral("\\bSelf\\b"),     QStringLiteral("\\bstatic\\b"),
      QStringLiteral("\\bstruct\\b"),   QStringLiteral("\\bsuper\\b"),
      QStringLiteral("\\btrait\\b"),    QStringLiteral("\\btrue\\b"),
      QStringLiteral("\\btype\\b"),     QStringLiteral("\\bunsafe\\b"),
      QStringLiteral("\\buse\\b"),      QStringLiteral("\\bwhere\\b"),
      QStringLiteral("\\bwhile\\b"),    QStringLiteral("\\basync\\b"),
      QStringLiteral("\\bawait\\b"),    QStringLiteral("\\bdyn\\b"),
      QStringLiteral("\\babstract\\b"), QStringLiteral("\\bbecome\\b"),
      QStringLiteral("\\bbox\\b"),      QStringLiteral("\\bdo\\b"),
      QStringLiteral("\\bfinal\\b"),    QStringLiteral("\\bmacro\\b"),
      QStringLiteral("\\boverride\\b"), QStringLiteral("\\bpriv\\b"),
      QStringLiteral("\\btypeof\\b"),   QStringLiteral("\\bunsized\\b"),
      QStringLiteral("\\bvirtual\\b"),  QStringLiteral("\\byield\\b"),
      QStringLiteral("\\btry\\b"),      QStringLiteral("\\bgen\\b"),

      QStringLiteral("#!?\\[.*\\]"),
  };

  _highlightingRules.clear(); // Clear old rules.

  HighlightingRule rule;

  // Set class format and expression.
  rule.pattern = QRegularExpression(QStringLiteral("\\b[A-Z][A-z0-9_]*\\b"));
  rule.format = _classFormat;
  _highlightingRules.append(rule);

  // Set quote format and expression.
  rule.pattern = QRegularExpression(QStringLiteral("\".*\""));
  rule.format = _quotationFormat;
  _highlightingRules.append(rule);

  // Set function format and expression.
  rule.pattern =
      QRegularExpression(QStringLiteral("\\b[A-Za-z0-9_!]+(?=\\s*\\()"));
  rule.format = _functionFormat;
  _highlightingRules.append(rule);

  // Set keywords format and expression (must have precedence over functions)
  for (const QString &pattern : keywordPatterns) {
    rule.pattern = QRegularExpression(pattern);
    rule.format = _keywordFormat;
    _highlightingRules.append(rule);
  }

  // Set single line comment format and expression.
  rule.pattern = QRegularExpression(QStringLiteral("//[^\n]*"));
  rule.format = _singleLineCommentFormat;
  _highlightingRules.append(rule);

  // Set multi-line comment expression. Format is defined later.
  _commentStartExpression = QRegularExpression(QStringLiteral("/\\*"));
  _commentEndExpression = QRegularExpression(QStringLiteral("\\*/"));
}
