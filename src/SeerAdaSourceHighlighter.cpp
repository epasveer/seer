// SPDX-FileCopyrightText: 2024 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerAdaSourceHighlighter.h"

SeerAdaSourceHighlighter::SeerAdaSourceHighlighter(QTextDocument *parent) : SeerSourceHighlighter(parent) {

    // Set to default formats.
    setHighlighterSettings(SeerHighlighterSettings::populate(""));
}

void SeerAdaSourceHighlighter::setHighlighterSettings(const SeerHighlighterSettings &settings) {

    _highlighterSettings     = settings;

    _classFormat             = _highlighterSettings.get("Class");
    _quotationFormat         = _highlighterSettings.get("Quotation");
    _functionFormat          = _highlighterSettings.get("Function");
    _singleLineCommentFormat = _highlighterSettings.get("Comment");
    _multiLineCommentFormat  = _highlighterSettings.get("Multiline Comment");
    _keywordFormat           = _highlighterSettings.get("Keyword");

    const QString keywordPatterns[] = {
        QStringLiteral("\\babort\\b"),        QStringLiteral("\\babs\\b"),         QStringLiteral("\\babstract\\b"),
        QStringLiteral("\\baccept\\b"),       QStringLiteral("\\baccess\\b"),      QStringLiteral("\\baliased\\b"),
        QStringLiteral("\\ball\\b"),          QStringLiteral("\\band\\b"),         QStringLiteral("\\barray\\b"),
        QStringLiteral("\\bat\\b"),           QStringLiteral("\\bbegin\\b"),       QStringLiteral("\\bbody\\b"),
        QStringLiteral("\\bcase\\b"),         QStringLiteral("\\bconstant\\b"),    QStringLiteral("\\bdeclare\\b"),
        QStringLiteral("\\bdelay\\b"),        QStringLiteral("\\bdelta\\b"),       QStringLiteral("\\bdigits\\b"),
        QStringLiteral("\\bdo\\b"),           QStringLiteral("\\belse\\b"),        QStringLiteral("\\belsif\\b"),
        QStringLiteral("\\bend\\b"),          QStringLiteral("\\bentry\\b"),       QStringLiteral("\\bexception\\b"),
        QStringLiteral("\\bexit\\b"),         QStringLiteral("\\bfor\\b"),         QStringLiteral("\\bfunction\\b"),
        QStringLiteral("\\bgeneric\\b"),      QStringLiteral("\\bgoto\\b"),        QStringLiteral("\\bif\\b"),
        QStringLiteral("\\bin\\b"),           QStringLiteral("\\binterface\\b"),   QStringLiteral("\\bis\\b"),
        QStringLiteral("\\blimited\\b"),      QStringLiteral("\\bloop\\b"),        QStringLiteral("\\bmod\\b"),
        QStringLiteral("\\bnew\\b"),          QStringLiteral("\\bnot\\b"),         QStringLiteral("\\bnull\\b"),
        QStringLiteral("\\bof\\b"),           QStringLiteral("\\bor\\b"),          QStringLiteral("\\bothers\\b"),
        QStringLiteral("\\bout\\b"),          QStringLiteral("\\boverriding\\b"),  QStringLiteral("\\bpackage\\b"),
        QStringLiteral("\\bpragma\\b"),       QStringLiteral("\\bprivate\\b"),     QStringLiteral("\\bprocedure\\b"),
        QStringLiteral("\\bprotected\\b"),    QStringLiteral("\\braise\\b"),       QStringLiteral("\\brange\\b"),
        QStringLiteral("\\brecord\\b"),       QStringLiteral("\\brem\\b"),         QStringLiteral("\\brenames\\b"),
        QStringLiteral("\\brequeue\\b"),      QStringLiteral("\\breturn\\b"),      QStringLiteral("\\breverse\\b"),
        QStringLiteral("\\bselect\\b"),       QStringLiteral("\\bseparate\\b"),    QStringLiteral("\\bsome\\b"),
        QStringLiteral("\\bsubtype\\b"),      QStringLiteral("\\bsynchronized\\b"),QStringLiteral("\\btagged\\b"),
        QStringLiteral("\\btask\\b"),         QStringLiteral("\\bterminate\\b"),   QStringLiteral("\\bthen\\b"),
        QStringLiteral("\\btype\\b"),         QStringLiteral("\\buntil\\b"),       QStringLiteral("\\buse\\b"),
        QStringLiteral("\\bwhen\\b"),         QStringLiteral("\\bwhile\\b"),       QStringLiteral("\\bwith\\b"),
        QStringLiteral("\\bxor\\b"),
    };

    _highlightingRules.clear(); // Clear old rules.

    HighlightingRule rule;

    // Set class format and expression.
    // (Ada typically doesn't have a distinguished class case convention, but we can reuse uppercase Start)
    rule.pattern = QRegularExpression(QStringLiteral("\\b[A-Z][A-z0-9_]*\\b"));
    rule.format  = _classFormat;
    _highlightingRules.append(rule);

    // Set function format and expression.
    // This won't highlight parameterless subprograms, but it's still useful for most cases.
    rule.pattern = QRegularExpression(QStringLiteral("\\b[A-Za-z0-9_!]+(?=\\s*\\()"));
    rule.format  = _functionFormat;
    _highlightingRules.append(rule);

    // Set keywords format and expression (must have precedence over functions)
    // Ada is case insensitive, so we use case insensitive option.
    for (const QString &pattern : keywordPatterns) {
        rule.pattern = QRegularExpression(pattern, QRegularExpression::CaseInsensitiveOption);
        rule.format  = _keywordFormat;
        _highlightingRules.append(rule);
    }

    // Set quote format and expression.
    rule.pattern = QRegularExpression(QStringLiteral("\".*\""));
    rule.format  = _quotationFormat;
    _highlightingRules.append(rule);

    // Set single line comment format and expression. (Ada uses --)
    rule.pattern = QRegularExpression(QStringLiteral("--[^\n]*"));
    rule.format  = _singleLineCommentFormat;
    _highlightingRules.append(rule);

    // Multi-line comments are not possible in Ada.
    // Using a regex that matches nothing to avoid any unintended highlighting.
    _commentStartExpression = QRegularExpression(QStringLiteral("(?!)"));
    _commentEndExpression   = QRegularExpression(QStringLiteral("(?!)"));
}
