// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerOdinSourceHighlighter.h"

SeerOdinSourceHighlighter::SeerOdinSourceHighlighter (QTextDocument* parent) : SeerSourceHighlighter(parent) {

    // Set to default formats.
    setHighlighterSettings(SeerHighlighterSettings::populateForCPP(""));
}

const SeerHighlighterSettings& SeerOdinSourceHighlighter::highlighterSettings() {

    return _highlighterSettings;
}

void SeerOdinSourceHighlighter::setHighlighterSettings (const SeerHighlighterSettings& settings) {

    _highlighterSettings     = settings;

    _classFormat             = _highlighterSettings.get("Class");
    _quotationFormat         = _highlighterSettings.get("Quotation");
    _functionFormat          = _highlighterSettings.get("Function");
    _singleLineCommentFormat = _highlighterSettings.get("Comment");
    _multiLineCommentFormat  = _highlighterSettings.get("Multiline Comment");
    _keywordFormat           = _highlighterSettings.get("Keyword");

    const QString keywordPatterns[] = {
        QStringLiteral("\\package\\b"),
        QStringLiteral("\\import\\b"),
        QStringLiteral("\\foreign\\b"),
        QStringLiteral("\\proc\\b"),
        QStringLiteral("\\return\\b"),
        QStringLiteral("\\if\\b"),
        QStringLiteral("\\else\\b"),
        QStringLiteral("\\switch\\b"),
        QStringLiteral("\\case\\b"),
        QStringLiteral("\\break\\b"),
        QStringLiteral("\\continue\\b"),
        QStringLiteral("\\fallthrough\\b"),
        QStringLiteral("\\for\\b"),
        QStringLiteral("\\do\\b"),
        QStringLiteral("\\in\\b"),
        QStringLiteral("\\not_in\\b"),
        QStringLiteral("\\defer\\b"),
        QStringLiteral("\\dynamic\\b"),
        QStringLiteral("\\const\\b"),
        QStringLiteral("\\where\\b"),
        QStringLiteral("\\using\\b"),
        QStringLiteral("\\or_else\\b"),
        QStringLiteral("\\or_break\\b"),
        QStringLiteral("\\or_continue\\b"),
        QStringLiteral("\\or_return\\b"),
        QStringLiteral("\\asm\\b"),
        QStringLiteral("\\distinct\\b"),
        QStringLiteral("\\context\\b"),
        QStringLiteral("\\nil\\b"),
        QStringLiteral("\\true\\b"),
        QStringLiteral("\\false\\b"),
        QStringLiteral("\\struct\\b"),
        QStringLiteral("\\enum\\b"),
        QStringLiteral("\\union\\b"),
        QStringLiteral("\\map\\b"),
        QStringLiteral("\\bit_set\\b"),
        QStringLiteral("\\bit_field\\b"),
        QStringLiteral("\\cast\\b"),
        QStringLiteral("\\transmute\\b"),
        QStringLiteral("\\auto_cast\\b"),
    };

    _highlightingRules.clear(); // Clear old rules.

    HighlightingRule rule;

    // Set class format and expression.
    rule.pattern = QRegularExpression(QStringLiteral("\\bQ[A-Za-z]+\\b"));
    rule.format = _classFormat;
    _highlightingRules.append(rule);

    // Set quote format and expression.
    rule.pattern = QRegularExpression(QStringLiteral("\".*\""));
    rule.format = _quotationFormat;
    _highlightingRules.append(rule);

    // Set function format and expression.
    rule.pattern = QRegularExpression(QStringLiteral("\\b[A-Za-z0-9_]+(?=\\s*\\()"));
    rule.format = _functionFormat;
    _highlightingRules.append(rule);

	// Set keywords format and expression (must have precedence over functions)
    for (const QString& pattern : keywordPatterns) {
        rule.pattern = QRegularExpression(pattern);
        rule.format  = _keywordFormat;

        _highlightingRules.append(rule);
    }

    // Set single line comment format and expression.
    rule.pattern = QRegularExpression(QStringLiteral("//[^\n]*"));
    rule.format  = _singleLineCommentFormat;
    _highlightingRules.append(rule);

    // Set multi-line comment expression. Format is defined later.
    _commentStartExpression = QRegularExpression(QStringLiteral("/\\*"));
    _commentEndExpression   = QRegularExpression(QStringLiteral("\\*/"));
}

void SeerOdinSourceHighlighter::highlightBlock (const QString& text) {

    for (const HighlightingRule& rule : std::as_const(_highlightingRules)) {

        QRegularExpressionMatchIterator matchIterator = rule.pattern.globalMatch(text);

        while (matchIterator.hasNext()) {
            QRegularExpressionMatch match = matchIterator.next();
            setFormat(match.capturedStart(), match.capturedLength(), rule.format);
        }
    }

    setCurrentBlockState(0);

    int startIndex = 0;

    if (previousBlockState() != 1) {
        startIndex = text.indexOf(_commentStartExpression);
    }

    while (startIndex >= 0) {

        QRegularExpressionMatch match = _commentEndExpression.match(text, startIndex);

        int endIndex      = match.capturedStart();
        int commentLength = 0;

        if (endIndex == -1) {
            setCurrentBlockState(1);
            commentLength = text.length() - startIndex;

        }else{
            commentLength = endIndex - startIndex + match.capturedLength();
        }

        setFormat(startIndex, commentLength, _multiLineCommentFormat);
        startIndex = text.indexOf(_commentStartExpression, startIndex + commentLength);
    }
}

