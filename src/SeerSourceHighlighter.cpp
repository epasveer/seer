// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerSourceHighlighter.h"
#include "SeerOdinSourceHighlighter.h"
#include "SeerCppSourceHighlighter.h"
#include "SeerRustSourceHighlighter.h"

SeerSourceHighlighter::SeerSourceHighlighter (QTextDocument* parent) : QSyntaxHighlighter(parent) {}

const SeerHighlighterSettings& SeerSourceHighlighter::highlighterSettings() {
    return _highlighterSettings;
}

SeerSourceHighlighter* SeerSourceHighlighter::getSourceHighlighter(QString const& file, SeerHighlighterSettings settings) {
    QRegularExpression cpp_re("(?:" + settings.cppSourceSuffixes() + ")$");
    if (file.contains(cpp_re)) {
      return new SeerCppSourceHighlighter(0);
    }

    if (file.endsWith(".odin")) {
      return new SeerOdinSourceHighlighter(0);
    }

    if (file.endsWith(".rs")) {
        return new SeerRustSourceHighlighter(0);
    }

    return nullptr;
}

void SeerSourceHighlighter::highlightBlock (const QString& text) {

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

