// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerOdinSourceHighlighter.h"

SeerOdinSourceHighlighter::SeerOdinSourceHighlighter (QTextDocument* parent) : SeerSourceHighlighter(parent) {

    // Set to default formats.
    setHighlighterSettings(SeerHighlighterSettings::populate(""));
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
        QStringLiteral("\\bpackage\\b"), QStringLiteral("\\bimport\\b"), QStringLiteral("\\bforeign\\b"),
        QStringLiteral("\\bproc\\b"), QStringLiteral("\\breturn\\b"), QStringLiteral("\\bif\\b"),
        QStringLiteral("\\belse\\b"), QStringLiteral("\\bswitch\\b"), QStringLiteral("\\bcase\\b"),
        QStringLiteral("\\bbreak\\b"), QStringLiteral("\\bcontinue\\b"), QStringLiteral("\\bfallthrough\\b"),
        QStringLiteral("\\bfor\\b"), QStringLiteral("\\bdo\\b"), QStringLiteral("\\bin\\b"),
        QStringLiteral("\\bnot_in\\b"), QStringLiteral("\\bdefer\\b"), QStringLiteral("\\bdynamic\\b"),
        QStringLiteral("\\bconst\\b"), QStringLiteral("\\bwhere\\b"), QStringLiteral("\\busing\\b"),
        QStringLiteral("\\bor_else\\b"), QStringLiteral("\\bor_break\\b"), QStringLiteral("\\bor_continue\\b"),
        QStringLiteral("\\bor_return\\b"), QStringLiteral("\\basm\\b"), QStringLiteral("\\bdistinct\\b"),
        QStringLiteral("\\bcontext\\b"), QStringLiteral("\\bnil\\b"), QStringLiteral("\\btrue\\b"),
        QStringLiteral("\\bfalse\\b"), QStringLiteral("\\bstruct\\b"), QStringLiteral("\\benum\\b"),
        QStringLiteral("\\bunion\\b"), QStringLiteral("\\bmap\\b"), QStringLiteral("\\bbit_set\\b"),
        QStringLiteral("\\bbit_field\\b"), QStringLiteral("\\bcast\\b"), QStringLiteral("\\btransmute\\b"),
        QStringLiteral("\\bauto_cast\\b"), QStringLiteral("\\bwhen\\b"),

        // builtin types
        QStringLiteral("\\b(i8|i16|i32|i64|i128|int)\\b"),
        QStringLiteral("\\b(u8|u16|u32|u64|u128|uint|uintptr)\\b"),
        QStringLiteral("\\b(f16|f32|f64|f128)\\b"),
        QStringLiteral("\\bf16le|f32le|f64le|f128le\\b"),
        QStringLiteral("\\b(f16be|f32be|f64be|f128be)\\b"),
        QStringLiteral("\\b(complex32|complex64|complex128)\\b"),
        QStringLiteral("\\b(quaternion64|quaternion128|quaternion256)\\b"),
        QStringLiteral("\\b(bool|b8|b16|b32|b64)\\b"),
        QStringLiteral("\\b(string|cstring|rune)\\b"),
        QStringLiteral("\\b(rawptr)\\b"),
        QStringLiteral("\\b(any|typeid)\\b"),
        QStringLiteral("\\b(byte)\\b"),
        QStringLiteral("\\b(u16le|u32le|u64le|u128le|i16le|i32le|i64le|i128le)\\b"),
        QStringLiteral("\\b(i16be|i32be|i64be|i128be|u16be|u32be|u64be|u128be)\\b"),

        // matches all the #+ and # compiler directives/builtins
        QStringLiteral("#([^\\s\\(]+)(\\([^\\)]*\\))?"),
        // matches all attributes like @(attribute) with optional parentheses
        QStringLiteral("@(\\([^\\)]*\\)|\\w+)"),
    };

    _highlightingRules.clear(); // Clear old rules.

    HighlightingRule rule;

    // Set class format and expression.
    rule.pattern = QRegularExpression(QStringLiteral("\\b[A-Z][A-Z0-9_]*\\b"));
    rule.format  = _classFormat;
    _highlightingRules.append(rule);

    // Set quote format and expression.
    rule.pattern = QRegularExpression(QStringLiteral("\".*\""));
    rule.format  = _quotationFormat;
    _highlightingRules.append(rule);

    // Set function format and expression.
    rule.pattern = QRegularExpression(QStringLiteral("\\b[A-Za-z0-9_]+(?=\\s*\\()"));
    rule.format  = _functionFormat;
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


