// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerFortranSourceHighlighter.h"

SeerFortranSourceHighlighter::SeerFortranSourceHighlighter(QTextDocument *parent) : SeerSourceHighlighter(parent) {

    // Set to default formats.
    setHighlighterSettings(SeerHighlighterSettings::populate(""));
}

void SeerFortranSourceHighlighter::setHighlighterSettings(const SeerHighlighterSettings &settings) {

    _highlighterSettings     = settings;

    _classFormat             = _highlighterSettings.get("Class");
    _quotationFormat         = _highlighterSettings.get("Quotation");
    _functionFormat          = _highlighterSettings.get("Function");
    _singleLineCommentFormat = _highlighterSettings.get("Comment");
    _multiLineCommentFormat  = _highlighterSettings.get("Multiline Comment");
    _keywordFormat           = _highlighterSettings.get("Keyword");

    // Free-form Fortran (90 and later).
    const QString keywordPatterns[] = {
        QStringLiteral("\\babstract\\b"),     QStringLiteral("\\ballocatable\\b"), QStringLiteral("\\ballocate\\b"),
        QStringLiteral("\\bassignment\\b"),   QStringLiteral("\\bassociate\\b"),   QStringLiteral("\\basynchronous\\b"),
        QStringLiteral("\\bbackspace\\b"),    QStringLiteral("\\bbind\\b"),        QStringLiteral("\\bblock\\b"),
        QStringLiteral("\\bcall\\b"),         QStringLiteral("\\bcase\\b"),        QStringLiteral("\\bcharacter\\b"),
        QStringLiteral("\\bclass\\b"),        QStringLiteral("\\bclose\\b"),       QStringLiteral("\\bcommon\\b"),
        QStringLiteral("\\bcomplex\\b"),      QStringLiteral("\\bconcurrent\\b"),  QStringLiteral("\\bcontains\\b"),
        QStringLiteral("\\bcontinue\\b"),     QStringLiteral("\\bcritical\\b"),    QStringLiteral("\\bcycle\\b"),
        QStringLiteral("\\bdata\\b"),         QStringLiteral("\\bdeallocate\\b"),  QStringLiteral("\\bdefault\\b"),
        QStringLiteral("\\bdimension\\b"),    QStringLiteral("\\bdo\\b"),          QStringLiteral("\\bdouble\\b"),
        QStringLiteral("\\belemental\\b"),    QStringLiteral("\\belse\\b"),        QStringLiteral("\\belseif\\b"),
        QStringLiteral("\\belsewhere\\b"),    QStringLiteral("\\bend\\b"),         QStringLiteral("\\benddo\\b"),
        QStringLiteral("\\bendfile\\b"),      QStringLiteral("\\bendif\\b"),       QStringLiteral("\\bentry\\b"),
        QStringLiteral("\\benum\\b"),         QStringLiteral("\\benumerator\\b"),  QStringLiteral("\\bequivalence\\b"),
        QStringLiteral("\\berror\\b"),        QStringLiteral("\\bexit\\b"),        QStringLiteral("\\bextends\\b"),
        QStringLiteral("\\bexternal\\b"),     QStringLiteral("\\bfinal\\b"),       QStringLiteral("\\bforall\\b"),
        QStringLiteral("\\bformat\\b"),       QStringLiteral("\\bfunction\\b"),    QStringLiteral("\\bgeneric\\b"),
        QStringLiteral("\\bgoto\\b"),         QStringLiteral("\\bif\\b"),          QStringLiteral("\\bimplicit\\b"),
        QStringLiteral("\\bimport\\b"),       QStringLiteral("\\binclude\\b"),     QStringLiteral("\\binout\\b"),
        QStringLiteral("\\binquire\\b"),      QStringLiteral("\\binteger\\b"),     QStringLiteral("\\bintent\\b"),
        QStringLiteral("\\binterface\\b"),    QStringLiteral("\\bintrinsic\\b"),   QStringLiteral("\\bkind\\b"),
        QStringLiteral("\\blen\\b"),          QStringLiteral("\\blogical\\b"),     QStringLiteral("\\bmodule\\b"),
        QStringLiteral("\\bnamelist\\b"),     QStringLiteral("\\bnone\\b"),        QStringLiteral("\\bnullify\\b"),
        QStringLiteral("\\bonly\\b"),         QStringLiteral("\\bopen\\b"),        QStringLiteral("\\boperator\\b"),
        QStringLiteral("\\boptional\\b"),     QStringLiteral("\\bparameter\\b"),   QStringLiteral("\\bpointer\\b"),
        QStringLiteral("\\bprecision\\b"),    QStringLiteral("\\bprint\\b"),       QStringLiteral("\\bprivate\\b"),
        QStringLiteral("\\bprocedure\\b"),    QStringLiteral("\\bprogram\\b"),     QStringLiteral("\\bprotected\\b"),
        QStringLiteral("\\bpublic\\b"),       QStringLiteral("\\bpure\\b"),        QStringLiteral("\\bread\\b"),
        QStringLiteral("\\breal\\b"),         QStringLiteral("\\brecursive\\b"),   QStringLiteral("\\bresult\\b"),
        QStringLiteral("\\breturn\\b"),       QStringLiteral("\\brewind\\b"),      QStringLiteral("\\bsave\\b"),
        QStringLiteral("\\bselect\\b"),       QStringLiteral("\\bsequence\\b"),    QStringLiteral("\\bstop\\b"),
        QStringLiteral("\\bsubmodule\\b"),    QStringLiteral("\\bsubroutine\\b"),  QStringLiteral("\\btarget\\b"),
        QStringLiteral("\\bthen\\b"),         QStringLiteral("\\btype\\b"),        QStringLiteral("\\buse\\b"),
        QStringLiteral("\\bvalue\\b"),        QStringLiteral("\\bvolatile\\b"),    QStringLiteral("\\bwhere\\b"),
        QStringLiteral("\\bwhile\\b"),        QStringLiteral("\\bwrite\\b"),
        // Dotted logical/relational operators and constants. (The symbolic
        // forms '==', '/=', ... are left unstyled, like in the C++ highlighter.)
        QStringLiteral("\\.and\\."),          QStringLiteral("\\.or\\."),          QStringLiteral("\\.not\\."),
        QStringLiteral("\\.eqv\\."),          QStringLiteral("\\.neqv\\."),        QStringLiteral("\\.true\\."),
        QStringLiteral("\\.false\\."),        QStringLiteral("\\.eq\\."),          QStringLiteral("\\.ne\\."),
        QStringLiteral("\\.lt\\."),           QStringLiteral("\\.le\\."),          QStringLiteral("\\.gt\\."),
        QStringLiteral("\\.ge\\."),
    };

    _highlightingRules.clear(); // Clear old rules.

    HighlightingRule rule;

    // No class rule: Fortran is case insensitive with no reliable case convention,
    // and derived types are already covered by the 'type'/'class' keywords.

    // No function rule either: 'name(...)' is also how Fortran references arrays,
    // so highlighting it as a function call would be wrong half of the time.

    // Set keywords format and expression.
    // Fortran is case insensitive, so we use case insensitive option.
    for (const QString &pattern : keywordPatterns) {
        rule.pattern = QRegularExpression(pattern, QRegularExpression::CaseInsensitiveOption);
        rule.format  = _keywordFormat;
        _highlightingRules.append(rule);
    }

    // Set keyword format for the symbolic operators too, so '==' renders like
    // its word form '.eq.'. Safe in Fortran: no templates, '<'/'>' are always
    // relational. Two-character operators first in the alternation.
    rule.pattern = QRegularExpression(QStringLiteral("//|==|/=|<=|>=|=>|<|>"));
    rule.format  = _keywordFormat;
    _highlightingRules.append(rule);

    // Set function format for the common intrinsics, when actually called.
    // A generic before-parenthesis rule is out (see above), but a closed list
    // of intrinsics followed by '(' is safe. As a side effect, 'len'/'kind'
    // stay keywords in declarations ('len=32') and become functions in calls.
    rule.pattern = QRegularExpression(QStringLiteral(
        "\\b(?:abs|achar|adjustl|adjustr|aimag|allocated|associated|atan2?|ceiling|char|cmplx|conjg|cosh?|count"
        "|cpu_time|dble|dot_product|epsilon|exp|floor|huge|iachar|index|int|kind|lbound|len|len_trim|log|log10"
        "|matmul|max|maxval|merge|min|minval|mod|modulo|nint|norm2|pack|present|product|random_number|random_seed"
        "|real|repeat|reshape|scan|sign|sinh?|size|spread|sqrt|sum|system_clock|tanh?|tiny|transfer|transpose"
        "|trim|ubound|unpack|verify)\\b(?=\\s*\\()"), QRegularExpression::CaseInsensitiveOption);
    rule.format  = _functionFormat;
    _highlightingRules.append(rule);

    // Set quote format and expression. Fortran strings use either quote character.
    rule.pattern = QRegularExpression(QStringLiteral("\"[^\"]*\""));
    rule.format  = _quotationFormat;
    _highlightingRules.append(rule);

    rule.pattern = QRegularExpression(QStringLiteral("'[^']*'"));
    rule.format  = _quotationFormat;
    _highlightingRules.append(rule);

    // Set single line comment format and expression. (Fortran uses !)
    // Note '//' is the string concatenation operator in Fortran, not a comment.
    rule.pattern = QRegularExpression(QStringLiteral("![^\n]*"));
    rule.format  = _singleLineCommentFormat;
    _highlightingRules.append(rule);

    // Multi-line comments are not possible in Fortran.
    // Using a regex that matches nothing to avoid any unintended highlighting.
    _commentStartExpression = QRegularExpression(QStringLiteral("(?!)"));
    _commentEndExpression   = QRegularExpression(QStringLiteral("(?!)"));
}
