#include "SeerCppSourceHighlighter.h"

SeerCppSourceHighlighter::SeerCppSourceHighlighter (QTextDocument* parent) : QSyntaxHighlighter(parent) {

    // Set to default formats.
    setHighlighterSettings(SeerHighlighterSettings::populateForCPP());
}

const SeerHighlighterSettings& SeerCppSourceHighlighter::highlighterSettings() {

    return _highlighterSettings;
}

void SeerCppSourceHighlighter::setHighlighterSettings (const SeerHighlighterSettings& settings) {

    _highlighterSettings     = settings;

    _classFormat             = _highlighterSettings.get("Class");
    _quotationFormat         = _highlighterSettings.get("Quotation");
    _functionFormat          = _highlighterSettings.get("Function");
    _singleLineCommentFormat = _highlighterSettings.get("Comment");
    _multiLineCommentFormat  = _highlighterSettings.get("Multiline Comment");
    _keywordFormat           = _highlighterSettings.get("Keyword");


    /* List from QT example.
    const QString keywordPatterns[] = {
        QStringLiteral("\\bchar\\b"),     QStringLiteral("\\bclass\\b"),     QStringLiteral("\\bconst\\b"),
        QStringLiteral("\\bdouble\\b"),   QStringLiteral("\\benum\\b"),      QStringLiteral("\\bexplicit\\b"),
        QStringLiteral("\\bfriend\\b"),   QStringLiteral("\\binline\\b"),    QStringLiteral("\\bint\\b"),
        QStringLiteral("\\blong\\b"),     QStringLiteral("\\bnamespace\\b"), QStringLiteral("\\boperator\\b"),
        QStringLiteral("\\bprivate\\b"),  QStringLiteral("\\bprotected\\b"), QStringLiteral("\\bpublic\\b"),
        QStringLiteral("\\bshort\\b"),    QStringLiteral("\\bsignals\\b"),   QStringLiteral("\\bsigned\\b"),
        QStringLiteral("\\bslots\\b"),    QStringLiteral("\\bstatic\\b"),    QStringLiteral("\\bstruct\\b"),
        QStringLiteral("\\btemplate\\b"), QStringLiteral("\\btypedef\\b"),   QStringLiteral("\\btypename\\b"),
        QStringLiteral("\\bunion\\b"),    QStringLiteral("\\bunsigned\\b"),  QStringLiteral("\\bvirtual\\b"),
        QStringLiteral("\\bvoid\\b"),     QStringLiteral("\\bvolatile\\b"),  QStringLiteral("\\bbool\\b")
    };
    */

    // List from the kdbg debugger.
    const QString keywordPatterns[] = {
        QStringLiteral("\\balignas\\b"),    QStringLiteral("\\balignof\\b"),            QStringLiteral("\\band\\b"),
        QStringLiteral("\\band_eq\\b"),     QStringLiteral("\\basm\\b"),                QStringLiteral("\\bauto\\b"),
        QStringLiteral("\\bbitand\\b"),     QStringLiteral("\\bbitor\\b"),              QStringLiteral("\\bbool\\b"),
        QStringLiteral("\\bbreak\\b"),      QStringLiteral("\\bcase\\b"),               QStringLiteral("\\bcatch\\b"),
        QStringLiteral("\\bchar\\b"),       QStringLiteral("\\bchar16_t\\b"),           QStringLiteral("\\bchar32_t\\b"),
        QStringLiteral("\\bclass\\b"),      QStringLiteral("\\bcompl\\b"),              QStringLiteral("\\bconst\\b"),
        QStringLiteral("\\bconst_cast\\b"), QStringLiteral("\\bconstexpr\\b"),          QStringLiteral("\\bcontinue\\b"),
        QStringLiteral("\\bdecltype\\b"),   QStringLiteral("\\bdefault\\b"),            QStringLiteral("\\bdelete\\b"),
        QStringLiteral("\\bdo\\b"),         QStringLiteral("\\bdouble\\b"),             QStringLiteral("\\bdynamic_cast\\b"),
        QStringLiteral("\\belse\\b"),       QStringLiteral("\\benum\\b"),               QStringLiteral("\\bexplicit\\b"),
        QStringLiteral("\\bexport\\b"),     QStringLiteral("\\bextern\\b"),             QStringLiteral("\\bfalse\\b"),
        QStringLiteral("\\bfloat\\b"),      QStringLiteral("\\bfor\\b"),                QStringLiteral("\\bfriend\\b"),
        QStringLiteral("\\bgoto\\b"),       QStringLiteral("\\bif\\b"),                 QStringLiteral("\\binline\\b"),
        QStringLiteral("\\bint\\b"),        QStringLiteral("\\blong\\b"),               QStringLiteral("\\bmutable\\b"),
        QStringLiteral("\\bnamespace\\b"),  QStringLiteral("\\bnew\\b"),                QStringLiteral("\\bnoexcept\\b"),
        QStringLiteral("\\bnot\\b"),        QStringLiteral("\\bnot_eq\\b"),             QStringLiteral("\\bnullptr\\b"),
        QStringLiteral("\\boperator\\b"),   QStringLiteral("\\bor\\b"),                 QStringLiteral("\\bor_eq\\b"),
        QStringLiteral("\\bprivate\\b"),    QStringLiteral("\\bprotected\\b"),          QStringLiteral("\\bpublic\\b"),
        QStringLiteral("\\bregister\\b"),   QStringLiteral("\\breinterpret_cast\\b"),   QStringLiteral("\\breturn\\b"),
        QStringLiteral("\\bshort\\b"),      QStringLiteral("\\bsigned\\b"),             QStringLiteral("\\bsizeof\\b"),
        QStringLiteral("\\bstatic\\b"),     QStringLiteral("\\bstatic_assert\\b"),      QStringLiteral("\\bstatic_cast\\b"),
        QStringLiteral("\\bstruct\\b"),     QStringLiteral("\\bswitch\\b"),             QStringLiteral("\\btemplate\\b"),
        QStringLiteral("\\bthis\\b"),       QStringLiteral("\\bthread_local\\b"),       QStringLiteral("\\bthrow\\b"),
        QStringLiteral("\\btrue\\b"),       QStringLiteral("\\btry\\b"),                QStringLiteral("\\btypedef\\b"),
        QStringLiteral("\\btypeid\\b"),     QStringLiteral("\\btypename\\b"),           QStringLiteral("\\bunion\\b"),
        QStringLiteral("\\bunsigned\\b"),   QStringLiteral("\\busing\\b"),              QStringLiteral("\\bvirtual\\b"),
        QStringLiteral("\\bvoid\\b"),       QStringLiteral("\\bvolatile\\b"),           QStringLiteral("\\bwchar_t\\b"),
        QStringLiteral("\\bwhile\\b"),      QStringLiteral("\\bxor\\b"),                QStringLiteral("\\bxor_eq\\b"),
        QStringLiteral("\\bslots\\b"),      QStringLiteral("\\bsignals\\b")
    };

    _highlightingRules.clear(); // Clear old rules.

    HighlightingRule rule;

    for (const QString& pattern : keywordPatterns) {

        rule.pattern = QRegularExpression(pattern);
        rule.format  = _keywordFormat;

        _highlightingRules.append(rule);
    }

    // Set class format and expression.
    rule.pattern = QRegularExpression(QStringLiteral("\\bQ[A-Za-z]+\\b"));
    rule.format = _classFormat;
    _highlightingRules.append(rule);

    // Set quote format and expression.
    rule.pattern = QRegularExpression(QStringLiteral("\".*\""));
    rule.format = _quotationFormat;
    _highlightingRules.append(rule);

    // Set function format and expression.
    rule.pattern = QRegularExpression(QStringLiteral("\\b[A-Za-z0-9_]+(?=\\()"));
    rule.format = _functionFormat;
    _highlightingRules.append(rule);

    // Set single line comment format and expression.
    rule.pattern = QRegularExpression(QStringLiteral("//[^\n]*"));
    rule.format  = _singleLineCommentFormat;
    _highlightingRules.append(rule);

    // Set multi-line comment expression. Format is defined later.
    _commentStartExpression = QRegularExpression(QStringLiteral("/\\*"));
    _commentEndExpression   = QRegularExpression(QStringLiteral("\\*/"));
}

void SeerCppSourceHighlighter::highlightBlock (const QString& text) {

    for (const HighlightingRule& rule : qAsConst(_highlightingRules)) {

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

