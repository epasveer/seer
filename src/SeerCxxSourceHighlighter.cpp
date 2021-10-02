#include "SeerCxxSourceHighlighter.h"
#include <algorithm>

//
// This C++ syntax highlighter is based on the one in the kdbg debugger.
// However, it doesn't work well.
//
// I'll leave it in the Seer source directory for reference purposes.
//
// Instead, the SeerCppSourceHighlighter is used.
//

SeerCxxSourceHighlighter::SeerCxxSourceHighlighter (QTextDocument* parent) : QSyntaxHighlighter(parent) {
}

enum HLState {
    hlCommentLine = 1,
    hlCommentBlock,
    hlIdent,
    hlString
};

static const QString ckw[] = {
    "alignas",
    "alignof",
    "and",
    "and_eq",
    "asm",
    "auto",
    "bitand",
    "bitor",
    "bool",
    "break",
    "case",
    "catch",
    "char",
    "char16_t",
    "char32_t",
    "class",
    "compl",
    "const",
    "const_cast",
    "constexpr",
    "continue",
    "decltype",
    "default",
    "delete",
    "do",
    "double",
    "dynamic_cast",
    "else",
    "enum",
    "explicit",
    "export",
    "extern",
    "false",
    "float",
    "for",
    "friend",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "mutable",
    "namespace",
    "new",
    "noexcept",
    "not",
    "not_eq",
    "nullptr",
    "operator",
    "or",
    "or_eq",
    "private",
    "protected",
    "public",
    "register",
    "reinterpret_cast",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "static_assert",
    "static_cast",
    "struct",
    "switch",
    "template",
    "this",
    "thread_local",
    "throw",
    "true",
    "try",
    "typedef",
    "typeid",
    "typename",
    "union",
    "unsigned",
    "using",
    "virtual",
    "void",
    "volatile",
    "wchar_t",
    "while",
    "xor",
    "xor_eq"
};

void SeerCxxSourceHighlighter::highlightBlock (const QString& text) {

    int state = previousBlockState();

    state = highlight(text, state);

    setCurrentBlockState(state);
}

int SeerCxxSourceHighlighter::highlight (const QString& text, int state) {

    if (state == -2) // initial state
        state = 0;

    // check for preprocessor line
    if (state == 0 && text.trimmed().startsWith("#")) {
        setFormat(0, text.length(), QColor("darkgreen"));
        return 0;
    }

    // a font for keywords
    QTextCharFormat identFont;
    identFont.setFontWeight(QFont::Bold);

    int start = 0;
    while (start < text.length()) {
        int end;
        switch (state) {
            case hlCommentLine:
                end = text.length();
                state = 0;
                setFormat(start, end-start, QColor("gray"));
                break;

            case hlCommentBlock:
                end = text.indexOf("*/", start);

                if (end >= 0) {
                    end += 2, state = 0;
                }else{
                    end = text.length();
                }

                setFormat(start, end-start, QColor("gray"));
                break;

            case hlString:
                for (end = start+1; end < int(text.length()); end++) {
                    if (text[end] == '\\') {
                        if (end < int(text.length())) {
                            ++end;
                        }

                    }else if (text[end] == text[start]) {
                        ++end;
                        break;
                    }
                }
                state = 0;
                setFormat(start, end-start, QColor("darkred"));
                break;

            case hlIdent:
                for (end = start+1; end < int(text.length()); end++) {
                    if (!text[end].isLetterOrNumber() && text[end] != '_') {
                        break;
                    }
                }

                state = 0;

                if (isCppKeyword(text.mid(start, end-start))) {
                    setFormat(start, end-start, identFont);
                }else{
                    //setFormat(start, end-start, Normal Format);
                }
                break;

            default:
                for (end = start; end < int(text.length()); end++) {
                    if (text[end] == '/') {
                        if (end+1 < int(text.length())) {
                            if (text[end+1] == '/') {
                                state = hlCommentLine;
                                break;
                            }else if (text[end+1] == '*') {
                                state = hlCommentBlock;
                                break;
                            }
                        }

                    }else if (text[end] == '"' || text[end] == '\'') {
                        state = hlString;
                        break;
                    }else if ((text[end] >= 'A' && text[end] <= 'Z') || (text[end] >= 'a' && text[end] <= 'z') || text[end] == '_') {
                        state = hlIdent;
                        break;
                    }
                }

                //setFormat(start, end-start, Normal Format);
        }
        start = end;
    }

    return state;
}

bool SeerCxxSourceHighlighter::isCppKeyword (const QString& word) {

    // std::binary_search requires the search list to be sorted
    static bool keyword_order_verified = false;

    if (keyword_order_verified == false) {
        for (size_t i = 1; i < sizeof(ckw)/sizeof(ckw[0]); ++i) {
            if (ckw[i-1] > ckw[i]) {
                qDebug("\"%s\" > \"%s\"", qPrintable(ckw[i-1]), qPrintable(ckw[i]));
                assert(0);
            }
        }

        keyword_order_verified = true;
    }

    return std::binary_search(ckw, ckw + sizeof(ckw)/sizeof(ckw[0]), word);
}

