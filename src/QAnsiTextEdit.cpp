#include "QAnsiTextEdit.h"
#include <QtCore/QDebug>

//
//
//
QAnsiTextEditFormattedText::QAnsiTextEditFormattedText(const QString& txt, const QTextCharFormat& fmt) {
    text   = txt;
    format = fmt;
}

//
//
//
QList<QAnsiTextEditFormattedText> QAnsiTextEditEscapeCodeHandler::parseText (const QAnsiTextEditFormattedText& input) {

    enum AnsiEscapeCodes {
        ResetFormat            =  0,
        BoldText               =  1,
        UnderLinedText         =  4,
        TextColorStart         = 30,
        TextColorEnd           = 37,
        RgbTextColor           = 38,
        DefaultTextColor       = 39,
        BackgroundColorStart   = 40,
        BackgroundColorEnd     = 47,
        RgbBackgroundColor     = 48,
        DefaultBackgroundColor = 49
    };

    const QString escape        = "\x1b[";
    const QChar semicolon       = ';';
    const QChar colorTerminator = 'm';
    const QChar eraseToEol      = 'K';

    QList<QAnsiTextEditFormattedText> outputData;
    QTextCharFormat                   charFormat = _previousFormatClosed ? input.format : _previousFormat;
    QString                           strippedText;

    if (_pendingText.isEmpty()) {
        strippedText = input.text;
    }else{
        strippedText = _pendingText.append(input.text);
        _pendingText.clear();
    }

    while (!strippedText.isEmpty()) {
        if (_waitingForTerminator) {
            // We ignore all escape codes taking string arguments.
            QString terminator = "\x1b\\";
            int terminatorPos = strippedText.indexOf(terminator);
            if (terminatorPos == -1 && !_alternateTerminator.isEmpty()) {
                terminator    = _alternateTerminator;
                terminatorPos = strippedText.indexOf(terminator);
            }
            if (terminatorPos == -1) {
                _pendingText = strippedText;
                break;
            }
            _waitingForTerminator = false;
            _alternateTerminator.clear();
            strippedText.remove(0, terminatorPos + terminator.length());
            if (strippedText.isEmpty()) {
                break;
            }
        }

        const int escapePos = strippedText.indexOf(escape.at(0));
        if (escapePos < 0) {
            outputData << QAnsiTextEditFormattedText(strippedText, charFormat);
            break;
        }else if (escapePos != 0) {
            outputData << QAnsiTextEditFormattedText(strippedText.left(escapePos), charFormat);
            strippedText.remove(0, escapePos);
        }

        while (!strippedText.isEmpty() && escape.at(0) == strippedText.at(0)) {

            if (escape.startsWith(strippedText)) {
                // control secquence is not complete
                _pendingText += strippedText;
                strippedText.clear();
                break;
            }

            if (!strippedText.startsWith(escape)) {
                switch (strippedText.at(1).toLatin1()) {
                    case '\\': // Unexpected terminator sequence.
                        Q_FALLTHROUGH();
                    case 'N': case 'O': // Ignore unsupported single-character sequences.
                        strippedText.remove(0, 2);
                        break;
                    case ']':
                        _alternateTerminator = QChar(7);
                        Q_FALLTHROUGH();
                    case 'P':  case 'X': case '^': case '_':
                        strippedText.remove(0, 2);
                        _waitingForTerminator = true;
                        break;
                    default:
                        // not a control sequence
                        _pendingText.clear();
                        outputData << QAnsiTextEditFormattedText(strippedText.left(1), charFormat);
                        strippedText.remove(0, 1);
                        continue;
                }
                break;
            }

            _pendingText += strippedText.mid(0, escape.length());
            strippedText.remove(0, escape.length());

            // \e[K is not supported. Just strip it.
            if (strippedText.startsWith(eraseToEol)) {
                _pendingText.clear();
                strippedText.remove(0, 1);
                continue;
            }

            // get the number
            QString     strNumber;
            QStringList numbers;

            while (!strippedText.isEmpty()) {
                if (strippedText.at(0).isDigit()) {
                    strNumber += strippedText.at(0);
                }else{
                    if (!strNumber.isEmpty()) {
                        numbers << strNumber;
                    }
                    if (strNumber.isEmpty() || strippedText.at(0) != semicolon) {
                        break;
                    }
                    strNumber.clear();
                }
                _pendingText += strippedText.mid(0, 1);
                strippedText.remove(0, 1);
            }

            if (strippedText.isEmpty()) {
                break;
            }

            // remove terminating char
            if (!strippedText.startsWith(colorTerminator)) {
                _pendingText.clear();
                strippedText.remove(0, 1);
                break;
            }

            // got consistent control sequence, ok to clear pending text
            _pendingText.clear();
            strippedText.remove(0, 1);

            if (numbers.isEmpty()) {
                charFormat = input.format;
                endFormatScope();
            }

            for (int i = 0; i < numbers.size(); ++i) {

                const uint code = numbers.at(i).toUInt();

                if (code >= TextColorStart && code <= TextColorEnd) {
                    //qDebug() << "TextColorStart/TextColorEnd called";
                    charFormat.setForeground(ansiColor(code - TextColorStart));
                    setFormatScope(charFormat);
                }else if (code >= BackgroundColorStart && code <= BackgroundColorEnd) {
                    //qDebug() << "BackgroundColorStart/BackgroundColorEnd called";
                    charFormat.setBackground(ansiColor(code - BackgroundColorStart));
                    setFormatScope(charFormat);
                }else{
                    switch (code) {
                        case ResetFormat:
                            //qDebug() << "ResetFormat called";
                            charFormat = QTextCharFormat();
                            setFormatScope(charFormat);
                            endFormatScope();
                            break;
                        case BoldText:
                            //qDebug() << "BoldText called";
                            charFormat.setFontWeight(QFont::ExtraBold);
                            setFormatScope(charFormat);
                            break;
                        case UnderLinedText:
                            //qDebug() << "UnderLinedText called";
                            charFormat.setFontUnderline(true);
                            setFormatScope(charFormat);
                            break;
                        case DefaultTextColor:
                            //qDebug() << "DefaultTextColor called";
                            charFormat.setForeground(input.format.foreground());
                            setFormatScope(charFormat);
                            break;
                        case DefaultBackgroundColor:
                            //qDebug() << "DefaultBackgroundColor called";
                            charFormat.setBackground(input.format.background());
                            setFormatScope(charFormat);
                            break;
                        case RgbTextColor:
                        case RgbBackgroundColor:
                            //qDebug() << "RgbTextColor/RgbBackgroundColor called";
                            // See http://en.wikipedia.org/wiki/ANSI_escape_code#Colors
                            if (++i >= numbers.size()) {
                                break;
                            }

                            switch (numbers.at(i).toInt()) {
                                case 2:
                                    // RGB set with format: 38;2;<r>;<g>;<b>
                                    if ((i + 3) < numbers.size()) {
                                        (code == RgbTextColor) ?
                                            charFormat.setForeground(QColor(numbers.at(i + 1).toInt(),
                                                        numbers.at(i + 2).toInt(),
                                                        numbers.at(i + 3).toInt())) :
                                            charFormat.setBackground(QColor(numbers.at(i + 1).toInt(),
                                                        numbers.at(i + 2).toInt(),
                                                        numbers.at(i + 3).toInt()));
                                        setFormatScope(charFormat);
                                    }
                                    i += 3;
                                    break;
                                case 5:
                                    // 256 color mode with format: 38;5;<i>
                                    uint index = numbers.at(i + 1).toUInt();

                                    QColor color;
                                    if (index < 8) {
                                        // The first 8 colors are standard low-intensity ANSI colors.
                                        color = ansiColor(index);
                                    }else if (index < 16) {
                                        // The next 8 colors are standard high-intensity ANSI colors.
                                        color = ansiColor(index - 8).lighter(150);
                                    }else if (index < 232) {
                                        // The next 216 colors are a 6x6x6 RGB cube.
                                        uint o = index - 16;
                                        color = QColor((o / 36) * 51, ((o / 6) % 6) * 51, (o % 6) * 51);
                                    }else{
                                        // The last 24 colors are a greyscale gradient.
                                        int grey = int((index - 232) * 11);
                                        color = QColor(grey, grey, grey);
                                    }

                                    if (code == RgbTextColor) {
                                        charFormat.setForeground(color);
                                    }else{
                                        charFormat.setBackground(color);
                                    }

                                    setFormatScope(charFormat);
                                    ++i;
                                    break;
                            }
                            break;
                        default:
                            qDebug() << "Unkown code:" << code;
                            break;
                    }
                }
            }
        }
    }

    return outputData;
}

void QAnsiTextEditEscapeCodeHandler::endFormatScope () {
    _previousFormatClosed = true;
}

void QAnsiTextEditEscapeCodeHandler::setFormatScope (const QTextCharFormat& charFormat) {
    _previousFormat       = charFormat;
    _previousFormatClosed = false;
}

QTextCharFormat QAnsiTextEditEscapeCodeHandler::formatScope  () const {
    return _previousFormat;
}

QColor QAnsiTextEditEscapeCodeHandler::ansiColor(uint code) {

    const int red   = code & 1 ? 170 : 0;
    const int green = code & 2 ? 170 : 0;
    const int blue  = code & 4 ? 170 : 0;

    return QColor(red, green, blue);
}

//
//
//
QAnsiTextEdit::QAnsiTextEdit (QWidget* parent) : QPlainTextEdit(parent) {
}

QAnsiTextEdit::QAnsiTextEdit (const QString& text, QWidget* parent) : QPlainTextEdit(parent) {

    setAnsiText(text);
}

QAnsiTextEdit::~QAnsiTextEdit () {
}

void QAnsiTextEdit::dumpCharFormat (QString string, QTextCharFormat format) {

    qDebug() << "dumpCharFormat" << string << "Color" << format.foreground().color();
    qDebug() << "dumpCharFormat" << string << "FontWeight" << format.fontWeight();
    qDebug() << "dumpCharFormat" << string << "UnderLined" << format.fontUnderline();
    qDebug() << "";
}

void QAnsiTextEdit::setAnsiText (const QString& text) {

    // Reset the text edit
    clear();

    // Create the default text object;
    QAnsiTextEditFormattedText ftext;
    ftext.text   = text;
    ftext.format = currentCharFormat(); // Use cleared format.

    // Parse it. A list of sub text objects is created.
    QList<QAnsiTextEditFormattedText> ftexts = _escapeCodeHandler.parseText(ftext);

    // Print each sub text object. Each one has its own text format.
    for (const QAnsiTextEditFormattedText& ftext : ftexts) {

        //dumpCharFormat(ftext.text, ftext.format);

        QTextCursor cursor = textCursor();
        cursor.setCharFormat(ftext.format);
        cursor.insertText(ftext.text);
        setTextCursor(cursor);
    }
}

void QAnsiTextEdit::appendAnsiText (const QString& text) {

    // Create the default text object;
    QAnsiTextEditFormattedText ftext;
    ftext.text   = text;
    // XXX ftext.format = currentCharFormat();
    ftext.format = _escapeCodeHandler.formatScope();

    // Parse it. A list of sub text objects is created.
    QList<QAnsiTextEditFormattedText> ftexts = _escapeCodeHandler.parseText(ftext);

    // Print each sub text object. Each one has its own text format.
    for (const QAnsiTextEditFormattedText& ftext : ftexts) {
        QTextCursor cursor = textCursor();
        cursor.setCharFormat(ftext.format);
        cursor.insertText(ftext.text);
        setTextCursor(cursor);
    }
}

void QAnsiTextEdit::insertAnsiText (const QString& text) {

    // Create the default text object;
    QAnsiTextEditFormattedText ftext;
    ftext.text   = text;
    // XXX ftext.format = currentCharFormat();
    ftext.format = _escapeCodeHandler.formatScope();

    // Parse it. A list of sub text objects is created.
    QList<QAnsiTextEditFormattedText> ftexts = _escapeCodeHandler.parseText(ftext);

    // Print each sub text object. Each one has its own text format.
    for (const QAnsiTextEditFormattedText& ftext : ftexts) {
        QTextCursor cursor = textCursor();
        cursor.setCharFormat(ftext.format);
        cursor.insertText(ftext.text);
        setTextCursor(cursor);
    }
}

