// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "GdbMonitor.h"
#include <QtCore/QtCore>
#include <QtCore/QProcess>
#include <QtCore/QLoggingCategory>
#include <QtCore/QDebug>
#include <iostream>

static QLoggingCategory LC("seer.gdbmonitor");

GdbMonitor::GdbMonitor (QObject* parent) : QObject(parent) {
    _process = 0;
}

GdbMonitor::~GdbMonitor () {
}

void GdbMonitor::handleErrorOccurred (QProcess::ProcessError error) {

    Q_UNUSED(error);

    qCDebug(LC) << error;

    qApp->exit();
}

void GdbMonitor::handleFinished (int exitCode, QProcess::ExitStatus exitStatus) {

    Q_UNUSED(exitCode);
    Q_UNUSED(exitStatus);

    qCDebug(LC) << exitCode << exitStatus;

    qApp->exit();
}

void GdbMonitor::handleReadyReadStandardError () {

    qCDebug(LC) << "Ready to read stderr";

    QProcess* p = (QProcess*)sender();

    // canReadLine()/readLine() work on the current read channel, which
    // defaults to stdout. Point them at stderr for this handler, then
    // switch back for the stdout handler.
    p->setReadChannel(QProcess::StandardError);

    // Read a line at a time.
    while (p->canReadLine()) {

        QByteArray buf = p->readLine();

        // Chop off trailing RETURN character.
        if (buf[buf.size()-1] == '\n') {
            buf.chop(1);
        }

        // Convert to a string. On Windows gdb emits MI output in the ANSI
        // code page of the console locale (non-ASCII paths only appear
        // correctly in that encoding); on other platforms it is UTF-8.
#if defined(Q_OS_WIN)
        QString text = QString::fromLocal8Bit(buf);
#else
        QString text(buf);
#endif

        qCDebug(LC) << text;

        // Start broadcasting it around.
        emit allTextOutput(text);
    }

    p->setReadChannel(QProcess::StandardOutput);
}

void GdbMonitor::handleReadyReadStandardOutput () {

    qCDebug(LC) << "Ready to read stdout";

    QProcess* p = (QProcess*)sender();

    // Read a line at a time.
    while (p->canReadLine()) {

        QByteArray buf = p->readLine();

        // Chop off trailing RETURN character.
        if (buf[buf.size()-1] == '\n') {
            buf.chop(1);
        }

        if (buf.size() == 0) { // Ignore empty lines.
            continue;
        }

        // Convert to a string. On Windows gdb emits MI output in the ANSI
        // code page of the console locale (non-ASCII paths only appear
        // correctly in that encoding); on other platforms it is UTF-8.
#if defined(Q_OS_WIN)
        QString text = QString::fromLocal8Bit(buf);
#else
        QString text(buf);
#endif

        // Resolve MI escapes inside path fields. gdb emits non-ASCII path
        // bytes as '\NNN' octal escapes and a literal backslash as '\\'
        // (eg '中文' -> '\344\270\255\346\226\207'), so an unescaped parse
        // yields a filename with literal '\344...' text in it and the
        // editor can't open the source. Call the helper only when the
        // field carries escapes (ASCII paths take the fast path).
        //
        // 'field' is the whole captured 'name="..."' text. 'recordUtf8'
        // /'recordDecided' carry this record's encoding decision from
        // one field to the next (one record = one inferior = one
        // encoding); they are locals of the per-line handler, so every
        // record starts fresh.
        bool recordUtf8    = true;
        bool recordDecided = false;
        auto unescapePathField = [&recordUtf8, &recordDecided](const QString& field) -> QString {

            if (field.contains('\\') == false) {
                return field;
            }

            const int open = field.indexOf('"');   // Value starts after this.
            const QString inner = field.mid(open + 1, field.size() - open - 2);  // strip name=" and "

            // First pass over the wire text: resolve '\NNN' octal
            // escapes to raw bytes, consuming '\\' pairs ATOMICALLY so
            // the second backslash of a pair can never be misread as
            // the start of an octal escape. A consumed pair becomes a
            // placeholder byte that no real path contains.
            static const char marker = 0x01;

            QByteArray bytes;
            bytes.reserve(inner.size());

            for (int i = 0; i < inner.size(); ) {

                const QChar c = inner.at(i);

                if (c == '\\' && i + 1 < inner.size() && inner.at(i+1) == '\\') {
                    bytes.append(marker);
                    i += 2;

                }else if (c == '\\' && i + 3 < inner.size() &&
                    inner.at(i+1).isDigit() && inner.at(i+2).isDigit() && inner.at(i+3).isDigit() &&
                    inner.at(i+1).digitValue() < 8 && inner.at(i+2).digitValue() < 8 && inner.at(i+3).digitValue() < 8) {

                    bytes.append(char(inner.at(i+1).digitValue()*64 + inner.at(i+2).digitValue()*8 + inner.at(i+3).digitValue()));
                    i += 4;

                }else{
                    bytes.append(c.toLatin1());
                    i++;
                }
            }

            // The marker re-expands to the backslash's wire meaning: a
            // bare separator backslash on Windows (the caller converts
            // separators to '/', and a Windows filename can never hold a
            // literal backslash), or the escaped '\\\\' form on Linux —
            // downstream consumers like Seer::filterEscapes() reduce it
            // to a literal backslash, which a Linux filename CAN hold.
            auto decode = [&bytes](const QString& probe) -> QString {
                QString value = probe;
#if defined(Q_OS_WIN)
                value.replace(QChar(marker), QString("\\"));
#else
                value.replace(QChar(marker), QString("\\\\"));
#endif
                return value;
            };

            const QString prefix = field.left(open + 1);  // 'name="'

#if defined(Q_OS_WIN)
            // The bytes are the raw filename bytes gdb read from the
            // inferior's debug info, and their encoding depends on how the
            // inferior was built: builds driven from an MSYS2 shell
            // record paths as UTF-8, builds driven from cmd/MSVC record
            // the ANSI code page. The two spellings can each be valid in
            // the other encoding, so when the two decodings differ, pick
            // the one that names an existing file: the source file is on
            // disk while it is being debugged. A relative 'file=' cannot
            // be probed, so the record's 'fullname' (absolute) decides
            // and the sibling reuses that decision - one inferior has
            // one encoding. When neither candidate exists (e.g. the
            // source moved after the build), stay with UTF-8: it is the
            // byte form MinGW gdb prints unescaped in the same records.
            //
            // Filesystem probes see the marker as a literal backslash;
            // the returned text keeps the escaped form.
            const bool isFullname = field.startsWith("fullname=\"");
            const bool isAbsolute = (bytes.size() > 0 && (bytes.at(0) == '/' || bytes.at(1) == ':'));

            QStringDecoder utf8Decoder(QStringConverter::Utf8, QStringConverter::Flag::Stateless);
            const QString utf8Probe = utf8Decoder.decode(bytes);
            const bool    utf8Valid = (utf8Decoder.hasError() == false);
            const QString localProbe = QString::fromLocal8Bit(bytes);

            auto expand = [](const QString& probe) -> QString {
                QString value = probe;
                value.replace(QChar(marker), QString("\\"));
                return value;
            };

            if (utf8Valid == false || utf8Probe == localProbe) {
                // Not UTF-8, or the two decodings agree: no ambiguity.
                recordUtf8    = utf8Valid;
                recordDecided = true;
                return prefix + decode(utf8Valid ? utf8Probe : localProbe) + '"';
            }

            if (recordDecided == false && (isFullname || isAbsolute)) {
                // Absolute paths can be checked against the filesystem.
                const bool utf8Exists  = QFile::exists(expand(utf8Probe));
                const bool localExists = QFile::exists(expand(localProbe));
                recordUtf8    = (localExists == false) || utf8Exists;
                recordDecided = true;
            }

            if (recordDecided) {
                return prefix + decode(recordUtf8 ? utf8Probe : localProbe) + '"';
            }

            return prefix + decode(utf8Probe) + '"';
#else
            // On Linux the path bytes gdb escapes are UTF-8 (locale
            // bytes), matching the UTF-8 wire used elsewhere.
            return prefix + decode(QString::fromUtf8(bytes)) + '"';
#endif
        };

        // Rewrite every 'fullname="..."' and 'file="..."' value once, so
        // all consumers (editor keys, tab labels, breakpoint dialogs,
        // tables) see a single spelling. On Windows, gdb 17.x reports
        // 'fullname' MI-escaped with backslashes while its symbol table
        // uses forward slashes: turn backslash separators into '/' AFTER
        // the wire escapes are resolved and the bytes are decoded, so a
        // 0x5C byte inside a multi-byte character (e.g. the second byte
        // of some GBK/CP932 characters) is never mistaken for a
        // separator. The line is rebuilt by segments because lengths
        // change.
        {
            static const QRegularExpression pathFieldRe("(?:fullname|file)=\"[^\"]*\"");
            QRegularExpressionMatchIterator it = pathFieldRe.globalMatch(text);

            QString newText;
            int     last = 0;

            while (it.hasNext()) {
                QRegularExpressionMatch m = it.next();
                newText += text.mid(last, m.capturedStart() - last);

                QString value = unescapePathField(m.captured(0));
#if defined(Q_OS_WIN)
                // Backslash separators become '/' - multi-byte characters
                // were already resolved above, so every remaining
                // backslash in the value is a separator.
                value.replace('\\', '/');
#endif
                newText += value;

                last = m.capturedStart() + m.capturedLength();
            }

            newText += text.mid(last);
            text = newText;
        }

        //qDebug() << "Read buffer" << buf.size() << (int)buf[buf.size()-1] << text;
        qCDebug(LC) << text;

#if SEER_GDB_LOGOUT == 1
        // Start broadcasting it around. For debugging
        emit allTextOutput("From GdbMonitor: " + text + "\n");
#endif

        if (text[0] == '~') {
            emit tildeTextOutput(text);
        }else if (text[0] == '=') {
            emit equalTextOutput(text);
        }else if (text[0] == '*') {
            emit astrixTextOutput(text);
        }else if (text[0] == '^') {
            emit caretTextOutput(text);
        }else if (text[0] == '&') {
            emit ampersandTextOutput(text);
        }else if (text[0] == '@') {
            emit atsignTextOutput(text);
        }else if (text.contains(QRegularExpression("^([0-9]+)\\~"))) {
            emit tildeTextOutput(text);
        }else if (text.contains(QRegularExpression("^([0-9]+)\\="))) {
            emit equalTextOutput(text);
        }else if (text.contains(QRegularExpression("^([0-9]+)\\*"))) {
            emit astrixTextOutput(text);
        }else if (text.contains(QRegularExpression("^([0-9]+)\\^"))) {
            emit caretTextOutput(text);
        }else if (text.contains(QRegularExpression("^([0-9]+)\\&"))) {
            emit ampersandTextOutput(text);
        }else{
            emit textOutput(text);
        }
    }

    qCDebug(LC) << "Finished reading stdout";
}

void GdbMonitor::handleTextOutput (QString text) {

    qCDebug(LC) << "Ready to handle text output";
    qCDebug(LC) << text;

    emit allTextOutput(text);

    if (text[0] == '~') {
        emit tildeTextOutput(text);
    }else if (text[0] == '=') {
        emit equalTextOutput(text);
    }else if (text[0] == '*') {
        emit astrixTextOutput(text);
    }else if (text[0] == '^') {
        emit caretTextOutput(text);
    }else if (text[0] == '&') {
        emit ampersandTextOutput(text);
    }else if (text.contains(QRegularExpression("^([0-9]+)\\~"))) {
        emit tildeTextOutput(text);
    }else if (text.contains(QRegularExpression("^([0-9]+)\\="))) {
        emit equalTextOutput(text);
    }else if (text.contains(QRegularExpression("^([0-9]+)\\*"))) {
        emit astrixTextOutput(text);
    }else if (text.contains(QRegularExpression("^([0-9]+)\\^"))) {
        emit caretTextOutput(text);
    }else if (text.contains(QRegularExpression("^([0-9]+)\\&"))) {
        emit ampersandTextOutput(text);
    }else{
        emit textOutput(text);
    }

    qCDebug(LC) << "Finished handling text output";
}

void GdbMonitor::handleStarted() {

    qCDebug(LC);
}

void GdbMonitor::handleStateChanged(QProcess::ProcessState newState) {

    Q_UNUSED(newState);

    qCDebug(LC) << newState;
}

void GdbMonitor::setProcess (QProcess* process) {
    _process = process;
}

QProcess* GdbMonitor::process () {
    return _process;
}

