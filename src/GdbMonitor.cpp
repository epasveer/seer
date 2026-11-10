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

        // Resolve MI '\NNN' octal escapes inside a 'fullname="..."' field.
        // gdb emits non-ASCII path bytes as escaped UTF-8 octal sequences
        // (eg '中文' -> '\344\270\255\346\226\207'), so an unescaped parse
        // yields a filename with literal '\344...' text in it and the
        // editor can't open the source. Call the helper only when the
        // field carries escapes (ASCII paths take the fast path).
        //
        // 'field' is the whole captured 'fullname="..."' text.
        static auto unescapeFullnameField = [](const QString& field) -> QString {

            if (field.contains('\\') == false) {
                return field;
            }

            QString inner = field.mid(10, field.size() - 11);  // strip fullname=" and "

            QByteArray bytes;
            bytes.reserve(inner.size());

            for (int i = 0; i < inner.size(); ) {

                const QChar c = inner.at(i);

                if (c == '\\' && i + 3 < inner.size() &&
                    inner.at(i+1).isDigit() && inner.at(i+2).isDigit() && inner.at(i+3).isDigit() &&
                    inner.at(i+1).digitValue() < 8 && inner.at(i+2).digitValue() < 8 && inner.at(i+3).digitValue() < 8) {

                    bytes.append(char(inner.at(i+1).digitValue()*64 + inner.at(i+2).digitValue()*8 + inner.at(i+3).digitValue()));
                    i += 4;

                }else{
                    bytes.append(c.toLatin1());
                    i++;
                }
            }

#if defined(Q_OS_WIN)
            // gdb resolves the octal escapes in its host charset, which,
            // under the 'LANG=C' Seer gives it on Windows, is the ANSI
            // code page — the same rule the whole line is decoded with.
            return "fullname=\"" + QString::fromLocal8Bit(bytes) + "\"";
#else
            // On Linux the path bytes gdb escapes are UTF-8 (locale
            // bytes), matching the UTF-8 wire used elsewhere.
            return "fullname=\"" + QString::fromUtf8(bytes) + "\"";
#endif
        };

        // Rewrite every 'fullname="..."' value once, so all consumers
        // (editor keys, breakpoint dialogs, tables) see a single spelling.
        // On Windows, gdb 17.x reports 'fullname' MI-escaped with
        // backslashes while its symbol table uses forward slashes: turn
        // the '\\' pair into one '/'. On all platforms, resolve the MI
        // octal escapes of non-ASCII (UTF-8) paths. The line is rebuilt
        // by segments because lengths change.
        {
            static const QRegularExpression fullnameRe("fullname=\"[^\"]*\"");
            QRegularExpressionMatchIterator it = fullnameRe.globalMatch(text);

            QString newText;
            int     last = 0;

            while (it.hasNext()) {
                QRegularExpressionMatch m = it.next();
                newText += text.mid(last, m.capturedStart() - last);

                QString value = m.captured(0);
#if defined(Q_OS_WIN)
                value.replace("\\\\", "/");
#endif
                newText += unescapeFullnameField(value);

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

