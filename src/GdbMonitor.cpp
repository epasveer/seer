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

    // Read a line at a time.
    while (p->canReadLine()) {

        QByteArray buf = p->readLine();

        // Chop off trailing RETURN character.
        if (buf[buf.size()-1] == '\n') {
            buf.chop(1);
        }

        // Convert to a string.
        QString text(buf);

        qCDebug(LC) << text;

        // Start broadcasting it around.
        emit allTextOutput(text);
    }
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

        // Convert to a string.
        QString text(buf);

        //qDebug() << "Read buffer" << buf.size() << (int)buf[buf.size()-1] << text;
        qCDebug(LC) << text;

        // Start broadcasting it around.
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

