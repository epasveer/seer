// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerRunStatusIndicator.h"
#include <QtGui/QFontMetrics>
#include <QtWidgets/QApplication>
#include <QtCore/QDebug>

SeerRunStatusIndicator::SeerRunStatusIndicator(QWidget* parent) : QLabel(parent) {

    QSize size = fontMetrics().size(Qt::TextSingleLine, "XXXXXXXXXXXXXXXXXX");

    setAlignment(Qt::AlignCenter);
    setFixedWidth(size.width());
    setText("Status");
    setStyleSheet("background-color: lightgray; color: black; font-weight: bold;");
}

SeerRunStatusIndicator::~SeerRunStatusIndicator() {
}

void SeerRunStatusIndicator::setRunStatus (SeerRunStatusIndicator::RunStatus status) {

    if (status == RunStatus::Idle) {
        setText("Session Terminated");
        setStyleSheet("background-color: lightgray; color: black; font-weight: bold;");

    }else if (status == RunStatus::Stopped) {
        setText("Stopped");
        setStyleSheet("background-color: red; color: black; font-weight: bold;");

    }else if (status == RunStatus::Stop_By_Breakpoint) {
        setText("Stop by breakpoint");
        setStyleSheet("background-color: yellow; color: black; font-weight: bold;");

    }else if (status == RunStatus::Running) {
        setText("Running");
        setStyleSheet("background-color: green; color: black; font-weight: bold;");

    }else{
        setText("Unknown");
    }

    emit statusChanged(status);
}

void SeerRunStatusIndicator::handleText (const QString& text) {

    if (text.startsWith("*running,thread-id=\"")) {

        // *running,thread-id="all"
        // *running,thread-id="2"
        setRunStatus(SeerRunStatusIndicator::Running);

    }else if (text.startsWith("*stopped")) {
        if (text.startsWith("*stopped,reason=\"breakpoint-hit\""))
            setRunStatus(SeerRunStatusIndicator::Stop_By_Breakpoint);
        else
            //^connected,frame={level=\"0\",addr=\"0x00007f48351f80c1\",func=\"read\",args=[],from=\"/lib64/libc.so.6\",arch=\"i386:x86-64\"}"
            setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("=thread-exited")) {

        //=thread-exited,id="1",group-id="i1"
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("=thread-group-exited")) {

        //=thread-group-exited,id="i1"
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }
    else if (text.startsWith("^done,stack")) {
        // When finish command is invoked
        setRunStatus(SeerRunStatusIndicator::Stopped);
    }
    else{
        // All other text is ignored by this widget.
        // qDebug() << text;
    }
}

// handle when program stop/ killed
void SeerRunStatusIndicator::handleSessionTerminated() {

    setText("Session Terminated");
    setStyleSheet("background-color: lightgray; color: black; font-weight: bold;");

    // also tell SeerProgressIndicator to stop spinning
    setRunStatus(SeerRunStatusIndicator::Idle);
}

