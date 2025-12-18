// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerRunStatusIndicator.h"
#include <QtWidgets/QApplication>
#include <QtCore/QDebug>
#include <QGroupBox>
#include <QPainter>

SeerRunStatusIndicator::SeerRunStatusIndicator(QWidget* parent) : QLabel(parent) {
    // Create group box
    _groupBox = new QGroupBox();
    _groupBox->setTitle("");
    _groupBox->setFlat(true);
    _groupBox->setStyleSheet("QGroupBox { border: none; }");

    _statusLabel = new QLabel("Status");
    _statusLabel->setAlignment(Qt::AlignCenter);
    _statusLabel->setFixedSize(150, 25);
    _statusLabel->setStyleSheet("background-color: lightgray; color: black; font-weight: bold;");

    //  Layout
    auto *layout = new QHBoxLayout();
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);
    layout->addWidget(_statusLabel);
    _groupBox->setLayout(layout);
}

SeerRunStatusIndicator::~SeerRunStatusIndicator() {
}

void SeerRunStatusIndicator::setRunStatus (SeerRunStatusIndicator::RunStatus status) {
    if (status == RunStatus::Idle) {
        _statusLabel->setText("Session Terminated");
        _statusLabel->setStyleSheet("background-color: lightgray; color: black; font-weight: bold;");

    }else if (status == RunStatus::Stopped) {
        _statusLabel->setText("Stopped");
        _statusLabel->setStyleSheet("background-color: red; color: black; font-weight: bold;");

    }
    else if (status == RunStatus::Stop_By_Breakpoint) {
        _statusLabel->setText("Stop by breakpoint");
        _statusLabel->setStyleSheet("background-color: yellow; color: black; font-weight: bold;");
    }
    else if (status == RunStatus::Running) {
        _statusLabel->setText("Running");
        _statusLabel->setStyleSheet("background-color: green; color: black; font-weight: bold;");

    }
    else {
        _statusLabel->setText("Unknown");
    }

    emit statusChanged(status);
}

QGroupBox* SeerRunStatusIndicator::indicatorBox() {
    return _groupBox;
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
    _statusLabel->setText("Session Terminated");
    _statusLabel->setStyleSheet("background-color: lightgray; color: black; font-weight: bold;");
    // also tell SeerProgressIndicator to stop spinning
    setRunStatus(SeerRunStatusIndicator::Idle);
}