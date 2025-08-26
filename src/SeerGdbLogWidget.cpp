// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerGdbLogWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QScrollBar>
#include <QRegularExpression>
#include <QtCore/QTime>
#include <QtCore/QDebug>

SeerGdbLogWidget::SeerGdbLogWidget (QWidget* parent) : SeerLogWidget(parent) {
}

SeerGdbLogWidget::~SeerGdbLogWidget () {
}

void SeerGdbLogWidget::processText (const QString& text) {

    QString str;

    // Remove leading "~"
    // Remove leading "&"
    // Remove leading "@"
    switch (text.front().unicode()) {
        case QChar('~').unicode():
        case QChar('&').unicode():
        case QChar('@').unicode():

            str = text.mid(1);

            // Remove leading """
            if (str.front() == '"') {
                str = str.mid(1);
            }

            // Remove trailing """
            if (str.back() == '"') {
                str.chop(1);
            }

            break;

        default:
            str = text;
    }

    // https://github.com/epasveer/seer/issues/238
    str = Seer::unescape(str);

    // Add timestamp.
    if (isTimeStampEnabled()) {
        QString text = QString("[") + QTime::currentTime().toString("hh:mm:ss.zz") + QString("] ") + str;
        // Write the string to the log.
        textEdit->insertPlainText(text);
    }else{
        // Write the string to the log.
        textEdit->insertPlainText(str);
    }

    // If there is breakpoint message (via a manual command), ask
    // for the breakpoint list to be refreshed.
    //
    // Breakpoint 2 at 0x403a40: file explorer.cpp, line 78.
    //
    if (str.contains(QRegularExpression("^Breakpoint ([0-9]+) at (0[xX][0-9a-fA-F]+): file (.*\\,) (line) ([0-9]+)"))) {
        emit refreshBreakpointsList();
    }
}

