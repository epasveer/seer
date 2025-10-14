// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerTildeLogWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QScrollBar>

SeerTildeLogWidget::SeerTildeLogWidget (QWidget* parent) : SeerLogWidget(parent) {
}

SeerTildeLogWidget::~SeerTildeLogWidget () {
}

void SeerTildeLogWidget::processText (const QString& text) {

    QString str = text.mid(1); // Remove leading "~"

    if (str.front() == '"') { // Remove leading """
        str = str.mid(1);
    }

    if (str.back() == '"') { // Remove trailing """
        str.chop(1);
    }

    str = Seer::filterEscapes(str);

    textEdit->insertPlainText(str);
}

