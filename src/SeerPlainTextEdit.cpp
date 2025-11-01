// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include <QtWidgets/QApplication>
#include "SeerPlainTextEdit.h"
#include <QtGui/QPainter>
#include <QtGui/QTextCursor>
#include <QtGui/QPaintEvent>

SeerPlainTextEdit::SeerPlainTextEdit(const QString& text, QWidget* parent) : QPlainTextEdit(text, parent) {

    _cursorVisible = true;
    _cursorTimer   = new QTimer(this);

    QObject::connect(_cursorTimer, &QTimer::timeout, this, &SeerPlainTextEdit::blinkCursor);

    _cursorTimer->start(500);
}

SeerPlainTextEdit::SeerPlainTextEdit(QWidget* parent) : QPlainTextEdit(parent) {

    _cursorVisible = true;
    _cursorTimer   = new QTimer(this);

    QObject::connect(_cursorTimer, &QTimer::timeout, this, &SeerPlainTextEdit::blinkCursor);

    _cursorTimer->start(500);
}

SeerPlainTextEdit::~SeerPlainTextEdit () {
}

void SeerPlainTextEdit::forwardViewportEvent(QEvent* event) {

    viewportEvent(event);
}

void SeerPlainTextEdit::blinkCursor() {

    _cursorVisible = !_cursorVisible;

    viewport()->update(); // Trigger a repaint of the viewport
}

void SeerPlainTextEdit::paintEvent(QPaintEvent* event) {

    // First, let the standard QPlainTextEdit handle its painting.
    QPlainTextEdit::paintEvent(event);

    // If the widget does not have focus, draw the cursor manually.
    if (!hasFocus() && _cursorVisible) {
        QRect r = cursorRect();
        if (r.isValid()) {
            QPainter p(viewport());
            p.fillRect(r, palette().text().color());
        }
    }
}

//
//
//

SeerPlainTextWheelEventForwarder::SeerPlainTextWheelEventForwarder (SeerPlainTextEdit* target ) : QObject(), _target(target) {
}

SeerPlainTextWheelEventForwarder::~SeerPlainTextWheelEventForwarder() {
    _target = NULL;
}

bool SeerPlainTextWheelEventForwarder::eventFilter (QObject* obj, QEvent* event) {

    Q_UNUSED(obj);

    if (_target == 0) {
        return false;
    }

    if (event->type() == QEvent::Wheel) {
        _target->forwardViewportEvent(event);
    }

    // do not filter the event
    return false;
}

