// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QPlainTextEdit>
#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QEvent>
#include <QtCore/QObject>
#include <QtCore/QTimer>

class SeerPlainTextEdit : public QPlainTextEdit {

    Q_OBJECT

    public:
        explicit SeerPlainTextEdit (const QString& text, QWidget* parent = 0);
        explicit SeerPlainTextEdit (QWidget* parent = 0);
       ~SeerPlainTextEdit ();

        void                    forwardViewportEvent        (QEvent* event);

    protected:
        void                    paintEvent                  (QPaintEvent* event);

    private slots:
        void                    blinkCursor                 ();
        void                    handleCursorPositionChanged ();

    private:
        constexpr static int    CURSOR_WIDTH = 2;
        QTimer*                 _cursorTimer;
        bool                    _cursorVisible;
};


class SeerPlainTextWheelEventForwarder : public QObject {

    Q_OBJECT

    public:
        explicit SeerPlainTextWheelEventForwarder (SeerPlainTextEdit* target);
       ~SeerPlainTextWheelEventForwarder();

        bool                    eventFilter                 (QObject* obj, QEvent* event);

    private:
        SeerPlainTextEdit*      _target;
};

