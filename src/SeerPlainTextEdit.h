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
#include <QtCore/QRect>
#include <QtGui/QPixmap>
#include <QtGui/QColor>

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


//
// Shared mini-map support for the Source and Assembly editors.
//
// Like Kate, the mini-map always shows the whole document. Each character is drawn as a one pixel
// wide mark in its syntax color, faded by how dense the character is, rather than as shrunken text. Lines are LINE_PITCH pixels apart when the document fits;
// otherwise they're squeezed together. When several lines fall on the same pixel row, only the
// first of them is drawn, rather than drawing them on top of each other.
//

class SeerPlainTextMiniMap {

    public:
        constexpr static int    LINE_PITCH  = 3;    // Maximum pixels from one line to the next.
        constexpr static int    LINE_HEIGHT = 2;    // Pixels high of each line's marks, at LINE_PITCH.
        constexpr static int    TAB_WIDTH   = 4;    // Columns per tab stop.

        static qreal            pixelsPerLine               (const QPlainTextEdit* editor, int height);
        static int              lineAt                      (const QPlainTextEdit* editor, int height, int y);
        static QRectF           overlayRect                 (const QPlainTextEdit* editor, int width, int height);
        static int              dragValue                   (const QPlainTextEdit* editor, int height, int startValue, int dy);
        static void             render                      (QPixmap& pixmap, const QTextDocument* document, const QColor& background, const QColor& foreground);
        static void             showVisibleLinesTip         (const QPlainTextEdit* editor, QWidget* miniMap, const QPoint& globalPos);

    private:
        static int              characterOpacity            (QChar ch);
};


class SeerPlainTextMiniMapPreview : public QWidget {

    Q_OBJECT

    public:
        explicit SeerPlainTextMiniMapPreview (QWidget* parent = 0);
       ~SeerPlainTextMiniMapPreview ();

        void                    showPreview                 (QPlainTextEdit* editor, int lineno, const QRect& globalMiniMapRect, int globalY, const QColor& background, const QColor& foreground, const QColor& highlight);

    protected:
        void                    paintEvent                  (QPaintEvent* event) override;

    private:
        qreal                   previewScale                () const;

        constexpr static int    PREVIEW_LINES          = 15;
        constexpr static int    PREVIEW_COLUMNS        = 80;
        constexpr static int    PREVIEW_PADDING        = 4;
        constexpr static int    PREVIEW_SMALLER_POINTS = 2;     // Points smaller than the editor's font.

        QPlainTextEdit*         _editor;
        int                     _lineno;
        QColor                  _background;
        QColor                  _foreground;
        QColor                  _highlight;
};
