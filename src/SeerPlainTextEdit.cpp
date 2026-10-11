// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include <QtWidgets/QApplication>
#include "SeerPlainTextEdit.h"
#include <QtGui/QPainter>
#include <QtGui/QTextCursor>
#include <QtGui/QPaintEvent>
#include <QTextBlock>
#include <QtGui/QTextLayout>
#include <QtGui/QTextDocument>
#include <QtGui/QAbstractTextDocumentLayout>
#include <QtGui/QGuiApplication>
#include <QtGui/QScreen>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QToolTip>
#include <QColor>

SeerPlainTextEdit::SeerPlainTextEdit(const QString& text, QWidget* parent) : QPlainTextEdit(text, parent) {

    // Turn off the widget's cursor. We'll do our own in the paintEvent.
    setCursorWidth(0);

    _cursorVisible = true;
    _cursorTimer   = new QTimer(this);

    QObject::connect(_cursorTimer, &QTimer::timeout, this, &SeerPlainTextEdit::blinkCursor);
    QObject::connect(this, &QPlainTextEdit::cursorPositionChanged, this, &SeerPlainTextEdit::handleCursorPositionChanged);

    _cursorTimer->start(500);
}

SeerPlainTextEdit::SeerPlainTextEdit(QWidget* parent) : QPlainTextEdit(parent) {

    // Turn off the widget's cursor. We'll do our own in the paintEvent.
    setCursorWidth(0);

    _cursorVisible = true;
    _cursorTimer   = new QTimer(this);

    QObject::connect(_cursorTimer, &QTimer::timeout, this, &SeerPlainTextEdit::blinkCursor);
    QObject::connect(this, &QPlainTextEdit::cursorPositionChanged, this, &SeerPlainTextEdit::handleCursorPositionChanged);

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

    // Get our painter.
    QPainter painter(viewport());

    // Draw the cursor manually.
    if (_cursorVisible) {
        QRect r = cursorRect();
        r.setWidth(CURSOR_WIDTH);
        painter.fillRect(r, palette().text().color());
    }

    // Add margin highlight for current line
    QTextCursor cursor = textCursor();
    QTextBlock  block  = cursor.block();

    // Get the rectangle of the current line (block)
    QRectF rect = blockBoundingGeometry(block).translated(contentOffset());

    // Optional: add a few pixels margin around the block
    rect.adjust(-2, 0, 2, 0);

    // Draw a visible border
    QPen pen(QColor(200, 200, 200), 0.5);
    painter.setPen(pen);
    painter.drawRect(rect);
}

// Add margin highlight for current line
void SeerPlainTextEdit::handleCursorPositionChanged() {
    viewport()->update();
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


//
//
//

qreal SeerPlainTextMiniMap::pixelsPerLine (const QPlainTextEdit* editor, int height) {

    int totalLines = qMax(1, editor->document()->blockCount());

    return qMin(qreal(LINE_PITCH), qreal(qMax(1, height)) / qreal(totalLines));
}

int SeerPlainTextMiniMap::lineAt (const QPlainTextEdit* editor, int height, int y) {

    int totalLines = qMax(1, editor->document()->blockCount());

    return qBound(0, int(y / pixelsPerLine(editor, height)), totalLines - 1);
}

QRectF SeerPlainTextMiniMap::overlayRect (const QPlainTextEdit* editor, int width, int height) {

    const QScrollBar* scrollBar = editor->verticalScrollBar();

    qreal ppl = pixelsPerLine(editor, height);
    qreal y   = scrollBar->value() * ppl;
    qreal h   = qMax(4.0, qMax(1, scrollBar->pageStep()) * ppl);

    return QRectF(0, y, width, h);
}

int SeerPlainTextMiniMap::dragValue (const QPlainTextEdit* editor, int height, int startValue, int dy) {

    const QScrollBar* scrollBar = editor->verticalScrollBar();

    // Keep the overlay under the mouse.
    int value = startValue + qRound(qreal(dy) / pixelsPerLine(editor, height));

    return qBound(scrollBar->minimum(), value, scrollBar->maximum());
}

void SeerPlainTextMiniMap::render (QPixmap& pixmap, const QTextDocument* document, const QColor& background, const QColor& foreground) {

    pixmap.fill(background);

    QPainter painter(&pixmap);

    int   totalLines    = qMax(1, document->blockCount());
    int   width         = pixmap.width();
    qreal pixelsPerLine = qMin(qreal(LINE_PITCH), qreal(qMax(1, pixmap.height())) / qreal(totalLines));
    int   blockHeight   = qMax(1, qRound(pixelsPerLine * LINE_HEIGHT / LINE_PITCH));
    int   lastY         = -1;

    for (QTextBlock block = document->firstBlock(); block.isValid(); block = block.next()) {

        int y = int(block.blockNumber() * pixelsPerLine);

        // Only the first line that falls on a pixel row is drawn.
        if (y == lastY) {
            continue;
        }

        lastY = y;

        QString text = block.text();

        // Per-character colors from the syntax highlighter, if there is one.
        QVector<QColor> colors(text.length(), foreground);

        const auto formats = block.layout()->formats();

        for (const QTextLayout::FormatRange& range : formats) {

            if (range.format.hasProperty(QTextFormat::ForegroundBrush) == false) {
                continue;
            }

            QColor color = range.format.foreground().color();

            for (int i = qMax(0, range.start); i < qMin(text.length(), range.start + range.length); i++) {
                colors[i] = color;
            }
        }

        // Draw each character as a one pixel wide mark, whose opacity follows
        // how much "ink" the character has. Rows of marks then read like tiny
        // text rather than solid blocks.
        int column = 0;

        for (int i = 0; i < text.length() && column < width; i++) {

            QChar ch = text.at(i);

            if (ch == QLatin1Char('\t')) {
                column = (column / TAB_WIDTH + 1) * TAB_WIDTH;
                continue;
            }

            if (ch.isSpace() == false) {

                QColor color = colors[i];
                color.setAlpha(characterOpacity(ch));

                painter.fillRect(column, y, 1, blockHeight, color);
            }

            column++;
        }
    }
}

void SeerPlainTextMiniMap::showVisibleLinesTip (const QPlainTextEdit* editor, QWidget* miniMap, const QPoint& globalPos) {

    // Like Kate, show the range of lines visible in the editor while scrolling with the mini-map.
    const QScrollBar* scrollBar = editor->verticalScrollBar();

    int totalLines = qMax(1, editor->document()->blockCount());
    int firstLine  = scrollBar->value() + 1;
    int lastLine   = qMin(totalLines, scrollBar->value() + qMax(1, scrollBar->pageStep()));

    QToolTip::showText(globalPos, QString("Lines %1 - %2 of %3").arg(firstLine).arg(lastLine).arg(totalLines), miniMap);
}

int SeerPlainTextMiniMap::characterOpacity (QChar ch) {

    // Light punctuation.
    if (QStringLiteral(".,:;'`\"-_^~").contains(ch)) {
        return 70;
    }

    // Other punctuation and brackets.
    if (ch.isPunct() || ch.isSymbol()) {
        return 130;
    }

    // Lowercase letters.
    if (ch.isLower()) {
        return 170;
    }

    // Uppercase letters, digits, and anything else dense.
    return 220;
}

//
//
//

SeerPlainTextMiniMapPreview::SeerPlainTextMiniMapPreview (QWidget* parent) : QWidget(parent, Qt::ToolTip | Qt::FramelessWindowHint) {

    setAttribute(Qt::WA_ShowWithoutActivating);
    setAttribute(Qt::WA_TransparentForMouseEvents);

    _editor = 0;
    _lineno = 0;
}

SeerPlainTextMiniMapPreview::~SeerPlainTextMiniMapPreview () {
}

void SeerPlainTextMiniMapPreview::showPreview (QPlainTextEdit* editor, int lineno, const QRect& globalMiniMapRect, int globalY, const QColor& background, const QColor& foreground, const QColor& highlight) {

    _editor     = editor;
    _lineno     = lineno;
    _background = background;
    _foreground = foreground;
    _highlight  = highlight;

    QFontMetricsF fm(editor->font());

    qreal scale = previewScale();

    int w = qMin(qRound(PREVIEW_COLUMNS * fm.horizontalAdvance(QLatin1Char('9')) * scale) + 2 * PREVIEW_PADDING, editor->viewport()->width());
    int h = qRound(PREVIEW_LINES * fm.lineSpacing() * scale) + 2 * PREVIEW_PADDING;

    // To the left of the mini-map, centered on the mouse, kept on the screen.
    int x = globalMiniMapRect.left() - w - 4;
    int y = globalY - h / 2;

    QScreen* screen = QGuiApplication::screenAt(QPoint(globalMiniMapRect.left(), globalY));

    if (screen) {
        QRect avail = screen->availableGeometry();

        x = qMax(avail.left(), x);
        y = qBound(avail.top(), y, qMax(avail.top(), avail.bottom() - h + 1));
    }

    resize(w, h);
    move(x, y);
    show();
    update();
}

qreal SeerPlainTextMiniMapPreview::previewScale () const {

    if (_editor == 0) {
        return 1.0;
    }

    // A couple of points smaller than the editor's font.
    QFont font = _editor->font();
    qreal size = font.pointSizeF() > 0 ? font.pointSizeF() : qreal(font.pixelSize());

    if (size <= 0) {
        return 1.0;
    }

    return qMax(qreal(1), size - PREVIEW_SMALLER_POINTS) / size;
}

void SeerPlainTextMiniMapPreview::paintEvent (QPaintEvent* event) {

    Q_UNUSED(event);

    QPainter painter(this);

    painter.fillRect(rect(), _background);

    if (_editor) {

        const QTextDocument*        document = _editor->document();
        QAbstractTextDocumentLayout* layout  = document->documentLayout();

        painter.setClipRect(rect().adjusted(1, 1, -1, -1));
        painter.save();

        // Draw the code a little smaller than the editor's. Scale the painter rather
        // than shrinking the font, which is known to break tab-stop rendering (see
        // setEditorFont()'s comments), and lets the editor's own layouts be reused.
        qreal scale = previewScale();

        painter.translate(PREVIEW_PADDING, PREVIEW_PADDING);
        painter.scale(scale, scale);
        painter.setFont(_editor->font());

        qreal y      = 0;
        qreal bottom = (height() - PREVIEW_PADDING) / scale;
        qreal right  = width() / scale;

        for (QTextBlock block = document->findBlockByNumber(qMax(0, _lineno - PREVIEW_LINES / 2)); block.isValid() && y < bottom; block = block.next()) {

            QRectF bounds = layout->blockBoundingRect(block);   // also forces the block's layout

            if (block.blockNumber() == _lineno) {
                painter.fillRect(QRectF(-PREVIEW_PADDING / scale, y, right, bounds.height()), _highlight);
            }

            // Draw the block's layout directly so the syntax highlighter's colors are honored.
            QTextLayout* textLayout = block.layout();
            QPointF      origin(0, y);

            if (textLayout->lineCount() > 0) {
                origin -= textLayout->lineAt(0).position();
            }

            painter.setPen(_foreground);
            textLayout->draw(&painter, origin);

            y += bounds.height();
        }

        painter.restore();
        painter.setClipping(false);
    }

    painter.setPen(_foreground);
    painter.drawRect(rect().adjusted(0, 0, -1, -1));
}
