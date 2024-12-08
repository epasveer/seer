#include "QColorSwatch.h"
#include <QtGui/QPalette>
#include <QtGui/QPainter>
#include <QtCore/QDebug>

QColorSwatch::QColorSwatch(QWidget* parent) : QWidget(parent) {
    setColor(palette().color(QPalette::Window));
}

QColorSwatch::QColorSwatch(const QColor& color, QWidget* parent) : QWidget(parent) {
    setColor(color);
}

QColorSwatch::~QColorSwatch() {
}

void QColorSwatch::setColor (const QColor& color) {
    _color = color;
}

QColor QColorSwatch::color () const {
    return _color;
}

void QColorSwatch::paintEvent(QPaintEvent* e) {

    Q_UNUSED(e)

    // Draw the color to the widget.
    QPainter painter(this);

    painter.setPen(Qt::NoPen);
    painter.setBrush(color());
    painter.drawRect(rect());
}

