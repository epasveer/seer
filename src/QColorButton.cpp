#include "QColorButton.h"
#include <QtWidgets/QColorDialog>
#include <QtGui/QPalette>
#include <QtGui/QPainter>
#include <QtCore/QDebug>

QColorButton::QColorButton(QWidget* parent) : QWidget(parent) {

    _color = palette().color(QPalette::Background);
}

QColorButton::QColorButton(const QColor& color, QWidget* parent) : QWidget(parent) {

    _color = color;
}

QColorButton::~QColorButton() {
}

void QColorButton::mouseDoubleClickEvent (QMouseEvent* e) {

    Q_UNUSED(e)

    // Open a color dialog if someone double-clicked.
    QColor c = QColorDialog::getColor(color(), this);

    if (c.isValid()) {
        setColor(c);
    }
}

void QColorButton::paintEvent(QPaintEvent* e) {

    Q_UNUSED(e)

    // Draw the color to the widget.
    QPainter painter(this);

    painter.setPen(Qt::NoPen);
    painter.setBrush(color());
    painter.drawRect(rect());
}

const QColor& QColorButton::color () {

    // Return the current color.
    return _color;
}

void QColorButton::setColor (const QColor& color) {

    // Save the new color.
    _color = color;

    // Force it to be redrawn.
    update();

    // Notify others that are listening.
    emit colorChanged();
}

