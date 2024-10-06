#include "QColorButton.h"
#include <QtWidgets/QColorDialog>
#include <QtGui/QPalette>
#include <QtGui/QPainter>
#include <QtCore/QDebug>

QColorButton::QColorButton(QWidget* parent) : QFrame(parent) {

    // Construct the UI.
    setupUi(this);

    setColor(palette().color(QPalette::Window));
}

QColorButton::QColorButton(const QColor& color, QWidget* parent) : QFrame(parent) {

    // Construct the UI.
    setupUi(this);

    // Create color swatch
    setColor(color);
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

QColor QColorButton::color () const {

    // Return the current color.
    return colorSwatch->color();
}

void QColorButton::setColor (const QColor& color) {

    // Save the new color.
    colorSwatch->setColor(color);
    colorSwatch->update();

    // Notify others that are listening.
    emit colorChanged();
}

