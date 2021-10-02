#include "SeerAboutDialog.h"
#include <QtGui/QColor>
#include <QtGui/QPalette>

SeerAboutDialog::SeerAboutDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    // Set the TextEdit's background to the same as the window's.
    QColor windowColor = palette().color(QWidget::backgroundRole());

    QPalette p = textEdit->palette();
    p.setColor(QPalette::Base, windowColor);
    textEdit->setPalette(p);

    // Connect things.
}

SeerAboutDialog::~SeerAboutDialog () {
}

