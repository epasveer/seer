#include "SeerAboutDialog.h"
#include "SeerUtl.h"
#include <QtGui/QColor>
#include <QtGui/QPalette>

SeerAboutDialog::SeerAboutDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets

    // Add the Version number to the end of the About text.
    // Scroll back to the top.
    textEdit->moveCursor (QTextCursor::End);
    textEdit->insertPlainText ("\n");
    textEdit->insertPlainText ("\n");
    textEdit->insertPlainText ("Version: " + Seer::version());
    textEdit->insertPlainText ("\n");
    textEdit->moveCursor (QTextCursor::Start);

    // Set the TextEdit's background to the same as the window's.
    QColor windowColor = palette().color(QWidget::backgroundRole());

    QPalette p = textEdit->palette();
    p.setColor(QPalette::Base, windowColor);
    textEdit->setPalette(p);
}

SeerAboutDialog::~SeerAboutDialog () {
}

