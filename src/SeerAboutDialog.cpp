#include "SeerAboutDialog.h"
#include "SeerUtl.h"
#include <QtGui/QColor>
#include <QtGui/QPalette>
#include <QtCore/QFile>
#include <QtCore/QTextStream>

SeerAboutDialog::SeerAboutDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets

    // Add the Version number to the beggining of the About text.
    textEdit->moveCursor (QTextCursor::Start);
    textEdit->setText ("\n");
    textEdit->append ("Version: " + Seer::version());

    // Add the About text from the resource.
    QFile file(":/seer/resources/about.txt");
    file.open(QFile::ReadOnly | QFile::Text);

    QTextStream stream(&file);
    textEdit->append(stream.readAll());

    textEdit->moveCursor (QTextCursor::Start);

    // Set the TextEdit's background to the same as the window's.
    QColor windowColor = palette().color(QWidget::backgroundRole());

    QPalette p = textEdit->palette();
    p.setColor(QPalette::Base, windowColor);
    textEdit->setPalette(p);
}

SeerAboutDialog::~SeerAboutDialog () {
}

