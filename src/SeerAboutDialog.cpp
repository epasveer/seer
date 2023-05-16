#include "SeerAboutDialog.h"
#include "SeerUtl.h"
#include <QtGui/QColor>
#include <QtGui/QPalette>
#include <QtCore/QFile>
#include <QtCore/QString>
#include <QtCore/QDebug>

SeerAboutDialog::SeerAboutDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Get the About text from the resource.
    QFile file(":/seer/resources/ABOUT.md");
    file.open(QFile::ReadOnly | QFile::Text);

    QTextStream stream(&file);

    QString text = stream.readAll();

    // Substitute the text holder with the version number.
    text.replace("VERSIONNUMBER", Seer::version());

    // Put the About text in as markdown. Move back to the begining.
    textBrowser->setMarkdown(text);
    textBrowser->moveCursor (QTextCursor::Start);

    // Set the TextBrowser's background to the same as the window's.
    QColor windowColor = palette().color(QWidget::backgroundRole());

    QPalette p = textBrowser->palette();
    p.setColor(QPalette::Base, windowColor);
    textBrowser->setPalette(p);
}

SeerAboutDialog::~SeerAboutDialog () {
}

