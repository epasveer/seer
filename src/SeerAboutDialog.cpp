// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerAboutDialog.h"
#include "SeerUtl.h"
#include <QtGui/QColor>
#include <QtGui/QPalette>
#include <QtGui/QGuiApplication>
#include <QtGui/QStyleHints>
#include <QtCore/QFile>
#include <QtCore/QString>
#include <QtCore/QDebug>

SeerAboutDialog::SeerAboutDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Handle when theme is changed. Change the color of all icons.
    QObject::connect(QGuiApplication::styleHints(),  &QStyleHints::colorSchemeChanged,         this, &SeerAboutDialog::handleThemeChanged);

    // Get the About text from the resource.
    QFile file(":/seer/resources/ABOUT.md");

    bool f = file.open(QFile::ReadOnly | QFile::Text);
    if (f == false) {
        return;
    }

    QTextStream stream(&file);

    QString text = stream.readAll();

    // Substitute the text holder with the version number.
    text.replace("VERSIONNUMBER", Seer::version());

    // Put the About text in as markdown. Move back to the begining.
    textBrowser->setMarkdown(text);
    textBrowser->moveCursor (QTextCursor::Start);

    // Set the label's icon.
    handleThemeChanged();
}

SeerAboutDialog::~SeerAboutDialog () {
}

void SeerAboutDialog::handleThemeChanged () {

    // Get the colorized icon.
    QIcon icon = Seer::colorizeIcon(QIcon(":/seer/resources/icons/hicolor/256x256/seergdb.png"), QSize(256,256));

    // Set the label's pixmap
    label->setPixmap(icon.pixmap(QSize(256,256)));
}

