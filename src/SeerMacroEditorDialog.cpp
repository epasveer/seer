// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerMacroEditorDialog.h"
#include <QtCore/QDebug>

SeerMacroEditorDialog::SeerMacroEditorDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    setWindowFlags(Qt::Tool);   // lightweight tool window
    setModal(false);

    // Connect things.
}

SeerMacroEditorDialog::~SeerMacroEditorDialog () {
}

void SeerMacroEditorDialog::setMacroName (const QString& name) {

    setWindowTitle("Seer - Macro Editor for '" + name + "'");
}

void SeerMacroEditorDialog::setMacroNickname (const QString& nickname) {

    nicknameLineEdit->setText(nickname);
}

QString SeerMacroEditorDialog::macroNickname () const {

    return nicknameLineEdit->text();
}

void SeerMacroEditorDialog::setCommands (const QStringList& commands) {

    textEditor->clear();
    textEditor->setPlainText(commands.join('\n'));
    textEditor->moveCursor(QTextCursor::Start);
    textEditor->setFocus();
}

QStringList SeerMacroEditorDialog::commands () const {

    return textEditor->toPlainText().split('\n');
}

