// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerTracepointCreateDialog.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"

SeerTracepointCreateDialog::SeerTracepointCreateDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    conditionalLineEdit->setEnabled(false);
    passcountSpinBox->setEnabled(false);
    actionsPlainTextEdit->setPlainText("collect $args, $locals");

    filenameLineEdit->setFocus();

    // Connect things.
    QObject::connect(conditionalCheckBox, &QCheckBox::toggled,   conditionalLineEdit, &QLineEdit::setEnabled);
    QObject::connect(passcountCheckBox,   &QCheckBox::toggled,   passcountSpinBox,    &QSpinBox::setEnabled);
    QObject::connect(helpToolButton,      &QToolButton::clicked, this,                &SeerTracepointCreateDialog::handleHelpToolButtonClicked);
}

SeerTracepointCreateDialog::~SeerTracepointCreateDialog () {
}

void SeerTracepointCreateDialog::setFilename (const QString& text) {
    filenameLineEdit->setText(text);
}

void SeerTracepointCreateDialog::setLineNumber (const QString& text) {
    lineNumberLineEdit->setText(text);
}

QString SeerTracepointCreateDialog::tracepointParameters () const {

    // Build a tracepoint specification.
    //
    //  -break-insert -a [ -h ] [ -f ] [ -d ] [ -c condition ] [ location ]

    QString location;

    if (filenameLineEdit->text() != "") {
        location += " --source \"" + filenameLineEdit->text() + "\"";
    }

    if (functionLineEdit->text() != "") {
        location += " --function \"" + Seer::quoteChars(functionLineEdit->text(), "\"") + "\"";
    }

    if (labelLineEdit->text() != "") {
        location += " --label " + labelLineEdit->text();
    }

    if (lineNumberLineEdit->text() != "") {
        location += " --line " + lineNumberLineEdit->text();
    }

    if (location == "") {
        return "";
    }

    QString parameters = "-a";

    if (fastCheckBox->isChecked()) {
        parameters += " -h";
    }

    if (pendingCheckBox->isChecked()) {
        parameters += " -f";
    }

    if (disabledCheckBox->isChecked()) {
        parameters += " -d";
    }

    if (conditionalCheckBox->isChecked() && conditionalLineEdit->text() != "") {
        parameters += " -c \"" + Seer::quoteChars(conditionalLineEdit->text(), "\"") + "\"";
    }

    return parameters + location;
}

QStringList SeerTracepointCreateDialog::actions () const {

    return Seer::quoteChars(actionsPlainTextEdit->toPlainText().split('\n', Qt::SkipEmptyParts), "\"");
}

int SeerTracepointCreateDialog::passcount () const {

    return passcountCheckBox->isChecked() ? passcountSpinBox->value() : 0;
}

void SeerTracepointCreateDialog::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog(this);
    help->loadFile(":/seer/resources/help/Tracepoints.md");
    help->show();
    help->raise();
}
