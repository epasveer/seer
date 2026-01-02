// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerSignalEditValueDialog.h"
#include <QtCore/QDebug>

SeerSignalEditValueDialog::SeerSignalEditValueDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    set("","","","","");

    // Connect things.
}

SeerSignalEditValueDialog::~SeerSignalEditValueDialog () {
}

void SeerSignalEditValueDialog::set (const QString& signame, const QString& stopvalue, const QString& printvalue, const QString& passvalue, const QString& description) {

    signalNameLineEdit->setText(signame);
    signalDescriptionLabel->setText(description);

    signalStopCheckBox->setCheckState(Qt::Unchecked);
    signalPrintCheckBox->setCheckState(Qt::Unchecked);
    signalPassCheckBox->setCheckState(Qt::Unchecked);

    if (stopvalue == "stop" || stopvalue.compare("Yes", Qt::CaseInsensitive) == 0) {
        signalStopCheckBox->setCheckState(Qt::Checked);
    }

    if (printvalue == "print" || printvalue.compare("Yes", Qt::CaseInsensitive) == 0) {
        signalPrintCheckBox->setCheckState(Qt::Checked);
    }

    if (passvalue == "pass" || passvalue.compare("Yes", Qt::CaseInsensitive) == 0) {
        signalPassCheckBox->setCheckState(Qt::Checked);
    }
}

QString SeerSignalEditValueDialog::nameText () const {

    return signalNameLineEdit->text();
}

QString SeerSignalEditValueDialog::stopText () const {

    if (signalStopCheckBox->isChecked()) {
        return "stop";
    }

    return "nostop";
}

QString SeerSignalEditValueDialog::printText () const {

    if (signalPrintCheckBox->isChecked()) {
        return "print";
    }

    return "noprint";
}

QString SeerSignalEditValueDialog::passText () const {

    if (signalPassCheckBox->isChecked()) {
        return "pass";
    }

    return "nopass";
}

QString SeerSignalEditValueDialog::descriptionText () const {

    return signalDescriptionLabel->text();
}

