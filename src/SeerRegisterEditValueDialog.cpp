// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerRegisterEditValueDialog.h"
#include <QtCore/QDebug>

SeerRegisterEditValueDialog::SeerRegisterEditValueDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    set("","");

    // Connect things.
}

SeerRegisterEditValueDialog::~SeerRegisterEditValueDialog () {
}

void SeerRegisterEditValueDialog::set (const QString& regname, const QString& regvalue) {
    registerNameLineEdit->setText(regname);
    registerValueLineEdit->setText(regvalue);
}

QString SeerRegisterEditValueDialog::nameText () const {
    return registerNameLineEdit->text();
}

QString SeerRegisterEditValueDialog::valueText () const {
    return registerValueLineEdit->text();
}

