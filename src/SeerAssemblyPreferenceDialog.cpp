// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerAssemblyPreferenceDialog.h"
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerAssemblyPreferenceDialog::SeerAssemblyPreferenceDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets

    // Connect things.

    // Restore window settings.
    readSettings();
}

SeerAssemblyPreferenceDialog::~SeerAssemblyPreferenceDialog () {
}

void SeerAssemblyPreferenceDialog::setRegiserNamePC (const QString& name) {

    pcLineEdit->setText(name);
}

QString SeerAssemblyPreferenceDialog::regiserNamePC () const {

    return pcLineEdit->text();
}

void SeerAssemblyPreferenceDialog::setRegiserNameFLAGS (const QString& name) {

    flagsLineEdit->setText(name);
}

QString SeerAssemblyPreferenceDialog::regiserNameFLAGS () const {

    return flagsLineEdit->text();
}

void SeerAssemblyPreferenceDialog::setRegiserNameSP (const QString& name) {

    spLineEdit->setText(name);
}

QString SeerAssemblyPreferenceDialog::regiserNameSP () const {

    return spLineEdit->text();
}

void SeerAssemblyPreferenceDialog::setShowAssemblyAddress (bool flag) {

    showAddressCheckBox->setChecked(flag);
}

bool SeerAssemblyPreferenceDialog::showAssemblyAddress () const {

    return showAddressCheckBox->isChecked();
}

void SeerAssemblyPreferenceDialog::setShowAssemblyOffset (bool flag) {

    showOffsetCheckBox->setChecked(flag);
}

bool SeerAssemblyPreferenceDialog::showAssemblyOffset () const {

    return showOffsetCheckBox->isChecked();
}

void SeerAssemblyPreferenceDialog::setShowAssemblyOpcode (bool flag) {

    showOpcodeCheckBox->setChecked(flag);
}

bool SeerAssemblyPreferenceDialog::showAssemblyOpcode () const {

    return showOpcodeCheckBox->isChecked();
}

void SeerAssemblyPreferenceDialog::setShowAssemblySource (bool flag) {

    showSourceCheckBox->setChecked(flag);
}

bool SeerAssemblyPreferenceDialog::showAssemblySource () const {

    return showSourceCheckBox->isChecked();
}

void SeerAssemblyPreferenceDialog::writeSettings() {

    QSettings settings;

    settings.beginGroup("registerpreferencedialog"); {
        settings.setValue("size", size());
    }settings.endGroup();
}

void SeerAssemblyPreferenceDialog::readSettings() {

    QSettings settings;

    settings.beginGroup("registerpreferencedialog"); {
        resize(settings.value("size", QSize(350, 300)).toSize());
    } settings.endGroup();
}

void SeerAssemblyPreferenceDialog::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QWidget::resizeEvent(event);
}

