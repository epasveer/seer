// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerParallelStacksSettingsDialog.h"
#include <QtWidgets/QMessageBox>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerParallelStacksSettingsDialog::SeerParallelStacksSettingsDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets

    // Connect things.


    setShowMinimap("WhenNeeded");
    setShowFullFunctionName(true);
    setFunctionNameLength(64);
    setShowFullStackSize(true);
    setStackSize(20);

    // Restore window settings.
    readSettings();
}

SeerParallelStacksSettingsDialog::~SeerParallelStacksSettingsDialog () {
}

void SeerParallelStacksSettingsDialog::setShowMinimap (const QString& when) {

    if (when == "Always") {
        showMinimapAlwaysRadioButton->setChecked(true);
    }else if (when == "WhenNeeded") {
        showMinimapWhenNeededRadioButton->setChecked(true);
    }else{
        qDebug() << "Invalid Minimap mode of:" << when;
        showMinimapWhenNeededRadioButton->setChecked(true);
    }
}

QString SeerParallelStacksSettingsDialog::showMinimap () const {

    if (showMinimapAlwaysRadioButton->isChecked()) {
        return "Always";
    }

    if (showMinimapWhenNeededRadioButton->isChecked()) {
        return "WhenNeeded";
    }

    return "WhenNeeded";
}

void SeerParallelStacksSettingsDialog::setShowFullFunctionName (bool flag) {
    functionNameLengthAllCheckBox->setChecked(flag);
}

bool SeerParallelStacksSettingsDialog::showFullFunctionName () const {
    return functionNameLengthAllCheckBox->isChecked();
}

void SeerParallelStacksSettingsDialog::setFunctionNameLength (int length) {
    functionNameLengthSpinBox->setValue(length);
}

int SeerParallelStacksSettingsDialog::functionNameLength () const {
    return functionNameLengthSpinBox->value();
}

void SeerParallelStacksSettingsDialog::setShowFullStackSize (bool flag) {
    stackFrameSizeAllCheckBox->setChecked(flag);
}

bool SeerParallelStacksSettingsDialog::showFullStackSize () const {
    return stackFrameSizeAllCheckBox->isChecked();
}

void SeerParallelStacksSettingsDialog::setStackSize (int size) {
    stackFrameSizeSpinBox->setValue(size);
}

int SeerParallelStacksSettingsDialog::stackSize () const {
    return stackFrameSizeSpinBox->value();
}

void SeerParallelStacksSettingsDialog::writeSettings() {

    QSettings settings;

    settings.beginGroup("parallelstackssettingsdialog"); {
        settings.setValue("size", size());
    }settings.endGroup();
}

void SeerParallelStacksSettingsDialog::readSettings() {

    QSettings settings;

    settings.beginGroup("parallelstackssettingsdialog"); {
        if (settings.contains("size")) {
            resize(settings.value("size", QSize(400, 225)).toSize());
        }
    } settings.endGroup();
}

void SeerParallelStacksSettingsDialog::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QWidget::resizeEvent(event);
}

