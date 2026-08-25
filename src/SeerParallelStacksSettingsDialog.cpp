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

    // Restore window settings.
    readSettings();
}

SeerParallelStacksSettingsDialog::~SeerParallelStacksSettingsDialog () {
}

void SeerParallelStacksSettingsDialog::setShowMinimap (const QString& when) {
}

QString SeerParallelStacksSettingsDialog::showMinimap () const {
}

void SeerParallelStacksSettingsDialog::setShowFullFunctionName (bool flag) {
}

bool SeerParallelStacksSettingsDialog::showFullFunctionName () const {
}

void SeerParallelStacksSettingsDialog::setFunctionNameLenght (int length) {
}

int SeerParallelStacksSettingsDialog::functionNameLenght () const {
}

void SeerParallelStacksSettingsDialog::setShowFullStackSize (bool flag) {
}

bool SeerParallelStacksSettingsDialog::showFullStackSize () const {
}

void SeerParallelStacksSettingsDialog::setStackSize (int size) {
}

int  SeerParallelStacksSettingsDialog::stackSize () const {
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

