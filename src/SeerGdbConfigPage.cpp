// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerGdbConfigPage.h"
#include <QtWidgets/QWidget>
#include <QtWidgets/QFileDialog>
#include <QtGlobal>

SeerGdbConfigPage::SeerGdbConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Connect things.
    QObject::connect(gdbProgramToolButton, &QToolButton::clicked,      this, &SeerGdbConfigPage::handleGdbProgramToolButton);

    // Setup the defaults.
    reset();
}

SeerGdbConfigPage::~SeerGdbConfigPage() {
}

QString SeerGdbConfigPage::gdbProgram () const {

    return gdbProgramLineEdit->text();
}

QString SeerGdbConfigPage::gdbArguments () const {

    return gdbArgumentsLineEdit->text();
}

bool SeerGdbConfigPage::gdbAsyncMode () const {

    return gdbAsyncModeCheckBox->isChecked();
}

bool SeerGdbConfigPage::gdbNonStopMode () const {

    return gdbNonStopModeCheckBox->isChecked();
}

bool SeerGdbConfigPage::gdbHandleTerminatingException () const {

    return gdbHandleTerminateExceptionCheckBox->isChecked();
}

bool SeerGdbConfigPage::gdbRandomizeStartAddress () const {

    return gdbRandomizeStartAddressCheckBox->isChecked();
}

bool SeerGdbConfigPage::gdbEnablePrettyPrinting () const {

    return gdbEnablePrettyPrintingCheckBox->isChecked();
}

QString SeerGdbConfigPage::gdbRemoteTargetType () const {

    return gdbRemoteTargetTypeCombo->currentText();
}

QString SeerGdbConfigPage::gdbArchitectureType () const {

    QString type = gdbArchitectureLineEdit->text();

    if (type == "") type = "auto";

    return type;
}

void SeerGdbConfigPage::setGdbProgram (const QString& program) {

    gdbProgramLineEdit->setText(program);
}

void SeerGdbConfigPage::setGdbArguments (const QString& arguments) {

    gdbArgumentsLineEdit->setText(arguments);
}

void SeerGdbConfigPage::setGdbAsyncMode (bool flag) {

    gdbAsyncModeCheckBox->setChecked(flag);
}

void SeerGdbConfigPage::setGdbNonStopMode (bool flag) {

    gdbNonStopModeCheckBox->setChecked(flag);
}

void SeerGdbConfigPage::setGdbHandleTerminatingException (bool flag) {

    gdbHandleTerminateExceptionCheckBox->setChecked(flag);
}

void SeerGdbConfigPage::setGdbRandomizeStartAddress (bool flag) {

    gdbRandomizeStartAddressCheckBox->setChecked(flag);
}

void SeerGdbConfigPage::setGdbEnablePrettyPrinting (bool flag) {

    gdbEnablePrettyPrintingCheckBox->setChecked(flag);
}

void SeerGdbConfigPage::setGdbRemoteTargetType (const QString& type) {

    gdbRemoteTargetTypeCombo->setCurrentText(type);
}

void SeerGdbConfigPage::setGdbArchitectureType (const QString& type) {

    gdbArchitectureLineEdit->setText(type);
}

void SeerGdbConfigPage::reset () {

    setGdbProgram("/usr/bin/gdb");
    setGdbArguments("--interpreter=mi");
    setGdbAsyncMode(true);
    setGdbNonStopMode(false);
    setGdbHandleTerminatingException(true);
    setGdbRandomizeStartAddress(false);
    setGdbEnablePrettyPrinting(true);
    setGdbRemoteTargetType("extended-remote");
    setGdbArchitectureType("auto");
}

void SeerGdbConfigPage::handleGdbProgramToolButton () {

    QString program = QFileDialog::getOpenFileName(this, "Select a gdb program to use as the debugger.", gdbProgram(), "", nullptr, QFileDialog::DontUseNativeDialog);

    if (program != "") {
        setGdbProgram(program);
    }
}

