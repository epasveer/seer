// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerOpenocdConfigPage.h"
#include "SeerUtl.h"
#include <QtWidgets/QWidget>
#include <QtWidgets/QFileDialog>
#include <QtGlobal>

SeerOpenocdConfigPage::SeerOpenocdConfigPage(QWidget* parent) : QWidget(parent) {
    // if your header inherits Ui::SeerOpenocdConfigPage, call setupUi(this)
    setupUi(this);
    reset();
}

SeerOpenocdConfigPage::~SeerOpenocdConfigPage() {
}

QString SeerOpenocdConfigPage::openocdGdbExe () const {
    return openocdGdbLineEdit->text();
}

QString SeerOpenocdConfigPage::openocdExe () const {
    return openocdProgramLineEdit->text();
}

QString SeerOpenocdConfigPage::openocdGdbPort () const {
    return openocdGdbPortLineEdit->text();
}

QString SeerOpenocdConfigPage::openocdTelnetPort () const {
    return openocdTelnetPortLineEdit->text();
}

void SeerOpenocdConfigPage::reset () {
    // Reset all fields to their default values
    setOpenocdGdbExe("/usr/bin/gdb-multiarch");
    setOpenocdExe("/usr/local/bin/openocd");
    setOpenocdGdbPort("3333");
    setOpenocdTelnetPort("4444");
}

void SeerOpenocdConfigPage::setOpenocdGdbExe (const QString& program) {
    openocdGdbLineEdit->setText(program);
}

void SeerOpenocdConfigPage::setOpenocdExe (const QString& program) {
    openocdProgramLineEdit->setText(program);
}

void SeerOpenocdConfigPage::setOpenocdGdbPort (const QString& port) {
    openocdGdbPortLineEdit->setText(port);
}

void SeerOpenocdConfigPage::setOpenocdTelnetPort (const QString& port) {
    openocdTelnetPortLineEdit->setText(port);
}
