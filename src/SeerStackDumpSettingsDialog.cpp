// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerStackDumpSettingsDialog.h"
#include "SeerRegisterTreeWidgetItem.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtGui/QRegularExpressionValidator>
#include <QtCore/QRegularExpression>
#include <QtCore/QSettings>
#include <QtCore/QList>
#include <QtCore/QDebug>

SeerStackDumpSettingsDialog::SeerStackDumpSettingsDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    spHighLightColorButton->setFrameStyle(QFrame::Box|QFrame::Plain);
    spHighLightColorButton->setLineWidth(1);

    // Setup the widgets
    setStackPointerExpression("$sp");
    setStackPointerColor(QColor("lightGray"));
    setBytesBeforeSP(16);
    setBytesAfterSP(16);
    setAsciiBytes(8);

    // Connect things.
}

SeerStackDumpSettingsDialog::~SeerStackDumpSettingsDialog () {
}

void SeerStackDumpSettingsDialog::setStackPointerExpression (const QString& expression) {
    spExpressionLineEdit->setText(expression);
}

void SeerStackDumpSettingsDialog::setStackPointerColor (const QColor& color) {
    spHighLightColorButton->setColor(color);
}

QColor SeerStackDumpSettingsDialog::stackPointerColor () const {
    return spHighLightColorButton->color();
}

QString SeerStackDumpSettingsDialog::stackPointerExpression () const {
    return spExpressionLineEdit->text();
}

void SeerStackDumpSettingsDialog::setBytesBeforeSP (int nbytes) {
    preBytesSpinBox->setValue(nbytes);
}

int SeerStackDumpSettingsDialog::bytesBeforeSP () const {
    return preBytesSpinBox->value();
}

void SeerStackDumpSettingsDialog::setBytesAfterSP (int nbytes) {
    postBytesSpinBox->setValue(nbytes);
}

int SeerStackDumpSettingsDialog::bytesAfterSP () const {
    return postBytesSpinBox->value();
}

void SeerStackDumpSettingsDialog::setAsciiBytes (int nbytes) {
    asciiBytesSpinBox->setValue(nbytes);
}

int SeerStackDumpSettingsDialog::asciiBytes () const {
    return asciiBytesSpinBox->value();
}

