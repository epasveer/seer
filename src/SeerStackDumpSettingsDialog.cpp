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

    // Setup the widgets
    setStackPointerExpression("$sp");
    setBytesBeforeSP(0);
    setBytesAfterSP(32);
    setAsciiBytes(8);

    // Connect things.
}

SeerStackDumpSettingsDialog::~SeerStackDumpSettingsDialog () {
}

void SeerStackDumpSettingsDialog::setStackPointerExpression (const QString& expression) {
    spExpressionLineEdit->setText(expression);
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

