#include "SeerWatchpointCreateDialog.h"
#include <QtWidgets/QButtonGroup>

SeerWatchpointCreateDialog::SeerWatchpointCreateDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    QButtonGroup* accessTypeButtonGroup = new QButtonGroup(this);
    accessTypeButtonGroup->addButton(writeRadioButton);
    accessTypeButtonGroup->addButton(readRadioButton);
    accessTypeButtonGroup->addButton(readWriteRadioButton);

    // Setup the widgets
    setExpression("");
    setReadAccessEnabled(false);
    setReadWriteAccessEnabled(false);
    setWriteAccessEnabled(true);

    // Connect things.
}

SeerWatchpointCreateDialog::~SeerWatchpointCreateDialog () {
}

void SeerWatchpointCreateDialog::setExpression (const QString& text) {
    expressionLineEdit->setText(text);
}

QString SeerWatchpointCreateDialog::expressionText () const {
    return expressionLineEdit->text();
}

void SeerWatchpointCreateDialog::setReadAccessEnabled (bool flag) {
    readRadioButton->setChecked(flag);
}

void SeerWatchpointCreateDialog::setWriteAccessEnabled (bool flag) {
    writeRadioButton->setChecked(flag);
}

void SeerWatchpointCreateDialog::setReadWriteAccessEnabled (bool flag) {
    readWriteRadioButton->setChecked(flag);
}

bool SeerWatchpointCreateDialog::readAccessEnabled () const {
    return readRadioButton->isChecked();
}

bool SeerWatchpointCreateDialog::writeAccessEnabled () const {
    return writeRadioButton->isChecked();
}

bool SeerWatchpointCreateDialog::readWriteAccessEnabled () const {
    return readWriteRadioButton->isChecked();
}

QString SeerWatchpointCreateDialog::watchpointText () const {

    // Build a watchpoint specification.
    QString watchpointParameters;

    if (readAccessEnabled()) {
        watchpointParameters += " -r";
    }

    if (readWriteAccessEnabled()) {
        watchpointParameters += " -a";
    }

    if (writeAccessEnabled()) {
        watchpointParameters += "";
    }

    watchpointParameters += " " + expressionText();

    return watchpointParameters;
}

