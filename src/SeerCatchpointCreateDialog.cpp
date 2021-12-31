#include "SeerCatchpointCreateDialog.h"
#include <QtCore/QDebug>

SeerCatchpointCreateDialog::SeerCatchpointCreateDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    setFilename("");
    setFunctionName("");
    setLabelName("");
    setLineNumber("");

    setTemporaryEnabled (false);
    setHardwareEnabled (false);
    setPendingEnabled (false);
    setDisabledEnabled (false);
    setConditionalEnabled (false);
    setIgnoreCountEnabled (false);
    setThreadIdEnabled (false);

    setConditionalText ("");
    setIgnoreCountText ("");
    setThreadIdText ("");

    // Connect things.
    QObject::connect(conditionalCheckBox, &QCheckBox::clicked,    conditionalLineEdit,  &QLineEdit::setEnabled);
    QObject::connect(ignoreCountCheckBox, &QCheckBox::clicked,    ignoreCountLineEdit,  &QLineEdit::setEnabled);
    QObject::connect(threadIdCheckBox,    &QCheckBox::clicked,    threadIdLineEdit,     &QLineEdit::setEnabled);
}

SeerCatchpointCreateDialog::~SeerCatchpointCreateDialog () {
}

void SeerCatchpointCreateDialog::setFilename (const QString& text) {
    filenameLineEdit->setText(text);
}

void SeerCatchpointCreateDialog::setFunctionName (const QString& text) {
    functionLineEdit->setText(text);
}

void SeerCatchpointCreateDialog::setLabelName (const QString& text) {
    labelLineEdit->setText(text);
}

void SeerCatchpointCreateDialog::setLineNumber (const QString& text) {
    lineNumberLineEdit->setText(text);
}

QString SeerCatchpointCreateDialog::filenameText () const {
    return filenameLineEdit->text();
}

QString SeerCatchpointCreateDialog::functionNameText () const {
    return functionLineEdit->text();
}

QString SeerCatchpointCreateDialog::labelNameText () const {
    return labelLineEdit->text();
}

QString SeerCatchpointCreateDialog::lineNumberText () const {
    return lineNumberLineEdit->text();
}

void SeerCatchpointCreateDialog::setTemporaryEnabled (bool flag) {
    temporaryCheckBox->setChecked(flag);
}

void SeerCatchpointCreateDialog::setHardwareEnabled (bool flag) {
    hardwareCheckBox->setChecked(flag);
}

void SeerCatchpointCreateDialog::setPendingEnabled (bool flag) {
    pendingCheckBox->setChecked(flag);
}

void SeerCatchpointCreateDialog::setDisabledEnabled (bool flag) {
    disabledCheckBox->setChecked(flag);
}

void SeerCatchpointCreateDialog::setConditionalEnabled (bool flag) {
    conditionalCheckBox->setChecked(flag);
    conditionalLineEdit->setEnabled(flag);
}

void SeerCatchpointCreateDialog::setIgnoreCountEnabled (bool flag) {
    ignoreCountCheckBox->setChecked(flag);
    ignoreCountLineEdit->setEnabled(flag);
}

void SeerCatchpointCreateDialog::setThreadIdEnabled (bool flag) {
    threadIdCheckBox->setChecked(flag);
    threadIdLineEdit->setEnabled(flag);
}

void SeerCatchpointCreateDialog::setConditionalText (const QString& text) {
    conditionalLineEdit->setText(text);
}

void SeerCatchpointCreateDialog::setIgnoreCountText (const QString& text) {
    ignoreCountLineEdit->setText(text);
}

void SeerCatchpointCreateDialog::setThreadIdText (const QString& text) {
    threadIdLineEdit->setText(text);
}

bool SeerCatchpointCreateDialog::temporaryEnabled () const {
    return temporaryCheckBox->isChecked();
}

bool SeerCatchpointCreateDialog::hardwareEnabled () const {
    return hardwareCheckBox->isChecked();
}

bool SeerCatchpointCreateDialog::pendingEnabled () const {
    return pendingCheckBox->isChecked();
}

bool SeerCatchpointCreateDialog::disabledEnabled () const {
    return disabledCheckBox->isChecked();
}

bool SeerCatchpointCreateDialog::conditionalEnabled () const {
    return conditionalCheckBox->isChecked();
}

bool SeerCatchpointCreateDialog::ignoreCountEnabled () const {
    return ignoreCountCheckBox->isChecked();
}

bool SeerCatchpointCreateDialog::threadIdEnabled () const {
    return threadIdCheckBox->isChecked();
}

QString SeerCatchpointCreateDialog::conditionalText () const {
    return conditionalLineEdit->text();
}

QString SeerCatchpointCreateDialog::ignoreCountText () const {
    return ignoreCountLineEdit->text();
}

QString SeerCatchpointCreateDialog::threadIdText () const {
    return threadIdLineEdit->text();
}

QString SeerCatchpointCreateDialog::catchpointText () const {

    // Build a catchpoint specification.
    QString catchpointParameters;

    if (temporaryEnabled()) {
        catchpointParameters += " -t";
    }

    if (hardwareEnabled()) {
        catchpointParameters += " -h";
    }

    if (pendingEnabled()) {
        catchpointParameters += " -f";
    }

    if (disabledEnabled()) {
        catchpointParameters += " -d";
    }

    if (conditionalEnabled()) {
        if (conditionalText() != "") {
            catchpointParameters += " -c " + conditionalText();
        }
    }

    if (ignoreCountEnabled()) {
        if (ignoreCountText() != "") {
            catchpointParameters += " -i " + ignoreCountText();
        }
    }

    if (threadIdEnabled()) {
        if (threadIdText() != "") {
            catchpointParameters += " -p " + threadIdText();
        }
    }

    if (filenameText() != "") {
        catchpointParameters += " --source " + filenameText();
    }

    if (functionNameText() != "") {
        catchpointParameters += " --function " + functionNameText();
    }

    if (labelNameText() != "") {
        catchpointParameters += " --label " + labelNameText();
    }

    if (lineNumberText() != "") {
        catchpointParameters += " --line " + lineNumberText();
    }

    return catchpointParameters;
}

