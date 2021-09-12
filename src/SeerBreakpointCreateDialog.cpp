#include "SeerBreakpointCreateDialog.h"
#include <QtCore/QDebug>

SeerBreakpointCreateDialog::SeerBreakpointCreateDialog (QWidget* parent) : QDialog(parent) {

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

SeerBreakpointCreateDialog::~SeerBreakpointCreateDialog () {
}

void SeerBreakpointCreateDialog::setFilename (const QString& text) {
    filenameLineEdit->setText(text);
}

void SeerBreakpointCreateDialog::setFunctionName (const QString& text) {
    functionLineEdit->setText(text);
}

void SeerBreakpointCreateDialog::setLabelName (const QString& text) {
    labelLineEdit->setText(text);
}

void SeerBreakpointCreateDialog::setLineNumber (const QString& text) {
    lineNumberLineEdit->setText(text);
}

QString SeerBreakpointCreateDialog::filenameText () const {
    return filenameLineEdit->text();
}

QString SeerBreakpointCreateDialog::functionNameText () const {
    return functionLineEdit->text();
}

QString SeerBreakpointCreateDialog::labelNameText () const {
    return labelLineEdit->text();
}

QString SeerBreakpointCreateDialog::lineNumberText () const {
    return lineNumberLineEdit->text();
}

void SeerBreakpointCreateDialog::setTemporaryEnabled (bool flag) {
    temporaryCheckBox->setChecked(flag);
}

void SeerBreakpointCreateDialog::setHardwareEnabled (bool flag) {
    hardwareCheckBox->setChecked(flag);
}

void SeerBreakpointCreateDialog::setPendingEnabled (bool flag) {
    pendingCheckBox->setChecked(flag);
}

void SeerBreakpointCreateDialog::setDisabledEnabled (bool flag) {
    disabledCheckBox->setChecked(flag);
}

void SeerBreakpointCreateDialog::setConditionalEnabled (bool flag) {
    conditionalCheckBox->setChecked(flag);
    conditionalLineEdit->setEnabled(flag);
}

void SeerBreakpointCreateDialog::setIgnoreCountEnabled (bool flag) {
    ignoreCountCheckBox->setChecked(flag);
    ignoreCountLineEdit->setEnabled(flag);
}

void SeerBreakpointCreateDialog::setThreadIdEnabled (bool flag) {
    threadIdCheckBox->setChecked(flag);
    threadIdLineEdit->setEnabled(flag);
}

void SeerBreakpointCreateDialog::setConditionalText (const QString& text) {
    conditionalLineEdit->setText(text);
}

void SeerBreakpointCreateDialog::setIgnoreCountText (const QString& text) {
    ignoreCountLineEdit->setText(text);
}

void SeerBreakpointCreateDialog::setThreadIdText (const QString& text) {
    threadIdLineEdit->setText(text);
}

bool SeerBreakpointCreateDialog::temporaryEnabled () const {
    return temporaryCheckBox->isChecked();
}

bool SeerBreakpointCreateDialog::hardwareEnabled () const {
    return hardwareCheckBox->isChecked();
}

bool SeerBreakpointCreateDialog::pendingEnabled () const {
    return pendingCheckBox->isChecked();
}

bool SeerBreakpointCreateDialog::disabledEnabled () const {
    return disabledCheckBox->isChecked();
}

bool SeerBreakpointCreateDialog::conditionalEnabled () const {
    return conditionalCheckBox->isChecked();
}

bool SeerBreakpointCreateDialog::ignoreCountEnabled () const {
    return ignoreCountCheckBox->isChecked();
}

bool SeerBreakpointCreateDialog::threadIdEnabled () const {
    return threadIdCheckBox->isChecked();
}

QString SeerBreakpointCreateDialog::conditionalText () const {
    return conditionalLineEdit->text();
}

QString SeerBreakpointCreateDialog::ignoreCountText () const {
    return ignoreCountLineEdit->text();
}

QString SeerBreakpointCreateDialog::threadIdText () const {
    return threadIdLineEdit->text();
}

QString SeerBreakpointCreateDialog::breakpointText () const {

    // Build a breakpoint specification.
    QString breakpointParameters;

    if (temporaryEnabled()) {
        breakpointParameters += " -t";
    }

    if (hardwareEnabled()) {
        breakpointParameters += " -h";
    }

    if (pendingEnabled()) {
        breakpointParameters += " -f";
    }

    if (disabledEnabled()) {
        breakpointParameters += " -d";
    }

    if (conditionalEnabled()) {
        if (conditionalText() != "") {
            breakpointParameters += " -c " + conditionalText();
        }
    }

    if (ignoreCountEnabled()) {
        if (ignoreCountText() != "") {
            breakpointParameters += " -i " + ignoreCountText();
        }
    }

    if (threadIdEnabled()) {
        if (threadIdText() != "") {
            breakpointParameters += " -p " + threadIdText();
        }
    }

    if (filenameText() != "") {
        breakpointParameters += " --source " + filenameText();
    }

    if (functionNameText() != "") {
        breakpointParameters += " --function " + functionNameText();
    }

    if (labelNameText() != "") {
        breakpointParameters += " --label " + labelNameText();
    }

    if (lineNumberText() != "") {
        breakpointParameters += " --line " + lineNumberText();
    }

    return breakpointParameters;
}

