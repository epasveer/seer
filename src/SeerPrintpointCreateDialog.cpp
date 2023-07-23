#include "SeerPrintpointCreateDialog.h"
#include <QtCore/QDebug>

SeerPrintpointCreateDialog::SeerPrintpointCreateDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    setFilename("");
    setFunctionName("");
    setLabelName("");
    setLineNumber("");

    setTemporaryEnabled (false);
    setPendingEnabled (false);
    setDisabledEnabled (false);
    setConditionalEnabled (false);
    setIgnoreCountEnabled (false);
    setThreadIdEnabled (false);

    setConditionalText ("");
    setIgnoreCountText ("");
    setThreadIdText ("");

    setFormat("");
    setArguments("");

    // Connect things.
    QObject::connect(conditionalCheckBox, &QCheckBox::clicked,    conditionalLineEdit,  &QLineEdit::setEnabled);
    QObject::connect(ignoreCountCheckBox, &QCheckBox::clicked,    ignoreCountLineEdit,  &QLineEdit::setEnabled);
    QObject::connect(threadIdCheckBox,    &QCheckBox::clicked,    threadIdLineEdit,     &QLineEdit::setEnabled);
}

SeerPrintpointCreateDialog::~SeerPrintpointCreateDialog () {
}

void SeerPrintpointCreateDialog::setFilename (const QString& text) {
    filenameLineEdit->setText(text);
}

void SeerPrintpointCreateDialog::setFunctionName (const QString& text) {
    functionLineEdit->setText(text);
}

void SeerPrintpointCreateDialog::setLabelName (const QString& text) {
    labelLineEdit->setText(text);
}

void SeerPrintpointCreateDialog::setLineNumber (const QString& text) {
    lineNumberLineEdit->setText(text);
}

QString SeerPrintpointCreateDialog::filenameText () const {
    return filenameLineEdit->text();
}

QString SeerPrintpointCreateDialog::functionNameText () const {
    return functionLineEdit->text();
}

QString SeerPrintpointCreateDialog::labelNameText () const {
    return labelLineEdit->text();
}

QString SeerPrintpointCreateDialog::lineNumberText () const {
    return lineNumberLineEdit->text();
}

void SeerPrintpointCreateDialog::setTemporaryEnabled (bool flag) {
    temporaryCheckBox->setChecked(flag);
}

void SeerPrintpointCreateDialog::setPendingEnabled (bool flag) {
    pendingCheckBox->setChecked(flag);
}

void SeerPrintpointCreateDialog::setDisabledEnabled (bool flag) {
    disabledCheckBox->setChecked(flag);
}

void SeerPrintpointCreateDialog::setConditionalEnabled (bool flag) {
    conditionalCheckBox->setChecked(flag);
    conditionalLineEdit->setEnabled(flag);
}

void SeerPrintpointCreateDialog::setIgnoreCountEnabled (bool flag) {
    ignoreCountCheckBox->setChecked(flag);
    ignoreCountLineEdit->setEnabled(flag);
}

void SeerPrintpointCreateDialog::setThreadIdEnabled (bool flag) {
    threadIdCheckBox->setChecked(flag);
    threadIdLineEdit->setEnabled(flag);
}

void SeerPrintpointCreateDialog::setConditionalText (const QString& text) {
    conditionalLineEdit->setText(text);
}

void SeerPrintpointCreateDialog::setIgnoreCountText (const QString& text) {
    ignoreCountLineEdit->setText(text);
}

void SeerPrintpointCreateDialog::setThreadIdText (const QString& text) {
    threadIdLineEdit->setText(text);
}

bool SeerPrintpointCreateDialog::temporaryEnabled () const {
    return temporaryCheckBox->isChecked();
}

bool SeerPrintpointCreateDialog::pendingEnabled () const {
    return pendingCheckBox->isChecked();
}

bool SeerPrintpointCreateDialog::disabledEnabled () const {
    return disabledCheckBox->isChecked();
}

bool SeerPrintpointCreateDialog::conditionalEnabled () const {
    return conditionalCheckBox->isChecked();
}

bool SeerPrintpointCreateDialog::ignoreCountEnabled () const {
    return ignoreCountCheckBox->isChecked();
}

bool SeerPrintpointCreateDialog::threadIdEnabled () const {
    return threadIdCheckBox->isChecked();
}

QString SeerPrintpointCreateDialog::conditionalText () const {
    return conditionalLineEdit->text();
}

QString SeerPrintpointCreateDialog::ignoreCountText () const {
    return ignoreCountLineEdit->text();
}

QString SeerPrintpointCreateDialog::threadIdText () const {
    return threadIdLineEdit->text();
}

void SeerPrintpointCreateDialog::setFormat (const QString& text) {
    formatLineEdit->setText(text);
}

void SeerPrintpointCreateDialog::setArguments (const QString& text) {
    argumentsLineEdit->setText(text);
}

QString SeerPrintpointCreateDialog::format () const {
    return formatLineEdit->text();
}

QString SeerPrintpointCreateDialog::arguments () const {
    return argumentsLineEdit->text();
}

QString SeerPrintpointCreateDialog::printpointText () const {

    // Build a printpoint specification.
    //
    //  -dprintf-insert [ -t ] [ -f ] [ -d ] [ --qualified ]
    //     [ -c condition ] [--force-condition] [ -i ignore-count ]
    //     [ -p thread-id ] [ location ]
    //     [ format ] [ argument ]

    QString printpointParameters;

    if (temporaryEnabled()) {
        printpointParameters += " -t";
    }

    if (pendingEnabled()) {
        printpointParameters += " -f";
    }

    if (disabledEnabled()) {
        printpointParameters += " -d";
    }

    if (conditionalEnabled()) {
        if (conditionalText() != "") {
            printpointParameters += " -c \"" + conditionalText() + "\"";
        }
    }

    if (ignoreCountEnabled()) {
        if (ignoreCountText() != "") {
            printpointParameters += " -i " + ignoreCountText();
        }
    }

    if (threadIdEnabled()) {
        if (threadIdText() != "") {
            printpointParameters += " -p " + threadIdText();
        }
    }

    if (filenameText() != "") {
        printpointParameters += " --source \"" + filenameText() +"\"";
    }

    if (functionNameText() != "") {
        printpointParameters += " --function " + functionNameText();
    }

    if (labelNameText() != "") {
        printpointParameters += " --label " + labelNameText();
    }

    if (lineNumberText() != "") {
        printpointParameters += " --line " + lineNumberText();
    }

    // Build the format string, ensuring a \" at the beginning and end of the string.
    printpointParameters += " ";

    if (format().front() != QString("\"")) {
        printpointParameters += "\"";
    }

    printpointParameters += format();

    if (format().back() != QString("\"")) {
        printpointParameters += "\"";
    }

    // Add the arguments to the string.
    printpointParameters += " " + arguments();

    return printpointParameters;
}

