#include "SeerArgumentsDialog.h"

SeerArgumentsDialog::SeerArgumentsDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    setExecutableArguments("");

    // Connect things.
}

SeerArgumentsDialog::~SeerArgumentsDialog () {
}

void SeerArgumentsDialog::setExecutableArguments (const QString& executableArguments) {
    executableArgumentsLineEdit->setText(executableArguments);
}

QString SeerArgumentsDialog::executableArguments () {
    return executableArgumentsLineEdit->text();
}

