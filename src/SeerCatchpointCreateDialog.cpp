#include "SeerCatchpointCreateDialog.h"
#include <QtCore/QDebug>

SeerCatchpointCreateDialog::SeerCatchpointCreateDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    setType("");
    setNameText("");

    setTemporaryEnabled (false);

    // Connect things.
}

SeerCatchpointCreateDialog::~SeerCatchpointCreateDialog () {
}

void SeerCatchpointCreateDialog::setType (const QString& text) {

    if (text == "throw") {
        throwRadioButton->setChecked(true);

    }else if (text == "rethrow") {
        rethrowRadioButton->setChecked(true);

    }else if (text == "catch") {
        catchRadioButton->setChecked(true);

    }else if (text == "load") {
        loadRadioButton->setChecked(true);

    }else if (text == "unload") {
        unloadRadioButton->setChecked(true);

    }else{
        throwRadioButton->setChecked(true);
    }
}

QString SeerCatchpointCreateDialog::typeText () const {

    if (throwRadioButton->isChecked()) {
        return "throw";

    }else if (rethrowRadioButton->isChecked()) {
        return "rethrow";

    }else if (catchRadioButton->isChecked()) {
        return "catch";

    }else if (loadRadioButton->isChecked()) {
        return "load";

    }else if (unloadRadioButton->isChecked()) {
        return "unload";

    }else{
        return "throw";
    }
}

void SeerCatchpointCreateDialog::setTemporaryEnabled (bool flag) {
    temporaryCheckBox->setChecked(flag);
}

void SeerCatchpointCreateDialog::setNameText (const QString& text) {
    nameLineEdit->setText(text);
}

QString SeerCatchpointCreateDialog::nameText () const {
    return nameLineEdit->text();
}

bool SeerCatchpointCreateDialog::temporaryEnabled () const {
    return temporaryCheckBox->isChecked();
}

QString SeerCatchpointCreateDialog::catchpointText () const {

    // Build a catchpoint specification.
    QString catchpointParameters = typeText();

    if (temporaryEnabled()) {
        catchpointParameters += " -t";
    }

    if (typeText() == "load" || typeText() == "unload") { // These don't need a "-r"
        catchpointParameters += " " + nameText();

    }else{  // These do, but only if there is a name.
        if (nameText() != "") {
            catchpointParameters += " -r " + nameText();
        }
    }

    //qDebug() << catchpointParameters;

    return catchpointParameters;
}

