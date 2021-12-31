#include "SeerCatchpointCreateDialog.h"
#include <QtCore/QDebug>

SeerCatchpointCreateDialog::SeerCatchpointCreateDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    setType("");
    setNameText("");

    setTemporaryEnabled (false);
    setNameEnabled (false);

    // Connect things.
    QObject::connect(nameCheckBox, &QCheckBox::clicked,     nameLineEdit, &QLineEdit::setEnabled);
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

    }else{
        return "throw";
    }
}

void SeerCatchpointCreateDialog::setTemporaryEnabled (bool flag) {
    temporaryCheckBox->setChecked(flag);
}

void SeerCatchpointCreateDialog::setNameEnabled (bool flag) {
    nameCheckBox->setChecked(flag);
    nameLineEdit->setEnabled(flag);
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

bool SeerCatchpointCreateDialog::nameEnabled () const {
    return nameCheckBox->isChecked();
}

QString SeerCatchpointCreateDialog::catchpointText () const {

    // Build a catchpoint specification.
    QString catchpointParameters = typeText();

    if (temporaryEnabled()) {
        catchpointParameters += " -t";
    }

    if (nameEnabled()) {
        if (nameText() != "") {
            catchpointParameters += " -r " + nameText();
        }
    }

    //qDebug() << __PRETTY_FUNCTION__ << ":" << catchpointParameters;

    return catchpointParameters;
}

