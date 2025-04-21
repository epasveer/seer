#include "SeerSkipCreateDialog.h"
#include "SeerHelpPageDialog.h"
#include <QtCore/QDebug>

SeerSkipCreateDialog::SeerSkipCreateDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    fileRadioButton->setChecked(true);
    fileRadioButton->setFocus();

    // Connect things.
    QObject::connect(modeButtonGroup,  QOverload<int>::of(&QButtonGroup::idClicked),        this, &SeerSkipCreateDialog::handleModeButtonGroup);
    QObject::connect(helpToolButton,   &QToolButton::clicked,                               this, &SeerSkipCreateDialog::handleHelpToolButton);

    handleModeButtonGroup();
}

SeerSkipCreateDialog::~SeerSkipCreateDialog () {
}

void SeerSkipCreateDialog::handleModeButtonGroup () {

    // Disable all text fields. We'll enable one later.
    fileLineEdit->setEnabled(false);
    fileGlobLineEdit->setEnabled(false);
    functionLineEdit->setEnabled(false);
    functionRegexLineEdit->setEnabled(false);

    // Enable the one that is selected.
    QAbstractButton* button = modeButtonGroup->checkedButton();
    if (button == dynamic_cast<QAbstractButton*>(fileRadioButton)) {
        fileLineEdit->setEnabled(true);
        fileLineEdit->setFocus();
    } else if (button == dynamic_cast<QAbstractButton*>(fileGlobRadioButton)) {
        fileGlobLineEdit->setEnabled(true);
        fileGlobLineEdit->setFocus();
    } else if (button == dynamic_cast<QAbstractButton*>(functionRadioButton)) {
        functionLineEdit->setEnabled(true);
        functionLineEdit->setFocus();
    } else if (button == dynamic_cast<QAbstractButton*>(functionRegexRadioButton)) {
        functionRegexLineEdit->setEnabled(true);
        functionRegexLineEdit->setFocus();
    }
}

void SeerSkipCreateDialog::handleHelpToolButton () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog(this);
    help->loadFile(":/seer/resources/help/Skips.md");
    help->show();
    help->raise();
}

QString SeerSkipCreateDialog::skipMode () const {

    // Build a catchpoint specification.
    QString skipMode;

    if (fileRadioButton->isChecked()) {
        skipMode = "file";
    }else if (fileGlobRadioButton->isChecked()) {
        skipMode = "gfile";
    }else if (functionRadioButton->isChecked()) {
        skipMode = "function";
    }else if (functionRegexRadioButton->isChecked()) {
        skipMode = "rfunction";
    }

    qDebug() << skipMode;

    return skipMode;
}

QString SeerSkipCreateDialog::skipParameters () const {

    // Build a catchpoint specification.
    QString skipParameters;

    if (fileRadioButton->isChecked()) {
        skipParameters = fileLineEdit->text();
    }else if (fileGlobRadioButton->isChecked()) {
        skipParameters = fileGlobLineEdit->text();
    }else if (functionRadioButton->isChecked()) {
        skipParameters = functionLineEdit->text();
    }else if (functionRegexRadioButton->isChecked()) {
        skipParameters = functionRegexLineEdit->text();
    }

    qDebug() << skipParameters;

    return skipParameters;
}

