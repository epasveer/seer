#include "SeerSeerConfigPage.h"
#include <QtWidgets/QWidget>

SeerSeerConfigPage::SeerSeerConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    setConsoleMode("normal");
    setRememberWindowSizes(true);

    // Connect things.
}

SeerSeerConfigPage::~SeerSeerConfigPage() {
}

void SeerSeerConfigPage::setConsoleMode (const QString& mode) {

    if (mode == "normal") {
        normalRadioButton->setChecked(true);

    }else if (mode == "minimized") {
        minimizedRadioButton->setChecked(true);

    }else if (mode == "hidden") {
        hiddenRadioButton->setChecked(true);

    }else{
        normalRadioButton->setChecked(true);
    }
}

QString SeerSeerConfigPage::consoleMode () const {

    if (normalRadioButton->isChecked()) {
        return "normal";

    }else if (minimizedRadioButton->isChecked()) {
        return "minimized";

    }else if (hiddenRadioButton->isChecked()) {
        return "hidden";

    }else{
        return "normal";
    }
}

void SeerSeerConfigPage::setRememberWindowSizes (bool flag) {

    rememberSizescheckBox->setChecked(flag);
}

bool SeerSeerConfigPage::rememberWindowSizes () const {

    return rememberSizescheckBox->isChecked();
}

