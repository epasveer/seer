#include "SeerSeerConfigPage.h"
#include <QtWidgets/QWidget>

SeerSeerConfigPage::SeerSeerConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    setConsoleMode("normal");
    setConsoleScrollLines(1000);
    setRememberWindowSizes(true);
    setRememberManualCommandCount(10);
    setClearManualCommandHistory(false);

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

void SeerSeerConfigPage::setConsoleScrollLines (int count) {

    consoleScrollLinesSpinBox->setValue(count);
}

int SeerSeerConfigPage::consoleScrollLines () const {

    return consoleScrollLinesSpinBox->value();
}

void SeerSeerConfigPage::setRememberWindowSizes (bool flag) {

    rememberSizescheckBox->setChecked(flag);
}

bool SeerSeerConfigPage::rememberWindowSizes () const {

    return rememberSizescheckBox->isChecked();
}

void SeerSeerConfigPage::setRememberManualCommandCount (int count) {

    rememberGdbCommandsSpinBox->setValue(count);
}

int SeerSeerConfigPage::rememberManualCommandCount () const {

    return rememberGdbCommandsSpinBox->value();
}

void SeerSeerConfigPage::setClearManualCommandHistory (bool flag) {

    clearHistoryCheckBox->setChecked(flag);
}

bool SeerSeerConfigPage::clearManualCommandHistory () const {

    return clearHistoryCheckBox->isChecked();
}

