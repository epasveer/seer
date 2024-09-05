#include "SeerSeerConfigPage.h"
#include <QtWidgets/QWidget>

SeerSeerConfigPage::SeerSeerConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Connect things.

    // Setup the defaults.
    reset();
}

SeerSeerConfigPage::~SeerSeerConfigPage() {
}

void SeerSeerConfigPage::setConsoleMode (const QString& mode) {

    if (mode == "attached") {
        attachedRadioButton->setChecked(true);

    }else if (mode == "detached") {
        detachedRadioButton->setChecked(true);

    }else if (mode == "detachedminimized") {
        detachedMinimizedRadioButton->setChecked(true);

    }else{
        attachedRadioButton->setChecked(true);
    }
}

QString SeerSeerConfigPage::consoleMode () const {

    if (attachedRadioButton->isChecked()) {
        return "attached";

    }else if (detachedRadioButton->isChecked()) {
        return "detached";

    }else if (detachedMinimizedRadioButton->isChecked()) {
        return "detachedminimized";

    }else{
        return "attached";
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

void SeerSeerConfigPage::reset () {

    setConsoleMode("attached");
    setConsoleScrollLines(1000);
    setRememberWindowSizes(true);
    setRememberManualCommandCount(10);
    setClearManualCommandHistory(false);
}

