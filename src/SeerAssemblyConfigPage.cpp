#include "SeerAssemblyConfigPage.h"
#include <QtWidgets/QWidget>
#include <QtGlobal>

SeerAssemblyConfigPage::SeerAssemblyConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    reset();

    // Connect things.
}

SeerAssemblyConfigPage::~SeerAssemblyConfigPage() {
}

bool SeerAssemblyConfigPage::showAssemblyTabOnStartup () const {

    return assemblyTabOnStartupCheckBox->isChecked();
}

bool SeerAssemblyConfigPage::keepAssemblyTabOnTop () const {

    return assemblyTabOnTopCheckBox->isChecked();
}

QString SeerAssemblyConfigPage::disassembyFlavor () const {

    if (attFlavorRadioButton->isChecked()) {
        return "att";
    }else if (intelFlavorRadioButton->isChecked()) {
        return "intel";
    }else{
        return "";
    }
}

QString SeerAssemblyConfigPage::symbolDemagling () const {

    if (demanglingOnRadioButton->isChecked()) {
        return "on";
    }else if (demanglingOffRadioButton->isChecked()) {
        return "off";
    }else{
        return "";
    }
}

QString SeerAssemblyConfigPage::registerFormat () const {

    return registerFormatComboBox->currentText();
}

void SeerAssemblyConfigPage::setShowAssemblyTabOnStartup (bool flag) const {

    assemblyTabOnStartupCheckBox->setChecked(flag);
}

void SeerAssemblyConfigPage::setKeepAssemblyTabOnTop (bool flag) const {

    assemblyTabOnTopCheckBox->setChecked(flag);
}

void SeerAssemblyConfigPage::setDisassembyFlavor (const QString& flavor) const {

    if (flavor == "att") {
        attFlavorRadioButton->setChecked(true);
    }else if (flavor == "intel") {
        intelFlavorRadioButton->setChecked(true);
    }else{
    }
}

void SeerAssemblyConfigPage::setSymbolDemagling (const QString& yesno) const {

    if (yesno == "yes") {
        demanglingOnRadioButton->setChecked(true);
    }else if (yesno == "no") {
        demanglingOffRadioButton->setChecked(true);
    }else{
    }
}

void SeerAssemblyConfigPage::setRegisterFormat (const QString& format) const {

    registerFormatComboBox->setCurrentText(format);
}

void SeerAssemblyConfigPage::reset () {

    setShowAssemblyTabOnStartup(false);
    setKeepAssemblyTabOnTop(true);
    setDisassembyFlavor("att");
    setSymbolDemagling("yes");
    setRegisterFormat("Natural");
}

