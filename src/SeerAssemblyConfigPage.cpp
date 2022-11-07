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

bool SeerAssemblyConfigPage::showAddressColumn () const {

    return showAddressCheckBox->isChecked();
}

bool SeerAssemblyConfigPage::showOffsetColumn () const {

    return showOffsetCheckBox->isChecked();
}

bool SeerAssemblyConfigPage::showOpcodeColumn () const {

    return showOpcodeCheckBox->isChecked();
}

bool SeerAssemblyConfigPage::showSourceLines () const {

    return showSourceCheckBox->isChecked();
}

QString SeerAssemblyConfigPage::registerFormat () const {

    return registerFormatComboBox->currentText();
}

void SeerAssemblyConfigPage::setShowAssemblyTabOnStartup (bool flag) {

    assemblyTabOnStartupCheckBox->setChecked(flag);
}

void SeerAssemblyConfigPage::setKeepAssemblyTabOnTop (bool flag) {

    assemblyTabOnTopCheckBox->setChecked(flag);
}

void SeerAssemblyConfigPage::setDisassembyFlavor (const QString& flavor) {

    if (flavor == "att") {
        attFlavorRadioButton->setChecked(true);
    }else if (flavor == "intel") {
        intelFlavorRadioButton->setChecked(true);
    }else{
    }
}

void SeerAssemblyConfigPage::setSymbolDemagling (const QString& onoff) {

    if (onoff == "on") {
        demanglingOnRadioButton->setChecked(true);
    }else if (onoff == "off") {
        demanglingOffRadioButton->setChecked(true);
    }else{
    }
}

void SeerAssemblyConfigPage::setShowAddressColumn (bool flag) {

    showAddressCheckBox->setChecked(flag);
}

void SeerAssemblyConfigPage::setShowOffsetColumn (bool flag) {

    showOffsetCheckBox->setChecked(flag);
}

void SeerAssemblyConfigPage::setShowOpcodeColumn (bool flag) {

    showOpcodeCheckBox->setChecked(flag);
}

void SeerAssemblyConfigPage::setShowSourceLines (bool flag) {

    showSourceCheckBox->setChecked(flag);
}

void SeerAssemblyConfigPage::setRegisterFormat (const QString& format) {

    registerFormatComboBox->setCurrentText(format);
}

void SeerAssemblyConfigPage::reset () {

    setShowAssemblyTabOnStartup(false);
    setKeepAssemblyTabOnTop(true);
    setDisassembyFlavor("att");
    setSymbolDemagling("on");
    setShowAddressColumn(true);
    setShowOffsetColumn(false);
    setShowOpcodeColumn(false);
    setShowSourceLines(false);
    setRegisterFormat("Natural");
}

