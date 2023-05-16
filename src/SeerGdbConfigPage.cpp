#include "SeerGdbConfigPage.h"
#include <QtWidgets/QWidget>
#include <QtWidgets/QFileDialog>
#include <QtGlobal>

SeerGdbConfigPage::SeerGdbConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Connect things.
    QObject::connect(gdbProgramToolButton, &QToolButton::clicked,                                  this, &SeerGdbConfigPage::handleGdbProgramToolButton);
    QObject::connect(styleButtonGroup,     QOverload<int>::of(&QButtonGroup::idClicked),           this, &SeerGdbConfigPage::handleDprintfButtonGroup);

    // Setup the defaults.
    reset();
}

SeerGdbConfigPage::~SeerGdbConfigPage() {
}

QString SeerGdbConfigPage::gdbProgram () const {

    return gdbProgramLineEdit->text();
}

QString SeerGdbConfigPage::gdbArguments () const {

    return gdbArgumentsLineEdit->text();
}

bool SeerGdbConfigPage::gdbAsyncMode () const {

    return gdbAsyncModeCheckBox->isChecked();
}

bool SeerGdbConfigPage::gdbNonStopMode () const {

    return gdbNonStopModeCheckBox->isChecked();
}

bool SeerGdbConfigPage::gdbHandleTerminatingException () const {

    return gdbHandleTerminateExceptionCheckBox->isChecked();
}

bool SeerGdbConfigPage::gdbRandomizeStartAddress () const {

    return gdbRandomizeStartAddressCheckBox->isChecked();
}

bool SeerGdbConfigPage::gdbEnablePrettyPrinting () const {

    return gdbEnablePrettyPrintingCheckBox->isChecked();
}

void SeerGdbConfigPage::setGdbProgram (const QString& program) {

    gdbProgramLineEdit->setText(program);
}

void SeerGdbConfigPage::setGdbArguments (const QString& arguments) {

    gdbArgumentsLineEdit->setText(arguments);
}

void SeerGdbConfigPage::setGdbAsyncMode (bool flag) {

    gdbAsyncModeCheckBox->setChecked(flag);
}

void SeerGdbConfigPage::setGdbNonStopMode (bool flag) {

    gdbNonStopModeCheckBox->setChecked(flag);
}

void SeerGdbConfigPage::setGdbHandleTerminatingException (bool flag) {

    gdbHandleTerminateExceptionCheckBox->setChecked(flag);
}

void SeerGdbConfigPage::setGdbRandomizeStartAddress (bool flag) {

    gdbRandomizeStartAddressCheckBox->setChecked(flag);
}

void SeerGdbConfigPage::setGdbEnablePrettyPrinting (bool flag) {

    gdbEnablePrettyPrintingCheckBox->setChecked(flag);
}

QString SeerGdbConfigPage::dprintfStyle () const {

    if (styleGdbRadioButton->isChecked()) {
        return "gdb";
    }else if (styleCallRadioButton->isChecked()) {
        return "call";
    }else if (styleAgentRadioButton->isChecked()) {
        return "agent";
    }else{
        return "";
    }
}

QString SeerGdbConfigPage::dprintfFunction () const {

    return functionLineEdit->text();
}

QString SeerGdbConfigPage::dprintfChannel () const {

    return channelLineEdit->text();
}

void SeerGdbConfigPage::setDprintfStyle (const QString& style) {

    if (style == "gdb") {
        styleGdbRadioButton->setChecked(true);
    }else if (style == "call") {
        styleCallRadioButton->setChecked(true);
    }else if (style == "agent") {
        styleAgentRadioButton->setChecked(true);
    }else{
    }

    handleDprintfButtonGroup();
}

void SeerGdbConfigPage::setDprintfFunction (const QString& function) {

    functionLineEdit->setText(function);
}

void SeerGdbConfigPage::setDprintfChannel (const QString& channel) {

    channelLineEdit->setText(channel);
}

void SeerGdbConfigPage::reset () {

    setGdbProgram("/usr/bin/gdb");
    setGdbArguments("--interpreter=mi");
    setGdbAsyncMode(true);
    setGdbNonStopMode(false);
    setGdbHandleTerminatingException(true);
    setGdbRandomizeStartAddress(false);
    setGdbEnablePrettyPrinting(true);

    setDprintfStyle("gdb");
    setDprintfFunction("printf");
    setDprintfChannel("");
}

void SeerGdbConfigPage::handleGdbProgramToolButton () {

    QString program = QFileDialog::getOpenFileName(this, "Select a gdb program to use as the debugger.", gdbProgram(), "", nullptr, QFileDialog::DontUseNativeDialog);

    if (program != "") {
        setGdbProgram(program);
    }
}

void SeerGdbConfigPage::handleDprintfButtonGroup () {

    functionLineEdit->setEnabled(false);
    channelLineEdit->setEnabled(false);

    if (styleCallRadioButton->isChecked()) {
        functionLineEdit->setEnabled(true);
        channelLineEdit->setEnabled(true);
    }
}

