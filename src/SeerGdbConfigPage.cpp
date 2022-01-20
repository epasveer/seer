#include "SeerGdbConfigPage.h"
#include <QtWidgets/QWidget>
#include <QtWidgets/QFileDialog>

SeerGdbConfigPage::SeerGdbConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets

    // Connect things.
    QObject::connect(gdbProgramToolButton, &QToolButton::clicked,       this, &SeerGdbConfigPage::handleGdbProgramToolButton);
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

void SeerGdbConfigPage::setGdbProgram (const QString& program) {

    gdbProgramLineEdit->setText(program);
}

void SeerGdbConfigPage::setGdbArguments (const QString& arguments) {

    gdbArgumentsLineEdit->setText(arguments);
}

void SeerGdbConfigPage::setGdbAsyncMode (bool flag) {

    gdbAsyncModeCheckBox->setChecked(flag);
}

void SeerGdbConfigPage::handleGdbProgramToolButton () {

    QString program = QFileDialog::getOpenFileName(this, "Select a gdb program to use as the debugger.", gdbProgram(), "", nullptr, QFileDialog::DontUseNativeDialog);

    if (program != "") {
        setGdbProgram(program);
    }
}

