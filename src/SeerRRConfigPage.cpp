#include "SeerRRConfigPage.h"
#include <QtWidgets/QWidget>
#include <QtWidgets/QFileDialog>
#include <QtGlobal>

SeerRRConfigPage::SeerRRConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Connect things.

    // Setup the defaults.
    reset();
}

SeerRRConfigPage::~SeerRRConfigPage() {
}

QString SeerRRConfigPage::gdbProgram () const {

    return gdbProgramLineEdit->text();
}

QString SeerRRConfigPage::gdbArguments () const {

    return gdbArgumentsLineEdit->text();
}

void SeerRRConfigPage::setGdbProgram (const QString& program) {

    gdbProgramLineEdit->setText(program);
}

void SeerRRConfigPage::setGdbArguments (const QString& arguments) {

    gdbArgumentsLineEdit->setText(arguments);
}

void SeerRRConfigPage::reset () {

    setGdbProgram("/usr/bin/gdb");
    setGdbArguments("--interpreter=mi -l 10000");
}

