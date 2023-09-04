#include "SeerRRConfigPage.h"
#include <QtWidgets/QWidget>
#include <QtWidgets/QFileDialog>
#include <QtGlobal>

SeerRRConfigPage::SeerRRConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Connect things.
    QObject::connect(rrProgramToolButton,  &QToolButton::clicked,           this, &SeerRRConfigPage::handleRRProgramToolButton);

    // Setup the defaults.
    reset();
}

SeerRRConfigPage::~SeerRRConfigPage() {
}

QString SeerRRConfigPage::rrProgram () const {

    return rrProgramLineEdit->text();
}

QString SeerRRConfigPage::rrArguments () const {

    return rrArgumentsLineEdit->text();
}

QString SeerRRConfigPage::gdbArguments () const {

    return gdbArgumentsLineEdit->text();
}

void SeerRRConfigPage::setRRProgram (const QString& program) {

    rrProgramLineEdit->setText(program);
}

void SeerRRConfigPage::setRRArguments (const QString& arguments) {

    rrArgumentsLineEdit->setText(arguments);
}

void SeerRRConfigPage::setGdbArguments (const QString& arguments) {

    gdbArgumentsLineEdit->setText(arguments);
}

void SeerRRConfigPage::reset () {

    setRRProgram("/usr/bin/rr");
    setRRArguments("replay --interpreter=mi");
    setGdbArguments("");
}

void SeerRRConfigPage::handleRRProgramToolButton () {

    QString program = QFileDialog::getOpenFileName(this, "Select a RR program to use.", rrProgram(), "", nullptr, QFileDialog::DontUseNativeDialog);

    if (program != "") {
        setRRProgram(program);
    }
}

