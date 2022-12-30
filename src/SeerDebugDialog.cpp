#include "SeerDebugDialog.h"
#include "SeerExecutableFilterProxyModel.h"
#include "SeerDirectoryFilterProxyModel.h"
#include "SeerSlashProcDialog.h"
#include <QtWidgets/QFileDialog>
#include <QtCore/QDir>
#include <QtCore/QDebug>
#include <QtGlobal>

SeerDebugDialog::SeerDebugDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    _runModeButtonGroup = new QButtonGroup(this); // ID's 1 thru 4.
    _runModeButtonGroup->addButton(runProgramRadioButton,     1); // "run" or "start". See breakpointMode().
    _runModeButtonGroup->addButton(attachProgramRadioButton,  2); // "attach"
    _runModeButtonGroup->addButton(connectProgramRadioButton, 3); // "connect"
    _runModeButtonGroup->addButton(loadCoreRadioButton,       4); // "corefile"

    // Setup the widgets
    setExecutableName("");
    setExecutableSymbolName("");
    setExecutableArguments("");
    setBreakpointsFilename("");
    setExecutableWorkingDirectory(QDir::currentPath());
    setBreakpointMode("none");
    setBreakpointFunctionName("");
    setShowAssemblyTab(false);
    setRandomizeStartAddress(false);
    setNonStopMode(false);
    setAttachPid(0);
    setConnectHostPort("");
    setCoreFilename("");

    // Connect things.
    QObject::connect(executableNameToolButton,             &QToolButton::clicked,                              this, &SeerDebugDialog::handleExecutableNameToolButton);
    QObject::connect(executableSymbolNameToolButton,       &QToolButton::clicked,                              this, &SeerDebugDialog::handleExecutableSymbolNameToolButton);
    QObject::connect(executableWorkingDirectoryToolButton, &QToolButton::clicked,                              this, &SeerDebugDialog::handleExecutableWorkingDirectoryToolButton);
    QObject::connect(loadBreakpointsFilenameToolButton,    &QToolButton::clicked,                              this, &SeerDebugDialog::handleLoadBreakpointsFilenameToolButton);
    QObject::connect(loadCoreFilenameToolButton,           &QToolButton::clicked,                              this, &SeerDebugDialog::handleLoadCoreFilenameToolButton);
    QObject::connect(breakpointInFunctionLineEdit,         &QLineEdit::textChanged,                            this, &SeerDebugDialog::handleBreakpointInFunctionLineEdit);
    QObject::connect(attachProgramPidToolButton,           &QToolButton::clicked,                              this, &SeerDebugDialog::handleProgramPidToolButton);

#if (QT_VERSION >= QT_VERSION_CHECK(5, 15, 0))
    QObject::connect(_runModeButtonGroup,                  QOverload<int>::of(&QButtonGroup::idClicked),       this, &SeerDebugDialog::handleRunModeChanged);
#else
    QObject::connect(_runModeButtonGroup,                  QOverload<int>::of(&QButtonGroup::buttonClicked),   this, &SeerDebugDialog::handleRunModeChanged);
#endif

    // Set initial run mode.
    handleRunModeChanged(-1);
}

SeerDebugDialog::~SeerDebugDialog () {
}

void SeerDebugDialog::setExecutableName (const QString& executableName) {
    executableNameLineEdit->setText(executableName);
}

QString SeerDebugDialog::executableName () const {
    return executableNameLineEdit->text();
}

void SeerDebugDialog::setExecutableSymbolName (const QString& executableSymbolName) {
    executableSymbolNameLineEdit->setText(executableSymbolName);
}

QString SeerDebugDialog::executableSymbolName () const {
    return executableSymbolNameLineEdit->text();
}

void SeerDebugDialog::setExecutableWorkingDirectory (const QString& executableWorkingDirectory) {
    executableWorkingDirectoryLineEdit->setText(executableWorkingDirectory);
}

QString SeerDebugDialog::executableWorkingDirectory () const {
    return executableWorkingDirectoryLineEdit->text();
}

void SeerDebugDialog::setExecutableArguments (const QString& executableArguments) {
    runProgramArgumentsLineEdit->setText(executableArguments);
}

QString SeerDebugDialog::executableArguments () const {
    return runProgramArgumentsLineEdit->text();
}

void SeerDebugDialog::setBreakpointsFilename (const QString& breakpointsFilename) {
    loadBreakpointsFilenameLineEdit->setText(breakpointsFilename);
}

QString SeerDebugDialog::breakpointsFilename () const {
    return loadBreakpointsFilenameLineEdit->text();
}

void SeerDebugDialog::setBreakpointMode (const QString& mode) {

    if (mode == "none") {
        noBreakpointRadioButton->setChecked(true);
        return;
    }else if (mode == "inmain") {
        breakpointInMainRadioButton->setChecked(true);
        return;
    }else if (mode == "infunction") {
        breakpointInFunctionRadioButton->setChecked(true);
        return;
    }

    noBreakpointRadioButton->setChecked(true);
}

QString SeerDebugDialog::breakpointMode () const {

    if (noBreakpointRadioButton->isChecked()) {
        return "none";
    }else if (breakpointInMainRadioButton->isChecked()) {
        return "inmain";
    }else if (breakpointInFunctionRadioButton->isChecked()) {
        return "infunction";
    }

    return "none";
}

void SeerDebugDialog::setBreakpointFunctionName (const QString& nameoraddress) {
    breakpointInFunctionLineEdit->setText(nameoraddress);
}

QString SeerDebugDialog::breakpointFunctionName () const {
    return breakpointInFunctionLineEdit->text();
}

void SeerDebugDialog::setShowAssemblyTab (bool flag) {
    showAsseblyTabCheckBox->setChecked(flag);
}

bool SeerDebugDialog::showAssemblyTab () const {
    return showAsseblyTabCheckBox->isChecked();
}

void SeerDebugDialog::setRandomizeStartAddress (bool flag) {
    randomizeStartAddressCheckBox->setChecked(flag);
}

bool SeerDebugDialog::randomizeStartAddress () const {
    return randomizeStartAddressCheckBox->isChecked();
}

void SeerDebugDialog::setNonStopMode (bool flag) {
    nonStopModeCheckBox->setChecked(flag);
}

bool SeerDebugDialog::nonStopMode () const {
    return nonStopModeCheckBox->isChecked();
}

void SeerDebugDialog::setCoreFilename (const QString& coreFilename) {
    loadCoreFilenameLineEdit->setText(coreFilename);
}

QString SeerDebugDialog::coreFilename () const {
    return loadCoreFilenameLineEdit->text();
}

void SeerDebugDialog::setAttachPid (int pid) {

    if (pid < 1) {
        attachProgramPidLineEdit->clear();
    }else{
        attachProgramPidLineEdit->setText(QString::number(pid));
    }
}

int SeerDebugDialog::attachPid () const {
    return attachProgramPidLineEdit->text().toInt();
}

void SeerDebugDialog::setConnectHostPort (const QString& connectHostPort) {
    connectProgramHostPortLineEdit->setText(connectHostPort);
}

QString SeerDebugDialog::connectHostPort () const {
    return connectProgramHostPortLineEdit->text();
}

void SeerDebugDialog::setConnectSerialBaud (int connectBaudRate) {

    if (connectBaudRate < 1) {
        connectProgramBaudLineEdit->setText("");
    }else{
        connectProgramBaudLineEdit->setText(QString::number(connectBaudRate));
    }
}

int SeerDebugDialog::connectSerialBaud () const {

    if (connectProgramBaudLineEdit->text() == "") {
        return -1;
    }else{
        return connectProgramBaudLineEdit->text().toInt();
    }
}

void SeerDebugDialog::setConnectSerialParity (const QString& connectParity) {

    connectProgramParityComboBox->setCurrentText(connectParity);
}

QString SeerDebugDialog::connectSerialParity () const {

    return connectProgramParityComboBox->currentText();
}

void SeerDebugDialog::setLaunchMode (const QString& mode) {

    if (mode == "start") {
        runProgramRadioButton->click();

        setBreakpointMode("inmain");

    }else if (mode == "run") {
        runProgramRadioButton->click();

        setBreakpointMode("none");

    }else if (mode == "attach") {
        attachProgramRadioButton->click();

    }else if (mode == "connect") {
        connectProgramRadioButton->click();

    }else if (mode == "corefile") {
        loadCoreRadioButton->click();

    }else if (mode == "") {
        runProgramRadioButton->click();

        setBreakpointMode("none");

    }else{
        qWarning() << "Unknown launch mode of:" << mode;
    }
}

QString SeerDebugDialog::launchMode () {

    if (_runModeButtonGroup->checkedId() == 1) {
        if (breakpointMode() == "inmain") {
            return "start";
        }else if (breakpointMode() == "infunction") {
            return "run";
        }else if (breakpointMode() == "none") {
            return "run";
        }else{
            return "run";
        }

    }else if (_runModeButtonGroup->checkedId() == 2) {
        return "attach";

    }else if (_runModeButtonGroup->checkedId() == 3) {
        return "connect";

    }else if (_runModeButtonGroup->checkedId() == 4) {
        return "corefile";
    }

    qWarning() << "Unknown launch mode of:" << _runModeButtonGroup->checkedId();

    return "";
}

void SeerDebugDialog::handleExecutableNameToolButton () {

    QString name = QFileDialog::getOpenFileName(this, "Select an Executable to debug.", executableName(), "", nullptr, QFileDialog::DontUseNativeDialog);

    if (name != "") {
        setExecutableName(name);
    }
}

void SeerDebugDialog::handleExecutableSymbolNameToolButton () {

    QString name = QFileDialog::getOpenFileName(this, "Select a Symbol File for the executable.", executableSymbolName(), "", nullptr, QFileDialog::DontUseNativeDialog);

    if (name != "") {
        setExecutableSymbolName(name);
    }
}

void SeerDebugDialog::handleExecutableWorkingDirectoryToolButton () {

    QString name = QFileDialog::getExistingDirectory(this, "Select a Working Directory to run in.", executableWorkingDirectory(), QFileDialog::ShowDirsOnly|QFileDialog::DontUseNativeDialog);

    if (name != "") {
        setExecutableWorkingDirectory(name);
    }
}

void SeerDebugDialog::handleLoadBreakpointsFilenameToolButton () {

    QString name = QFileDialog::getOpenFileName(this, "Select a breakpoints file to load.", breakpointsFilename(), "Breakpoints (*.brk);;All files (*.*)", nullptr, QFileDialog::DontUseNativeDialog);

    if (name != "") {
        setBreakpointsFilename(name);
    }
}

void SeerDebugDialog::handleBreakpointInFunctionLineEdit () {

    breakpointInFunctionRadioButton->setChecked(true);
}

void SeerDebugDialog::handleLoadCoreFilenameToolButton () {

    QString name = QFileDialog::getOpenFileName(this, "Select a core file to debug.", coreFilename(), "Core Files (core core.*)", nullptr, QFileDialog::DontUseNativeDialog);

    if (name != "") {
        setCoreFilename(name);
    }
}

void SeerDebugDialog::handleProgramPidToolButton () {

    SeerSlashProcDialog dlg(this);

    // Execute the dialog and get the result.
    if (dlg.exec()) {
        setAttachPid(dlg.selectedPid());
    }
}

void SeerDebugDialog::handleRunModeChanged (int id) {

    //
    // Disable all imprortant widgets.
    //

    // ID == 1
    runProgramArgumentsLabel->setEnabled(false);
    runProgramArgumentsLineEdit->setEnabled(false);
    loadBreakpointsFilenameLabel->setEnabled(false);
    loadBreakpointsFilenameLineEdit->setEnabled(false);
    loadBreakpointsFilenameToolButton->setEnabled(false);
    noBreakpointRadioButton->setEnabled(false);
    breakpointInMainRadioButton->setEnabled(false);
    breakpointInFunctionRadioButton->setEnabled(false);
    breakpointInFunctionLineEdit->setEnabled(false);
    showAsseblyTabCheckBox->setEnabled(false);
    randomizeStartAddressCheckBox->setEnabled(false);
    nonStopModeCheckBox->setEnabled(false);

    // ID == 2
    attachProgramPidLabel->setEnabled(false);
    attachProgramPidLineEdit->setEnabled(false);
    attachProgramPidToolButton->setEnabled(false);

    // ID == 3
    connectProgramHostPortLabel->setEnabled(false);
    connectProgramHostPortLineEdit->setEnabled(false);
    connectProgramSerialLabel->setEnabled(false);
    connectProgramBaudLineEdit->setEnabled(false);
    connectProgramParityLabel->setEnabled(false);
    connectProgramParityComboBox->setEnabled(false);

    // ID == 4
    loadCoreFilenameLabel->setEnabled(false);
    loadCoreFilenameLineEdit->setEnabled(false);
    loadCoreFilenameToolButton->setEnabled(false);

    //
    // Enable the newly selected one.
    //

    // ID == 1
    if (id == 1) {
        runProgramArgumentsLabel->setEnabled(true);
        runProgramArgumentsLineEdit->setEnabled(true);
        loadBreakpointsFilenameLabel->setEnabled(true);
        loadBreakpointsFilenameLineEdit->setEnabled(true);
        loadBreakpointsFilenameToolButton->setEnabled(true);
        noBreakpointRadioButton->setEnabled(true);
        breakpointInMainRadioButton->setEnabled(true);
        breakpointInFunctionRadioButton->setEnabled(true);
        breakpointInFunctionLineEdit->setEnabled(true);
        showAsseblyTabCheckBox->setEnabled(true);
        randomizeStartAddressCheckBox->setEnabled(true);
        nonStopModeCheckBox->setEnabled(true);
    }

    // ID == 2
    if (id == 2) {
        attachProgramPidLabel->setEnabled(true);
        attachProgramPidLineEdit->setEnabled(true);
        attachProgramPidToolButton->setEnabled(true);
    }

    // ID == 3
    if (id == 3) {
        connectProgramHostPortLabel->setEnabled(true);
        connectProgramHostPortLineEdit->setEnabled(true);
        connectProgramSerialLabel->setEnabled(true);
        connectProgramBaudLineEdit->setEnabled(true);
        connectProgramParityLabel->setEnabled(true);
        connectProgramParityComboBox->setEnabled(true);
    }

    // ID == 4
    if (id == 4) {
        loadCoreFilenameLabel->setEnabled(true);
        loadCoreFilenameLineEdit->setEnabled(true);
        loadCoreFilenameToolButton->setEnabled(true);
    }
}

