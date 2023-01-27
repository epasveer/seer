#include "SeerDebugDialog.h"
#include "SeerExecutableFilterProxyModel.h"
#include "SeerDirectoryFilterProxyModel.h"
#include "SeerSlashProcDialog.h"
#include "SeerHelpPageDialog.h"
#include <QtWidgets/QFileDialog>
#include <QtCore/QDir>
#include <QtCore/QSettings>
#include <QtCore/QDebug>
#include <QtGlobal>

SeerDebugDialog::SeerDebugDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

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
    QObject::connect(executableNameToolButton,             &QToolButton::clicked,               this, &SeerDebugDialog::handleExecutableNameToolButton);
    QObject::connect(executableSymbolNameToolButton,       &QToolButton::clicked,               this, &SeerDebugDialog::handleExecutableSymbolNameToolButton);
    QObject::connect(executableWorkingDirectoryToolButton, &QToolButton::clicked,               this, &SeerDebugDialog::handleExecutableWorkingDirectoryToolButton);
    QObject::connect(loadBreakpointsFilenameToolButton,    &QToolButton::clicked,               this, &SeerDebugDialog::handleLoadBreakpointsFilenameToolButton);
    QObject::connect(loadCoreFilenameToolButton,           &QToolButton::clicked,               this, &SeerDebugDialog::handleLoadCoreFilenameToolButton);
    QObject::connect(breakpointInFunctionLineEdit,         &QLineEdit::textChanged,             this, &SeerDebugDialog::handleBreakpointInFunctionLineEdit);
    QObject::connect(attachProgramPidToolButton,           &QToolButton::clicked,               this, &SeerDebugDialog::handleProgramPidToolButton);
    QObject::connect(helpCoreToolButton,                   &QToolButton::clicked,               this, &SeerDebugDialog::handleHelpCoreToolButtonClicked);

#if (QT_VERSION >= QT_VERSION_CHECK(5, 15, 0))
    QObject::connect(runModeTabWidget,                     &QTabWidget::currentChanged,         this, &SeerDebugDialog::handleRunModeChanged);
#else
    QObject::connect(runModeTabWidget,                     &QTabWidget::currentChanged,         this, &SeerDebugDialog::handleRunModeChanged);
#endif

    // Set initial run mode.
    handleRunModeChanged(0);

    // Restore window settings.
    readSettings();
}

SeerDebugDialog::~SeerDebugDialog () {
}

void SeerDebugDialog::setExecutableName (const QString& executableName) {
    executableNameLineEdit->setText(executableName);
}

QString SeerDebugDialog::executableName () const {

    if (executableNameLineEdit->isEnabled()) {
        return executableNameLineEdit->text();
    }

    return "";
}

void SeerDebugDialog::setExecutableSymbolName (const QString& executableSymbolName) {
    executableSymbolNameLineEdit->setText(executableSymbolName);
}

QString SeerDebugDialog::executableSymbolName () const {

    if (executableSymbolNameLineEdit->isEnabled()) {
        return executableSymbolNameLineEdit->text();
    }

    return "";
}

void SeerDebugDialog::setExecutableWorkingDirectory (const QString& executableWorkingDirectory) {
    executableWorkingDirectoryLineEdit->setText(executableWorkingDirectory);
}

QString SeerDebugDialog::executableWorkingDirectory () const {

    if (executableWorkingDirectoryLineEdit->isEnabled()) {
        return executableWorkingDirectoryLineEdit->text();
    }

    return "";
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

void SeerDebugDialog::setLaunchMode (const QString& mode) {

    if (mode == "start") {

        runModeTabWidget->setCurrentIndex(0);

        setBreakpointMode("inmain");

    }else if (mode == "run") {

        runModeTabWidget->setCurrentIndex(0);

        setBreakpointMode("none");

    }else if (mode == "attach") {

        runModeTabWidget->setCurrentIndex(1);

    }else if (mode == "connect") {

        runModeTabWidget->setCurrentIndex(2);

    }else if (mode == "corefile") {

        runModeTabWidget->setCurrentIndex(3);

    }else if (mode == "") {

        runModeTabWidget->setCurrentIndex(0);

        setBreakpointMode("none");

    }else{

        qWarning() << "Unknown launch mode of:" << mode;
    }
}

QString SeerDebugDialog::launchMode () {

    if (runModeTabWidget->currentIndex() == 0) {

        if (breakpointMode() == "inmain") {
            return "start";
        }else if (breakpointMode() == "infunction") {
            return "run";
        }else if (breakpointMode() == "none") {
            return "run";
        }else{
            return "run";
        }

    }else if (runModeTabWidget->currentIndex() == 1) {

        return "attach";

    }else if (runModeTabWidget->currentIndex() == 2) {

        return "connect";

    }else if (runModeTabWidget->currentIndex() == 3) {

        return "corefile";
    }

    qWarning() << "Unknown launch mode of:" << runModeTabWidget->currentIndex();

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

    executableNameLineEdit->setEnabled(false);
    executableNameToolButton->setEnabled(false);
    executableSymbolNameLineEdit->setEnabled(false);
    executableSymbolNameToolButton->setEnabled(false);
    executableWorkingDirectoryLineEdit->setEnabled(false);
    executableWorkingDirectoryToolButton->setEnabled(false);

    // ID == 0   RUN/START
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

    // ID == 1   ATTACH
    attachProgramPidLabel->setEnabled(false);
    attachProgramPidLineEdit->setEnabled(false);
    attachProgramPidToolButton->setEnabled(false);

    // ID == 2   CONNECT
    connectProgramHostPortLabel->setEnabled(false);
    connectProgramHostPortLineEdit->setEnabled(false);

    // ID == 3   CORE
    loadCoreFilenameLabel->setEnabled(false);
    loadCoreFilenameLineEdit->setEnabled(false);
    loadCoreFilenameToolButton->setEnabled(false);

    //
    // Enable the newly selected one.
    //

    // ID == 0   RUN/START
    if (id == 0) {
        executableNameLineEdit->setEnabled(true);
        executableNameToolButton->setEnabled(true);
        executableSymbolNameLineEdit->setEnabled(true);
        executableSymbolNameToolButton->setEnabled(true);
        executableWorkingDirectoryLineEdit->setEnabled(true);
        executableWorkingDirectoryToolButton->setEnabled(true);
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

    // ID == 1   ATTACH
    if (id == 1) {
        executableNameLineEdit->setEnabled(true);
        executableNameToolButton->setEnabled(true);
        executableSymbolNameLineEdit->setEnabled(true);
        executableSymbolNameToolButton->setEnabled(true);
        attachProgramPidLabel->setEnabled(true);
        attachProgramPidLineEdit->setEnabled(true);
        attachProgramPidToolButton->setEnabled(true);
    }

    // ID == 2   CONNECT
    if (id == 2) {
        executableSymbolNameLineEdit->setEnabled(true);
        executableSymbolNameToolButton->setEnabled(true);
        connectProgramHostPortLabel->setEnabled(true);
        connectProgramHostPortLineEdit->setEnabled(true);
    }

    // ID == 3   CORE
    if (id == 3) {
        executableSymbolNameLineEdit->setEnabled(true);
        executableSymbolNameToolButton->setEnabled(true);
        loadCoreFilenameLabel->setEnabled(true);
        loadCoreFilenameLineEdit->setEnabled(true);
        loadCoreFilenameToolButton->setEnabled(true);
    }
}

void SeerDebugDialog::handleHelpCoreToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog(this);
    help->loadFile(":/seer/resources/help/CorefileDebugMode.md");
    help->show();
    help->raise();
}

void SeerDebugDialog::writeSettings() {

    QSettings settings;

    settings.beginGroup("debugdialog"); {
        settings.setValue("size", size());
    }settings.endGroup();

    //qDebug() << size();
}

void SeerDebugDialog::readSettings() {

    QSettings settings;

    settings.beginGroup("debugdialog"); {
        resize(settings.value("size", QSize(800, 600)).toSize());
    } settings.endGroup();

    //qDebug() << size();
}

void SeerDebugDialog::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QWidget::resizeEvent(event);
}

