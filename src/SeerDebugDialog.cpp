// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerDebugDialog.h"
#include "SeerExecutableFilterProxyModel.h"
#include "SeerDirectoryFilterProxyModel.h"
#include "SeerSlashProcDialog.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QMainWindow>
#include <QtCore/QDir>
#include <QtCore/QSettings>
#include <QtCore/QJsonObject>
#include <QtCore/QJsonArray>
#include <QtCore/QJsonValue>
#include <QtCore/QJsonDocument>
#include <QtCore/QDebug>
#include <QtGlobal>

SeerDebugDialog::SeerDebugDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    buttonBox->button(QDialogButtonBox::Ok)->setText("Launch");
    buttonBox->button(QDialogButtonBox::Reset)->setText("Clear");

    // Setup the widgets
    reset();

    // Create editor options bar.
    QToolButton* loadProjectToolButton = new QToolButton(runModeTabWidget);
    loadProjectToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/document-open.svg"));
    loadProjectToolButton->setToolTip("Load a Seer project file.");

    QToolButton* saveProjectToolButton = new QToolButton(runModeTabWidget);
    saveProjectToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/document-save.svg"));
    saveProjectToolButton->setToolTip("Save a Seer project file.");

    QToolButton* helpModeToolButton = new QToolButton(runModeTabWidget);
    helpModeToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpModeToolButton->setToolTip("Help on the debug launch modes.");

    QHContainerWidget* hcontainer = new QHContainerWidget(this);
    hcontainer->setSpacing(3);
    hcontainer->addWidget(loadProjectToolButton);
    hcontainer->addWidget(saveProjectToolButton);
    hcontainer->addWidget(helpModeToolButton);

    runModeTabWidget->setCornerWidget(hcontainer, Qt::TopRightCorner);

    // Connect things.
    QObject::connect(executableNameToolButton,             &QToolButton::clicked,               this, &SeerDebugDialog::handleExecutableNameToolButton);
    QObject::connect(executableSymbolNameToolButton,       &QToolButton::clicked,               this, &SeerDebugDialog::handleExecutableSymbolNameToolButton);
    QObject::connect(executableWorkingDirectoryToolButton, &QToolButton::clicked,               this, &SeerDebugDialog::handleExecutableWorkingDirectoryToolButton);
    QObject::connect(loadBreakpointsFilenameToolButton,    &QToolButton::clicked,               this, &SeerDebugDialog::handleLoadBreakpointsFilenameToolButton);
    QObject::connect(rrLoadTraceDirectoryToolButton,       &QToolButton::clicked,               this, &SeerDebugDialog::handleLoadRRTraceDirectoryToolButton);
    QObject::connect(rrLoadBreakpointsFilenameToolButton,  &QToolButton::clicked,               this, &SeerDebugDialog::handleLoadBreakpointsFilenameToolButton);
    QObject::connect(loadCoreFilenameToolButton,           &QToolButton::clicked,               this, &SeerDebugDialog::handleLoadCoreFilenameToolButton);
    QObject::connect(breakpointInFunctionLineEdit,         &QLineEdit::textChanged,             this, &SeerDebugDialog::handleBreakpointInFunctionLineEdit);
    QObject::connect(attachProgramPidToolButton,           &QToolButton::clicked,               this, &SeerDebugDialog::handleProgramPidToolButton);
    QObject::connect(loadProjectToolButton,                &QToolButton::clicked,               this, &SeerDebugDialog::handleLoadProjectToolButton);
    QObject::connect(saveProjectToolButton,                &QToolButton::clicked,               this, &SeerDebugDialog::handleSaveProjectToolButton);
    QObject::connect(helpModeToolButton,                   &QToolButton::clicked,               this, &SeerDebugDialog::handleHelpModeToolButtonClicked);
    QObject::connect(helpRunToolButton,                    &QToolButton::clicked,               this, &SeerDebugDialog::handleHelpRunToolButtonClicked);
    QObject::connect(helpAttachToolButton,                 &QToolButton::clicked,               this, &SeerDebugDialog::handleHelpAttachToolButtonClicked);
    QObject::connect(helpConnectToolButton,                &QToolButton::clicked,               this, &SeerDebugDialog::handleHelpConnectToolButtonClicked);
    QObject::connect(helpRRToolButton,                     &QToolButton::clicked,               this, &SeerDebugDialog::handleHelpRRToolButtonClicked);
    QObject::connect(helpCorefileToolButton,               &QToolButton::clicked,               this, &SeerDebugDialog::handleHelpCorefileToolButtonClicked);
    QObject::connect(runModeTabWidget,                     &QTabWidget::currentChanged,         this, &SeerDebugDialog::handleRunModeChanged);
    QObject::connect(buttonBox,                            &QDialogButtonBox::accepted,         this, &SeerDebugDialog::handleLaunchButtonClicked);
    QObject::connect(buttonBox,                            &QDialogButtonBox::clicked,          this, &SeerDebugDialog::handleResetButtonClicked);


    // Set initial run mode.
    handleRunModeChanged(0);

    // Restore window settings.
    readSettings();

    // Read default project settings, if any.
    loadDefaultProjectSettings();
}

SeerDebugDialog::~SeerDebugDialog () {
}

void SeerDebugDialog::reset () {

    // At least retain the launch mode.
    QString launchmode = launchMode();

    // Reset everything else.
    setExecutableName("");
    setExecutableSymbolName("");
    setExecutableWorkingDirectory("");
    setExecutableArguments("");
    setBreakpointsFilename("");
    setBreakpointFunctionName("");
    setBreakpointSourceName("");
    setBreakpointMode("inmain");
    setShowAssemblyTabMode("never");
    setRandomizeStartAddress(false);
    setNonStopMode(false);
    setPreGdbCommands(QStringList());
    setPostGdbCommands(QStringList());
    setAttachPid(0);
    setConnectHostPort("");
    setConnectRemoteTargetType("remote");
    setConnectGdbserverDebug(false);
    setRRTraceDirectory("");
    setCoreFilename("");
    setLaunchMode(launchmode);
    setProjectFilename("");
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

    // Try to keep these in sync. Arg!
    loadBreakpointsFilenameLineEdit->setText(breakpointsFilename);
    rrLoadBreakpointsFilenameLineEdit->setText(breakpointsFilename);
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
    }else if (mode == "insource") {
        breakpointAtSourceRadioButton->setChecked(true);
        return;
    }

    // Default of "inmain".
    breakpointInMainRadioButton->setChecked(true);
}

QString SeerDebugDialog::breakpointMode () const {

    if (noBreakpointRadioButton->isChecked()) {
        return "none";
    }else if (breakpointInMainRadioButton->isChecked()) {
        return "inmain";
    }else if (breakpointInFunctionRadioButton->isChecked()) {
        return "infunction";
    }else if (breakpointAtSourceRadioButton->isChecked()) {
        return "insource";
    }

    return "inmain";
}

void SeerDebugDialog::setBreakpointFunctionName (const QString& nameoraddress) {

    breakpointInFunctionLineEdit->setText(nameoraddress);

    if (nameoraddress != "") {
        breakpointInFunctionRadioButton->setChecked(true);
    }
}

QString SeerDebugDialog::breakpointFunctionName () const {
    return breakpointInFunctionLineEdit->text();
}

void SeerDebugDialog::setBreakpointSourceName (const QString& sourceFilenameAndLineno) {

    breakpointAtSourceLineEdit->setText(sourceFilenameAndLineno);

    if (sourceFilenameAndLineno != "") {
        breakpointAtSourceRadioButton->setChecked(true);
    }
}

QString SeerDebugDialog::breakpointSourceName () const {
    return breakpointAtSourceLineEdit->text();
}

void SeerDebugDialog::setShowAssemblyTabMode (const QString& mode) {
    showAssemblyTabComboBox->setCurrentText(mode);
}

QString SeerDebugDialog::showAssemblyTabMode () const {
    return showAssemblyTabComboBox->currentText();
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

void SeerDebugDialog::setPreGdbCommands (const QStringList& preGdbCommands) {
    preCommandsPlainTextEdit->setPlainText(preGdbCommands.join("\n"));
}

QStringList SeerDebugDialog::preGdbCommands () const {
    return preCommandsPlainTextEdit->toPlainText().split("\n");
}

void SeerDebugDialog::setPostGdbCommands (const QStringList& postGdbCommands) {
    postCommandsPlainTextEdit->setPlainText(postGdbCommands.join("\n"));
}

QStringList SeerDebugDialog::postGdbCommands () const {
    return postCommandsPlainTextEdit->toPlainText().split("\n");
}

void SeerDebugDialog::setCoreFilename (const QString& coreFilename) {
    loadCoreFilenameLineEdit->setText(coreFilename);
}

QString SeerDebugDialog::coreFilename () const {
    return loadCoreFilenameLineEdit->text();
}

void SeerDebugDialog::setAttachPid (int pid) {

    if (pid < 1) {
        attachProgramPidLineEdit->setText("");
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

void SeerDebugDialog::setConnectRemoteTargetType (const QString& type) {
    connectRemoteTargetTypeCombo->setCurrentText(type);
}

QString SeerDebugDialog::connectRemoteTargetType () const {
    return connectRemoteTargetTypeCombo->currentText();
}

void SeerDebugDialog::setConnectGdbserverDebug (bool enable) {
    connectGdbserverDebugCheckBox->setChecked(enable);
}

bool SeerDebugDialog::connectGdbserverDebug () const {
    return connectGdbserverDebugCheckBox->isChecked();
}

void SeerDebugDialog::setRRTraceDirectory (const QString& rrTraceDirectory) {
    rrTraceDirectoryLineEdit->setText(rrTraceDirectory);
}

QString SeerDebugDialog::rrTraceDirectory () const {
    return rrTraceDirectoryLineEdit->text();
}

void SeerDebugDialog::setLaunchMode (const QString& mode) {

    if (mode == "start") {

        runModeTabWidget->setCurrentIndex(0);

        setBreakpointMode(breakpointMode());

    }else if (mode == "run") {

        runModeTabWidget->setCurrentIndex(0);

        setBreakpointMode(breakpointMode());

    }else if (mode == "attach") {

        runModeTabWidget->setCurrentIndex(1);

    }else if (mode == "connect") {

        runModeTabWidget->setCurrentIndex(2);

    }else if (mode == "rr") {

        runModeTabWidget->setCurrentIndex(3);

    }else if (mode == "corefile") {

        runModeTabWidget->setCurrentIndex(4);

    }else if (mode == "") {

        runModeTabWidget->setCurrentIndex(0);

        setBreakpointMode("inmain");

    }else{

        qWarning() << "Unknown launch mode of:" << mode;

        runModeTabWidget->setCurrentIndex(0);

        setBreakpointMode("inmain");
    }
}

QString SeerDebugDialog::launchMode () const {

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

        return "rr";

    }else if (runModeTabWidget->currentIndex() == 4) {

        return "corefile";
    }

    qWarning() << "Unknown launch mode of:" << runModeTabWidget->currentIndex();

    return "";
}

void SeerDebugDialog::setProjectFilename (const QString& filename) {

    _projectFilename = filename;

    if (_projectFilename != "") {
        loadProject(_projectFilename, false);
    }
}

QString SeerDebugDialog::projectFilename () const {

    return _projectFilename;
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

void SeerDebugDialog::handleBreakpointInFunctionLineEdit () {

    breakpointInFunctionRadioButton->setChecked(true);
}

void SeerDebugDialog::handleLoadRRTraceDirectoryToolButton () {

    QString name = QFileDialog::getExistingDirectory(this, "Select a RR trace-directory to load.", rrTraceDirectory(), QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks);

    if (name != "") {
        setRRTraceDirectory(name);
    }
}

void SeerDebugDialog::handleLoadBreakpointsFilenameToolButton () {

    QString name = QFileDialog::getOpenFileName(this, "Select a breakpoints file to load.", breakpointsFilename(), "Breakpoints (*.seer);;All files (*.*)", nullptr, QFileDialog::DontUseNativeDialog);

    if (name != "") {
        setBreakpointsFilename(name);
    }
}

void SeerDebugDialog::handleLoadCoreFilenameToolButton () {

    QString name = QFileDialog::getOpenFileName(this, "Select a core file to debug.", coreFilename(), "Core Files (core core.* *.core)", nullptr, QFileDialog::DontUseNativeDialog);

    if (name != "") {
        setCoreFilename(name);
    }
}

void SeerDebugDialog::handleProgramPidToolButton () {

    SeerSlashProcDialog dlg(this);

    // Execute the dialog and get the result.
    if (dlg.exec()) {
        setAttachPid(dlg.selectedPid());
        if (executableName() == "") {
            setExecutableName(dlg.selectedFullname());
        }
    }
}

void SeerDebugDialog::handleLoadProjectToolButton () {

    // Get the filename to load from.
    QString fname = QFileDialog::getOpenFileName(this, "Load a project file.", "project.seer", "Projects (*.seer);;All files (*.*)", nullptr, QFileDialog::DontUseNativeDialog);

    if (fname == "") {
        return;
    }

    loadProject(fname, true);
}

void SeerDebugDialog::loadProject (const QString& filename, bool notify) {

    // Look for the mainwindow to place possible QMessageBox on.
    QWidget* p = this;

    if (isHidden() == true) {

        foreach (QWidget* w, qApp->topLevelWidgets()) {
            if (QMainWindow* mainWin = qobject_cast<QMainWindow*>(w)) {
                p = mainWin;
                break;
            }
        }
    }

    // Open the project file.
    QFile loadFile(filename);;

    bool f = loadFile.open(QIODevice::ReadOnly);
    if (f == false) {
        QMessageBox::critical(p, "Error", QString("Can't open the Seer project file '%1'.").arg(filename));
        return;
    }

    // Populate the JSON document from the project file.
    QJsonDocument jsonDoc = QJsonDocument::fromJson(loadFile.readAll());

    f = loadJsonDoc(jsonDoc, filename);
    if (f == false) {
        return;
    }

    if (notify) {
        QMessageBox::information(p, "Success", QString("Loaded the Seer project file '%1'.").arg(filename));
    }
}

QJsonDocument SeerDebugDialog::makeJsonDoc() const {

    // Build the JSON document.
    QJsonDocument jsonDoc;
    QJsonObject   rootJson;
    QJsonObject   seerProjectJson;
    QJsonArray    preConnectCommands;
    QJsonArray    postConnectCommands;

    // Save pre/post gdb commands.
    QStringList   preCommands  = preGdbCommands();
    QStringList   postCommands = postGdbCommands();

    for (const auto& i : preCommands) {
        preConnectCommands.push_back(QJsonValue(i));
    }

    for (const auto& i : postCommands) {
        postConnectCommands.push_back(QJsonValue(i));
    }

    seerProjectJson["executable"]        = QJsonValue(executableNameLineEdit->text());
    seerProjectJson["symbolfile"]        = QJsonValue(executableSymbolNameLineEdit->text());
    seerProjectJson["workingdirectory"]  = QJsonValue(executableWorkingDirectoryLineEdit->text());
    seerProjectJson["pregdbcommands"]    = preConnectCommands;
    seerProjectJson["postgdbcommands"]   = postConnectCommands;

    // Save RUN project.
    if (launchMode() == "run") {

        QJsonObject modeJson;

        modeJson["arguments"]             = runProgramArgumentsLineEdit->text();
        modeJson["breakpointsfile"]       = loadBreakpointsFilenameLineEdit->text();
        modeJson["nobreak"]               = noBreakpointRadioButton->isChecked();
        modeJson["breakinmain"]           = breakpointInMainRadioButton->isChecked();
        modeJson["breakinfunction"]       = breakpointInFunctionRadioButton->isChecked();
        modeJson["breakinfunctionname"]   = breakpointInFunctionLineEdit->text();
        modeJson["showassemblytabmode"]   = showAssemblyTabComboBox->currentText();
        modeJson["nonstopmode"]           = nonStopModeCheckBox->isChecked();
        modeJson["randomizestartaddress"] = randomizeStartAddressCheckBox->isChecked();

        seerProjectJson["runmode"]        = modeJson;
    }

    // Save START project.
    if (launchMode() == "start") {

        QJsonObject modeJson;

        modeJson["arguments"]             = runProgramArgumentsLineEdit->text();
        modeJson["breakpointsfile"]       = loadBreakpointsFilenameLineEdit->text();
        modeJson["nobreak"]               = noBreakpointRadioButton->isChecked();
        modeJson["breakinmain"]           = breakpointInMainRadioButton->isChecked();
        modeJson["breakinfunction"]       = breakpointInFunctionRadioButton->isChecked();
        modeJson["breakinfunctionname"]   = breakpointInFunctionLineEdit->text();
        modeJson["showassemblytabmode"]   = showAssemblyTabComboBox->currentText();
        modeJson["nonstopmode"]           = nonStopModeCheckBox->isChecked();
        modeJson["randomizestartaddress"] = randomizeStartAddressCheckBox->isChecked();

        seerProjectJson["startmode"]      = modeJson;
    }

    // Save ATTACH project.
    if (launchMode() == "attach") {

        QJsonObject modeJson;

        modeJson["pid"]               = attachProgramPidLineEdit->text();

        seerProjectJson["attachmode"] = modeJson;
    }

    // Save CONNECT project.
    if (launchMode() == "connect") {

        QJsonObject modeJson;

        modeJson["gdbserver"]          = connectProgramHostPortLineEdit->text();
        modeJson["targettype"]         = connectRemoteTargetTypeCombo->currentText();
        modeJson["gdbserverdebug"]     = connectGdbserverDebugCheckBox->isChecked();

        seerProjectJson["connectmode"] = modeJson;
    }

    // Save RR project.
    if (launchMode() == "rr") {

        QJsonObject modeJson;

        modeJson["tracedirectory"]     = rrTraceDirectoryLineEdit->text();
        modeJson["breakpointsfile"]    = rrLoadBreakpointsFilenameLineEdit->text();

        seerProjectJson["rrmode"]      = modeJson;
    }

    // Save COREFILE project.
    if (launchMode() == "corefile") {

        QJsonObject modeJson;

        modeJson["corefile"]            = loadCoreFilenameLineEdit->text();

        seerProjectJson["corefilemode"] = modeJson;
    }

    rootJson["seerproject"] = seerProjectJson;

    jsonDoc.setObject(rootJson);

    return jsonDoc;
}

bool SeerDebugDialog::loadJsonDoc (const QJsonDocument& jsonDoc, const QString& filename) {

    // Look for the mainwindow to place possible QMessageBox on.
    QWidget* p = this;

    if (isHidden() == true) {

        foreach (QWidget* w, qApp->topLevelWidgets()) {
            if (QMainWindow* mainWin = qobject_cast<QMainWindow*>(w)) {
                p = mainWin;
                break;
            }
        }
    }

    QJsonObject   rootJson;
    QJsonObject   seerProjectJson;
    QJsonObject   runModeJson;
    QJsonObject   startModeJson;
    QJsonObject   attachModeJson;
    QJsonObject   connectModeJson;
    QJsonObject   rrModeJson;
    QJsonObject   corefileModeJson;
    QJsonArray    preConnectCommands;
    QJsonArray    postConnectCommands;

    if (jsonDoc.isObject() == false) {
        QMessageBox::critical(p, "Error", QString("'%1' is not a Seer project file (bad Json format).").arg(filename));
        return false;
    }

    rootJson            = jsonDoc.object();
    seerProjectJson     = rootJson.value("seerproject").toObject();
    runModeJson         = seerProjectJson.value("runmode").toObject();
    startModeJson       = seerProjectJson.value("startmode").toObject();
    attachModeJson      = seerProjectJson.value("attachmode").toObject();
    connectModeJson     = seerProjectJson.value("connectmode").toObject();
    rrModeJson          = seerProjectJson.value("rrmode").toObject();
    corefileModeJson    = seerProjectJson.value("corefilemode").toObject();
    preConnectCommands  = seerProjectJson.value("pregdbcommands").toArray();
    postConnectCommands = seerProjectJson.value("postgdbcommands").toArray();

    if (seerProjectJson.isEmpty() == true) {
        QMessageBox::critical(p, "Error", QString("'%1' is not a Seer project file (missing 'seerproject' section).").arg(filename));
        return false;
    }

    // Load executable/symbol/working directory.
    executableNameLineEdit->setText(seerProjectJson["executable"].toString());
    executableSymbolNameLineEdit->setText(seerProjectJson["symbolfile"].toString());
    executableWorkingDirectoryLineEdit->setText(seerProjectJson["workingdirectory"].toString());

    // Load pre/post gdb commands. Good for all modes.
    QStringList preCommands;
    QStringList postCommands;

    for (const auto& i : preConnectCommands) {
        preCommands.push_back(i.toString());
    }

    for (const auto& i : postConnectCommands) {
        postCommands.push_back(i.toString());
    }

    setPreGdbCommands(preCommands);
    setPostGdbCommands(postCommands);

    // Load RUN/START project.
    if (runModeJson.isEmpty() == false || startModeJson.isEmpty() == false) {

        if (runModeJson.isEmpty() == false) {

            runProgramArgumentsLineEdit->setText(runModeJson["arguments"].toString());
            loadBreakpointsFilenameLineEdit->setText(runModeJson["breakpointsfile"].toString());
            rrLoadBreakpointsFilenameLineEdit->setText(startModeJson["breakpointsfile"].toString());

            if (runModeJson["nobreak"].toBool()) {
                noBreakpointRadioButton->setChecked(true);
            }

            if (runModeJson["breakinmain"].toBool()) {
                breakpointInMainRadioButton->setChecked(true);
            }

            if (runModeJson["breakinfunction"].toBool()) {
                breakpointInFunctionRadioButton->setChecked(true);
            }

            breakpointInFunctionLineEdit->setText(runModeJson["breakinfunctionname"].toString());
            showAssemblyTabComboBox->setCurrentText(runModeJson["showassemblytabmode"].toString());
            nonStopModeCheckBox->setChecked(runModeJson["nonstopmode"].toBool());
            randomizeStartAddressCheckBox->setChecked(runModeJson["randomizestartaddress"].toBool());

            setLaunchMode("run");

        }else if (startModeJson.isEmpty() == false) {

            runProgramArgumentsLineEdit->setText(startModeJson["arguments"].toString());
            loadBreakpointsFilenameLineEdit->setText(startModeJson["breakpointsfile"].toString());
            rrLoadBreakpointsFilenameLineEdit->setText(startModeJson["breakpointsfile"].toString());

            if (startModeJson["nobreak"].toBool()) {
                noBreakpointRadioButton->setChecked(true);
            }

            if (startModeJson["breakinmain"].toBool()) {
                breakpointInMainRadioButton->setChecked(true);
            }

            if (startModeJson["breakinfunction"].toBool()) {
                breakpointInFunctionRadioButton->setChecked(true);
            }

            breakpointInFunctionLineEdit->setText(startModeJson["breakinfunctionname"].toString());
            showAssemblyTabComboBox->setCurrentText(startModeJson["showassemblytabmode"].toString());
            nonStopModeCheckBox->setChecked(startModeJson["nonstopmode"].toBool());
            randomizeStartAddressCheckBox->setChecked(startModeJson["randomizestartaddress"].toBool());

            setLaunchMode("start");

        }else{
            setLaunchMode("");
        }
    }

    // Load ATTACH project.
    if (attachModeJson.isEmpty() == false) {

        attachProgramPidLineEdit->setText(attachModeJson["pid"].toString());

        setLaunchMode("attach");
    }

    // Load CONNECT project.
    if (connectModeJson.isEmpty() == false) {

        connectProgramHostPortLineEdit->setText(connectModeJson["gdbserver"].toString());

        if (connectModeJson.contains("targettype")) {
            connectRemoteTargetTypeCombo->setCurrentText(connectModeJson["targettype"].toString());
        }

        if (connectModeJson.contains("gdbserverdebug")) {
            connectGdbserverDebugCheckBox->setChecked(connectModeJson["gdbserverdebug"].toBool());
        }

        setLaunchMode("connect");
    }

    // Load RR project.
    if (rrModeJson.isEmpty() == false) {

        rrTraceDirectoryLineEdit->setText(rrModeJson["tracedirectory"].toString());
        loadBreakpointsFilenameLineEdit->setText(rrModeJson["breakpointsfile"].toString());
        rrLoadBreakpointsFilenameLineEdit->setText(rrModeJson["breakpointsfile"].toString());

        setLaunchMode("rr");
    }

    // Load COREFILE project.
    if (corefileModeJson.isEmpty() == false) {

        loadCoreFilenameLineEdit->setText(corefileModeJson["corefile"].toString());

        setLaunchMode("corefile");
    }

    return true;
}

void SeerDebugDialog::handleSaveProjectToolButton () {

    // Get the filename to save to.
    QString fname = QFileDialog::getSaveFileName(this, "Save to a project file.", "project.seer", "Projects (*.seer);;All files (*.*)", nullptr, QFileDialog::DontUseNativeDialog);

    if (fname == "") {
        return;
    }

    // Make the json document for the debug dialog settings.
    QJsonDocument jsonDoc = makeJsonDoc();

    // Write the JSON document to the project file.
    QFile saveFile(fname);

    if (saveFile.open(QIODevice::WriteOnly) == false) {
        QMessageBox::critical(this, "Error", QString("Can't create the Seer project file '%1'.").arg(fname));
        return;
    }

    saveFile.write(jsonDoc.toJson());

    QMessageBox::information(this, "Success", QString("Created the Seer project file '%1'.").arg(fname));
}

void SeerDebugDialog::handleRunModeChanged (int id) {

    //
    // Disable all important widgets.
    //

    executableNameLineEdit->setEnabled(false);
    executableNameToolButton->setEnabled(false);
    executableSymbolNameLineEdit->setEnabled(false);
    executableSymbolNameToolButton->setEnabled(false);
    executableWorkingDirectoryLineEdit->setEnabled(false);
    executableWorkingDirectoryToolButton->setEnabled(false);

    preCommandsPlainTextEdit->setPlaceholderText("");
    postCommandsPlainTextEdit->setPlaceholderText("");

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
        preCommandsPlainTextEdit->setPlaceholderText("gdb commands before \"run\"");
        postCommandsPlainTextEdit->setPlaceholderText("gdb commands after \"run\"");
    }

    // ID == 1   ATTACH
    if (id == 1) {
        executableNameLineEdit->setEnabled(true);
        executableNameToolButton->setEnabled(true);
        executableSymbolNameLineEdit->setEnabled(true);
        executableSymbolNameToolButton->setEnabled(true);
        executableWorkingDirectoryLineEdit->setEnabled(true);
        executableWorkingDirectoryToolButton->setEnabled(true);
        preCommandsPlainTextEdit->setPlaceholderText("gdb commands before \"attach\"");
        postCommandsPlainTextEdit->setPlaceholderText("gdb commands after \"attach\"");
    }

    // ID == 2   CONNECT
    if (id == 2) {
        executableNameLineEdit->setEnabled(true);
        executableNameToolButton->setEnabled(true);
        executableSymbolNameLineEdit->setEnabled(true);
        executableSymbolNameToolButton->setEnabled(true);
        executableWorkingDirectoryLineEdit->setEnabled(true);
        executableWorkingDirectoryToolButton->setEnabled(true);
        preCommandsPlainTextEdit->setPlaceholderText("gdb commands before \"connect\"");
        postCommandsPlainTextEdit->setPlaceholderText("gdb commands after \"connect\"");
    }

    // ID == 3   RR
    if (id == 3) {
        executableSymbolNameLineEdit->setEnabled(true);
        executableSymbolNameToolButton->setEnabled(true);
        preCommandsPlainTextEdit->setPlaceholderText("gdb commands before \"RR trace-directory load\"");
        postCommandsPlainTextEdit->setPlaceholderText("gdb commands after \"RR trace-directory load\"");

        // Set default if we can. Otherwise, leave it blank.
        //
        if (rrTraceDirectoryLineEdit->text() == "") {
            QStringList searchPaths = {
                "$_RR_TRACE_DIR/latest-trace",
                "$XDG_DATA_HOME/rr/latest-trace",
                "$HOME/.local/share/rr/latest-trace"
            };

            QString defaultPath = "";
            bool    f;

            for (const auto& path : searchPaths) {
               defaultPath = Seer::expandEnv(path, &f);
               if (f == false) {
                   continue;
               }
               if (QFile::exists(defaultPath)) {
                   setRRTraceDirectory(defaultPath);
                   break;
               }
            }
        }
    }

    // ID == 4   COREFILE
    if (id == 4) {
        executableNameLineEdit->setEnabled(true);
        executableNameToolButton->setEnabled(true);
        executableSymbolNameLineEdit->setEnabled(true);
        executableSymbolNameToolButton->setEnabled(true);
        preCommandsPlainTextEdit->setPlaceholderText("gdb commands before loading \"corefile\"");
        postCommandsPlainTextEdit->setPlaceholderText("gdb commands after loading \"corefile\"");
    }
}

void SeerDebugDialog::handleLaunchButtonClicked () {

    QJsonDocument document = makeJsonDoc();

    writeDefaultProjectSettings(document);
}

void SeerDebugDialog::handleResetButtonClicked (QAbstractButton* button) {

    // Was the Reset button clicked?
    QAbstractButton* resetButton = buttonBox->button(QDialogButtonBox::Reset);

    if (button != resetButton) {
        return;
    }

    // Reset all parameters.
    reset();
}

void SeerDebugDialog::handleHelpModeToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog(this);
    help->loadFile(":/seer/resources/help/DebugModes.md");
    help->show();
    help->raise();
}

void SeerDebugDialog::handleHelpRunToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog(this);
    help->loadFile(":/seer/resources/help/RunDebugMode.md");
    help->show();
    help->raise();
}

void SeerDebugDialog::handleHelpAttachToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog(this);
    help->loadFile(":/seer/resources/help/AttachDebugMode.md");
    help->show();
    help->raise();
}

void SeerDebugDialog::handleHelpConnectToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog(this);
    help->loadFile(":/seer/resources/help/ConnectDebugMode.md");
    help->show();
    help->raise();
}

void SeerDebugDialog::handleHelpRRToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog(this);
    help->loadFile(":/seer/resources/help/RRDebugMode.md");
    help->show();
    help->raise();
}

void SeerDebugDialog::handleHelpCorefileToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog(this);
    help->loadFile(":/seer/resources/help/CorefileDebugMode.md");
    help->show();
    help->raise();
}

void SeerDebugDialog::writeSettings() {

    QSettings settings;

    settings.beginGroup("debugdialog"); {
        settings.setValue("size", size());
    } settings.endGroup();
}

void SeerDebugDialog::readSettings() {

    QSettings settings;

    settings.beginGroup("debugdialog"); {
        resize(settings.value("size", QSize(800, 600)).toSize());
    } settings.endGroup();
}

void SeerDebugDialog::writeDefaultProjectSettings (const QJsonDocument& document) {

    QSettings settings;

    settings.beginGroup("debugdialog"); {
        settings.setValue("defaultproject", document.toJson(QJsonDocument::Compact));
    } settings.endGroup();
}

void SeerDebugDialog::loadDefaultProjectSettings () {

    QSettings settings;

    settings.beginGroup("debugdialog"); {

        QVariant variantData = settings.value("defaultproject");

        if (variantData.isValid()) {
            QJsonDocument jsonDoc = QJsonDocument::fromJson(variantData.toByteArray());
            loadJsonDoc(jsonDoc, "defaultproject");
        }

    } settings.endGroup();
}

void SeerDebugDialog::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QWidget::resizeEvent(event);
}

