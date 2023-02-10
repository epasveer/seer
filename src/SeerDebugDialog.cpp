#include "SeerDebugDialog.h"
#include "SeerExecutableFilterProxyModel.h"
#include "SeerDirectoryFilterProxyModel.h"
#include "SeerSlashProcDialog.h"
#include "SeerHelpPageDialog.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMessageBox>
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

    // Create editor options bar.
    QToolButton* loadSessionToolButton = new QToolButton(runModeTabWidget);
    loadSessionToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/document-open.svg"));
    loadSessionToolButton->setToolTip("Load a Seer session file.");

    QToolButton* saveSessionToolButton = new QToolButton(runModeTabWidget);
    saveSessionToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/document-save.svg"));
    saveSessionToolButton->setToolTip("Save a Seer session file.");

    QToolButton* helpModeToolButton = new QToolButton(runModeTabWidget);
    helpModeToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpModeToolButton->setToolTip("Help on the debug launch modes.");

    QHContainerWidget* hcontainer = new QHContainerWidget(this);
    hcontainer->setSpacing(3);
    hcontainer->addWidget(loadSessionToolButton);
    hcontainer->addWidget(saveSessionToolButton);
    hcontainer->addWidget(helpModeToolButton);

    runModeTabWidget->setCornerWidget(hcontainer, Qt::TopRightCorner);

    // Connect things.
    QObject::connect(executableNameToolButton,             &QToolButton::clicked,               this, &SeerDebugDialog::handleExecutableNameToolButton);
    QObject::connect(executableSymbolNameToolButton,       &QToolButton::clicked,               this, &SeerDebugDialog::handleExecutableSymbolNameToolButton);
    QObject::connect(executableWorkingDirectoryToolButton, &QToolButton::clicked,               this, &SeerDebugDialog::handleExecutableWorkingDirectoryToolButton);
    QObject::connect(loadBreakpointsFilenameToolButton,    &QToolButton::clicked,               this, &SeerDebugDialog::handleLoadBreakpointsFilenameToolButton);
    QObject::connect(loadCoreFilenameToolButton,           &QToolButton::clicked,               this, &SeerDebugDialog::handleLoadCoreFilenameToolButton);
    QObject::connect(breakpointInFunctionLineEdit,         &QLineEdit::textChanged,             this, &SeerDebugDialog::handleBreakpointInFunctionLineEdit);
    QObject::connect(attachProgramPidToolButton,           &QToolButton::clicked,               this, &SeerDebugDialog::handleProgramPidToolButton);
    QObject::connect(loadSessionToolButton,                &QToolButton::clicked,               this, &SeerDebugDialog::handleLoadSessionToolButton);
    QObject::connect(saveSessionToolButton,                &QToolButton::clicked,               this, &SeerDebugDialog::handleSaveSessionToolButton);
    QObject::connect(helpModeToolButton,                   &QToolButton::clicked,               this, &SeerDebugDialog::handleHelpModeToolButtonClicked);
    QObject::connect(helpRunToolButton,                    &QToolButton::clicked,               this, &SeerDebugDialog::handleHelpRunToolButtonClicked);
    QObject::connect(helpAttachToolButton,                 &QToolButton::clicked,               this, &SeerDebugDialog::handleHelpAttachToolButtonClicked);
    QObject::connect(helpConnectToolButton,                &QToolButton::clicked,               this, &SeerDebugDialog::handleHelpConnectToolButtonClicked);
    QObject::connect(helpCorefileToolButton,               &QToolButton::clicked,               this, &SeerDebugDialog::handleHelpCorefileToolButtonClicked);

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

    QString name = QFileDialog::getOpenFileName(this, "Select a breakpoints file to load.", breakpointsFilename(), "Breakpoints (*.seer);;All files (*.*)", nullptr, QFileDialog::DontUseNativeDialog);

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

void SeerDebugDialog::handleLoadSessionToolButton () {

    // Get the filename to load from.
    QString fname = QFileDialog::getOpenFileName(this, "Load a session file.", "sesssion.seer", "Sessions (*.seer);;All files (*.*)", nullptr, QFileDialog::DontUseNativeDialog);

    if (fname == "") {
        return;
    }

    // Open the session file.
    QFile loadFile(fname);;
    loadFile.open(QIODevice::ReadOnly);

    if (loadFile.error() != 0) {
        QMessageBox::critical(this, "Error", QString("Can't open %1.").arg(fname));
        return;
    }

    // Populate the JSON document from the session file.
    QJsonDocument jsonDoc = QJsonDocument::fromJson(loadFile.readAll());
    QJsonObject   rootJson;
    QJsonObject   seerSessionJson;
    QJsonObject   runModeJson;
    QJsonObject   startModeJson;
    QJsonObject   attachModeJson;
    QJsonObject   connectModeJson;
    QJsonObject   corefileModeJson;
    QJsonArray    preConnectCommands;
    QJsonArray    postConnectCommands;

    if (jsonDoc.isObject() == false) {
        QMessageBox::critical(this, "Error", QString("%1 is not a Seer session file (bad Json format).").arg(fname));
        return;
    }

    rootJson            = jsonDoc.object();
    seerSessionJson     = rootJson.value("seersession").toObject();
    runModeJson         = seerSessionJson.value("runmode").toObject();
    startModeJson       = seerSessionJson.value("startmode").toObject();
    attachModeJson      = seerSessionJson.value("attachmode").toObject();
    connectModeJson     = seerSessionJson.value("connectmode").toObject();
    corefileModeJson    = seerSessionJson.value("corefilemode").toObject();
    preConnectCommands  = seerSessionJson.value("pregdbcommands").toArray();
    postConnectCommands = seerSessionJson.value("postgdbcommands").toArray();

    if (seerSessionJson.isEmpty() == true) {
        QMessageBox::critical(this, "Error", QString("%1 is not a Seer session file (missing 'seersession' section).").arg(fname));
        return;
    }

    // Load executable/symbol/working directory.
    executableNameLineEdit->setText(seerSessionJson["executable"].toString());
    executableSymbolNameLineEdit->setText(seerSessionJson["symbolfile"].toString());
    executableWorkingDirectoryLineEdit->setText(seerSessionJson["workingdirectory"].toString());

    // Load pre/post gdb commands. Good for all modes.
    QStringList preCommands;
    QStringList postCommands;

    for (const auto& i : preConnectCommands) {
        preCommands.push_back(i.toString());
    }

    for (const auto& i : postConnectCommands) {
        postCommands.push_back(i.toString());
    }

    preCommandsPlainTextEdit->setPlainText(preCommands.join("\n"));
    postCommandsPlainTextEdit->setPlainText(postCommands.join("\n"));

    // Load ATTACH session.
    if (attachModeJson.isEmpty() == false) {

        QString pid = attachModeJson["pid"].toString();

        attachProgramPidLineEdit->setText(pid);

        setLaunchMode("attach");
    }

    // Load CONNECT session.
    if (connectModeJson.isEmpty() == false) {

        QString gdbServer = connectModeJson["gdbserver"].toString();

        connectProgramHostPortLineEdit->setText(gdbServer);

        setLaunchMode("connect");
    }

    // Load COREFILE session.
    if (corefileModeJson.isEmpty() == false) {

        QString corefile = corefileModeJson["corefile"].toString();

        loadCoreFilenameLineEdit->setText(corefile);

        setLaunchMode("corefile");
    }

    QMessageBox::information(this, "Success", QString("Loaded %1.").arg(fname));
}

void SeerDebugDialog::handleSaveSessionToolButton () {

    // Get the filename to save to.
    QString fname = QFileDialog::getSaveFileName(this, "Save to a session file.", "session.seer", "Sessions (*.seer);;All files (*.*)", nullptr, QFileDialog::DontUseNativeDialog);

    if (fname == "") {
        return;
    }

    // Build the JSON document.
    QJsonDocument jsonDoc;
    QJsonObject   rootJson;
    QJsonObject   seerSessionJson;
    QJsonArray    preConnectCommands;
    QJsonArray    postConnectCommands;

    // Save pre/post gdb commands.
    QStringList   preCommands  = preCommandsPlainTextEdit->toPlainText().split("\n");
    QStringList   postCommands = postCommandsPlainTextEdit->toPlainText().split("\n");

    for (const auto& i : preCommands) {
        preConnectCommands.push_back(QJsonValue(i));
    }

    for (const auto& i : postCommands) {
        postConnectCommands.push_back(QJsonValue(i));
    }

    seerSessionJson["executable"]        = QJsonValue(executableNameLineEdit->text());
    seerSessionJson["symbolfile"]        = QJsonValue(executableSymbolNameLineEdit->text());
    seerSessionJson["workingdirectory"]  = QJsonValue(executableWorkingDirectoryLineEdit->text());
    seerSessionJson["pregdbcommands"]    = preConnectCommands;
    seerSessionJson["postgdbcommands"]   = postConnectCommands;

    // Save RUN session.
    if (launchMode() == "run") {

        QJsonObject modeJson;

        modeJson["arguments"]             = runProgramArgumentsLineEdit->text();
        modeJson["breakpointsfile"]       = loadBreakpointsFilenameLineEdit->text();
        modeJson["nobreak"]               = noBreakpointRadioButton->isChecked();
        modeJson["breakinmain"]           = breakpointInMainRadioButton->isChecked();
        modeJson["breakinfunction"]       = breakpointInFunctionRadioButton->isChecked();
        modeJson["breakinfunctionname"]   = breakpointInFunctionLineEdit->text();
        modeJson["showassemblytab"]       = showAsseblyTabCheckBox->isChecked();
        modeJson["nonstopmode"]           = nonStopModeCheckBox->isChecked();
        modeJson["randomizestartaddress"] = randomizeStartAddressCheckBox->isChecked();
        seerSessionJson["runmode"]        = modeJson;
    }

    // Save START session.
    if (launchMode() == "start") {

        QJsonObject modeJson;

        modeJson["arguments"]             = runProgramArgumentsLineEdit->text();
        modeJson["breakpointsfile"]       = loadBreakpointsFilenameLineEdit->text();
        modeJson["nobreak"]               = noBreakpointRadioButton->isChecked();
        modeJson["breakinmain"]           = breakpointInMainRadioButton->isChecked();
        modeJson["breakinfunction"]       = breakpointInFunctionRadioButton->isChecked();
        modeJson["breakinfunctionname"]   = breakpointInFunctionLineEdit->text();
        modeJson["showassemblytab"]       = showAsseblyTabCheckBox->isChecked();
        modeJson["nonstopmode"]           = nonStopModeCheckBox->isChecked();
        modeJson["randomizestartaddress"] = randomizeStartAddressCheckBox->isChecked();
        seerSessionJson["startmode"]      = modeJson;
    }

    // Save ATTACH session.
    if (launchMode() == "attach") {

        QJsonObject modeJson;

        modeJson["pid"]               = attachProgramPidLineEdit->text();
        seerSessionJson["attachmode"] = modeJson;
    }

    // Save CONNECT session.
    if (launchMode() == "connect") {

        QJsonObject modeJson;

        modeJson["gdbserver"]          = connectProgramHostPortLineEdit->text();
        seerSessionJson["connectmode"] = modeJson;
    }

    // Save COREFILE session.
    if (launchMode() == "corefile") {

        QJsonObject modeJson;

        modeJson["corefile"]            = loadCoreFilenameLineEdit->text();
        seerSessionJson["corefilemode"] = modeJson;
    }

    rootJson["seersession"] = seerSessionJson;

    jsonDoc.setObject(rootJson);

    // Write the JSON document to the session file.
    QFile saveFile(fname);

    if (saveFile.open(QIODevice::WriteOnly) == false) {
        QMessageBox::critical(this, "Error", QString("Can't create %1.").arg(fname));
        return;
    }

    saveFile.write(jsonDoc.toJson());

    QMessageBox::information(this, "Success", QString("Created %1.").arg(fname));
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
        preCommandsPlainTextEdit->setPlaceholderText("gdb commands before \"attach\"");
        postCommandsPlainTextEdit->setPlaceholderText("gdb commands after \"attach\"");
    }

    // ID == 2   CONNECT
    if (id == 2) {
        executableSymbolNameLineEdit->setEnabled(true);
        executableSymbolNameToolButton->setEnabled(true);
        preCommandsPlainTextEdit->setPlaceholderText("gdb commands before \"connect\"");
        postCommandsPlainTextEdit->setPlaceholderText("gdb commands after \"connect\"");
    }

    // ID == 3   COREFILE
    if (id == 3) {
        executableNameLineEdit->setEnabled(true);
        executableNameToolButton->setEnabled(true);
        executableSymbolNameLineEdit->setEnabled(true);
        executableSymbolNameToolButton->setEnabled(true);
        preCommandsPlainTextEdit->setPlaceholderText("gdb commands before loading \"corefile\"");
        postCommandsPlainTextEdit->setPlaceholderText("gdb commands after loading  \"corefile\"");
    }
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

