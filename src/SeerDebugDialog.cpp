#include "SeerDebugDialog.h"
#include "SeerExecutableFilterProxyModel.h"
#include "SeerDirectoryFilterProxyModel.h"
#include <QtWidgets/QFileDialog>
#include <QtCore/QDir>
#include <QtCore/QDebug>

SeerDebugDialog::SeerDebugDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    attachProgramPidLineEdit->setMaximumWidth(fontMetrics().horizontalAdvance("888888888888888"));
    connectProgramHostPortLineEdit->setMaximumWidth(fontMetrics().horizontalAdvance("XXXXXXXXXXXXXXX"));

    _runModeButtonGroup = new QButtonGroup(this); // ID's 1 thru 4.
    _runModeButtonGroup->addButton(runProgramRadioButton,     1); // "run" or "start". See breakInMain().
    _runModeButtonGroup->addButton(attachProgramRadioButton,  2); // "attach"
    _runModeButtonGroup->addButton(connectProgramRadioButton, 3); // "connect"
    _runModeButtonGroup->addButton(loadCoreRadioButton,       4); // "corefile"

    // Setup the widgets
    setExecutableName("");
    setExecutableArguments("");
    setExecutableWorkingDirectory(QDir::currentPath());
    setBreakInMain(true);
    setAttachPid(0);
    setConnectHostPort("");
    setCoreFilename("");

    // Connect things.
    QObject::connect(executableNameToolButton,             &QToolButton::clicked,                              this, &SeerDebugDialog::handleExecutableNameToolButton);
    QObject::connect(executableWorkingDirectoryToolButton, &QToolButton::clicked,                              this, &SeerDebugDialog::handleExecutableWorkingDirectoryToolButton);
    QObject::connect(loadCoreFilenameToolButton,           &QToolButton::clicked,                              this, &SeerDebugDialog::handleLoadCoreFilenameToolButton);
    QObject::connect(_runModeButtonGroup,                  QOverload<int>::of(&QButtonGroup::buttonClicked),   this, &SeerDebugDialog::handleRunModeChanged);

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

void SeerDebugDialog::setBreakInMain (bool flag) {
    runProgramBreakInMainCheckBox->setChecked(flag);
}

bool SeerDebugDialog::breakInMain () const {
    return runProgramBreakInMainCheckBox->isChecked();
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

void SeerDebugDialog::setConnectDownloadExecutable (bool flag) {
    connectProgramDownloadCheckBox->setChecked(flag);
}

bool SeerDebugDialog::connectDownloadExecutable () const {
    return connectProgramDownloadCheckBox->isChecked();
}

void SeerDebugDialog::setLaunchMode (const QString& mode) {

    if (mode == "start") {
        runProgramRadioButton->click();

        setBreakInMain(true);

    }else if (mode == "run") {
        runProgramRadioButton->click();

        setBreakInMain(false);

    }else if (mode == "attach") {
        attachProgramRadioButton->click();

    }else if (mode == "connect") {
        connectProgramRadioButton->click();

    }else if (mode == "corefile") {
        loadCoreRadioButton->click();

    }else if (mode == "") {
        runProgramRadioButton->click();

        setBreakInMain(true);

    }else{
        qDebug() << __PRETTY_FUNCTION__ << ":" << "Unknown launch mode of:" << mode;
    }
}

QString SeerDebugDialog::launchMode () {

    if (_runModeButtonGroup->checkedId() == 1) {
        if (breakInMain()) {
            return "start";
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

    return "";
}

void SeerDebugDialog::handleExecutableNameToolButton () {

    /*
     * To filter files to show just executables means to called the long
     * method for creating a QFileDialog. It also means turning off
     * the Native Dialog. It doesn't honor file filtering that way.
     * Then it uses Qt's internal file dialog, which means to use
     * a filter proxy.

    // A proxy to select files that are executables and directories.
    // Need to use a proxy because native dialog doesn't honor filter settings.
    SeerExecutableFilterProxyModel* proxyModel = new SeerExecutableFilterProxyModel;

    // Create the file dialog.
    QFileDialog dlg(this, "Select an Executable to debug.", executableName());
    dlg.setOption(QFileDialog::DontUseNativeDialog);
    dlg.setProxyModel(proxyModel);
    dlg.setFileMode(QFileDialog::ExistingFile);

    // Execute the dialog and get the result.
    if (dlg.exec()) {

        QStringList files = dlg.selectedFiles();

        if (files.size() > 0) {
            setExecutableName(files[0]);
        }
    }

    */

    QString name = QFileDialog::getOpenFileName(this, "Select an Executable to debug.", executableName());

    if (name != "") {
        setExecutableName(name);
    }
}

void SeerDebugDialog::handleExecutableWorkingDirectoryToolButton () {

    /*
     * If handleExecutableNamePushButton uses the internal Qt file dialog, then
     * handleExecutableWorkingDirectoryPushButton should too.

    // A proxy to select files that are executables and directories.
    // Need to use a proxy because native dialog doesn't honor filter settings.
    SeerDirectoryFilterProxyModel* proxyModel = new SeerDirectoryFilterProxyModel;

    // Create the file dialog.
    QFileDialog dlg(this, "Select a Working Directory to run in.", workingDirectory());
    dlg.setOption(QFileDialog::DontUseNativeDialog);
    dlg.setProxyModel(proxyModel);
    dlg.setFileMode(QFileDialog::Directory);

    // Execute the dialog and get the result.
    if (dlg.exec()) {

        QStringList files = dlg.selectedFiles();

        if (files.size() > 0) {
            setExecutableWorkingDirectory(files[0]);
        }
    }

    */

    QString name = QFileDialog::getExistingDirectory(this, "Select a Working Directory to run in.", executableWorkingDirectory());

    if (name != "") {
        setExecutableWorkingDirectory(name);
    }
}

void SeerDebugDialog::handleLoadCoreFilenameToolButton () {

    QString name = QFileDialog::getOpenFileName(this, "Select a core file to debug.", coreFilename(), "Core Files (core core.*)");

    if (name != "") {
        setCoreFilename(name);
    }
}

void SeerDebugDialog::handleRunModeChanged (int id) {

    //
    // Disable all imprortant widgets.
    //

    // ID == 1
    runProgramArgumentsLineEdit->setEnabled(false);
    runProgramBreakInMainCheckBox->setEnabled(false);

    // ID == 2
    attachProgramPidLineEdit->setEnabled(false);

    // ID == 3
    connectProgramHostPortLineEdit->setEnabled(false);
    connectProgramDownloadCheckBox->setEnabled(false);

    // ID == 4
    loadCoreFilenameLineEdit->setEnabled(false);

    //
    // Enable the newly selected one.
    //

    // ID == 1
    if (id == 1) {
        runProgramArgumentsLineEdit->setEnabled(true);
        runProgramBreakInMainCheckBox->setEnabled(true);
    }

    // ID == 2
    if (id == 2) {
        attachProgramPidLineEdit->setEnabled(true);
    }

    // ID == 3
    if (id == 3) {
        connectProgramHostPortLineEdit->setEnabled(true);
        connectProgramDownloadCheckBox->setEnabled(true);
    }

    // ID == 4
    if (id == 4) {
        loadCoreFilenameLineEdit->setEnabled(true);
    }

}

