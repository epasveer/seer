#include "SeerOpenOCDDebugOnInit.h"
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QToolButton>
#include <QLineEdit>
#include <QComboBox>
#include <QDialogButtonBox>
#include <QSerialPortInfo>
#include <QtCore/QSettings>

SeerOpenOCDDebugOnInit::SeerOpenOCDDebugOnInit (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Connect signal and slots
    QObject::connect(kernelModuleSymbolButton,          &QToolButton::clicked,          this,           &SeerOpenOCDDebugOnInit::handleKernelModuleSymbolButton);
    QObject::connect(kernelModuleSourceCodeButton,      &QToolButton::clicked,          this,           &SeerOpenOCDDebugOnInit::handleKernelModuleSourceCodeButton);
    QObject::connect(moduleNameLineEdit,                &QLineEdit::textChanged,        [&](const QString &text){   commandLineEdit->setText("insmod " + text + ".ko");   });
    QObject::connect(buttonBox,                         &QDialogButtonBox::accepted,    this,           &SeerOpenOCDDebugOnInit::onAccepted);
    QObject::connect(serialComboBox,                    &QComboBox::currentTextChanged, this,           &SeerOpenOCDDebugOnInit::handleComboBoxTextChanged);
    
    _moduleName                     = "";
    _commandToTerm                  = "insmod ";;
    _kernelModuleSymbolPath         = "";
    _kernelModuleSourceCodePath     = "";
    _serialPortPath                 = "";

    const auto ports = QSerialPortInfo::availablePorts();

    for (const QSerialPortInfo &port : ports)
    {
        if (port.portName().contains("ttyUSB"))
        {
            serialComboBox->addItem(QString(port.systemLocation()));
        }
    }

    for (const QSerialPortInfo &port : ports)
    {
        if (!port.portName().contains("ttyUSB"))
            serialComboBox->addItem(QString(port.systemLocation()));
    }

    readSettings();
}

SeerOpenOCDDebugOnInit::~SeerOpenOCDDebugOnInit () {
}

/***********************************************************************************************************************
 * Getter and setter
 **********************************************************************************************************************/
void SeerOpenOCDDebugOnInit::setModuleName (const QString& name)
{
    _moduleName = name;
}

const QString SeerOpenOCDDebugOnInit::moduleName ()
{
    return _moduleName;
}

void SeerOpenOCDDebugOnInit::setCommandToTerm (const QString& command)
{
    _commandToTerm = command;
}

const QString SeerOpenOCDDebugOnInit::commandToTerm ()
{
    return _commandToTerm;
}

void SeerOpenOCDDebugOnInit::setkernelModuleSymbolPath (const QString& path)
{
    _kernelModuleSymbolPath = path;
}

const QString SeerOpenOCDDebugOnInit::kernelModuleSymbolPath ()
{
    return _kernelModuleSymbolPath;
}

void SeerOpenOCDDebugOnInit::setKernelModuleSourceCodePath (const QString& path)
{
    _kernelModuleSourceCodePath = path;
}

const QString SeerOpenOCDDebugOnInit::kernelModuleSourceCodePath ()
{
    return _kernelModuleSourceCodePath;
}

void SeerOpenOCDDebugOnInit::setSerialPortPath (const QString& path)
{
    _serialPortPath = path;
}

const QString SeerOpenOCDDebugOnInit::serialPortPath()
{
    return _serialPortPath;
}
/***********************************************************************************************************************
 * Slot handling open file / folder
 **********************************************************************************************************************/
void SeerOpenOCDDebugOnInit::handleKernelModuleSymbolButton () {
    QString name = QFileDialog::getOpenFileName(this, "Select Kernel Module Symbol.", kernelModuleSymbolPath(), "", nullptr, QFileDialog::DontUseNativeDialog);

    if (name != "") {
        setkernelModuleSymbolPath(name);
        kernelModuleSymbolLineEdit->setText(kernelModuleSymbolPath());
    }
}

void SeerOpenOCDDebugOnInit::handleKernelModuleSourceCodeButton () {
    QString name = QFileDialog::getExistingDirectory(this, "Select kernel source code directory.", kernelModuleSourceCodePath(), QFileDialog::ShowDirsOnly|QFileDialog::DontUseNativeDialog);

    if (name != "") {
        setKernelModuleSourceCodePath(name);
        kernelModuleSourceCodeLineEdit->setText(kernelModuleSourceCodePath());
    }
}

void SeerOpenOCDDebugOnInit::handleComboBoxTextChanged()
{
    setSerialPortPath(serialComboBox->currentText());
}

void SeerOpenOCDDebugOnInit::onAccepted()
{
    // Push the current text into the history (in case focus was never lost).
    moduleNameLineEdit->execute();
    kernelModuleSymbolLineEdit->execute();
    kernelModuleSourceCodeLineEdit->execute();

    setModuleName(moduleNameLineEdit->text());
    setCommandToTerm(commandLineEdit->text());
    setkernelModuleSymbolPath(kernelModuleSymbolLineEdit->text());
    setKernelModuleSourceCodePath(kernelModuleSourceCodeLineEdit->text());

    writeSettings();
}

/***********************************************************************************************************************
 * Settings
 **********************************************************************************************************************/
void SeerOpenOCDDebugOnInit::readSettings () {

    QSettings settings;

    settings.beginGroup("openocddebuginit"); {
        moduleNameLineEdit->setHistory(settings.value("modulenamehistory").toStringList());
        kernelModuleSymbolLineEdit->setHistory(settings.value("kernelmodulesymbolhistory").toStringList());
        kernelModuleSourceCodeLineEdit->setHistory(settings.value("kernelmodulesourcecodehistory").toStringList());
    } settings.endGroup();
}

void SeerOpenOCDDebugOnInit::writeSettings () {

    QSettings settings;

    settings.beginGroup("openocddebuginit"); {
        settings.setValue("modulenamehistory", moduleNameLineEdit->history());
        settings.setValue("kernelmodulesymbolhistory", kernelModuleSymbolLineEdit->history());
        settings.setValue("kernelmodulesourcecodehistory", kernelModuleSourceCodeLineEdit->history());
    } settings.endGroup();
}
