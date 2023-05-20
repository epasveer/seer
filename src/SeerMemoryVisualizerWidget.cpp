#include "SeerMemoryVisualizerWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtGui/QIntValidator>
#include <QtGui/QIcon>
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>
#include <QtCore/QRegularExpression>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerMemoryVisualizerWidget::SeerMemoryVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _variableId = Seer::createID(); // Create two id's for queries.
    _memoryId   = Seer::createID();
    _asmId      = Seer::createID();

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/seergdb_64x64.png"));
    setWindowTitle("Seer Memory Visualizer");
    setAttribute(Qt::WA_DeleteOnClose);

    memoryLengthLineEdit->setValidator(new QRegularExpressionValidator(QRegularExpression("\\s*([1-9]\\d*\\s*)+"), this));
    columnCountSpinBox->setValue(memoryHexEditor->bytesPerLine());

    if (memoryHexEditor->memoryMode() == SeerHexWidget::HexMemoryMode) {
        memoryDisplayFormatComboBox->setCurrentIndex(0);
    }else if (memoryHexEditor->memoryMode() == SeerHexWidget::OctalMemoryMode) {
        memoryDisplayFormatComboBox->setCurrentIndex(1);
    }else if (memoryHexEditor->memoryMode() == SeerHexWidget::BinaryMemoryMode) {
        memoryDisplayFormatComboBox->setCurrentIndex(2);
    }else if (memoryHexEditor->memoryMode() == SeerHexWidget::DecimalMemoryMode) {
        memoryDisplayFormatComboBox->setCurrentIndex(3);
    }else{
        memoryDisplayFormatComboBox->setCurrentIndex(0);
    }

    if (memoryHexEditor->charMode() == SeerHexWidget::AsciiCharMode) {
        charDisplayFormatComboBox->setCurrentIndex(0);
    }else if (memoryHexEditor->charMode() == SeerHexWidget::EbcdicCharMode) {
        charDisplayFormatComboBox->setCurrentIndex(1);
    }else{
        charDisplayFormatComboBox->setCurrentIndex(0);
    }

    // Connect things.
    QObject::connect(refreshToolButton,             &QToolButton::clicked,                                     this,  &SeerMemoryVisualizerWidget::handleRefreshButton);
    QObject::connect(helpToolButton,                &QToolButton::clicked,                                     this,  &SeerMemoryVisualizerWidget::handleHelpButton);
    QObject::connect(memoryLengthLineEdit,          &QLineEdit::returnPressed,                                 this,  &SeerMemoryVisualizerWidget::handleRefreshButton);
    QObject::connect(variableNameLineEdit,          &QLineEdit::returnPressed,                                 this,  &SeerMemoryVisualizerWidget::handleVariableNameLineEdit);
    QObject::connect(memoryDisplayFormatComboBox,   QOverload<int>::of(&QComboBox::currentIndexChanged),       this,  &SeerMemoryVisualizerWidget::handleMemoryDisplayFormatComboBox);
    QObject::connect(charDisplayFormatComboBox,     QOverload<int>::of(&QComboBox::currentIndexChanged),       this,  &SeerMemoryVisualizerWidget::handleCharDisplayFormatComboBox);
    QObject::connect(columnCountSpinBox,            QOverload<int>::of(&QSpinBox::valueChanged),               this,  &SeerMemoryVisualizerWidget::handleColumnCountSpinBox);
    QObject::connect(printToolButton,               &QToolButton::clicked,                                     this,  &SeerMemoryVisualizerWidget::handlePrintButton);
    QObject::connect(saveToolButton,                &QToolButton::clicked,                                     this,  &SeerMemoryVisualizerWidget::handleSaveButton);

    // Restore window settings.
    readSettings();
}

SeerMemoryVisualizerWidget::~SeerMemoryVisualizerWidget () {
}

void SeerMemoryVisualizerWidget::setVariableName (const QString& name) {

    setWindowTitle("Seer Memory Visualizer - '" + name + "'");

    variableNameLineEdit->setText(name);
    setVariableAddress("");

    if (variableNameLineEdit->text() == "") {
        return;
    }

    // Clear old contents.
    QByteArray array;

    memoryHexEditor->setData(new SeerHexWidget::DataStorageArray(array));
    memoryAsmEditor->setData("");

    // Send signal to get variable address.
    emit evaluateVariableExpression(_variableId, variableNameLineEdit->text());
}

QString SeerMemoryVisualizerWidget::variableName () const {
    return variableNameLineEdit->text();
}

void SeerMemoryVisualizerWidget::setVariableAddress (const QString& address) {

    unsigned long offset  = 0;
    bool          ok      = false;
    bool          refresh = false;

    if (address == "") {

        variableAddressLineEdit->setText("");
        offset = 0;

    }else{

        // Test for base10
        if (ok == false) {
            offset = address.toULong(&ok, 10);
            if (ok) {
                variableAddressLineEdit->setText(QString("0x%1").arg(offset, 0, 16, QLatin1Char( '0' )));
                refresh = true;
            }
        }

        // Test for base16
        if (ok == false) {
            offset = address.toULong(&ok, 16);
            if (ok) {
                variableAddressLineEdit->setText(QString("0x%1").arg(offset, 0, 16, QLatin1Char( '0' )));
                refresh = true;
            }
        }

        if (ok == false) {
            variableAddressLineEdit->setText("not an address");
            offset = 0;
        }
    }

    memoryHexEditor->setAddressOffset(offset);

    // Show results immediately.
    if (refresh) {
        handleRefreshButton();
    }
}

QString SeerMemoryVisualizerWidget::variableAddress () const {

    return variableAddressLineEdit->text();
}

void SeerMemoryVisualizerWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    //qDebug() << text;

    if (text.contains(QRegularExpression("^([0-9]+)\\^done,value="))) {

        // 11^done,value="1"
        // 11^done,value="0x7fffffffd538"

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _variableId) {

            QString value_text = Seer::parseFirst(text, "value=", '"', '"', false);
            QString address    = "";

            // Look for an address in the value.
            if (address == "") {
                QRegularExpression      re("0[xX][0-9a-fA-F]+");
                QRegularExpressionMatch match = re.match(value_text);

                if (match.hasMatch()) {
                    address = match.captured();
                }
            }

            // Look for a number in the value.
            if (address == "") {
                QRegularExpression      re("[0-9]+");
                QRegularExpressionMatch match = re.match(value_text);

                if (match.hasMatch()) {
                    address = match.captured();
                }
            }

            // Set the variable address.
            setVariableAddress(address);
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,memory="))) {

        // 3^done,memory=[{begin="0x0000000000613e70",offset="0x0000000000000000",end="0x0000000000613e71",contents="00"}]
        // 4^done,memory=[{begin="0x0000000000613e70",offset="0x0000000000000000",end="0x0000000000613ed4",contents="000000000000000000000000"}]

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _memoryId) {

            //qDebug() << text;

            QString memory_text = Seer::parseFirst(text, "memory=", '[', ']', false);

            QStringList range_list = Seer::parse(memory_text, "", '{', '}', false);

            // Loop through the memory ranges.
            for ( const auto& range_text : range_list  ) {

                QString contents_text = Seer::parseFirst(range_text, "contents=", '"', '"', false);

                // Convert hex string to byte array.
                QByteArray array;

                for (int i = 0; i<contents_text.size(); i += 2) {
                    QString num = contents_text.mid(i, 2);
                    bool ok = false;
                    array.push_back(num.toInt(&ok, 16));
                    Q_ASSERT(ok);
                }

                // Give the byte array to the hex widget.
                memoryHexEditor->setData(new SeerHexWidget::DataStorageArray(array));

                break; // Take just the first range for now.
            }
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,asm_insns="))) {

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _asmId) {

            memoryAsmEditor->setData(text);
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^error,msg="))) {

        // 12^error,msg="No symbol \"return\" in current context."
        // 13^error,msg="No symbol \"cout\" in current context."
        // 3^error,msg="Unable to read memory."

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _variableId) {
            variableAddressLineEdit->setText( Seer::filterEscapes(Seer::parseFirst(text, "msg=", '"', '"', false)) );
        }

        if (id_text.toInt() == _memoryId) {
            // Display the error message.
            QString msg_text = Seer::parseFirst(text, "msg=", false);

            if (msg_text != "") {
                QMessageBox::warning(this, "Error.", Seer::filterEscapes(msg_text));
            }
        }

    // At a stopping point, refresh.
    }else if (text.startsWith("*stopped,reason=\"")) {

        if (autoRefreshCheckBox->isChecked()) {
            handleRefreshButton();
        }

    }else{
        // Ignore anything else.
    }

    QApplication::restoreOverrideCursor();
}

void SeerMemoryVisualizerWidget::handleRefreshButton () {

    if (variableNameLineEdit->text() == "") {
        return;
    }

    if (variableAddressLineEdit->text() == "") {
        return;
    }

    if (variableAddressLineEdit->text() == "not an address") {
        return;
    }

    int nbytes = 256;

    if (memoryLengthLineEdit->text() != "") {
        nbytes = memoryLengthLineEdit->text().toInt();
    }

    emit evaluateMemoryExpression(_memoryId, variableAddressLineEdit->text(), nbytes);
    emit evaluateAsmExpression(_asmId,       variableAddressLineEdit->text(), nbytes, 2);
}

void SeerMemoryVisualizerWidget::handleHelpButton () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/MemoryVisualizer.md");
    help->show();
    help->raise();
}

void SeerMemoryVisualizerWidget::handleVariableNameLineEdit () {

    setVariableName (variableNameLineEdit->text());
}

void SeerMemoryVisualizerWidget::handleMemoryDisplayFormatComboBox (int index) {

    if (index == 0) {
        memoryHexEditor->setMemoryMode(SeerHexWidget::HexMemoryMode);

    }else if (index == 1) {
        memoryHexEditor->setMemoryMode(SeerHexWidget::OctalMemoryMode);

    }else if (index == 2) {
        memoryHexEditor->setMemoryMode(SeerHexWidget::BinaryMemoryMode);

    }else if (index == 3) {
        memoryHexEditor->setMemoryMode(SeerHexWidget::DecimalMemoryMode);

    }else{
        // Do nothing.
    }
}

void SeerMemoryVisualizerWidget::handleCharDisplayFormatComboBox (int index) {

    if (index == 0) {
        memoryHexEditor->setCharMode(SeerHexWidget::AsciiCharMode);

    }else if (index == 1) {
        memoryHexEditor->setCharMode(SeerHexWidget::EbcdicCharMode);

    }else{
        // Do nothing.
    }
}

void SeerMemoryVisualizerWidget::handleColumnCountSpinBox (int value) {

    memoryHexEditor->setBytesPerLine(value);
}

void SeerMemoryVisualizerWidget::handlePrintButton () {

    QPrinter printer;

    QPrintDialog* dlg = new QPrintDialog(&printer, this);

    if (dlg->exec() != QDialog::Accepted) {
        return;
    }


    // Make a copy so we can temporarily add a header.
    QTextDocument* clone = 0;

    if (tabWidget->currentWidget()->objectName() == "hex_tab") {
        clone = memoryHexEditor->document()->clone(this);
    }else if (tabWidget->currentWidget()->objectName() == "asm_tab") {
        clone = memoryAsmEditor->document()->clone(this);
    }else{
        clone = new QTextDocument("NO TAB SELECTED!!!\n");
    }

    QTextCursor cursor(clone);
    QTextCharFormat format = cursor.charFormat();
    format.setBackground(QBrush(Qt::transparent));

    cursor.insertText("\n", format);
    cursor.insertText("name=" + variableName() + " address=" + variableAddress() + " bytesPerLine=" + QString::number(memoryHexEditor->bytesPerLine()) + " bytes=" + QString::number(memoryHexEditor->size()) + " memory=" + memoryHexEditor->memoryModeString() + " char=" + memoryHexEditor->charModeString() + "\n", format);
    cursor.insertText("\n", format);

    clone->print(&printer);

    delete clone;
}

void SeerMemoryVisualizerWidget::handleSaveButton () {

    QFileDialog dialog(this, "Seer visualizer file", "./", "Logs (*.log);;Text files (*.txt);;All files (*.*)");
    dialog.setAcceptMode(QFileDialog::AcceptSave);
    dialog.setFileMode(QFileDialog::AnyFile);
    dialog.setDefaultSuffix("log");
    dialog.selectFile("memory.log");

    if (dialog.exec() != QDialog::Accepted) {
        return;
    }

    QStringList files = dialog.selectedFiles();

    if (files.size() == 0) {
        return;
    }

    if (files.size() > 1) {
        QMessageBox::critical(this, tr("Error"), tr("Select only 1 file."));
        return;
    }

    QFile file(files[0]);

    if (file.open(QIODevice::ReadWrite)) {
        QTextStream stream(&file);

        stream << "\n";
        stream << "name=" << variableName() << " address=" << variableAddress() << " bytesPerLine=" << memoryHexEditor->bytesPerLine() << " bytes=" << memoryHexEditor->size() << " memory=" << memoryHexEditor->memoryModeString() << " char=" << memoryHexEditor->charModeString() << "\n";
        stream << "\n";
        if (tabWidget->currentWidget()->objectName() == "hex_tab") {
            stream << memoryHexEditor->toPlainText();
        }else if (tabWidget->currentWidget()->objectName() == "asm_tab") {
            stream << memoryAsmEditor->toPlainText();
        }else{
            stream << "NO TAB SELECTED!!!\n";
        }
        stream << "\n";

        file.flush();
        file.close();

    }else{
        QMessageBox::critical(this, tr("Error"), tr("Cannot save display to file."));
        return;
    }
}

void SeerMemoryVisualizerWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("memoryvisualizerwindow");
    settings.setValue("size", size());
    settings.endGroup();
}

void SeerMemoryVisualizerWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("memoryvisualizerwindow");
    resize(settings.value("size", QSize(800, 400)).toSize());
    settings.endGroup();
}

void SeerMemoryVisualizerWidget::resizeEvent (QResizeEvent* event) {

    writeSettings();

    QWidget::resizeEvent(event);
}

