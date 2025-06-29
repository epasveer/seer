#include "SeerMatrixVisualizerWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QToolTip>
#include <QtGui/QIntValidator>
#include <QtGui/QIcon>
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>
#include <QtCore/QRegularExpression>
#include <QtCore/QSettings>
#include <QtCore/QDebug>
#include <QtGlobal>

SeerMatrixVisualizerWidget::SeerMatrixVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _variableId = Seer::createID(); // Create id's for queries.
    _memoryId   = Seer::createID();
    _rowsId     = Seer::createID();
    _columnsId  = Seer::createID();
    _offsetId   = Seer::createID();
    _strideId   = Seer::createID();

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/seergdb_64x64.png"));
    setWindowTitle("Seer Matrix Visualizer");
    setAttribute(Qt::WA_DeleteOnClose);

    matrixDisplayFormatComboBox->setCurrentIndex(0);

    handleMatrixDisplayFormatComboBox(0);

    variableNameLineEdit->enableReturnPressedOnClear();

    // Connect things.
    QObject::connect(refreshToolButton,             &QToolButton::clicked,                                     this,            &SeerMatrixVisualizerWidget::handleRefreshButton);
    QObject::connect(variableNameLineEdit,          &SeerHistoryLineEdit::returnPressed,                       this,            &SeerMatrixVisualizerWidget::handleVariableNameLineEdit);
    QObject::connect(variableNameLineEdit,          &SeerHistoryLineEdit::editingFinished,                     this,            &SeerMatrixVisualizerWidget::handleVariableNameLineEdit);
    QObject::connect(matrixRowsLineEdit,            &SeerHistoryLineEdit::returnPressed,                       this,            &SeerMatrixVisualizerWidget::handleRefreshButton);
    QObject::connect(matrixRowsLineEdit,            &SeerHistoryLineEdit::editingFinished,                     this,            &SeerMatrixVisualizerWidget::handleElementRowsLineEdit);
    QObject::connect(matrixColumnsLineEdit,         &SeerHistoryLineEdit::returnPressed,                       this,            &SeerMatrixVisualizerWidget::handleRefreshButton);
    QObject::connect(matrixColumnsLineEdit,         &SeerHistoryLineEdit::editingFinished,                     this,            &SeerMatrixVisualizerWidget::handleElementColumnsLineEdit);
    QObject::connect(matrixOffsetLineEdit,          &SeerHistoryLineEdit::returnPressed,                       this,            &SeerMatrixVisualizerWidget::handleRefreshButton);
    QObject::connect(matrixOffsetLineEdit,          &SeerHistoryLineEdit::editingFinished,                     this,            &SeerMatrixVisualizerWidget::handleElementOffsetLineEdit);
    QObject::connect(matrixStrideLineEdit,          &SeerHistoryLineEdit::returnPressed,                       this,            &SeerMatrixVisualizerWidget::handleRefreshButton);
    QObject::connect(matrixStrideLineEdit,          &SeerHistoryLineEdit::editingFinished,                     this,            &SeerMatrixVisualizerWidget::handleElementStrideLineEdit);
    QObject::connect(matrixDisplayFormatComboBox,   QOverload<int>::of(&QComboBox::currentIndexChanged),       this,            &SeerMatrixVisualizerWidget::handleMatrixDisplayFormatComboBox);
    QObject::connect(matrixTableWidget,             &SeerMatrixWidget::dataChanged,                            this,            &SeerMatrixVisualizerWidget::handleDataChanged);

    // Restore window settings.
    readSettings();
}

SeerMatrixVisualizerWidget::~SeerMatrixVisualizerWidget () {
}

void SeerMatrixVisualizerWidget::setVariableName (const QString& name) {

    setWindowTitle("Seer Matrix Visualizer - '" + name + "'");

    variableNameLineEdit->setText(name);

    if (variableNameLineEdit->text() == "") {
        variableAddressLineEdit->setText("");
        matrixRowsLineEdit->setText("");
        matrixColumnsLineEdit->setText("");
        matrixOffsetLineEdit->setText("");
        matrixStrideLineEdit->setText("");

        matrixTableWidget->setData(0);

        return;
    }

    setVariableAddress("");

    // Clear old contents.
    QByteArray array;
    bool ok;

    matrixTableWidget->setData(new SeerMatrixWidget::DataStorageArray(array));

    if (matrixOffsetLineEdit->text() != "") {
        matrixTableWidget->setAddressOffset(matrixOffsetLineEdit->text().toULong(&ok));
        if (ok == false) {
            qWarning() << "Invalid string for address offset." << matrixOffsetLineEdit->text();
        }
    }else{
        matrixTableWidget->setAddressOffset(0);
    }

    if (matrixStrideLineEdit->text() != "") {
        matrixTableWidget->setAddressStride(matrixStrideLineEdit->text().toULong(&ok));
        if (ok == false) {
            qWarning() << "Invalid string for address stride." << matrixStrideLineEdit->text();
        }
    }else{
        matrixTableWidget->setAddressStride(1);
    }

    // Send signal to get variable address.
    emit evaluateVariableExpression(_variableId, variableNameLineEdit->text());
}

QString SeerMatrixVisualizerWidget::variableName () const {
    return variableNameLineEdit->text();
}

void SeerMatrixVisualizerWidget::setVariableAddress (const QString& address) {

    unsigned long offset  = 0;
    bool          ok      = false;

    if (address == "") {

        variableAddressLineEdit->setText("");
        offset = 0;

    }else{

        // Test for base10
        if (ok == false) {
            offset = address.toULong(&ok, 10);
            if (ok) {
                variableAddressLineEdit->setText(QString("0x%1").arg(offset, 0, 16, QLatin1Char( '0' )));
            }
        }

        // Test for base16
        if (ok == false) {
            offset = address.toULong(&ok, 16);
            if (ok) {
                variableAddressLineEdit->setText(QString("0x%1").arg(offset, 0, 16, QLatin1Char( '0' )));
            }
        }

        if (ok == false) {
            variableAddressLineEdit->setText("not an address");
            offset = 0;
        }
    }

    matrixTableWidget->setAddressOffset(0);
}

QString SeerMatrixVisualizerWidget::variableAddress () const {
    return variableAddressLineEdit->text();
}

void SeerMatrixVisualizerWidget::setVariableRows (const QString& rows) {
    matrixRowsLineEdit->setText(rows);
}

QString SeerMatrixVisualizerWidget::variableRows () const {
    return matrixRowsLineEdit->text();
}

void SeerMatrixVisualizerWidget::setVariableColumns (const QString& columns) {
    matrixColumnsLineEdit->setText(columns);
}

QString SeerMatrixVisualizerWidget::variableColumns () const {
    return matrixColumnsLineEdit->text();
}

void SeerMatrixVisualizerWidget::setVariableOffset (const QString& offset) {
    matrixOffsetLineEdit->setText(offset);
}

QString SeerMatrixVisualizerWidget::variableOffset () const {
    return matrixOffsetLineEdit->text();
}

void SeerMatrixVisualizerWidget::setVariableStride (const QString& stride) {
    matrixStrideLineEdit->setText(stride);
}

QString SeerMatrixVisualizerWidget::variableStride () const {
    return matrixStrideLineEdit->text();
}

void SeerMatrixVisualizerWidget::handleText (const QString& text) {

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

        if (id_text.toInt() == _rowsId) {

            // Set the memory length.
            QString value_text = Seer::parseFirst(text, "value=", '"', '"', false);

            setVariableRows(value_text);

            handleRefreshButton();
        }

        if (id_text.toInt() == _columnsId) {

            // Set the memory length.
            QString value_text = Seer::parseFirst(text, "value=", '"', '"', false);

            setVariableColumns(value_text);

            handleRefreshButton();
        }

        if (id_text.toInt() == _offsetId) {

            // Set the memory offset.
            QString value_text = Seer::parseFirst(text, "value=", '"', '"', false);

            setVariableOffset(value_text);

            handleRefreshButton();
        }

        if (id_text.toInt() == _strideId) {

            // Set the memory stride.
            QString value_text = Seer::parseFirst(text, "value=", '"', '"', false);

            setVariableStride(value_text);

            handleRefreshButton();
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,memory="))) {

        // 3^done,memory=[{begin="0x0000000000613e70",offset="0x0000000000000000",end="0x0000000000613e71",contents="00"}]
        // 4^done,memory=[{begin="0x0000000000613e70",offset="0x0000000000000000",end="0x0000000000613ed4",contents="000000000000000000000000"}]

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _memoryId) {

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
                bool ok;
                matrixTableWidget->setData(new SeerMatrixWidget::DataStorageArray(array));

                if (matrixOffsetLineEdit->text() != "") {
                    matrixTableWidget->setAddressOffset(matrixOffsetLineEdit->text().toULong(&ok));
                    if (ok == false) {
                        qWarning() << "Invalid string for address offset." << matrixOffsetLineEdit->text();
                    }
                }else{
                    matrixTableWidget->setAddressOffset(0);
                }

                if (matrixStrideLineEdit->text() != "") {
                    matrixTableWidget->setAddressStride(matrixStrideLineEdit->text().toULong(&ok));
                    if (ok == false) {
                        qWarning() << "Invalid string for address stride." << matrixStrideLineEdit->text();
                    }
                }else{
                    matrixTableWidget->setAddressStride(1);
                }

                // Set the dimension of the matrix table.
                if (matrixRowsLineEdit->text() != "" && matrixColumnsLineEdit->text() != "") {
                    int rows = matrixRowsLineEdit->text().toInt(&ok);
                    if (ok == false) {
                        qWarning() << "Invalid string for number of rows." << matrixRowsLineEdit->text();
                        rows = 0;
                    }

                    int columns = matrixColumnsLineEdit->text().toInt(&ok);
                    if (ok == false) {
                        qWarning() << "Invalid string for number of columns." << matrixColumnsLineEdit->text();
                        columns = 0;
                    }

                    matrixTableWidget->setDimensions(rows,columns);

                }else{
                    matrixTableWidget->setDimensions(0,0);
                }

                break; // Take just the first range for now.
            }
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

        if (id_text.toInt() == _rowsId) {
            // Display the error message.
            QString msg_text = Seer::parseFirst(text, "msg=", false);

            QMessageBox::warning(this, "Error.", Seer::filterEscapes(msg_text));

            matrixRowsLineEdit->setText("");
            matrixRowsLineEdit->setFocus();
        }

        if (id_text.toInt() == _columnsId) {
            // Display the error message.
            QString msg_text = Seer::parseFirst(text, "msg=", false);

            QMessageBox::warning(this, "Error.", Seer::filterEscapes(msg_text));

            matrixColumnsLineEdit->setText("");
            matrixColumnsLineEdit->setFocus();
        }

        if (id_text.toInt() == _offsetId) {
            // Display the error message.
            QString msg_text = Seer::parseFirst(text, "msg=", false);

            QMessageBox::warning(this, "Error.", Seer::filterEscapes(msg_text));

            matrixOffsetLineEdit->setText("");
            matrixOffsetLineEdit->setFocus();
        }

        if (id_text.toInt() == _strideId) {
            // Display the error message.
            QString msg_text = Seer::parseFirst(text, "msg=", false);

            QMessageBox::warning(this, "Error.", Seer::filterEscapes(msg_text));

            matrixStrideLineEdit->setText("");
            matrixStrideLineEdit->setFocus();
        }

    }else{
        // Ignore anything else.
    }
}

void SeerMatrixVisualizerWidget::handleRefreshButton () {

    if (variableNameLineEdit->text() == "") {
        return;
    }

    if (variableAddressLineEdit->text() == "") {
        return;
    }

    if (variableAddressLineEdit->text() == "not an address") {
        return;
    }

    int bytes = matrixRowsLineEdit->text().toInt() * matrixColumnsLineEdit->text().toInt() * Seer::typeBytes(matrixDisplayFormatComboBox->currentText());

    // qDebug() << "Asking for" << bytes << "bytes of matrix data.";

    emit evaluateMemoryExpression(_memoryId, variableAddressLineEdit->text(), bytes);
}

void SeerMatrixVisualizerWidget::handleVariableNameLineEdit () {

    setVariableName (variableNameLineEdit->text());
}

void SeerMatrixVisualizerWidget::handleElementRowsLineEdit () {

    // Nothing set? Return.
    if (variableRows() == "") {
        return;
    }

    // A regular integer? Return.
    QRegularExpression re("[0-9]+");
    QRegularExpressionMatch match = re.match(variableRows());

    if (match.hasMatch()) {
        return;
    }

    // Send for the value of the variable.
    emit evaluateVariableExpression(_rowsId, variableRows());
}

void SeerMatrixVisualizerWidget::handleElementColumnsLineEdit () {

    // Nothing set? Return.
    if (variableColumns() == "") {
        return;
    }

    // A regular integer? Return.
    QRegularExpression re("[0-9]+");
    QRegularExpressionMatch match = re.match(variableColumns());

    if (match.hasMatch()) {
        return;
    }

    // Send for the value of the variable.
    emit evaluateVariableExpression(_columnsId, variableColumns());
}

void SeerMatrixVisualizerWidget::handleElementOffsetLineEdit () {

    // Nothing set? Return.
    if (variableOffset() == "") {
        return;
    }

    // A regular integer? Return.
    QRegularExpression re("[0-9]+");
    QRegularExpressionMatch match = re.match(variableOffset());

    if (match.hasMatch()) {
        return;
    }

    emit evaluateVariableExpression(_offsetId, variableOffset());
}

void SeerMatrixVisualizerWidget::handleElementStrideLineEdit () {

    // Nothing set? Return.
    if (variableStride() == "") {
        return;
    }

    // A regular integer? Return.
    QRegularExpression re("[0-9]+");
    QRegularExpressionMatch match = re.match(variableRows());

    if (match.hasMatch()) {
        return;
    }

    // Send for the value of the variable.
    emit evaluateVariableExpression(_strideId, variableStride());
}

void SeerMatrixVisualizerWidget::handleMatrixDisplayFormatComboBox (int index) {

    //qDebug() << index;

    if (index == 0) {
        matrixTableWidget->setDataType(SeerMatrixWidget::Int16MatrixType);

    }else if (index == 1) {
        matrixTableWidget->setDataType(SeerMatrixWidget::Int32MatrixType);

    }else if (index == 2) {
        matrixTableWidget->setDataType(SeerMatrixWidget::Int64MatrixType);

    }else if (index == 3) {
        matrixTableWidget->setDataType(SeerMatrixWidget::UInt16MatrixType);

    }else if (index == 4) {
        matrixTableWidget->setDataType(SeerMatrixWidget::UInt32MatrixType);

    }else if (index == 5) {
        matrixTableWidget->setDataType(SeerMatrixWidget::UInt64MatrixType);

    }else if (index == 6) {
        matrixTableWidget->setDataType(SeerMatrixWidget::Float32MatrixType);

    }else if (index == 7) {
        matrixTableWidget->setDataType(SeerMatrixWidget::Float64MatrixType);

    }else{
        // Do nothing.
    }

    handleRefreshButton();
}

void SeerMatrixVisualizerWidget::handleDataChanged () {
    return;  // Do nothing for now.
}

void SeerMatrixVisualizerWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("matrixvisualizerwindow"); {
        settings.setValue("size", size());
    } settings.endGroup();
}

void SeerMatrixVisualizerWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("matrixvisualizerwindow"); {
        resize(settings.value("size", QSize(800, 400)).toSize());
    } settings.endGroup();
}

void SeerMatrixVisualizerWidget::resizeEvent (QResizeEvent* event) {

    writeSettings();

    QWidget::resizeEvent(event);
}

