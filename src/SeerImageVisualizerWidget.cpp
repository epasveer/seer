#include "SeerImageVisualizerWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtGui/QIntValidator>
#include <QtGui/QIcon>
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerImageVisualizerWidget::SeerImageVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _variableId = Seer::createID(); // Create two id's for queries.
    _memoryId   = Seer::createID();
    _formatName = "";
    _format     = QImage::Format_Invalid;
    _width      = 0;
    _height     = 0;
    _bytes      = 0;

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/seergdb_64x64.png"));
    setWindowTitle("Seer Image Visualizer");
    setAttribute(Qt::WA_DeleteOnClose);

    widthLineEdit->setValidator(new QIntValidator(1, 9999999, this));
    heightLineEdit->setValidator(new QIntValidator(1, 9999999, this));

    // Connect things.
    QObject::connect(refreshToolButton,             &QToolButton::clicked,                                     this,  &SeerImageVisualizerWidget::handleRefreshButton);
    QObject::connect(helpToolButton,                &QToolButton::clicked,                                     this,  &SeerImageVisualizerWidget::handleHelpButton);
    QObject::connect(variableNameLineEdit,          &QLineEdit::returnPressed,                                 this,  &SeerImageVisualizerWidget::handleVariableNameLineEdit);
    QObject::connect(widthLineEdit,                 &QLineEdit::returnPressed,                                 this,  &SeerImageVisualizerWidget::handleRefreshButton);
    QObject::connect(heightLineEdit,                &QLineEdit::returnPressed,                                 this,  &SeerImageVisualizerWidget::handleRefreshButton);
    QObject::connect(formatComboBox,                QOverload<int>::of(&QComboBox::currentIndexChanged),       this,  &SeerImageVisualizerWidget::handleFormatComboBox);
    QObject::connect(printToolButton,               &QToolButton::clicked,                                     this,  &SeerImageVisualizerWidget::handlePrintButton);
    QObject::connect(saveToolButton,                &QToolButton::clicked,                                     this,  &SeerImageVisualizerWidget::handleSaveButton);

    // Restore window settings.
    readSettings();
}

SeerImageVisualizerWidget::~SeerImageVisualizerWidget () {
}

void SeerImageVisualizerWidget::setVariableName (const QString& name) {

    setWindowTitle("Seer Image Visualizer - '" + name + "'");

    variableNameLineEdit->setText(name);
    setVariableAddress("");

    if (variableNameLineEdit->text() == "") {
        return;
    }

    // Clear old contents.
    QByteArray array;

    imageViewer->setImage(QImage());

    // Send signal to get variable address.
    emit evaluateVariableExpression(_variableId, variableNameLineEdit->text());
}

QString SeerImageVisualizerWidget::variableName () const {

    return variableNameLineEdit->text();
}

void SeerImageVisualizerWidget::setVariableAddress (const QString& address) {

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

    imageViewer->setImage(QImage());

    // Show results immediately.
    if (refresh) {
        handleRefreshButton();
    }
}

QString SeerImageVisualizerWidget::variableAddress () const {

    return variableAddressLineEdit->text();
}

void SeerImageVisualizerWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    //qDebug() << text;

    if (text.contains(QRegularExpression("^([0-9]+)\\^done,value="))) {

        // 10^done,value="1"
        // 11^done,value="0x7fffffffd538"

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _variableId) {

            QStringList words = Seer::filterEscapes(Seer::parseFirst(text, "value=", '"', '"', false)).split(' ', Qt::SkipEmptyParts);

            setVariableAddress(words.first());
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

                // Create the image.
                handleCreateImage(array);

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

void SeerImageVisualizerWidget::handleRefreshButton () {

    // Clear the status.
    messageLineEdit->setText("");

    if (variableNameLineEdit->text() == "") {
        return;
    }

    if (variableAddressLineEdit->text() == "") {
        return;
    }

    if (variableAddressLineEdit->text() == "not an address") {
        return;
    }

    if (widthLineEdit->text() == "") {
        return;
    }

    if (heightLineEdit->text() == "") {
        return;
    }

    // Get image dimensions.
    int w = widthLineEdit->text().toInt();
    int h = heightLineEdit->text().toInt();

    if (w < 1 || h < 1) {
        return;
    }

    // Get image format.
    QString format = formatComboBox->currentText();

    // Validate the image dimensions vs. the data size.
    _formatName = "";
    _format     = QImage::Format_Invalid;
    _width      = w;
    _height     = h;
    _bytes      = 0;

    if (format == "RGBA8888") {

        _formatName = format;
        _format     = QImage::Format_RGBA8888;
        _bytes      = _width * _height * 4;

    }else if (format == "RGB888") {

        _formatName = format;
        _format     = QImage::Format_RGB888;
        _bytes      = _width * _height * 3;
    }

    // Unknown format.
    if (_bytes <= 0) {

        _formatName = "";
        _format     = QImage::Format_Invalid;
        _width      = w;
        _height     = h;
        _bytes      = 0;

        messageLineEdit->setText("Unknown image format of '" + format + ".");
        return;
    }

    emit evaluateMemoryExpression(_memoryId, variableAddressLineEdit->text(), _bytes);
}

void SeerImageVisualizerWidget::handleHelpButton () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/ImageVisualizer.md");
    help->show();
    help->raise();
}

void SeerImageVisualizerWidget::handleVariableNameLineEdit () {

    setVariableName (variableNameLineEdit->text());
}

void SeerImageVisualizerWidget::handleFormatComboBox (int index) {

    Q_UNUSED(index);

    handleRefreshButton();
}

void SeerImageVisualizerWidget::handlePrintButton () {

    imageViewer->print();
}

void SeerImageVisualizerWidget::handleSaveButton () {

    imageViewer->saveFileDialog(variableNameLineEdit->text() + ".png");
}

void SeerImageVisualizerWidget::handleCreateImage (const QByteArray& array) {

    // Clear the status.
    messageLineEdit->setText("");

    // Nothing to do?
    if (_bytes == 0) {
        return;
    }

    if (_format == QImage::Format_Invalid) {
        return;
    }

    // Data is not large enough for requested image dimensions.
    if (_bytes > array.size()) {
        messageLineEdit->setText(QString("%1 memory bytes is not enough for the '%2' image (%3x%4). Need at least %5 bytes.").arg(array.size())
                                                                                                                             .arg(_formatName)
                                                                                                                             .arg(_width)
                                                                                                                             .arg(_height)
                                                                                                                             .arg(_bytes));
        return;
    }

    // Construct image.
    QImage image = QImage((const uchar*)array.data(), _width, _height, _format);

    imageViewer->setImage(image);
}

void SeerImageVisualizerWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("imagevisualizerwindow");
    settings.setValue("size", size());
    settings.endGroup();
}

void SeerImageVisualizerWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("imagevisualizerwindow");
    resize(settings.value("size", QSize(800, 400)).toSize());
    settings.endGroup();
}

void SeerImageVisualizerWidget::resizeEvent (QResizeEvent* event) {

    writeSettings();

    QWidget::resizeEvent(event);
}

