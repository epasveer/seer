#include "SeerArrayVisualizerWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include <QtCharts/QChart>
#include <QtCharts/QLineSeries>
#include <QtCharts/QSplineSeries>
#include <QtCharts/QScatterSeries>
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

SeerArrayVisualizerWidget::SeerArrayVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _aVariableId = Seer::createID(); // Create two id's for queries.
    _aMemoryId   = Seer::createID();

    _bVariableId = Seer::createID(); // Create two id's for queries.
    _bMemoryId   = Seer::createID();

    _aSeries     = 0;
    _bSeries     = 0;

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/seergdb_64x64.png"));
    setWindowTitle("Seer Array Visualizer");
    setAttribute(Qt::WA_DeleteOnClose);

    aArrayLengthLineEdit->setValidator(new QIntValidator(1, 9999999, this));
    bArrayLengthLineEdit->setValidator(new QIntValidator(1, 9999999, this));
    aArrayOffsetLineEdit->setValidator(new QIntValidator(0, 9999999, this));
    bArrayOffsetLineEdit->setValidator(new QIntValidator(0, 9999999, this));
    aArrayStrideLineEdit->setValidator(new QIntValidator(1, 9999999, this));
    bArrayStrideLineEdit->setValidator(new QIntValidator(1, 9999999, this));

    arrayTableWidget->setAAxis(aAxisComboBox->currentText());
    arrayTableWidget->setBAxis(bAxisComboBox->currentText());

    lineRadioButton->setChecked(true);

    aArrayDisplayFormatComboBox->setCurrentIndex(0);
    bArrayDisplayFormatComboBox->setCurrentIndex(0);

    handleaArrayDisplayFormatComboBox(0);
    handlebArrayDisplayFormatComboBox(0);

    aVariableNameLineEdit->enableReturnPressedOnClear();
    bVariableNameLineEdit->enableReturnPressedOnClear();

    // A single series chart.
    QChart* chart = new QChart;
    chart->legend()->hide();
    chart->createDefaultAxes();
    chart->legend()->setVisible(true);
    chart->legend()->setAlignment(Qt::AlignBottom);

    arrayChartView->setRenderHint(QPainter::Antialiasing);
    arrayChartView->setChart(chart);
    arrayChartView->setFocusPolicy(Qt::StrongFocus);

    // Connect things.
    QObject::connect(aRefreshToolButton,            &QToolButton::clicked,                                     this,            &SeerArrayVisualizerWidget::handleaRefreshButton);
    QObject::connect(bRefreshToolButton,            &QToolButton::clicked,                                     this,            &SeerArrayVisualizerWidget::handlebRefreshButton);
    QObject::connect(helpToolButton,                &QToolButton::clicked,                                     this,            &SeerArrayVisualizerWidget::handleHelpButton);
    QObject::connect(aArrayLengthLineEdit,          &QLineEdit::returnPressed,                                 this,            &SeerArrayVisualizerWidget::handleaRefreshButton);
    QObject::connect(bArrayLengthLineEdit,          &QLineEdit::returnPressed,                                 this,            &SeerArrayVisualizerWidget::handlebRefreshButton);
    QObject::connect(aArrayOffsetLineEdit,          &QLineEdit::returnPressed,                                 this,            &SeerArrayVisualizerWidget::handleaRefreshButton);
    QObject::connect(bArrayOffsetLineEdit,          &QLineEdit::returnPressed,                                 this,            &SeerArrayVisualizerWidget::handlebRefreshButton);
    QObject::connect(aArrayStrideLineEdit,          &QLineEdit::returnPressed,                                 this,            &SeerArrayVisualizerWidget::handleaRefreshButton);
    QObject::connect(bArrayStrideLineEdit,          &QLineEdit::returnPressed,                                 this,            &SeerArrayVisualizerWidget::handlebRefreshButton);
    QObject::connect(aVariableNameLineEdit,         &QLineEdit::returnPressed,                                 this,            &SeerArrayVisualizerWidget::handleaVariableNameLineEdit);
    QObject::connect(bVariableNameLineEdit,         &QLineEdit::returnPressed,                                 this,            &SeerArrayVisualizerWidget::handlebVariableNameLineEdit);
    QObject::connect(aArrayDisplayFormatComboBox,   QOverload<int>::of(&QComboBox::currentIndexChanged),       this,            &SeerArrayVisualizerWidget::handleaArrayDisplayFormatComboBox);
    QObject::connect(bArrayDisplayFormatComboBox,   QOverload<int>::of(&QComboBox::currentIndexChanged),       this,            &SeerArrayVisualizerWidget::handlebArrayDisplayFormatComboBox);
    QObject::connect(aAxisComboBox,                 QOverload<int>::of(&QComboBox::currentIndexChanged),       this,            &SeerArrayVisualizerWidget::handleaAxisComboBox);
    QObject::connect(bAxisComboBox,                 QOverload<int>::of(&QComboBox::currentIndexChanged),       this,            &SeerArrayVisualizerWidget::handlebAxisComboBox);

    QObject::connect(arrayTableWidget,              &SeerArrayWidget::dataChanged,                             this,            &SeerArrayVisualizerWidget::handleDataChanged);
    QObject::connect(splitter,                      &QSplitter::splitterMoved,                                 this,            &SeerArrayVisualizerWidget::handleSplitterMoved);
    QObject::connect(titleLineEdit,                 &QLineEdit::returnPressed,                                 this,            &SeerArrayVisualizerWidget::handleTitleLineEdit);
    QObject::connect(pointsCheckBox,                &QCheckBox::clicked,                                       this,            &SeerArrayVisualizerWidget::handlePointsCheckBox);
    QObject::connect(labelsCheckBox,                &QCheckBox::clicked,                                       this,            &SeerArrayVisualizerWidget::handleLabelsCheckBox);
    QObject::connect(lineTypeButtonGroup,           QOverload<int>::of(&QButtonGroup::idClicked),              this,            &SeerArrayVisualizerWidget::handleLineTypeButtonGroup);
    QObject::connect(printPushButton,               &QPushButton::clicked,                                     arrayChartView,  &QZoomChartView::printView);

    // Restore window settings.
    readSettings();
}

SeerArrayVisualizerWidget::~SeerArrayVisualizerWidget () {
}

void SeerArrayVisualizerWidget::setAVariableName (const QString& name) {

    setWindowTitle("Seer Array Visualizer - '" + name + "'");

    aVariableNameLineEdit->setText(name);

    if (aVariableNameLineEdit->text() == "") {
        aVariableAddressLineEdit->setText("");
        aArrayLengthLineEdit->setText("");
        aArrayOffsetLineEdit->setText("");
        aArrayStrideLineEdit->setText("");

        arrayTableWidget->setAData("deleted", 0);

        return;
    }

    setAVariableAddress("");

    // Clear old contents.
    QByteArray array;
    bool ok;

    arrayTableWidget->setAData(name, new SeerArrayWidget::DataStorageArray(array));

    if (aArrayOffsetLineEdit->text() != "") {
        arrayTableWidget->setAAddressOffset(aArrayOffsetLineEdit->text().toULong(&ok));
        if (ok == false) {
            qWarning() << "Invalid string for address offset." << aArrayOffsetLineEdit->text();
        }
    }else{
        arrayTableWidget->setAAddressOffset(0);
    }

    if (aArrayStrideLineEdit->text() != "") {
        arrayTableWidget->setAAddressStride(aArrayStrideLineEdit->text().toULong(&ok));
        if (ok == false) {
            qWarning() << "Invalid string for address stride." << aArrayStrideLineEdit->text();
        }
    }else{
        arrayTableWidget->setAAddressStride(1);
    }

    // Send signal to get variable address.
    emit evaluateVariableExpression(_aVariableId, aVariableNameLineEdit->text());
}

QString SeerArrayVisualizerWidget::aVariableName () const {
    return aVariableNameLineEdit->text();
}

void SeerArrayVisualizerWidget::setAVariableAddress (const QString& address) {

    unsigned long offset  = 0;
    bool          ok      = false;

    if (address == "") {

        aVariableAddressLineEdit->setText("");
        offset = 0;

    }else{

        // Test for base10
        if (ok == false) {
            offset = address.toULong(&ok, 10);
            if (ok) {
                aVariableAddressLineEdit->setText(QString("0x%1").arg(offset, 0, 16, QLatin1Char( '0' )));
            }
        }

        // Test for base16
        if (ok == false) {
            offset = address.toULong(&ok, 16);
            if (ok) {
                aVariableAddressLineEdit->setText(QString("0x%1").arg(offset, 0, 16, QLatin1Char( '0' )));
            }
        }

        if (ok == false) {
            aVariableAddressLineEdit->setText("not an address");
            offset = 0;
        }
    }

    arrayTableWidget->setAAddressOffset(0);
}

QString SeerArrayVisualizerWidget::aVariableAddress () const {
    return aVariableAddressLineEdit->text();
}

void SeerArrayVisualizerWidget::setBVariableName (const QString& name) {

    setWindowTitle("Seer Array Visualizer - '" + name + "'");

    bVariableNameLineEdit->setText(name);

    if (bVariableNameLineEdit->text() == "") {
        bVariableAddressLineEdit->setText("");
        bArrayLengthLineEdit->setText("");
        bArrayOffsetLineEdit->setText("");
        bArrayStrideLineEdit->setText("");

        arrayTableWidget->setBData("deleted", 0);

        return;
    }

    setBVariableAddress("");

    // Clear old contents.
    QByteArray array;
    bool ok;

    arrayTableWidget->setBData(name, new SeerArrayWidget::DataStorageArray(array));

    if (bArrayOffsetLineEdit->text() != "") {
        arrayTableWidget->setBAddressOffset(bArrayOffsetLineEdit->text().toULong(&ok));
        if (ok == false) {
            qWarning() << "Invalid string for address offset." << bArrayOffsetLineEdit->text();
        }
    }else{
        arrayTableWidget->setBAddressOffset(0);
    }

    if (bArrayStrideLineEdit->text() != "") {
        arrayTableWidget->setBAddressStride(bArrayStrideLineEdit->text().toULong(&ok));
        if (ok == false) {
            qWarning() << "Invalid string for address stride." << bArrayStrideLineEdit->text();
        }
    }else{
        arrayTableWidget->setBAddressStride(1);
    }

    // Send signal to get variable address.
    emit evaluateVariableExpression(_bVariableId, bVariableNameLineEdit->text());
}

QString SeerArrayVisualizerWidget::bVariableName () const {
    return bVariableNameLineEdit->text();
}

void SeerArrayVisualizerWidget::setBVariableAddress (const QString& address) {

    unsigned long offset  = 0;
    bool          ok      = false;

    if (address == "") {

        bVariableAddressLineEdit->setText("");
        offset = 0;

    }else{

        // Test for base10
        if (ok == false) {
            offset = address.toULong(&ok, 10);
            if (ok) {
                bVariableAddressLineEdit->setText(QString("0x%1").arg(offset, 0, 16, QLatin1Char( '0' )));
            }
        }

        // Test for base16
        if (ok == false) {
            offset = address.toULong(&ok, 16);
            if (ok) {
                bVariableAddressLineEdit->setText(QString("0x%1").arg(offset, 0, 16, QLatin1Char( '0' )));
            }
        }

        if (ok == false) {
            bVariableAddressLineEdit->setText("not an address");
            offset = 0;
        }
    }

    arrayTableWidget->setBAddressOffset(0);
}

QString SeerArrayVisualizerWidget::bVariableAddress () const {
    return bVariableAddressLineEdit->text();
}

void SeerArrayVisualizerWidget::handleText (const QString& text) {

    //qDebug() << text;

    if (text.contains(QRegularExpression("^([0-9]+)\\^done,value="))) {

        // 11^done,value="1"
        // 11^done,value="0x7fffffffd538"

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _aVariableId) {

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
            setAVariableAddress(address);
        }

        if (id_text.toInt() == _bVariableId) {

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
            setBVariableAddress(address);
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,memory="))) {

        // 3^done,memory=[{begin="0x0000000000613e70",offset="0x0000000000000000",end="0x0000000000613e71",contents="00"}]
        // 4^done,memory=[{begin="0x0000000000613e70",offset="0x0000000000000000",end="0x0000000000613ed4",contents="000000000000000000000000"}]

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _aMemoryId) {

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
                arrayTableWidget->setAData(arrayTableWidget->aLabel(), new SeerArrayWidget::DataStorageArray(array));

                if (aArrayOffsetLineEdit->text() != "") {
                    arrayTableWidget->setAAddressOffset(aArrayOffsetLineEdit->text().toULong(&ok));
                    if (ok == false) {
                        qWarning() << "Invalid string for address offset." << aArrayOffsetLineEdit->text();
                    }
                }else{
                    arrayTableWidget->setAAddressOffset(0);
                }

                if (aArrayStrideLineEdit->text() != "") {
                    arrayTableWidget->setAAddressStride(aArrayStrideLineEdit->text().toULong(&ok));
                    if (ok == false) {
                        qWarning() << "Invalid string for address stride." << aArrayStrideLineEdit->text();
                    }
                }else{
                    arrayTableWidget->setAAddressStride(1);
                }

                break; // Take just the first range for now.
            }
        }

        if (id_text.toInt() == _bMemoryId) {

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
                arrayTableWidget->setBData(arrayTableWidget->bLabel(), new SeerArrayWidget::DataStorageArray(array));

                if (bArrayOffsetLineEdit->text() != "") {
                    arrayTableWidget->setBAddressOffset(bArrayOffsetLineEdit->text().toULong(&ok));
                    if (ok == false) {
                        qWarning() << "Invalid string for address offset." << bArrayOffsetLineEdit->text();
                    }
                }else{
                    arrayTableWidget->setBAddressOffset(0);
                }

                if (bArrayStrideLineEdit->text() != "") {
                    arrayTableWidget->setBAddressStride(bArrayStrideLineEdit->text().toULong(&ok));
                    if (ok == false) {
                        qWarning() << "Invalid string for address stride." << bArrayStrideLineEdit->text();
                    }
                }else{
                    arrayTableWidget->setBAddressStride(1);
                }

                break; // Take just the first range for now.
            }
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^error,msg="))) {

        // 12^error,msg="No symbol \"return\" in current context."
        // 13^error,msg="No symbol \"cout\" in current context."
        // 3^error,msg="Unable to read memory."

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _aVariableId) {
            aVariableAddressLineEdit->setText( Seer::filterEscapes(Seer::parseFirst(text, "msg=", '"', '"', false)) );
        }

        if (id_text.toInt() == _aMemoryId) {
            // Display the error message.
            QString msg_text = Seer::parseFirst(text, "msg=", false);

            if (msg_text != "") {
                QMessageBox::warning(this, "Error.", Seer::filterEscapes(msg_text));
            }
        }

        if (id_text.toInt() == _bVariableId) {
            bVariableAddressLineEdit->setText( Seer::filterEscapes(Seer::parseFirst(text, "msg=", '"', '"', false)) );
        }

        if (id_text.toInt() == _bMemoryId) {
            // Display the error message.
            QString msg_text = Seer::parseFirst(text, "msg=", false);

            if (msg_text != "") {
                QMessageBox::warning(this, "Error.", Seer::filterEscapes(msg_text));
            }
        }

    }else{
        // Ignore anything else.
    }
}

void SeerArrayVisualizerWidget::handleaRefreshButton () {

    if (aVariableNameLineEdit->text() == "") {
        return;
    }

    if (aVariableAddressLineEdit->text() == "") {
        return;
    }

    if (aVariableAddressLineEdit->text() == "not an address") {
        return;
    }

    int bytes = aArrayLengthLineEdit->text().toInt() * Seer::typeBytes(aArrayDisplayFormatComboBox->currentText());

    //qDebug() << _aMemoryId << aVariableAddressLineEdit->text() << aArrayLengthLineEdit->text() << aArrayDisplayFormatComboBox->currentText() << bytes;

    emit evaluateMemoryExpression(_aMemoryId, aVariableAddressLineEdit->text(), bytes);
}

void SeerArrayVisualizerWidget::handlebRefreshButton () {

    if (bVariableNameLineEdit->text() == "") {
        return;
    }

    if (bVariableAddressLineEdit->text() == "") {
        return;
    }

    if (bVariableAddressLineEdit->text() == "not an address") {
        return;
    }

    int bytes = bArrayLengthLineEdit->text().toInt() * Seer::typeBytes(bArrayDisplayFormatComboBox->currentText());

    //qDebug() << _bMemoryId << bVariableAddressLineEdit->text() << bArrayLengthLineEdit->text() << bArrayDisplayFormatComboBox->currentText() << bytes;

    emit evaluateMemoryExpression(_bMemoryId, bVariableAddressLineEdit->text(), bytes);
}

void SeerArrayVisualizerWidget::handleHelpButton () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/ArrayVisualizer.md");
    help->show();
    help->raise();
}

void SeerArrayVisualizerWidget::handleaVariableNameLineEdit () {

    setAVariableName (aVariableNameLineEdit->text());
}

void SeerArrayVisualizerWidget::handlebVariableNameLineEdit () {

    setBVariableName (bVariableNameLineEdit->text());
}

void SeerArrayVisualizerWidget::handleaArrayDisplayFormatComboBox (int index) {

    //qDebug() << index;

    if (index == 0) {
        arrayTableWidget->setAArrayMode(SeerArrayWidget::Int16ArrayMode);

    }else if (index == 1) {
        arrayTableWidget->setAArrayMode(SeerArrayWidget::Int32ArrayMode);

    }else if (index == 2) {
        arrayTableWidget->setAArrayMode(SeerArrayWidget::Int64ArrayMode);

    }else if (index == 3) {
        arrayTableWidget->setAArrayMode(SeerArrayWidget::UInt16ArrayMode);

    }else if (index == 4) {
        arrayTableWidget->setAArrayMode(SeerArrayWidget::UInt32ArrayMode);

    }else if (index == 5) {
        arrayTableWidget->setAArrayMode(SeerArrayWidget::UInt64ArrayMode);

    }else if (index == 6) {
        arrayTableWidget->setAArrayMode(SeerArrayWidget::Float32ArrayMode);

    }else if (index == 7) {
        arrayTableWidget->setAArrayMode(SeerArrayWidget::Float64ArrayMode);

    }else{
        // Do nothing.
    }
}

void SeerArrayVisualizerWidget::handlebArrayDisplayFormatComboBox (int index) {

    //qDebug() << index;

    if (index == 0) {
        arrayTableWidget->setBArrayMode(SeerArrayWidget::Int16ArrayMode);

    }else if (index == 1) {
        arrayTableWidget->setBArrayMode(SeerArrayWidget::Int32ArrayMode);

    }else if (index == 2) {
        arrayTableWidget->setBArrayMode(SeerArrayWidget::Int64ArrayMode);

    }else if (index == 3) {
        arrayTableWidget->setBArrayMode(SeerArrayWidget::UInt16ArrayMode);

    }else if (index == 4) {
        arrayTableWidget->setBArrayMode(SeerArrayWidget::UInt32ArrayMode);

    }else if (index == 5) {
        arrayTableWidget->setBArrayMode(SeerArrayWidget::UInt64ArrayMode);

    }else if (index == 6) {
        arrayTableWidget->setBArrayMode(SeerArrayWidget::Float32ArrayMode);

    }else if (index == 7) {
        arrayTableWidget->setBArrayMode(SeerArrayWidget::Float64ArrayMode);

    }else{
        // Do nothing.
    }
}

void SeerArrayVisualizerWidget::handleaAxisComboBox (int index) {

    arrayTableWidget->setAAxis(aAxisComboBox->itemText(index));

    handleDataChanged();
}

void SeerArrayVisualizerWidget::handlebAxisComboBox (int index) {

    arrayTableWidget->setBAxis(bAxisComboBox->itemText(index));

    handleDataChanged();
}

void SeerArrayVisualizerWidget::handleDataChanged () {

    // Delete old series.
    if (_aSeries) {
        arrayChartView->chart()->removeSeries(_aSeries);
        arrayChartView->chart()->update();
        delete _aSeries;
        _aSeries = 0;
    }

    if (_bSeries) {
        arrayChartView->chart()->removeSeries(_bSeries);
        arrayChartView->chart()->update();
        delete _bSeries;
        _bSeries = 0;
    }

    // If only the first array is defined, create a series for it.
    // Handle the X and Y axis.
    if (arrayTableWidget->aSize() > 0 && arrayTableWidget->bSize() == 0) {

        createASeries();

    // If only the second array is defined, create a series for it.
    // Handle the X and Y axis.
    }else if (arrayTableWidget->aSize() == 0 && arrayTableWidget->bSize() > 0) {

        createBSeries();

    } else if (arrayTableWidget->aSize() > 0 && arrayTableWidget->bSize() > 0) {

        // If the axis is the same for the two arrays, plot them as
        // two unique series.
        if (arrayTableWidget->aAxis() == arrayTableWidget->bAxis()) {

            createASeries();
            createBSeries();

        // Otherwise, we have an X and Y axis.
        // Plot one series using the two arrays as an X and as a Y.
        }else{

            if (scatterRadioButton->isChecked()) {

                QScatterSeries* scatter = new QScatterSeries;
                scatter->setMarkerShape(QScatterSeries::MarkerShapeRectangle);
                scatter->setMarkerSize(7);
                _aSeries = scatter;

            }else if (lineRadioButton->isChecked()) {

                QLineSeries* line = new QLineSeries;
                _aSeries = line;

            }else if (splineRadioButton->isChecked()) {

                QSplineSeries* line  = new QSplineSeries;
                _aSeries = line;

            }else{
                qWarning() << "Invalid line type.";
                return;
            }

            _aSeries->setPointsVisible(false);
            _aSeries->setPointLabelsVisible(false);
            _aSeries->setPointLabelsClipping(true);

            const QVector<double>& xvalues = arrayTableWidget->aArrayValues();
            const QVector<double>& yvalues = arrayTableWidget->bArrayValues();

            if (arrayTableWidget->aAxis() == "Y" && arrayTableWidget->bAxis() == "X") {

                for (int i = 0; i < std::min(xvalues.size(),yvalues.size()); ++i) {
                    _aSeries->append(xvalues[i], yvalues[i]);
                }

            }else if (arrayTableWidget->aAxis() == "X" && arrayTableWidget->bAxis() == "Y") {

                for (int i = 0; i < std::min(xvalues.size(),yvalues.size()); ++i) {
                    _aSeries->append(yvalues[i], xvalues[i]);
                }

            }else{
                qWarning() << "Invalid axis type of '" << arrayTableWidget->aAxis() << "'. Not 'X' or 'Y'.";
            }

            _aSeries->setName(aVariableName() + "/" + bVariableName());
            _aSeries->setName(QString("%1:%2:%3 | %4:%5:%6")
                                      .arg(arrayTableWidget->aLabel()).arg(arrayTableWidget->aAddressOffset()).arg(arrayTableWidget->aAddressStride())
                                      .arg(arrayTableWidget->bLabel()).arg(arrayTableWidget->bAddressOffset()).arg(arrayTableWidget->bAddressStride()));
        }
    }

    if (_aSeries) {
        QObject::connect(_aSeries, &QLineSeries::hovered,    this, &SeerArrayVisualizerWidget::handleSeriesHovered);

        arrayChartView->chart()->addSeries(_aSeries);
        arrayChartView->chart()->createDefaultAxes();
    }

    if (_bSeries) {
        QObject::connect(_bSeries, &QLineSeries::hovered,    this, &SeerArrayVisualizerWidget::handleSeriesHovered);

        arrayChartView->chart()->addSeries(_bSeries);
        arrayChartView->chart()->createDefaultAxes();
    }

    // Zoom out slightly to allow for text label at edges.
    arrayChartView->chart()->zoomReset();
    arrayChartView->chart()->zoom(.9);
    arrayChartView->chart()->update();

    // Check for points or labels to be shown.
    handlePointsCheckBox();
    handleLabelsCheckBox();
}

void SeerArrayVisualizerWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("arrayvisualizerwindow"); {
        settings.setValue("size", size());
        settings.setValue("splitter", splitter->saveState());
    } settings.endGroup();
}

void SeerArrayVisualizerWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("arrayvisualizerwindow"); {
        resize(settings.value("size", QSize(800, 400)).toSize());
        splitter->restoreState(settings.value("splitter").toByteArray());
    } settings.endGroup();
}

void SeerArrayVisualizerWidget::resizeEvent (QResizeEvent* event) {

    writeSettings();

    QWidget::resizeEvent(event);
}

void SeerArrayVisualizerWidget::handleSplitterMoved (int pos, int index) {

    Q_UNUSED(pos);
    Q_UNUSED(index);

    writeSettings();
}

void SeerArrayVisualizerWidget::handleSeriesHovered (const QPointF& point, bool state) {

    //qDebug() << "QPointF=" << point << "State=" << state << "MapToPosition=" << arrayChartView->chart()->mapToPosition(point) << "MapFromScene=" << arrayChartView->mapFromScene(arrayChartView->chart()->mapToPosition(point));

    if (state) {
        QToolTip::showText(arrayChartView->mapToGlobal(arrayChartView->mapFromScene(arrayChartView->chart()->mapToPosition(point))), QString("%1 / %2").arg(point.x()).arg(point.y()), this, QRect(), 10000);
    }else{
        QToolTip::hideText();
    }
}

void SeerArrayVisualizerWidget::handleTitleLineEdit () {

    arrayChartView->chart()->setTitle(titleLineEdit->text());

    titleLineEdit->setText("");
}

void SeerArrayVisualizerWidget::handlePointsCheckBox () {

    if (_aSeries) {
        _aSeries->setPointsVisible(pointsCheckBox->isChecked());
        arrayChartView->chart()->update();
    }

    if (_bSeries) {
        _bSeries->setPointsVisible(pointsCheckBox->isChecked());
        arrayChartView->chart()->update();
    }
}

void SeerArrayVisualizerWidget::handleLabelsCheckBox () {

    if (_aSeries) {
        _aSeries->setPointLabelsVisible(labelsCheckBox->isChecked());
        arrayChartView->chart()->update();
    }

    if (_bSeries) {
        _bSeries->setPointLabelsVisible(labelsCheckBox->isChecked());
        arrayChartView->chart()->update();
    }
}

void SeerArrayVisualizerWidget::handleLineTypeButtonGroup () {

    handleDataChanged();
}

void SeerArrayVisualizerWidget::createASeries() {

    if (scatterRadioButton->isChecked()) {

        QScatterSeries* scatter = new QScatterSeries;
        scatter->setMarkerShape(QScatterSeries::MarkerShapeRectangle);
        scatter->setMarkerSize(7);
        _aSeries = scatter;

    }else if (lineRadioButton->isChecked()) {

        QLineSeries* line = new QLineSeries;
        _aSeries = line;

    }else if (splineRadioButton->isChecked()) {

        QSplineSeries* line  = new QSplineSeries;
        _aSeries = line;

    }else{
        qWarning() << "Invalid line type.";
        return;
    }

    _aSeries->setPointsVisible(false);
    _aSeries->setPointLabelsVisible(false);
    _aSeries->setPointLabelsClipping(true);

    const QVector<double>& values = arrayTableWidget->aArrayValues();

    if (arrayTableWidget->aAxis() == "Y") {

        for (int i = 0; i < values.size(); ++i) {
            _aSeries->append(i, values[i]);
        }

    }else if (arrayTableWidget->aAxis() == "X") {

        for (int i = 0; i < values.size(); ++i) {
            _aSeries->append(values[i], i);
        }

    }else{
        qWarning() << "Invalid axis type of '" << arrayTableWidget->aAxis() << "'. Not 'X' or 'Y'.";
    }

    _aSeries->setName(QString("%1:%2:%3").arg(arrayTableWidget->aLabel()).arg(arrayTableWidget->aAddressOffset()).arg(arrayTableWidget->aAddressStride()));
}

void SeerArrayVisualizerWidget::createBSeries() {

    if (scatterRadioButton->isChecked()) {

        QScatterSeries* scatter = new QScatterSeries;
        scatter->setMarkerShape(QScatterSeries::MarkerShapeRectangle);
        scatter->setMarkerSize(7);
        _bSeries = scatter;

    }else if (lineRadioButton->isChecked()) {

        QLineSeries* line = new QLineSeries;
        _bSeries = line;

    }else if (splineRadioButton->isChecked()) {

        QSplineSeries* line  = new QSplineSeries;
        _bSeries = line;

    }else{
        qWarning() << "Invalid line type.";
        return;
    }

    _bSeries->setPointsVisible(false);
    _bSeries->setPointLabelsVisible(false);
    _bSeries->setPointLabelsClipping(true);

    const QVector<double>& values = arrayTableWidget->bArrayValues();

    if (arrayTableWidget->bAxis() == "Y") {

        for (int i = 0; i < values.size(); ++i) {
            _bSeries->append(i, values[i]);
        }

    }else if (arrayTableWidget->bAxis() == "X") {

        for (int i = 0; i < values.size(); ++i) {
            _bSeries->append(values[i], i);
        }

    }else{
        qWarning() << "Invalid axis type of '" << arrayTableWidget->bAxis() << "'. Not 'X' or 'Y'.";
    }

    _bSeries->setName(QString("%1:%2:%3").arg(arrayTableWidget->bLabel()).arg(arrayTableWidget->bAddressOffset()).arg(arrayTableWidget->bAddressStride()));
}

