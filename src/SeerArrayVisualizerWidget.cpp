#include "SeerArrayVisualizerWidget.h"
#include "SeerUtl.h"
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
#include <QtCore/QRegExp>
#include <QtCore/QSettings>
#include <QtCore/QDebug>
#include <QtGlobal>

SeerArrayVisualizerWidget::SeerArrayVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _aVariableId = Seer::createID(); // Create two id's for queries.
    _aMemoryId   = Seer::createID();

    _bVariableId = Seer::createID(); // Create two id's for queries.
    _bMemoryId   = Seer::createID();

    _series      = 0;

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/seer_64x64.png"));
    setWindowTitle("Seer Array Visualizer");

    aArrayLengthLineEdit->setValidator(new QIntValidator(1, 9999999, this));
    bArrayLengthLineEdit->setValidator(new QIntValidator(1, 9999999, this));
    aArrayOffsetLineEdit->setValidator(new QIntValidator(0, 9999999, this));
    bArrayOffsetLineEdit->setValidator(new QIntValidator(0, 9999999, this));
    aArrayStrideLineEdit->setValidator(new QIntValidator(1, 9999999, this));
    bArrayStrideLineEdit->setValidator(new QIntValidator(1, 9999999, this));


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

    QObject::connect(arrayTableWidget,              &SeerArrayWidget::dataChanged,                             this,            &SeerArrayVisualizerWidget::handleDataChanged);
    QObject::connect(splitter,                      &QSplitter::splitterMoved,                                 this,            &SeerArrayVisualizerWidget::handleSplitterMoved);
    QObject::connect(titleLineEdit,                 &QLineEdit::returnPressed,                                 this,            &SeerArrayVisualizerWidget::handleTitleLineEdit);
    QObject::connect(pointsCheckBox,                &QCheckBox::clicked,                                       this,            &SeerArrayVisualizerWidget::handlePointsCheckBox);
    QObject::connect(labelsCheckBox,                &QCheckBox::clicked,                                       this,            &SeerArrayVisualizerWidget::handleLabelsCheckBox);

#if (QT_VERSION >= QT_VERSION_CHECK(5, 12, 0))
    QObject::connect(lineTypeButtonGroup,           QOverload<int>::of(&QButtonGroup::idClicked),              this,            &SeerArrayVisualizerWidget::handleLineTypeButtonGroup);
#else
    QObject::connect(lineTypeButtonGroup,           QOverload<int>::of(&QButtonGroup::buttonClicked),          this,            &SeerArrayVisualizerWidget::handleLineTypeButtonGroup);
#endif

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

    if (address.startsWith("0x")) {

        bool ok = false;

        address.toULong(&ok, 16);

        if (ok == true) {
            aVariableAddressLineEdit->setText(address);
        }else{
            aVariableAddressLineEdit->setText("not an address");
        }

    }else{
        if (aVariableAddressLineEdit->text() != "") {
            aVariableAddressLineEdit->setText("not an address");
        }else{
            aVariableAddressLineEdit->setText("");
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

    if (address.startsWith("0x")) {

        bool ok = false;

        address.toULong(&ok, 16);

        if (ok == true) {
            bVariableAddressLineEdit->setText(address);
        }else{
            bVariableAddressLineEdit->setText("not an address");
        }

    }else{
        if (bVariableAddressLineEdit->text() != "") {
            bVariableAddressLineEdit->setText("not an address");
        }else{
            bVariableAddressLineEdit->setText("");
        }
    }

    arrayTableWidget->setBAddressOffset(0);
}

QString SeerArrayVisualizerWidget::bVariableAddress () const {
    return bVariableAddressLineEdit->text();
}

void SeerArrayVisualizerWidget::handleText (const QString& text) {

    //qDebug() << text;

    if (text.contains(QRegExp("^([0-9]+)\\^done,value="))) {

        // 10^done,value="1"
        // 11^done,value="0x7fffffffd538"

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _aVariableId) {

            QStringList words = Seer::filterEscapes(Seer::parseFirst(text, "value=", '"', '"', false)).split(' ', Qt::SkipEmptyParts);

            setAVariableAddress(words.first());
        }

        if (id_text.toInt() == _bVariableId) {

            QStringList words = Seer::filterEscapes(Seer::parseFirst(text, "value=", '"', '"', false)).split(' ', Qt::SkipEmptyParts);

            setBVariableAddress(words.first());
        }

    }else if (text.contains(QRegExp("^([0-9]+)\\^done,memory="))) {

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

    }else if (text.contains(QRegExp("^([0-9]+)\\^error,msg="))) {

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

void SeerArrayVisualizerWidget::handleDataChanged () {

    if (_series) {
        arrayChartView->chart()->removeSeries(_series);
        arrayChartView->chart()->update();
        delete _series;
        _series = 0;
    }

    if (scatterRadioButton->isChecked()) {

        QScatterSeries* scatter = new QScatterSeries;
        scatter->setMarkerShape(QScatterSeries::MarkerShapeRectangle);
        scatter->setMarkerSize(7);
        _series = scatter;

    }else if (lineRadioButton->isChecked()) {

        QLineSeries* line = new QLineSeries;
        _series = line;

    }else if (splineRadioButton->isChecked()) {

        QSplineSeries* line  = new QSplineSeries;
        _series = line;
    }

    if (_series == 0) {
        return;
    }

    _series->setPointsVisible(false);
    _series->setPointLabelsVisible(false);
    _series->setPointLabelsClipping(true);


    if (arrayTableWidget->aSize() > 0 && arrayTableWidget->bSize() == 0) {

        const QVector<double>& values = arrayTableWidget->aArrayValues();

        for (int i = 0; i < values.size(); ++i) {
            _series->append(i, values[i]);
        }

        _series->setName(QString("%1:%2:%3").arg(arrayTableWidget->aLabel()).arg(arrayTableWidget->aAddressOffset()).arg(arrayTableWidget->aAddressStride()));

    }else if (arrayTableWidget->aSize() == 0 && arrayTableWidget->bSize() > 0) {

        const QVector<double>& values = arrayTableWidget->bArrayValues();

        for (int i = 0; i < values.size(); ++i) {
            _series->append(i, values[i]);
        }

        _series->setName(QString("%1:%2:%3").arg(arrayTableWidget->bLabel()).arg(arrayTableWidget->bAddressOffset()).arg(arrayTableWidget->bAddressStride()));

    } else if (arrayTableWidget->aSize() > 0 && arrayTableWidget->bSize() > 0) {

        const QVector<double>& xvalues = arrayTableWidget->aArrayValues();
        const QVector<double>& yvalues = arrayTableWidget->bArrayValues();

        for (int i = 0; i < std::min(xvalues.size(),yvalues.size()); ++i) {
            _series->append(xvalues[i], yvalues[i]);
        }

        _series->setName(aVariableName() + "/" + bVariableName());
        _series->setName(QString("%1:%2:%3 | %4:%5:%6")
                                                .arg(arrayTableWidget->aLabel()).arg(arrayTableWidget->aAddressOffset()).arg(arrayTableWidget->aAddressStride())
                                                .arg(arrayTableWidget->bLabel()).arg(arrayTableWidget->bAddressOffset()).arg(arrayTableWidget->bAddressStride()));
    }

    QObject::connect(_series, &QLineSeries::hovered,    this, &SeerArrayVisualizerWidget::handleSeriesHovered);

    arrayChartView->chart()->addSeries(_series);
    arrayChartView->chart()->createDefaultAxes();

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

    //qDebug() << size();
}

void SeerArrayVisualizerWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("arrayvisualizerwindow"); {
        resize(settings.value("size", QSize(800, 400)).toSize());
        splitter->restoreState(settings.value("splitter").toByteArray());
    } settings.endGroup();

    //qDebug() << size();
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

    if (_series) {
        _series->setPointsVisible(pointsCheckBox->isChecked());
        arrayChartView->chart()->update();
    }
}

void SeerArrayVisualizerWidget::handleLabelsCheckBox () {

    if (_series) {
        _series->setPointLabelsVisible(labelsCheckBox->isChecked());
        arrayChartView->chart()->update();
    }
}

void SeerArrayVisualizerWidget::handleLineTypeButtonGroup () {

    handleDataChanged();
}

