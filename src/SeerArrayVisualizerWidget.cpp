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

SeerArrayVisualizerWidget::SeerArrayVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _variableId = Seer::createID(); // Create two id's for queries.
    _memoryId   = Seer::createID();
    _series     = 0;

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/seer_64x64.png"));
    setWindowTitle("Seer Array Visualizer");

    arrayLengthLineEdit->setValidator(new QIntValidator(1, 9999999, this));
    arrayOffsetLineEdit->setValidator(new QIntValidator(0, 9999999, this));
    arrayStrideLineEdit->setValidator(new QIntValidator(1, 9999999, this));

    lineRadioButton->setChecked(true);

    arrayDisplayFormatComboBox->setCurrentIndex(0);
    handleArrayDisplayFormatComboBox(0);

    // A single series chart.
    QChart* chart = new QChart;
    chart->legend()->hide();
    chart->createDefaultAxes();
    chart->legend()->setVisible(true);
    chart->legend()->setAlignment(Qt::AlignBottom);

    arrayChartView->setRenderHint(QPainter::Antialiasing);
    arrayChartView->setChart(chart);

    // Connect things.
    QObject::connect(refreshToolButton,             &QToolButton::clicked,                                     this,  &SeerArrayVisualizerWidget::handleRefreshButton);
    QObject::connect(arrayLengthLineEdit,           &QLineEdit::returnPressed,                                 this,  &SeerArrayVisualizerWidget::handleRefreshButton);
    QObject::connect(arrayOffsetLineEdit,           &QLineEdit::returnPressed,                                 this,  &SeerArrayVisualizerWidget::handleRefreshButton);
    QObject::connect(arrayStrideLineEdit,           &QLineEdit::returnPressed,                                 this,  &SeerArrayVisualizerWidget::handleRefreshButton);
    QObject::connect(variableNameLineEdit,          &QLineEdit::returnPressed,                                 this,  &SeerArrayVisualizerWidget::handleVariableNameLineEdit);
    QObject::connect(arrayDisplayFormatComboBox,    QOverload<int>::of(&QComboBox::currentIndexChanged),       this,  &SeerArrayVisualizerWidget::handleArrayDisplayFormatComboBox);
    QObject::connect(arrayTableWidget,              &SeerArrayWidget::dataChanged,                             this,  &SeerArrayVisualizerWidget::handleDataChanged);
    QObject::connect(splitter,                      &QSplitter::splitterMoved,                                 this,  &SeerArrayVisualizerWidget::handleSplitterMoved);
    QObject::connect(titleLineEdit,                 &QLineEdit::returnPressed,                                 this,  &SeerArrayVisualizerWidget::handleTitleLineEdit);
    QObject::connect(pointsCheckBox,                &QCheckBox::clicked,                                       this,  &SeerArrayVisualizerWidget::handlePointsCheckBox);
    QObject::connect(labelsCheckBox,                &QCheckBox::clicked,                                       this,  &SeerArrayVisualizerWidget::handleLabelsCheckBox);
    QObject::connect(lineTypeButtonGroup,           QOverload<int>::of(&QButtonGroup::idClicked),              this,  &SeerArrayVisualizerWidget::handleLineTypeButtonGroup);

    // Restore window settings.
    readSettings();
}

SeerArrayVisualizerWidget::~SeerArrayVisualizerWidget () {
}

void SeerArrayVisualizerWidget::setVariableName (const QString& name) {

    setWindowTitle("Seer Array Visualizer - '" + name + "'");

    variableNameLineEdit->setText(name);
    setVariableAddress("");

    if (variableNameLineEdit->text() == "") {
        return;
    }

    // Clear old contents.
    QByteArray array;
    bool ok;

    arrayTableWidget->setData(new SeerArrayWidget::DataStorageArray(array));

    if (arrayOffsetLineEdit->text() != "") {
        arrayTableWidget->setAddressOffset(arrayOffsetLineEdit->text().toULong(&ok));
        if (ok == false) {
            qWarning() << "Invalid string for address offset." << arrayOffsetLineEdit->text();
        }
    }else{
        arrayTableWidget->setAddressOffset(0);
    }

    if (arrayStrideLineEdit->text() != "") {
        arrayTableWidget->setAddressStride(arrayStrideLineEdit->text().toULong(&ok));
        if (ok == false) {
            qWarning() << "Invalid string for address stride." << arrayStrideLineEdit->text();
        }
    }else{
        arrayTableWidget->setAddressStride(1);
    }

    // Send signal to get variable address.
    emit evaluateVariableExpression(_variableId, variableNameLineEdit->text());
}

QString SeerArrayVisualizerWidget::variableName () const {
    return variableNameLineEdit->text();
}

void SeerArrayVisualizerWidget::setVariableAddress (const QString& address) {

    if (address.startsWith("0x")) {

        bool ok = false;

        address.toULong(&ok, 16);

        if (ok == true) {
            variableAddressLineEdit->setText(address);
        }else{
            variableAddressLineEdit->setText("not an address");
        }

    }else{
        variableAddressLineEdit->setText("not an address");
    }

    arrayTableWidget->setAddressOffset(0);
}

QString SeerArrayVisualizerWidget::variableAddress () const {
    return variableAddressLineEdit->text();
}

void SeerArrayVisualizerWidget::handleText (const QString& text) {

    //qDebug() << text;

    if (text.contains(QRegExp("^([0-9]+)\\^done,value="))) {

        // 10^done,value="1"
        // 11^done,value="0x7fffffffd538"

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _variableId) {

            QStringList words = Seer::filterEscapes(Seer::parseFirst(text, "value=", '"', '"', false)).split(' ', Qt::SkipEmptyParts);

            setVariableAddress(words.first());
        }

    }else if (text.contains(QRegExp("^([0-9]+)\\^done,memory="))) {

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

                //qDebug() << contents_text;

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
                arrayTableWidget->setData(new SeerArrayWidget::DataStorageArray(array));

                if (arrayOffsetLineEdit->text() != "") {
                    arrayTableWidget->setAddressOffset(arrayOffsetLineEdit->text().toULong(&ok));
                    if (ok == false) {
                        qWarning() << "Invalid string for address offset." << arrayOffsetLineEdit->text();
                    }
                }else{
                    arrayTableWidget->setAddressOffset(0);
                }

                if (arrayStrideLineEdit->text() != "") {
                    arrayTableWidget->setAddressStride(arrayStrideLineEdit->text().toULong(&ok));
                    if (ok == false) {
                        qWarning() << "Invalid string for address stride." << arrayStrideLineEdit->text();
                    }
                }else{
                    arrayTableWidget->setAddressStride(1);
                }

                break; // Take just the first range for now.
            }
        }

    }else if (text.contains(QRegExp("^([0-9]+)\\^error,msg="))) {

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

    }else{
        // Ignore anything else.
    }
}

void SeerArrayVisualizerWidget::handleRefreshButton () {

    if (variableNameLineEdit->text() == "") {
        return;
    }

    if (variableAddressLineEdit->text() == "") {
        return;
    }

    if (variableAddressLineEdit->text() == "not an address") {
        return;
    }

    int bytes = arrayLengthLineEdit->text().toInt() * Seer::typeBytes(arrayDisplayFormatComboBox->currentText());

    //qDebug() << _memoryId << variableAddressLineEdit->text() << arrayLengthLineEdit->text() << arrayDisplayFormatComboBox->currentText() << bytes;

    emit evaluateMemoryExpression(_memoryId, variableAddressLineEdit->text(), bytes);
}

void SeerArrayVisualizerWidget::handleVariableNameLineEdit () {

    setVariableName (variableNameLineEdit->text());
}

void SeerArrayVisualizerWidget::handleArrayDisplayFormatComboBox (int index) {

    //qDebug() << index;

    if (index == 0) {
        arrayTableWidget->setArrayMode(SeerArrayWidget::Int16ArrayMode);

    }else if (index == 1) {
        arrayTableWidget->setArrayMode(SeerArrayWidget::Int32ArrayMode);

    }else if (index == 2) {
        arrayTableWidget->setArrayMode(SeerArrayWidget::Int64ArrayMode);

    }else if (index == 3) {
        arrayTableWidget->setArrayMode(SeerArrayWidget::UInt16ArrayMode);

    }else if (index == 4) {
        arrayTableWidget->setArrayMode(SeerArrayWidget::UInt32ArrayMode);

    }else if (index == 5) {
        arrayTableWidget->setArrayMode(SeerArrayWidget::UInt64ArrayMode);

    }else if (index == 6) {
        arrayTableWidget->setArrayMode(SeerArrayWidget::Float32ArrayMode);

    }else if (index == 7) {
        arrayTableWidget->setArrayMode(SeerArrayWidget::Float64ArrayMode);

    }else{
        // Do nothing.
    }
}

void SeerArrayVisualizerWidget::handleDataChanged () {

    if (_series) {
        arrayChartView->chart()->removeSeries(_series);
        delete _series;
        _series = 0;
    }

    if (scatterRadioButton->isChecked()) {

        QScatterSeries* scatter = new QScatterSeries;
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

    _series->setName(variableName());
    _series->setPointsVisible(false);
    _series->setPointLabelsVisible(false);

    const QVector<double>& values = arrayTableWidget->arrayValues();

    for (int i = 0; i < values.size(); ++i) {
        _series->append(i, values[i]);
    }

    QObject::connect(_series, &QLineSeries::hovered,     this, &SeerArrayVisualizerWidget::handleSeriesHovered);

    arrayChartView->chart()->addSeries(_series);
    arrayChartView->chart()->createDefaultAxes();
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
    }
}

void SeerArrayVisualizerWidget::handleLabelsCheckBox () {

    if (_series) {
        _series->setPointLabelsVisible(labelsCheckBox->isChecked());
    }
}

void SeerArrayVisualizerWidget::handleLineTypeButtonGroup () {

    handleDataChanged();
}


