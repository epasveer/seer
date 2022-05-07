#include "SeerArrayWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QHeaderView>
#include <QtCore/QDebug>
#include <stdexcept>

SeerArrayWidget::SeerArrayWidget(QWidget* parent) : QTableWidget(parent) {

    QFont font;
    font.setFamily("monospace [Consolas]");
    font.setFixedPitch(true);
    font.setStyleHint(QFont::TypeWriter);

    setFont(font);
    setFocusPolicy(Qt::StrongFocus);
    setEditTriggers(QAbstractItemView::NoEditTriggers);

    horizontalHeader()->setDefaultAlignment(Qt::AlignRight);

    _xData           = 0;
    _xArrayMode      = SeerArrayWidget::UnknownArrayMode;
    _xAddressOffset  = 0;
    _xAddressStride  = 1;

    _yData           = 0;
    _yArrayMode      = SeerArrayWidget::UnknownArrayMode;
    _yAddressOffset  = 0;
    _yAddressStride  = 1;

    setXAddressOffset(0);
    setXAddressStride(1);

    setYAddressOffset(0);
    setYAddressStride(1);
}

SeerArrayWidget::~SeerArrayWidget() {

    if (_xData) {
        delete _xData;
    }

    if (_yData) {
        delete _yData;
    }
}

int SeerArrayWidget::elementsPerLine () const {
    return 1;
}

void SeerArrayWidget::setXAddressOffset (unsigned long offset) {

    _xAddressOffset = offset;

    // Repaint the widget.
    create();
}

unsigned long SeerArrayWidget::xAddressOffset () const {

    return _xAddressOffset;
}

void SeerArrayWidget::setXAddressStride (unsigned long stride) {

    if (stride < 1) {
        qWarning() << "Stride is not valid." << stride;
        stride = 1;
    }

    _xAddressStride = stride;

    // Repaint the widget.
    create();
}

unsigned long SeerArrayWidget::xAddressStride () const {

    return _xAddressStride;
}

unsigned long SeerArrayWidget::xSize () const {

    if (_xData) {
        return _xData->size();
    }

    return 0;
}

unsigned long  SeerArrayWidget::xElementSize () const {

    if (xArrayMode() == SeerArrayWidget::Int16ArrayMode) {
        return 2;
    }else if (xArrayMode() == SeerArrayWidget::UInt16ArrayMode) {
        return 2;
    }else if (xArrayMode() == SeerArrayWidget::Int32ArrayMode) {
        return 4;
    }else if (xArrayMode() == SeerArrayWidget::UInt32ArrayMode) {
        return 4;
    }else if (xArrayMode() == SeerArrayWidget::Int64ArrayMode) {
        return 8;
    }else if (xArrayMode() == SeerArrayWidget::UInt64ArrayMode) {
        return 8;
    }else if (xArrayMode() == SeerArrayWidget::Float32ArrayMode) {
        return 4;
    }else if (xArrayMode() == SeerArrayWidget::Float64ArrayMode) {
        return 8;
    }

    return 0;
}

void SeerArrayWidget::setXArrayMode (SeerArrayWidget::ArrayMode arrayMode) {

    _xArrayMode = arrayMode;

    // This repaints the widget with the new array mode
    create();
}

SeerArrayWidget::ArrayMode SeerArrayWidget::xArrayMode () const {
    return _xArrayMode;
}

QString SeerArrayWidget::xArrayModeString () const {

    if (xArrayMode() == SeerArrayWidget::Int16ArrayMode) {
        return "int6";
    }else if (xArrayMode() == SeerArrayWidget::UInt16ArrayMode) {
        return "uint16";
    }else if (xArrayMode() == SeerArrayWidget::Int32ArrayMode) {
        return "int32";
    }else if (xArrayMode() == SeerArrayWidget::UInt32ArrayMode) {
        return "uint32";
    }else if (xArrayMode() == SeerArrayWidget::Int64ArrayMode) {
        return "int64";
    }else if (xArrayMode() == SeerArrayWidget::UInt64ArrayMode) {
        return "uint64";
    }else if (xArrayMode() == SeerArrayWidget::Float32ArrayMode) {
        return "float32";
    }else if (xArrayMode() == SeerArrayWidget::Float64ArrayMode) {
        return "float64";
    }

    return "???";
}

const QVector<double>& SeerArrayWidget::xArrayValues () const {

    return _xArrayValues;
}

void SeerArrayWidget::setYAddressOffset (unsigned long offset) {

    _yAddressOffset = offset;

    // Repaint the widget.
    create();
}

unsigned long SeerArrayWidget::yAddressOffset () const {

    return _yAddressOffset;
}

void SeerArrayWidget::setYAddressStride (unsigned long stride) {

    if (stride < 1) {
        qWarning() << "Stride is not valid." << stride;
        stride = 1;
    }

    _yAddressStride = stride;

    // Repaint the widget.
    create();
}

unsigned long SeerArrayWidget::yAddressStride () const {

    return _yAddressStride;
}

unsigned long SeerArrayWidget::ySize () const {

    if (_yData) {
        return _yData->size();
    }

    return 0;
}

unsigned long  SeerArrayWidget::yElementSize () const {

    if (yArrayMode() == SeerArrayWidget::Int16ArrayMode) {
        return 2;
    }else if (yArrayMode() == SeerArrayWidget::UInt16ArrayMode) {
        return 2;
    }else if (yArrayMode() == SeerArrayWidget::Int32ArrayMode) {
        return 4;
    }else if (yArrayMode() == SeerArrayWidget::UInt32ArrayMode) {
        return 4;
    }else if (yArrayMode() == SeerArrayWidget::Int64ArrayMode) {
        return 8;
    }else if (yArrayMode() == SeerArrayWidget::UInt64ArrayMode) {
        return 8;
    }else if (yArrayMode() == SeerArrayWidget::Float32ArrayMode) {
        return 4;
    }else if (yArrayMode() == SeerArrayWidget::Float64ArrayMode) {
        return 8;
    }

    return 0;
}

void SeerArrayWidget::setYArrayMode (SeerArrayWidget::ArrayMode arrayMode) {

    _yArrayMode = arrayMode;

    // This repaints the widget with the new array mode
    create();
}

SeerArrayWidget::ArrayMode SeerArrayWidget::yArrayMode () const {
    return _yArrayMode;
}

QString SeerArrayWidget::yArrayModeString () const {

    if (yArrayMode() == SeerArrayWidget::Int16ArrayMode) {
        return "int6";
    }else if (yArrayMode() == SeerArrayWidget::UInt16ArrayMode) {
        return "uint16";
    }else if (yArrayMode() == SeerArrayWidget::Int32ArrayMode) {
        return "int32";
    }else if (yArrayMode() == SeerArrayWidget::UInt32ArrayMode) {
        return "uint32";
    }else if (yArrayMode() == SeerArrayWidget::Int64ArrayMode) {
        return "int64";
    }else if (yArrayMode() == SeerArrayWidget::UInt64ArrayMode) {
        return "uint64";
    }else if (yArrayMode() == SeerArrayWidget::Float32ArrayMode) {
        return "float32";
    }else if (yArrayMode() == SeerArrayWidget::Float64ArrayMode) {
        return "float64";
    }

    return "???";
}

const QVector<double>& SeerArrayWidget::yArrayValues () const {

    return _yArrayValues;
}


void SeerArrayWidget::setXData(SeerArrayWidget::DataStorage* pData) {

    if (_xData) {
        delete _xData;
    }

    _xData = pData;

    // Repaint the widget.
    create();
}

void SeerArrayWidget::setYData(SeerArrayWidget::DataStorage* pData) {

    if (_yData) {
        delete _yData;
    }

    _yData = pData;

    // Repaint the widget.
    create();
}

void SeerArrayWidget::create () {

    // Clear the table. We're going to recreate it.
    clear();
    setRowCount(0);

    if (_yData) {
        setColumnCount(2);
    }else{
        setColumnCount(1);
    }

    // Clear the values.
    _xArrayValues.resize(0);
    _yArrayValues.resize(0);

    // If there's no data, do nothing.
    if (!_xData && !_yData) {
        emit dataChanged();
        return;
    }

    if (xElementSize() < 1 && yElementSize() < 1) {
        emit dataChanged();
        return;
    }

    if (_xData) {

        qDebug() << "_xData" << xElementSize() << xAddressOffset() << xAddressStride() << _xData->size();

        int row = 0;

        for (int i=xElementSize()*xAddressOffset(); i<_xData->size(); i+=xElementSize()*xAddressStride()) {

            // Add new row if we need to. Set its label.
            if (row == rowCount()) {

                qDebug() << "Adding row" << row << "for _xData";

                insertRow(rowCount());

                QTableWidgetItem* rowHeaderitem = new QTableWidgetItem(QString::number(i/xElementSize()));
                rowHeaderitem->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

                setVerticalHeaderItem(row, rowHeaderitem);
            }

            QTableWidgetItem* item = new QTableWidgetItem;
            item->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

            QByteArray element = _xData->getData(i, xElementSize());

            double val = 0.0;

            if (xArrayMode() == SeerArrayWidget::Int16ArrayMode) {

                short v = *reinterpret_cast<short*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (xArrayMode() == SeerArrayWidget::UInt16ArrayMode) {

                unsigned short v = *reinterpret_cast<unsigned short*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (xArrayMode() == SeerArrayWidget::Int32ArrayMode) {

                int v = *reinterpret_cast<int*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (xArrayMode() == SeerArrayWidget::UInt32ArrayMode) {

                unsigned int v = *reinterpret_cast<unsigned int*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (xArrayMode() == SeerArrayWidget::Int64ArrayMode) {

                long v = *reinterpret_cast<long*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (xArrayMode() == SeerArrayWidget::UInt64ArrayMode) {

                unsigned long v = *reinterpret_cast<unsigned long*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (xArrayMode() == SeerArrayWidget::Float32ArrayMode) {

                float v = *reinterpret_cast<float*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (xArrayMode() == SeerArrayWidget::Float64ArrayMode) {

                double v = *reinterpret_cast<double*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else{
                qWarning() << "Unknown data type.";

                val = 0.0;
            }

            qDebug() << "Adding item" << row << 0 << "for _xData";

            setItem(row, 0, item);

            _xArrayValues.push_back(val);

            row++;
        }
    }

    if (_yData) {

        qDebug() << "_yData" << yElementSize() << yAddressOffset() << yAddressStride() << _yData->size();

        int row = 0;

        for (int i=yElementSize()*yAddressOffset(); i<_yData->size(); i+=yElementSize()*yAddressStride()) {

            // Add new row if we need to. Set its label.
            if (row == rowCount()) {

                qDebug() << "Adding row" << row << "for _yData";

                insertRow(rowCount());

                QTableWidgetItem* rowHeaderitem = new QTableWidgetItem(QString::number(i/yElementSize()));
                rowHeaderitem->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

                setVerticalHeaderItem(row, rowHeaderitem);
            }

            QTableWidgetItem* item = new QTableWidgetItem;
            item->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

            QByteArray element = _yData->getData(i, yElementSize());

            double val = 0.0;

            if (yArrayMode() == SeerArrayWidget::Int16ArrayMode) {

                short v = *reinterpret_cast<short*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (yArrayMode() == SeerArrayWidget::UInt16ArrayMode) {

                unsigned short v = *reinterpret_cast<unsigned short*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (yArrayMode() == SeerArrayWidget::Int32ArrayMode) {

                int v = *reinterpret_cast<int*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (yArrayMode() == SeerArrayWidget::UInt32ArrayMode) {

                unsigned int v = *reinterpret_cast<unsigned int*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (yArrayMode() == SeerArrayWidget::Int64ArrayMode) {

                long v = *reinterpret_cast<long*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (yArrayMode() == SeerArrayWidget::UInt64ArrayMode) {

                unsigned long v = *reinterpret_cast<unsigned long*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (yArrayMode() == SeerArrayWidget::Float32ArrayMode) {

                float v = *reinterpret_cast<float*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (yArrayMode() == SeerArrayWidget::Float64ArrayMode) {

                double v = *reinterpret_cast<double*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else{
                qWarning() << "Unknown data type.";

                val = 0.0;
            }

            qDebug() << "Adding item" << row << 0 << "for _yData";

            setItem(row, 1, item);

            _yArrayValues.push_back(val);

            row++;
        }
    }

    emit dataChanged();
}

SeerArrayWidget::DataStorageArray::DataStorageArray(const QByteArray& arr) {
    _data = arr;
}

QByteArray SeerArrayWidget::DataStorageArray::getData(int position, int length) {
    return _data.mid(position, length);
}

int SeerArrayWidget::DataStorageArray::size() {
    return _data.count();
}

