#include "SeerArrayWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QHeaderView>
#include <QtCore/QDebug>
#include <stdexcept>

SeerArrayWidget::SeerArrayWidget(QWidget* parent) : QTableWidget(parent), _pdata(NULL) {

    QFont font;
    font.setFamily("monospace [Consolas]");
    font.setFixedPitch(true);
    font.setStyleHint(QFont::TypeWriter);

    setFont(font);
    setFocusPolicy(Qt::StrongFocus);
    setEditTriggers(QAbstractItemView::NoEditTriggers);
    horizontalHeader()->setDefaultAlignment(Qt::AlignRight);

    _arrayMode      = SeerArrayWidget::UnknownArrayMode;
    _addressOffset  = 0;
    _addressStride  = 1;

    setElementsPerLine(1);
    setAddressOffset(0);
    setAddressStride(1);
}

SeerArrayWidget::~SeerArrayWidget() {

    if (_pdata) {
        delete _pdata;
    }
}

void SeerArrayWidget::setElementsPerLine (int count) {

    _elementsPerLine = count;

    // Repaint the widget.
    create();
}

int SeerArrayWidget::elementsPerLine () const {

    return _elementsPerLine;
}

void SeerArrayWidget::setAddressOffset (unsigned long offset) {

    _addressOffset = offset;

    // Repaint the widget.
    create();
}

unsigned long SeerArrayWidget::addressOffset () const {

    return _addressOffset;
}

void SeerArrayWidget::setAddressStride (unsigned long stride) {

    if (stride < 1) {
        qWarning() << "Stride is not valid." << stride;
        stride = 1;
    }

    _addressStride = stride;

    // Repaint the widget.
    create();
}

unsigned long SeerArrayWidget::addressStride () const {

    return _addressStride;
}

unsigned long SeerArrayWidget::size () const {

    if (_pdata) {
        return _pdata->size();
    }

    return 0;
}

unsigned long  SeerArrayWidget::elementSize () const {

    if (arrayMode() == SeerArrayWidget::Int16ArrayMode) {
        return 2;
    }else if (arrayMode() == SeerArrayWidget::UInt16ArrayMode) {
        return 2;
    }else if (arrayMode() == SeerArrayWidget::Int32ArrayMode) {
        return 4;
    }else if (arrayMode() == SeerArrayWidget::UInt32ArrayMode) {
        return 4;
    }else if (arrayMode() == SeerArrayWidget::Int64ArrayMode) {
        return 8;
    }else if (arrayMode() == SeerArrayWidget::UInt64ArrayMode) {
        return 8;
    }else if (arrayMode() == SeerArrayWidget::Float32ArrayMode) {
        return 4;
    }else if (arrayMode() == SeerArrayWidget::Float64ArrayMode) {
        return 8;
    }

    return 0;
}


void SeerArrayWidget::setArrayMode (SeerArrayWidget::ArrayMode arrayMode) {

    _arrayMode = arrayMode;

    // This repaints the widget with the new array mode
    setElementsPerLine(elementsPerLine());
}

SeerArrayWidget::ArrayMode SeerArrayWidget::arrayMode () const {
    return _arrayMode;
}

QString SeerArrayWidget::arrayModeString () const {

    if (arrayMode() == SeerArrayWidget::Int16ArrayMode) {
        return "int6";
    }else if (arrayMode() == SeerArrayWidget::UInt16ArrayMode) {
        return "uint16";
    }else if (arrayMode() == SeerArrayWidget::Int32ArrayMode) {
        return "int32";
    }else if (arrayMode() == SeerArrayWidget::UInt32ArrayMode) {
        return "uint32";
    }else if (arrayMode() == SeerArrayWidget::Int64ArrayMode) {
        return "int64";
    }else if (arrayMode() == SeerArrayWidget::UInt64ArrayMode) {
        return "uint64";
    }else if (arrayMode() == SeerArrayWidget::Float32ArrayMode) {
        return "float32";
    }else if (arrayMode() == SeerArrayWidget::Float64ArrayMode) {
        return "float64";
    }

    return "???";
}

const QVector<double>& SeerArrayWidget::arrayValues () const {

    return _arrayValues;
}

void SeerArrayWidget::setData(SeerArrayWidget::DataStorage* pData) {

    if (_pdata) {
        delete _pdata;
    }

    _pdata = pData;

    // Repaint the widget.
    create();
}

void SeerArrayWidget::create () {

    // Clear the table. We're going to recreate it.
    clear();
    setRowCount(0);
    setColumnCount(elementsPerLine());

    // Clear the values.
    _arrayValues.resize(0);

    // If there's no data, do nothing.
    if (!_pdata) {
        emit dataChanged();
        return;
    }

    if (elementSize() < 1) {
        emit dataChanged();
        return;
    }

    int row = 0;
    int col = 0;

    for (int i=elementSize()*addressOffset(); i<_pdata->size(); i+=elementSize()*addressStride()) {

        // Add new row if we need to. Set its label.
        if (row == rowCount()) {
            insertRow(rowCount());

            QTableWidgetItem* rowHeaderitem = new QTableWidgetItem(QString::number(i/elementSize()));
            rowHeaderitem->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);
            setVerticalHeaderItem(row, rowHeaderitem);
        }

        QTableWidgetItem* item = new QTableWidgetItem;
        item->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

        QByteArray element = _pdata->getData(i, elementSize());

        QByteArray hex = element.toHex();
        double     val = 0.0;

        if (arrayMode() == SeerArrayWidget::Int16ArrayMode) {

            short v = *reinterpret_cast<short*>(element.data());

            val = v;

            item->setText(QString::number(v));

        }else if (arrayMode() == SeerArrayWidget::UInt16ArrayMode) {

            unsigned short v = *reinterpret_cast<unsigned short*>(element.data());

            val = v;

            item->setText(QString::number(v));

        }else if (arrayMode() == SeerArrayWidget::Int32ArrayMode) {

            int v = *reinterpret_cast<int*>(element.data());

            val = v;

            item->setText(QString::number(v));

        }else if (arrayMode() == SeerArrayWidget::UInt32ArrayMode) {

            unsigned int v = *reinterpret_cast<unsigned int*>(element.data());

            val = v;

            item->setText(QString::number(v));

        }else if (arrayMode() == SeerArrayWidget::Int64ArrayMode) {

            long v = *reinterpret_cast<long*>(element.data());

            val = v;

            item->setText(QString::number(v));

        }else if (arrayMode() == SeerArrayWidget::UInt64ArrayMode) {

            unsigned long v = *reinterpret_cast<unsigned long*>(element.data());

            val = v;

            item->setText(QString::number(v));

        }else if (arrayMode() == SeerArrayWidget::Float32ArrayMode) {

            float v = *reinterpret_cast<float*>(element.data());

            val = v;

            item->setText(QString::number(v));

        }else if (arrayMode() == SeerArrayWidget::Float64ArrayMode) {

            double v = *reinterpret_cast<double*>(element.data());

            val = v;

            item->setText(QString::number(v));

        }else{
            qWarning() << "Unknown data type.";

            val = 0.0;
        }

        setItem(row, col, item);

        _arrayValues.push_back(val);

        col++;

        if (col >= elementsPerLine()) {
            col = 0;
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

