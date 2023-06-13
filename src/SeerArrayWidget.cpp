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

    _aData           = 0;
    _aAxis           = "Y";
    _aArrayMode      = SeerArrayWidget::UnknownArrayMode;
    _aAddressOffset  = 0;
    _aAddressStride  = 1;

    _bData           = 0;
    _bAxis           = "X";
    _bArrayMode      = SeerArrayWidget::UnknownArrayMode;
    _bAddressOffset  = 0;
    _bAddressStride  = 1;

    setAAddressOffset(0);
    setAAddressStride(1);

    setBAddressOffset(0);
    setBAddressStride(1);
}

SeerArrayWidget::~SeerArrayWidget() {

    if (_aData) {
        delete _aData;
    }

    if (_bData) {
        delete _bData;
    }
}

int SeerArrayWidget::elementsPerLine () const {
    return 1;
}

const QString& SeerArrayWidget::aAxis () const {
    return _aAxis;
}

void SeerArrayWidget::setAAxis (const QString& axis) {
    _aAxis = axis;
}

const QString& SeerArrayWidget::aLabel () const {
    return _aLabel;
}

void SeerArrayWidget::setAAddressOffset (unsigned long offset) {

    _aAddressOffset = offset;

    // Repaint the widget.
    create();
}

unsigned long SeerArrayWidget::aAddressOffset () const {

    return _aAddressOffset;
}

void SeerArrayWidget::setAAddressStride (unsigned long stride) {

    if (stride < 1) {
        qWarning() << "Stride is not valid." << stride;
        stride = 1;
    }

    _aAddressStride = stride;

    // Repaint the widget.
    create();
}

unsigned long SeerArrayWidget::aAddressStride () const {

    return _aAddressStride;
}

unsigned long SeerArrayWidget::aSize () const {

    if (_aData) {
        return _aData->size();
    }

    return 0;
}

unsigned long  SeerArrayWidget::aElementSize () const {

    if (aArrayMode() == SeerArrayWidget::Int16ArrayMode) {
        return 2;
    }else if (aArrayMode() == SeerArrayWidget::UInt16ArrayMode) {
        return 2;
    }else if (aArrayMode() == SeerArrayWidget::Int32ArrayMode) {
        return 4;
    }else if (aArrayMode() == SeerArrayWidget::UInt32ArrayMode) {
        return 4;
    }else if (aArrayMode() == SeerArrayWidget::Int64ArrayMode) {
        return 8;
    }else if (aArrayMode() == SeerArrayWidget::UInt64ArrayMode) {
        return 8;
    }else if (aArrayMode() == SeerArrayWidget::Float32ArrayMode) {
        return 4;
    }else if (aArrayMode() == SeerArrayWidget::Float64ArrayMode) {
        return 8;
    }

    return 0;
}

void SeerArrayWidget::setAArrayMode (SeerArrayWidget::ArrayMode arrayMode) {

    _aArrayMode = arrayMode;

    // This repaints the widget with the new array mode
    create();
}

SeerArrayWidget::ArrayMode SeerArrayWidget::aArrayMode () const {
    return _aArrayMode;
}

QString SeerArrayWidget::aArrayModeString () const {

    if (aArrayMode() == SeerArrayWidget::Int16ArrayMode) {
        return "int6";
    }else if (aArrayMode() == SeerArrayWidget::UInt16ArrayMode) {
        return "uint16";
    }else if (aArrayMode() == SeerArrayWidget::Int32ArrayMode) {
        return "int32";
    }else if (aArrayMode() == SeerArrayWidget::UInt32ArrayMode) {
        return "uint32";
    }else if (aArrayMode() == SeerArrayWidget::Int64ArrayMode) {
        return "int64";
    }else if (aArrayMode() == SeerArrayWidget::UInt64ArrayMode) {
        return "uint64";
    }else if (aArrayMode() == SeerArrayWidget::Float32ArrayMode) {
        return "float32";
    }else if (aArrayMode() == SeerArrayWidget::Float64ArrayMode) {
        return "float64";
    }

    return "???";
}

const QVector<double>& SeerArrayWidget::aArrayValues () const {

    return _aArrayValues;
}

const QString& SeerArrayWidget::bAxis () const {
    return _bAxis;
}

void SeerArrayWidget::setBAxis (const QString& axis) {
    _bAxis = axis;
}

const QString& SeerArrayWidget::bLabel () const {
    return _bLabel;
}

void SeerArrayWidget::setBAddressOffset (unsigned long offset) {

    _bAddressOffset = offset;

    // Repaint the widget.
    create();
}

unsigned long SeerArrayWidget::bAddressOffset () const {

    return _bAddressOffset;
}

void SeerArrayWidget::setBAddressStride (unsigned long stride) {

    if (stride < 1) {
        qWarning() << "Stride is not valid." << stride;
        stride = 1;
    }

    _bAddressStride = stride;

    // Repaint the widget.
    create();
}

unsigned long SeerArrayWidget::bAddressStride () const {

    return _bAddressStride;
}

unsigned long SeerArrayWidget::bSize () const {

    if (_bData) {
        return _bData->size();
    }

    return 0;
}

unsigned long  SeerArrayWidget::bElementSize () const {

    if (bArrayMode() == SeerArrayWidget::Int16ArrayMode) {
        return 2;
    }else if (bArrayMode() == SeerArrayWidget::UInt16ArrayMode) {
        return 2;
    }else if (bArrayMode() == SeerArrayWidget::Int32ArrayMode) {
        return 4;
    }else if (bArrayMode() == SeerArrayWidget::UInt32ArrayMode) {
        return 4;
    }else if (bArrayMode() == SeerArrayWidget::Int64ArrayMode) {
        return 8;
    }else if (bArrayMode() == SeerArrayWidget::UInt64ArrayMode) {
        return 8;
    }else if (bArrayMode() == SeerArrayWidget::Float32ArrayMode) {
        return 4;
    }else if (bArrayMode() == SeerArrayWidget::Float64ArrayMode) {
        return 8;
    }

    return 0;
}

void SeerArrayWidget::setBArrayMode (SeerArrayWidget::ArrayMode arrayMode) {

    _bArrayMode = arrayMode;

    // This repaints the widget with the new array mode
    create();
}

SeerArrayWidget::ArrayMode SeerArrayWidget::bArrayMode () const {
    return _bArrayMode;
}

QString SeerArrayWidget::bArrayModeString () const {

    if (bArrayMode() == SeerArrayWidget::Int16ArrayMode) {
        return "int6";
    }else if (bArrayMode() == SeerArrayWidget::UInt16ArrayMode) {
        return "uint16";
    }else if (bArrayMode() == SeerArrayWidget::Int32ArrayMode) {
        return "int32";
    }else if (bArrayMode() == SeerArrayWidget::UInt32ArrayMode) {
        return "uint32";
    }else if (bArrayMode() == SeerArrayWidget::Int64ArrayMode) {
        return "int64";
    }else if (bArrayMode() == SeerArrayWidget::UInt64ArrayMode) {
        return "uint64";
    }else if (bArrayMode() == SeerArrayWidget::Float32ArrayMode) {
        return "float32";
    }else if (bArrayMode() == SeerArrayWidget::Float64ArrayMode) {
        return "float64";
    }

    return "???";
}

const QVector<double>& SeerArrayWidget::bArrayValues () const {

    return _bArrayValues;
}


void SeerArrayWidget::setAData(const QString& label, SeerArrayWidget::DataStorage* pData) {

    if (_aData) {
        delete _aData;
    }

    _aLabel = label;
    _aData  = pData;

    // Repaint the widget.
    create();
}

void SeerArrayWidget::setBData(const QString& label, SeerArrayWidget::DataStorage* pData) {

    if (_bData) {
        delete _bData;
    }

    _bLabel = label;
    _bData  = pData;

    // Repaint the widget.
    create();
}

void SeerArrayWidget::create () {

    // Clear the table. We're going to recreate it.
    clear();
    setRowCount(0);

    if (_aData && _bData) {

        setColumnCount(2);

        _aColumnId = 0;
        _bColumnId = 1;

    }else if (_aData) {

        setColumnCount(1);

        _aColumnId = 0;
        _bColumnId = -1;

    }else if (_bData) {

        setColumnCount(1);

        _aColumnId = -1;
        _bColumnId = 0;

    }else{
        setColumnCount(0);

        _aColumnId = -1;
        _bColumnId = -1;
    }

    // Clear the values.
    _aArrayValues.resize(0);
    _bArrayValues.resize(0);

    // If there's no data, do nothing.
    if (!_aData && !_bData) {
        emit dataChanged();
        return;
    }

    if (aElementSize() < 1 && bElementSize() < 1) {
        emit dataChanged();
        return;
    }

    if (_aData) {

        //qDebug() << "_aData" << aElementSize() << aAddressOffset() << aAddressStride() << _aData->size();

        setHorizontalHeaderItem(_aColumnId, new QTableWidgetItem(QString("%1:%2:%3").arg(aLabel()).arg(aAddressOffset()).arg(aAddressStride())));

        int row = 0;

        for (int i=aElementSize()*aAddressOffset(); i<_aData->size(); i+=aElementSize()*aAddressStride()) {

            // Add new row if we need to. Set its label.
            if (row == rowCount()) {

                //qDebug() << "Adding row" << row << "for _aData";

                insertRow(rowCount());

                QTableWidgetItem* rowHeaderitem = new QTableWidgetItem(QString::number(row));
                rowHeaderitem->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

                setVerticalHeaderItem(row, rowHeaderitem);
            }

            QTableWidgetItem* item = new QTableWidgetItem;
            item->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

            QByteArray element = _aData->getData(i, aElementSize());

            double val = 0.0;

            if (aArrayMode() == SeerArrayWidget::Int16ArrayMode) {

                short v = *reinterpret_cast<short*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (aArrayMode() == SeerArrayWidget::UInt16ArrayMode) {

                unsigned short v = *reinterpret_cast<unsigned short*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (aArrayMode() == SeerArrayWidget::Int32ArrayMode) {

                int v = *reinterpret_cast<int*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (aArrayMode() == SeerArrayWidget::UInt32ArrayMode) {

                unsigned int v = *reinterpret_cast<unsigned int*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (aArrayMode() == SeerArrayWidget::Int64ArrayMode) {

                long v = *reinterpret_cast<long*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (aArrayMode() == SeerArrayWidget::UInt64ArrayMode) {

                unsigned long v = *reinterpret_cast<unsigned long*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (aArrayMode() == SeerArrayWidget::Float32ArrayMode) {

                float v = *reinterpret_cast<float*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (aArrayMode() == SeerArrayWidget::Float64ArrayMode) {

                double v = *reinterpret_cast<double*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else{
                qWarning() << "Unknown data type.";

                val = 0.0;
            }

            //qDebug() << "Adding item" << row << 0 << "for _aData";

            setItem(row, _aColumnId, item);

            _aArrayValues.push_back(val);

            row++;
        }
    }

    if (_bData) {

        //qDebug() << "_bData" << bElementSize() << bAddressOffset() << bAddressStride() << _bData->size();

        setHorizontalHeaderItem(_bColumnId, new QTableWidgetItem(QString("%1:%2:%3").arg(bLabel()).arg(bAddressOffset()).arg(bAddressStride())));

        int row = 0;

        for (int i=bElementSize()*bAddressOffset(); i<_bData->size(); i+=bElementSize()*bAddressStride()) {

            // Add new row if we need to. Set its label.
            if (row == rowCount()) {

                //qDebug() << "Adding row" << row << "for _bData";

                insertRow(rowCount());

                QTableWidgetItem* rowHeaderitem = new QTableWidgetItem(QString::number(row));
                rowHeaderitem->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

                setVerticalHeaderItem(row, rowHeaderitem);
            }

            QTableWidgetItem* item = new QTableWidgetItem;
            item->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

            QByteArray element = _bData->getData(i, bElementSize());

            double val = 0.0;

            if (bArrayMode() == SeerArrayWidget::Int16ArrayMode) {

                short v = *reinterpret_cast<short*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (bArrayMode() == SeerArrayWidget::UInt16ArrayMode) {

                unsigned short v = *reinterpret_cast<unsigned short*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (bArrayMode() == SeerArrayWidget::Int32ArrayMode) {

                int v = *reinterpret_cast<int*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (bArrayMode() == SeerArrayWidget::UInt32ArrayMode) {

                unsigned int v = *reinterpret_cast<unsigned int*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (bArrayMode() == SeerArrayWidget::Int64ArrayMode) {

                long v = *reinterpret_cast<long*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (bArrayMode() == SeerArrayWidget::UInt64ArrayMode) {

                unsigned long v = *reinterpret_cast<unsigned long*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (bArrayMode() == SeerArrayWidget::Float32ArrayMode) {

                float v = *reinterpret_cast<float*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else if (bArrayMode() == SeerArrayWidget::Float64ArrayMode) {

                double v = *reinterpret_cast<double*>(element.data());

                val = v;

                item->setText(QString::number(v));

            }else{
                qWarning() << "Unknown data type.";

                val = 0.0;
            }

            //qDebug() << "Adding item" << row << 0 << "for _bData";

            setItem(row, _bColumnId, item);

            _bArrayValues.push_back(val);

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
    return _data.size();
}

