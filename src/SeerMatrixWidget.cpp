#include "SeerMatrixWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QHeaderView>
#include <QtCore/QDebug>
#include <stdexcept>

SeerMatrixWidget::SeerMatrixWidget(QWidget* parent) : QTableWidget(parent) {

    QFont font;
    font.setFamily("monospace [Consolas]");
    font.setFixedPitch(true);
    font.setStyleHint(QFont::TypeWriter);

    setFont(font);
    setFocusPolicy(Qt::StrongFocus);
    setEditTriggers(QAbstractItemView::NoEditTriggers);

    horizontalHeader()->setDefaultAlignment(Qt::AlignRight);

    _data           = 0;
    _dataRows       = 0;
    _dataColumns    = 0;
    _dataType       = SeerMatrixWidget::UnknownMatrixType;
    _addressOffset  = 0;
    _addressStride  = 1;

    setAddressOffset(0);
    setAddressStride(1);
}

SeerMatrixWidget::~SeerMatrixWidget() {

    if (_data) {
        delete _data;
    }
}

void SeerMatrixWidget::setAddressOffset (unsigned long offset) {

    _addressOffset = offset;

    // Repaint the widget.
    create();
}

unsigned long SeerMatrixWidget::addressOffset () const {

    return _addressOffset;
}

void SeerMatrixWidget::setAddressStride (unsigned long stride) {

    if (stride < 1) {
        qWarning() << "Stride is not valid." << stride;
        stride = 1;
    }

    _addressStride = stride;

    // Repaint the widget.
    create();
}

unsigned long SeerMatrixWidget::addressStride () const {

    return _addressStride;
}

int SeerMatrixWidget::dataRows () const {
    return _dataRows;
}

int SeerMatrixWidget::dataColumns () const {
    return _dataColumns;
}

unsigned long SeerMatrixWidget::dataSize () const {

    if (_data) {
        return _data->size();
    }

    return 0;
}

int SeerMatrixWidget::dataCount () const {

    if (_data == 0) {
        return 0;
    }

    if (elementSize() < 1) {
        return 0;
    }

    return _data->size() / elementSize();
}

unsigned long  SeerMatrixWidget::elementSize () const {

    if (dataType() == SeerMatrixWidget::Int16MatrixType) {
        return 2;
    }else if (dataType() == SeerMatrixWidget::UInt16MatrixType) {
        return 2;
    }else if (dataType() == SeerMatrixWidget::Int32MatrixType) {
        return 4;
    }else if (dataType() == SeerMatrixWidget::UInt32MatrixType) {
        return 4;
    }else if (dataType() == SeerMatrixWidget::Int64MatrixType) {
        return 8;
    }else if (dataType() == SeerMatrixWidget::UInt64MatrixType) {
        return 8;
    }else if (dataType() == SeerMatrixWidget::Float32MatrixType) {
        return 4;
    }else if (dataType() == SeerMatrixWidget::Float64MatrixType) {
        return 8;
    }

    return 0;
}

void SeerMatrixWidget::setDataType (SeerMatrixWidget::MatrixType dataType) {

    _dataType = dataType;

    // This repaints the widget with the new matrix mode
    create();
}

SeerMatrixWidget::MatrixType SeerMatrixWidget::dataType () const {
    return _dataType;
}

QString SeerMatrixWidget::dataTypeString () const {

    if (dataType() == SeerMatrixWidget::Int16MatrixType) {
        return "int8";
    }else if (dataType() == SeerMatrixWidget::UInt16MatrixType) {
        return "uint16";
    }else if (dataType() == SeerMatrixWidget::Int32MatrixType) {
        return "int32";
    }else if (dataType() == SeerMatrixWidget::UInt32MatrixType) {
        return "uint32";
    }else if (dataType() == SeerMatrixWidget::Int64MatrixType) {
        return "int64";
    }else if (dataType() == SeerMatrixWidget::UInt64MatrixType) {
        return "uint64";
    }else if (dataType() == SeerMatrixWidget::Float32MatrixType) {
        return "float32";
    }else if (dataType() == SeerMatrixWidget::Float64MatrixType) {
        return "float64";
    }

    return "???";
}

const QVector<double>& SeerMatrixWidget::dataValues () const {

    return _dataValues;
}

void SeerMatrixWidget::setData(SeerMatrixWidget::DataStorage* pData) {

    if (_data) {
        delete _data;
    }

    /*
    if (pData == 0) {
        qDebug() << "Setting new data with no size. Dimensions are reset. Assign new data and set them again!";
    }else{
        qDebug() << "Setting new data with size of" << pData->size() << ". Dimensions are reset. Set them again!";
    }
    */

    _data        = pData;
    _dataRows    = 0;
    _dataColumns = 0;

    // Repaint the widget.
    create();
}

void SeerMatrixWidget::setDimensions (int rows, int colums) {

    // qDebug() << "Setting dimensions to" << rows << "x" << colums;

    _dataRows    = rows;
    _dataColumns = colums;

    // Repaint the widget.
    create();
}

void SeerMatrixWidget::create () {

    // Clear the table. We're going to recreate it.
    clear();
    setRowCount(0);
    setColumnCount(0);

    if (_data) {
        // qDebug() << "Row:" << dataRows() << "Cols:" << dataColumns();
        setRowCount(dataRows());
        setColumnCount(dataColumns());
    }

    // If the number of rows/columns have not been assigned, return.
    if (dataRows() < 1 || dataColumns() < 1) {
        // qDebug() << "Zero rows or cols. Row:" << dataRows() << "Cols:" << dataColumns();
        emit dataChanged();
        return;
    }

    // Clear the values.
    _dataValues.resize(0);

    // If there's no data, do nothing.
    if (!_data) {
        // qDebug() << "No _data";
        emit dataChanged();
        return;
    }

    if (elementSize() < 1) {
        // qDebug() << "Bad element size." << elementSize();
        emit dataChanged();
        return;
    }

    if (_data) {

        int row = 0;
        int col = 0;

        // setHorizontalHeaderItem(_aColumnId, new QTableWidgetItem(QString("%1:%2:%3").arg(aLabel()).arg(aAddressOffset()).arg(aAddressStride())));

        for (int i=elementSize()*addressOffset(); i<_data->size(); i+=elementSize()*addressStride()) {

            // qDebug() << "Displaying element" << i;

            // Set column header if we need to. Set its label.
            if (row == 0) {
                for (int c=0; c < dataColumns(); c++) {
                    QTableWidgetItem* colHeaderitem = new QTableWidgetItem(QString::number(c));
                    colHeaderitem->setTextAlignment(Qt::AlignRight|Qt::AlignHCenter);

                    setHorizontalHeaderItem(c, colHeaderitem);
                }
            }

            // Set row header if we need to. Set its label.
            if (col == 0) {

                QTableWidgetItem* rowHeaderitem = new QTableWidgetItem(QString::number(row));
                rowHeaderitem->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

                setVerticalHeaderItem(row, rowHeaderitem);
            }

            // Set cell item.
            QTableWidgetItem* item = new QTableWidgetItem;
            item->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

            QByteArray element = _data->getData(i, elementSize());

            double val = 0.0;

            if (dataType() == SeerMatrixWidget::Int16MatrixType) {

                val = *reinterpret_cast<short*>(element.data());

            }else if (dataType() == SeerMatrixWidget::UInt16MatrixType) {

                val = *reinterpret_cast<unsigned short*>(element.data());

            }else if (dataType() == SeerMatrixWidget::Int32MatrixType) {

                val = *reinterpret_cast<int*>(element.data());

            }else if (dataType() == SeerMatrixWidget::UInt32MatrixType) {

                val = *reinterpret_cast<unsigned int*>(element.data());

            }else if (dataType() == SeerMatrixWidget::Int64MatrixType) {

                val = *reinterpret_cast<long*>(element.data());

            }else if (dataType() == SeerMatrixWidget::UInt64MatrixType) {

                val = *reinterpret_cast<unsigned long*>(element.data());

            }else if (dataType() == SeerMatrixWidget::Float32MatrixType) {

                val = *reinterpret_cast<float*>(element.data());

            }else if (dataType() == SeerMatrixWidget::Float64MatrixType) {

                val = *reinterpret_cast<double*>(element.data());

            }else{
                qWarning() << "Unknown data type.";

                val = 0.0;
            }

            item->setText(QString::number(val));
            item->setToolTip(QString("(%1,%2) = %3").arg(row).arg(col).arg(item->text()));

            setItem(row, col, item);

            _dataValues.push_back(val);

            col++;
            if (col == dataColumns()) {
                col = 0;
                row++;
            }
        }
    }

    emit dataChanged();
}

SeerMatrixWidget::DataStorageArray::DataStorageArray(const QByteArray& arr) {
    _data = arr;
}

QByteArray SeerMatrixWidget::DataStorageArray::getData(int position, int length) {
    return _data.mid(position, length);
}

int SeerMatrixWidget::DataStorageArray::size() {
    return _data.size();
}

