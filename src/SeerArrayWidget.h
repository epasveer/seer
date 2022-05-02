#pragma once

#include <QtWidgets/QTableWidget>
#include <QtCore/QByteArray>
#include <QtCore/QVector>

class SeerArrayWidget: public QTableWidget {

    Q_OBJECT

    public:
        class DataStorage {
            public:
                virtual ~DataStorage() {};
                virtual QByteArray getData(int position, int length) = 0;
                virtual int size() = 0;
        };

        class DataStorageArray: public DataStorage {
            public:
                DataStorageArray(const QByteArray& arr);
                virtual QByteArray getData(int position, int length);
                virtual int size();
            private:
                QByteArray _data;
        };

        enum ArrayMode {
            UnknownArrayMode    = 0,
            Int16ArrayMode      = 1,
            UInt16ArrayMode     = 2,
            Int32ArrayMode      = 3,
            UInt32ArrayMode     = 4,
            Int64ArrayMode      = 5,
            UInt64ArrayMode     = 6,
            Float32ArrayMode    = 7,
            Float64ArrayMode    = 8
        };

        SeerArrayWidget(QWidget* parent = 0);
       ~SeerArrayWidget();

        int                         elementsPerLine         () const;

        void                        setXAddressOffset       (unsigned long offset);
        unsigned long               xAddressOffset          () const;
        void                        setXAddressStride       (unsigned long stride);
        unsigned long               xAddressStride          () const;
        unsigned long               xSize                   () const;
        unsigned long               xElementSize            () const;

        void                        setXArrayMode           (SeerArrayWidget::ArrayMode arrayMode);
        SeerArrayWidget::ArrayMode  xArrayMode              () const;
        QString                     xArrayModeString        () const;
        const QVector<double>&      xArrayValues            () const;

        void                        setYAddressOffset       (unsigned long offset);
        unsigned long               yAddressOffset          () const;
        void                        setYAddressStride       (unsigned long stride);
        unsigned long               yAddressStride          () const;
        unsigned long               ySize                   () const;
        unsigned long               yElementSize            () const;

        void                        setYArrayMode           (SeerArrayWidget::ArrayMode arrayMode);
        SeerArrayWidget::ArrayMode  yArrayMode              () const;
        QString                     yArrayModeString        () const;
        const QVector<double>&      yArrayValues            () const;


    signals:
        void                        dataChanged             ();

    public slots:
        void                        setXData                (DataStorage* pData);
        void                        setYData                (DataStorage* pData);

    protected:

    private:
        void                        create                  ();

        DataStorage*                _xData;
        unsigned long               _xAddressOffset;
        unsigned long               _xAddressStride;
        SeerArrayWidget::ArrayMode  _xArrayMode;
        QVector<double>             _xArrayValues;

        DataStorage*                _yData;
        unsigned long               _yAddressOffset;
        unsigned long               _yAddressStride;
        SeerArrayWidget::ArrayMode  _yArrayMode;
        QVector<double>             _yArrayValues;
};

