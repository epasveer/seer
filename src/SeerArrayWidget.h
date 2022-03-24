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

        void                        setElementsPerLine      (int count);
        int                         elementsPerLine         () const;
        void                        setAddressOffset        (unsigned long offset);
        unsigned long               addressOffset           () const;
        void                        setAddressStride        (unsigned long stride);
        unsigned long               addressStride           () const;
        unsigned long               size                    () const;
        unsigned long               elementSize             () const;

        void                        setArrayMode            (SeerArrayWidget::ArrayMode arrayMode);
        SeerArrayWidget::ArrayMode  arrayMode               () const;
        QString                     arrayModeString         () const;
        const QVector<double>&      arrayValues             () const;

    signals:
        void                        dataChanged             ();

    public slots:
        void                        setData                 (DataStorage* pData);

    protected:

    private:
        void                        create                  ();

        DataStorage*                _pdata;

        int                         _elementsPerLine;
        unsigned long               _addressOffset;
        unsigned long               _addressStride;

        SeerArrayWidget::ArrayMode  _arrayMode;
        QVector<double>             _arrayValues;
};

