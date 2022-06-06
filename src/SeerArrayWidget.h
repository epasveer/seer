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

        const QString&              aAxis                   () const;
        void                        setAAxis                (const QString& axis);
        const QString&              aLabel                  () const;
        void                        setAAddressOffset       (unsigned long offset);
        unsigned long               aAddressOffset          () const;
        void                        setAAddressStride       (unsigned long stride);
        unsigned long               aAddressStride          () const;
        unsigned long               aSize                   () const;
        unsigned long               aElementSize            () const;

        void                        setAArrayMode           (SeerArrayWidget::ArrayMode arrayMode);
        SeerArrayWidget::ArrayMode  aArrayMode              () const;
        QString                     aArrayModeString        () const;
        const QVector<double>&      aArrayValues            () const;

        const QString&              bAxis                   () const;
        void                        setBAxis                (const QString& axis);
        const QString&              bLabel                  () const;
        void                        setBAddressOffset       (unsigned long offset);
        unsigned long               bAddressOffset          () const;
        void                        setBAddressStride       (unsigned long stride);
        unsigned long               bAddressStride          () const;
        unsigned long               bSize                   () const;
        unsigned long               bElementSize            () const;

        void                        setBArrayMode           (SeerArrayWidget::ArrayMode arrayMode);
        SeerArrayWidget::ArrayMode  bArrayMode              () const;
        QString                     bArrayModeString        () const;
        const QVector<double>&      bArrayValues            () const;


    signals:
        void                        dataChanged             ();

    public slots:
        void                        setAData                (const QString& label, DataStorage* pData);
        void                        setBData                (const QString& label, DataStorage* pData);

    protected:

    private:
        void                        create                  ();

        QString                     _aAxis;
        QString                     _aLabel;
        int                         _aColumnId;
        DataStorage*                _aData;
        unsigned long               _aAddressOffset;
        unsigned long               _aAddressStride;
        SeerArrayWidget::ArrayMode  _aArrayMode;
        QVector<double>             _aArrayValues;

        QString                     _bAxis;
        QString                     _bLabel;
        int                         _bColumnId;
        DataStorage*                _bData;
        unsigned long               _bAddressOffset;
        unsigned long               _bAddressStride;
        SeerArrayWidget::ArrayMode  _bArrayMode;
        QVector<double>             _bArrayValues;
};

