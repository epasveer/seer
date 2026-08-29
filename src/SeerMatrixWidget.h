// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QTableWidget>
#include <QtCore/QByteArray>
#include <QtCore/QVector>

class SeerMatrixWidget: public QTableWidget {

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

        enum MatrixType {
            UnknownMatrixType    = 0,
            Int16MatrixType      = 1,
            UInt16MatrixType     = 2,
            Int32MatrixType      = 3,
            UInt32MatrixType     = 4,
            Int64MatrixType      = 5,
            UInt64MatrixType     = 6,
            Float32MatrixType    = 7,
            Float64MatrixType    = 8
        };

        enum StorageOrder {
            RowMajor             = 0,
            ColumnMajor          = 1
        };

        // RAII guard: coalesces a burst of property changes into a single
        // rebuild. While a guard is alive create() is deferred; the single
        // rebuild happens when it goes out of scope. Nesting is supported.
        // Prefer a guard over the private begin/end pair so the matching end
        // is guaranteed even on early return or exception.
        class BulkUpdate {
            public:
                explicit BulkUpdate     (SeerMatrixWidget* widget);
                               ~BulkUpdate ();
                                BulkUpdate (const BulkUpdate&) = delete;
                BulkUpdate&     operator= (const BulkUpdate&) = delete;
            private:
                SeerMatrixWidget*       _widget;
        };

        SeerMatrixWidget(QWidget* parent = 0);
       ~SeerMatrixWidget();


        void                            setAddressOffset        (unsigned long offset);
        unsigned long                   addressOffset           () const;
        void                            setAddressStride        (unsigned long stride);
        unsigned long                   addressStride           () const;
        unsigned long                   elementSize             () const;

        void                            setDataType             (SeerMatrixWidget::MatrixType dataType);
        SeerMatrixWidget::MatrixType    dataType                () const;
        void                            setStorageOrder         (SeerMatrixWidget::StorageOrder order);
        SeerMatrixWidget::StorageOrder  storageOrder            () const;
        QString                         dataTypeString          () const;
        const QVector<double>&          dataValues              () const;
        int                             dataRows                () const;
        int                             dataColumns             () const;
        unsigned long                   dataSize                () const;
        int                             dataCount               () const;

    signals:
        void                            dataChanged             ();

    public slots:
        void                            setData                 (DataStorage* pData);
        void                            setDimensions           (int rows, int colums);

    protected:

    private:
        void                            create                  ();
        void                            beginBulkUpdate         ();
        void                            endBulkUpdate           ();

        int                             _bulkUpdateDepth;
        bool                            _createPending;

        unsigned long                   _addressOffset;
        unsigned long                   _addressStride;
        DataStorage*                    _data;
        int                             _dataRows;
        int                             _dataColumns;
        SeerMatrixWidget::MatrixType    _dataType;
        SeerMatrixWidget::StorageOrder  _storageOrder;
        QVector<double>                 _dataValues;
};

