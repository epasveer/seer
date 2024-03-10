#pragma once

#include <QtWidgets/QPlainTextEdit>
#include <QtCore/QByteArray>
#include "ui_SeerHexWidget.h"

//
// Hex Memory viewer widget.
// Based on virinext/ QHexView
// https://github.com/virinext/QHexView
//
// MIT License.
//

class SeerHexWidget: public QWidget, protected Ui::SeerHexWidgetForm {

    Q_OBJECT

    public:
        class DataStorage {
            public:
                virtual ~DataStorage() {};
                virtual QByteArray getData(int position, int length) = 0;
                virtual QByteArray getData() = 0;
                virtual int size() = 0;
        };

        class DataStorageArray: public DataStorage {
            public:
                DataStorageArray(const QByteArray& arr);
                virtual QByteArray getData(int position, int length);
                virtual QByteArray getData();
                virtual int size();
            private:
                QByteArray _data;
        };

        enum MemoryMode {
            UnknownMemoryMode = 0,
            HexMemoryMode     = 1,
            OctalMemoryMode   = 2,
            BinaryMemoryMode  = 3,
            DecimalMemoryMode = 4,
        };

        enum CharMode {
            UnknownCharMode = 0,
            AsciiCharMode   = 1,
            Utf8Mode        = 2,
            Utf16Mode       = 3,
            Utf32Mode       = 4,
            EbcdicCharMode  = 5
        };

        enum MagicNumbers {
            HexFieldWidth   = 18
        };

        SeerHexWidget(QWidget* parent = 0);
       ~SeerHexWidget();

        void                        setBytesPerLine                     (int count);
        int                         bytesPerLine                        () const;
        int                         hexCharsPerLine                     () const;
        int                         hexCharsPerByte                     () const;
        int                         nLines                              () const;
        int                         gapAddrHex                          () const;
        int                         gapHexAscii                         () const;
        void                        setAddressOffset                    (unsigned long offset);
        unsigned long               addressOffset                       () const;
        unsigned long               size                                () const;

        void                        setMemoryMode                       (SeerHexWidget::MemoryMode memoryMode);
        SeerHexWidget::MemoryMode   memoryMode                          () const;
        QString                     memoryModeString                    () const;

        void                        setCharMode                         (SeerHexWidget::CharMode charMode);
        SeerHexWidget::CharMode     charMode                            () const;
        QString                     charModeString                      () const;

        QTextDocument*              document                            ();
        QString                     toPlainText                         ();

    signals:
        void                        byteOffsetChanged                   (int byte);

    public slots:
        void                        setData                             (DataStorage* pData);

    protected:

    protected slots:
        void                        handleCursorPositionChanged         ();
        void                        handleByteOffsetChanged             (int byte);

    private:
        void                        create                              ();

        DataStorage*                _pdata;
        int                         _charWidth;
        int                         _charHeight;
        int                         _highlightByte;

        int                         _bytesPerLine;
        int                         _hexCharsPerLine;
        int                         _hexCharsPerByte;
        int                         _gapAddrHex;
        int                         _gapHexAscii;
        unsigned long               _addressOffset;

        SeerHexWidget::MemoryMode   _memoryMode;
        SeerHexWidget::CharMode     _charMode;
};

