#pragma once

#include <QtWidgets/QPlainTextEdit>
#include <QtCore/QByteArray>

//
// Hex Memory viewer widget.
// Based on virinext/ QHexView
// https://github.com/virinext/QHexView
//
// MIT License.
//

class SeerHexWidget: public QPlainTextEdit {

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
            EbcdicCharMode  = 2
        };

        SeerHexWidget(QWidget* parent = 0);
       ~SeerHexWidget();

        void                        setBytesPerLine         (int count);
        int                         bytesPerLine            () const;
        int                         hexCharsPerLine         () const;
        int                         gapAddrHex              () const;
        int                         gapHexAscii             () const;
        void                        setAddressOffset        (unsigned long offset);
        unsigned long               addressOffset           () const;
        unsigned long               size                    () const;

        void                        setMemoryMode           (SeerHexWidget::MemoryMode memoryMode);
        SeerHexWidget::MemoryMode   memoryMode              () const;
        QString                     memoryModeString        () const;

        void                        setCharMode             (SeerHexWidget::CharMode charMode);
        SeerHexWidget::CharMode     charMode                () const;
        QString                     charModeString          () const;

    public slots:
        void                        setData                 (DataStorage* pData);

    protected:

    private:
        void                        create                  ();

        DataStorage*                _pdata;
        int                         _posAddr;
        int                         _posHex;
        int                         _posAscii;
        int                         _charWidth;
        int                         _charHeight;
        int                         _selectBegin;
        int                         _selectEnd;
        int                         _selectInit;
        int                         _cursorPos;

        int                         _bytesPerLine;
        int                         _hexCharsPerLine;
        int                         _hexCharsPerByte;
        int                         _gapAddrHex;
        int                         _gapHexAscii;
        unsigned long               _addressOffset;

        SeerHexWidget::MemoryMode   _memoryMode;
        SeerHexWidget::CharMode     _charMode;
};

