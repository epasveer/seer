#include "SeerHexWidget.h"
#include "SeerUtl.h"
#include <QtGui/QPainter>
#include <QtGui/QPaintEvent>
#include <QtGui/QKeyEvent>
#include <QtGui/QClipboard>
#include <QtCore/QSize>
#include <QtCore/QDebug>
#include <stdexcept>

SeerHexWidget::SeerHexWidget(QWidget* parent) : QPlainTextEdit(parent), _pdata(NULL) {

    QFont font;
    font.setFamily("monospace [Consolas]");
    font.setFixedPitch(true);
    font.setStyleHint(QFont::TypeWriter);

    setFont(font);

    _memoryMode    = SeerHexWidget::HexMemoryMode;
    _charMode      = SeerHexWidget::AsciiCharMode;
    _addressOffset = 0;
    _charWidth     = fontMetrics().horizontalAdvance(QLatin1Char('9'));
    _charHeight    = fontMetrics().height();
    _gapAddrHex    = 10; // Gap between address and hex fields.
    _gapHexAscii   = 16; // Gap between hex and ascii fields.

    setBytesPerLine(16);

    setFocusPolicy(Qt::StrongFocus);
}

SeerHexWidget::~SeerHexWidget() {

    if (_pdata) {
        delete _pdata;
    }
}

void SeerHexWidget::setBytesPerLine (int count) {

    // 3 = 'a0 '
    // 4 = '007 '
    // 9 = '00011010 '
    // 4 = ' 10 '
    if (memoryMode() == SeerHexWidget::HexMemoryMode) {
        _hexCharsPerByte = 3;
    }else if (memoryMode() == SeerHexWidget::OctalMemoryMode) {
        _hexCharsPerByte = 4;
    }else if (memoryMode() == SeerHexWidget::BinaryMemoryMode) {
        _hexCharsPerByte = 9;
    }else if (memoryMode() == SeerHexWidget::DecimalMemoryMode) {
        _hexCharsPerByte = 4;
    }else{
        _hexCharsPerByte = 3;
    }

    _bytesPerLine    = count;
    _hexCharsPerLine = _bytesPerLine * _hexCharsPerByte - 1;
    _posAddr         = 0; // x position of address field.
    _posHex          = 12 * _charWidth + gapAddrHex(); // x position of hex field.
    _posAscii        = _posHex + hexCharsPerLine() * _charWidth + gapHexAscii(); // x position of ascii field.

    setMinimumWidth(_posAscii + (bytesPerLine() * _charWidth)); // x position after the ascii field.

    // Repaint the widget.
    create();
}

int SeerHexWidget::bytesPerLine () const {
    return _bytesPerLine;
}

int SeerHexWidget::hexCharsPerLine () const {
    return _hexCharsPerLine;
}

int SeerHexWidget::gapAddrHex () const {
    return _gapAddrHex;
}

int SeerHexWidget::gapHexAscii () const {
    return _gapHexAscii;
}

void SeerHexWidget::setAddressOffset (unsigned long offset) {

    _addressOffset = offset;

    // Repaint the widget.
    create();
}

unsigned long SeerHexWidget::addressOffset () const {
    return _addressOffset;
}

unsigned long SeerHexWidget::size () const {

    if (_pdata) {
        return _pdata->size();
    }

    return 0;
}

void SeerHexWidget::setMemoryMode (SeerHexWidget::MemoryMode memoryMode) {

    _memoryMode = memoryMode;

    // This repaints the widget with the new memory mode
    setBytesPerLine(bytesPerLine());
}

SeerHexWidget::MemoryMode SeerHexWidget::memoryMode () const {
    return _memoryMode;
}

QString SeerHexWidget::memoryModeString () const {

    if (memoryMode() == SeerHexWidget::HexMemoryMode) {
        return "hex";
    }else if (memoryMode() == SeerHexWidget::OctalMemoryMode) {
        return "octal";
    }else if (memoryMode() == SeerHexWidget::BinaryMemoryMode) {
        return "binary";
    }else if (memoryMode() == SeerHexWidget::DecimalMemoryMode) {
        return "decimal";
    }

    return "???";
}

void SeerHexWidget::setCharMode (SeerHexWidget::CharMode charMode) {

    _charMode = charMode;

    // Repaint the widget.
    create();
}

SeerHexWidget::CharMode SeerHexWidget::charMode() const {
    return _charMode;
}

QString SeerHexWidget::charModeString () const {

    if (charMode() == SeerHexWidget::AsciiCharMode) {
        return "ascii";
    }else if (charMode() == SeerHexWidget::EbcdicCharMode) {
        return "ebcdic";
    }

    return "???";
}

void SeerHexWidget::setData(SeerHexWidget::DataStorage* pData) {

    if (_pdata) {
        delete _pdata;
    }

    _pdata     = pData;
    _cursorPos = 0;

    // Repaint the widget.
    create();
}

void SeerHexWidget::create () {

    // Clear the current document. We're going to recreate it.
    clear();

    // If there's no data, do nothing.
    if (!_pdata) {
        return;
    }

    // Set text formats.
    QTextCharFormat defaultFormat = currentCharFormat();
    QTextCharFormat grayFormat    = defaultFormat;
    grayFormat.setBackground(QBrush(Qt::lightGray));

    // Get a cursor
    QTextCursor cursor(textCursor());

    cursor.movePosition(QTextCursor::Start);

    // Go through the data, one byte at a time.
    for (int i=0; i<_pdata->size(); i+=bytesPerLine()) {

        QByteArray data = _pdata->getData(i, bytesPerLine());

        // Place a new hex address on the left side.
        if (i % bytesPerLine() == 0) {

            QString address = QString("%1").arg(i + addressOffset(), 12, 16, QChar('0'));
            QString spacer(" ");

            // Write adress to document.
            cursor.insertText (address, grayFormat);

            // Write spacer to document.
            cursor.insertText (spacer, defaultFormat);
        }

        // Print N bytes in their datatype value.
        int b = 0;

        for (b=0; b<bytesPerLine() && i+b < _pdata->size(); b++) {

            unsigned char ch = data[b];

            QString val;

            if (memoryMode() == SeerHexWidget::HexMemoryMode) {
                val = QString("%1").arg(ushort(ch), int(2), int(16), QChar('0'));
            }else if (memoryMode() == SeerHexWidget::OctalMemoryMode) {
                val = QString("%1").arg(ushort(ch), int(3), int(8), QChar('0'));
            }else if (memoryMode() == SeerHexWidget::BinaryMemoryMode) {
                val = QString("%1").arg(ushort(ch), int(8), int(2), QChar('0'));
            }else if (memoryMode() == SeerHexWidget::DecimalMemoryMode) {
                val = QString("%1").arg(ushort(ch), int(3), int(10), QChar(' '));
            }else{
                val = "??";
            }

            QString spacer(" ");

            // Write memory value to document
            cursor.insertText (val, defaultFormat);

            // Write spacer to document.
            cursor.insertText (spacer, defaultFormat);
        }

        // Print N odd remainder bytes.
        for (; b<bytesPerLine(); b++) {

            QString val;

            if (memoryMode() == SeerHexWidget::HexMemoryMode) {
                val = "  ";
            }else if (memoryMode() == SeerHexWidget::OctalMemoryMode) {
                val = "   ";
            }else if (memoryMode() == SeerHexWidget::BinaryMemoryMode) {
                val = "        ";
            }else if (memoryMode() == SeerHexWidget::DecimalMemoryMode) {
                val = "   ";
            }else{
                val = "??";
            }

            QString spacer(" ");

            // Write memory value to document
            cursor.insertText (val, defaultFormat);

            // Write spacer to document.
            cursor.insertText (spacer, defaultFormat);
        }

        // Print vertical line.
        QString spacer("| ");

        // Write spacer to document.
        cursor.insertText (spacer, defaultFormat);

        // Print N bytes in their char value.
        if (charMode() == SeerHexWidget::AsciiCharMode) {
            for (int b=0; b<bytesPerLine() && i+b < _pdata->size(); b++) {

                unsigned char ch = Seer::ucharToAscii( data[b] );

                QString val(ch);

                // Write display character to document.
                cursor.insertText (val, defaultFormat);
            }

        }else if (charMode() == SeerHexWidget::EbcdicCharMode) {
            for (int b=0; b<bytesPerLine() && i+b < _pdata->size(); b++) {

                unsigned char ch = Seer::ebcdicToAscii( data[b] );

                QString val(ch);

                // Write display character to document.
                cursor.insertText (val, defaultFormat);
            }

        }else{
            // Don't print anything.
        }

        // Write eol to document.
        QString eol("\n");

        cursor.insertText (eol, defaultFormat);
    }
}

SeerHexWidget::DataStorageArray::DataStorageArray(const QByteArray& arr) {
    _data = arr;
}

QByteArray SeerHexWidget::DataStorageArray::getData(int position, int length) {
    return _data.mid(position, length);
}

int SeerHexWidget::DataStorageArray::size() {
    return _data.count();
}

