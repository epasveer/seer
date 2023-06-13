#include "SeerHexWidget.h"
#include "SeerUtl.h"
#include <QtGui/QPainter>
#include <QtGui/QPaintEvent>
#include <QtGui/QKeyEvent>
#include <QtGui/QClipboard>
#include <QtGui/QTextCursor>
#include <QtGui/QTextBlock>
#include <QtCore/QSize>
#include <QtCore/QDebug>
#include <stdexcept>

#define byteArrayToType( data, order, precision, type ) \
        QDataStream stream( data ); \
        stream.setByteOrder( order ); \
        stream.setFloatingPointPrecision( precision ); \
        type t; \
        stream >> t; \
        return t;

qint8 toQInt8 (const QByteArray& data, const QDataStream::ByteOrder order=QDataStream::BigEndian) {
    byteArrayToType(data, order, QDataStream::SinglePrecision, qint8)
}

quint8 toQUInt8 (const QByteArray& data, const QDataStream::ByteOrder order=QDataStream::BigEndian) {
    byteArrayToType(data, order, QDataStream::SinglePrecision, quint8)
}

qint16 toQInt16 (const QByteArray& data, const QDataStream::ByteOrder order=QDataStream::BigEndian) {
    byteArrayToType(data, order, QDataStream::SinglePrecision, qint16)
}

quint16 toQUInt16 (const QByteArray& data, const QDataStream::ByteOrder order=QDataStream::BigEndian) {
    byteArrayToType(data, order, QDataStream::SinglePrecision, quint16)
}

qint32 toQInt32 (const QByteArray& data, const QDataStream::ByteOrder order=QDataStream::BigEndian) {
    byteArrayToType(data, order, QDataStream::SinglePrecision, qint32)
}

quint32 toQUInt32 (const QByteArray& data, const QDataStream::ByteOrder order=QDataStream::BigEndian) {
    byteArrayToType(data, order, QDataStream::SinglePrecision, quint32)
}

qint64 toQInt64 (const QByteArray& data, const QDataStream::ByteOrder order=QDataStream::BigEndian) {
    byteArrayToType(data, order, QDataStream::SinglePrecision, qint64)
}

quint64 toQUInt64 (const QByteArray& data, const QDataStream::ByteOrder order=QDataStream::BigEndian) {
    byteArrayToType(data, order, QDataStream::SinglePrecision, quint64)
}

float toQFloat32 (const QByteArray& data, const QDataStream::ByteOrder order=QDataStream::BigEndian) {
    byteArrayToType(data, order, QDataStream::SinglePrecision, float)
}

double toQFloat64 (const QByteArray& data, const QDataStream::ByteOrder order=QDataStream::BigEndian) {
    byteArrayToType(data, order, QDataStream::DoublePrecision, double)
}

SeerHexWidget::SeerHexWidget(QWidget* parent) : QWidget(parent), _pdata(NULL) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    QFont font;
    font.setFamily("monospace [Consolas]");
    font.setFixedPitch(true);
    font.setStyleHint(QFont::TypeWriter);

    plainTextEdit->setFont(font);
    plainTextEdit->setFocusPolicy(Qt::StrongFocus);
    plainTextEdit->setTextInteractionFlags(Qt::TextSelectableByKeyboard|Qt::TextSelectableByMouse);
    plainTextEdit->setWordWrapMode(QTextOption::NoWrap);

    _memoryMode    = SeerHexWidget::HexMemoryMode;
    _charMode      = SeerHexWidget::AsciiCharMode;
    _addressOffset = 0;
    _charWidth     = plainTextEdit->fontMetrics().horizontalAdvance(QLatin1Char('9'));
    _charHeight    = plainTextEdit->fontMetrics().height();
    _gapAddrHex    = 10; // Gap between address and hex fields.
    _gapHexAscii   = 16; // Gap between hex and ascii fields.
    _highlightByte = -1;

    setBytesPerLine(16);

    // Connect things.
    QObject::connect(plainTextEdit,                   &QPlainTextEdit::cursorPositionChanged,        this,  &SeerHexWidget::handleCursorPositionChanged);
    QObject::connect(showAsLittleEndianCheckBox,      &QCheckBox::clicked,                           this,  &SeerHexWidget::handleCursorPositionChanged);
    QObject::connect(showUnsignedFloatAsHexCheckBox,  &QCheckBox::clicked,                           this,  &SeerHexWidget::handleCursorPositionChanged);
    QObject::connect(this,                            &SeerHexWidget::byteOffsetChanged,             this,  &SeerHexWidget::handleByteOffsetChanged);
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

    _bytesPerLine       = count;
    _hexCharsPerLine    = _bytesPerLine * _hexCharsPerByte - 1;

    int posHex          = SeerHexWidget::HexFieldWidth * _charWidth + gapAddrHex(); // x position of hex field.
    int posAscii        = posHex + hexCharsPerLine() * _charWidth + gapHexAscii(); // x position of ascii field.

    setMinimumWidth(posAscii + (bytesPerLine() * _charWidth)); // x position after the ascii field.

    // Repaint the widget.
    create();
}

int SeerHexWidget::bytesPerLine () const {
    return _bytesPerLine;
}

int SeerHexWidget::hexCharsPerLine () const {
    return _hexCharsPerLine;
}

int SeerHexWidget::hexCharsPerByte () const {
    return _hexCharsPerByte;
}

int SeerHexWidget::nLines () const {
    return plainTextEdit->blockCount();
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

QTextDocument* SeerHexWidget::document () {

    return plainTextEdit->document();
}

QString SeerHexWidget::toPlainText () {

    return plainTextEdit->toPlainText();
}

void SeerHexWidget::setData(SeerHexWidget::DataStorage* pData) {

    if (_pdata) {
        delete _pdata;
    }

    _pdata = pData;

    // Repaint the widget.
    create();
}

void SeerHexWidget::handleCursorPositionChanged () {

    // Get the current cursor position.
    QTextCursor cursor = plainTextEdit->textCursor();

    // Is it before the hex values? (address region)
    if (cursor.positionInBlock() < SeerHexWidget::HexFieldWidth) {
        emit byteOffsetChanged(-1);
        return;
    }

    // Is is after the hex values? (ascii/ebcdic region)
    if (cursor.positionInBlock() > SeerHexWidget::HexFieldWidth + hexCharsPerLine()) {
        emit byteOffsetChanged(-1);
        return;
    }

    int line = cursor.blockNumber();
    int col  = cursor.positionInBlock() - SeerHexWidget::HexFieldWidth;
    int byte = (col / hexCharsPerByte()) + (line * bytesPerLine());

    _highlightByte = byte;

    emit byteOffsetChanged(byte);
}

void SeerHexWidget::handleByteOffsetChanged (int byte) {

    // Clear all fields.
    lineEdit_1->setText("");
    lineEdit_2->setText("");
    lineEdit_3->setText("");
    lineEdit_4->setText("");
    lineEdit_5->setText("");
    lineEdit_6->setText("");
    lineEdit_7->setText("");
    lineEdit_8->setText("");
    lineEdit_9->setText("");
    lineEdit_10->setText("");
    lineEdit_11->setText("");
    lineEdit_12->setText("");
    lineEdit_13->setText("");
    lineEdit_14->setText("");

    // Remove previous highlight.
    plainTextEdit->setExtraSelections(QList<QTextEdit::ExtraSelection>());

    // Invalid byte number, do nothing.
    if (byte < 0) {
        return;
    }

    // If there's no data, do nothing.
    if (!_pdata) {
        return;
    }

    // Byte past the end, do nothing.
    if ((unsigned int)byte >= size()) {
        return;
    }

    // Calculate line and position in line.
    int line = (byte / bytesPerLine());
    int pos  = byte - (line * bytesPerLine());

    int pos_s = SeerHexWidget::HexFieldWidth + (pos * hexCharsPerByte());
    int pos_e = pos_s + hexCharsPerByte() - 1;

    int pos_a = SeerHexWidget::HexFieldWidth + hexCharsPerLine() + pos + 4; // 4 == ' ' .... ' | '

    // Highlight the text in the hex region and the ascii region.
    QList<QTextEdit::ExtraSelection> extraSelections; {

        // Get a cursor to the current line.
        QTextBlock  block  = plainTextEdit->document()->findBlockByLineNumber(line);
        QTextCursor cursor = QTextCursor(block);

        // Highlight the current hex value.
        cursor.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
        cursor.movePosition(QTextCursor::Right,       QTextCursor::MoveAnchor, pos_s);
        cursor.movePosition(QTextCursor::Right,       QTextCursor::KeepAnchor, pos_e - pos_s + 1);

        // Add it to the extra selections.
        QTextEdit::ExtraSelection extra_byte;
        extra_byte.format.setBackground(plainTextEdit->palette().highlight().color());
        extra_byte.cursor = cursor;

        extraSelections.append(extra_byte);

        // Highlight the current ascii value.
        cursor.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
        cursor.movePosition(QTextCursor::Right,       QTextCursor::MoveAnchor, pos_a);
        cursor.movePosition(QTextCursor::Right,       QTextCursor::KeepAnchor, 1);

        // Add it to the extra selections.
        QTextEdit::ExtraSelection extra_ascii;
        extra_ascii.format.setBackground(plainTextEdit->palette().highlight().color());
        extra_ascii.cursor = cursor;

        extraSelections.append(extra_ascii);

    } plainTextEdit->setExtraSelections(extraSelections);

    // Set the endian default.
    QDataStream::ByteOrder byteOrder = QDataStream::BigEndian;

    if (showAsLittleEndianCheckBox->isChecked()) {
        byteOrder = QDataStream::LittleEndian;
    }

    // Set the 'AsHex' default.
    bool unsignedAndFloatAsHex = false;

    if (showUnsignedFloatAsHexCheckBox->isChecked()) {
        unsignedAndFloatAsHex = true;
    }

    // Go through each one and display the value.
    {
        QByteArray arr = _pdata->getData(byte, sizeof(char));                       // Extract a bytearray from the data for the size of the value we are after.
        if (arr.size() == sizeof(char)) {                                           // If not the right size, skip it. Near the end of the data.
            lineEdit_1->setText(QString::number(toQInt8(arr, byteOrder)));          // Fill in the signed value.
            if (unsignedAndFloatAsHex) {                                            // Show unsigned value as hex?
                if (byteOrder == QDataStream::LittleEndian) {                       // Swap bytes to handle endianess.
                    std::reverse(arr.begin(), arr.end());
                }
                lineEdit_2->setText("0x"+QString(arr.toHex()));                     // Print value as hex.
            }else{
                lineEdit_2->setText(QString::number(toQUInt8(arr, byteOrder)));     // Print value as a value.
            }
        }
    }

    {
        QByteArray arr = _pdata->getData(byte, sizeof(short));
        if (arr.size() == sizeof(short)) {
            lineEdit_3->setText(QString::number(toQInt16(arr, byteOrder)));
            if (unsignedAndFloatAsHex) {
                if (byteOrder == QDataStream::LittleEndian) {
                    std::reverse(arr.begin(), arr.end());
                }
                lineEdit_4->setText("0x"+QString(arr.toHex()));
            }else{
                lineEdit_4->setText(QString::number(toQUInt16(arr, byteOrder)));
            }
        }
    }

    {
        QByteArray arr = _pdata->getData(byte, sizeof(float));
        if (arr.size() == sizeof(float)) {
            if (unsignedAndFloatAsHex) {
                if (byteOrder == QDataStream::LittleEndian) {
                    std::reverse(arr.begin(), arr.end());
                }
                lineEdit_5->setText("0x"+QString(arr.toHex()));
            }else{
                lineEdit_5->setText(QString::number(toQFloat32(arr, byteOrder)));
            }
        }
    }

    {
        QByteArray arr = _pdata->getData(byte, sizeof(int));
        if (arr.size() == sizeof(int)) {
            lineEdit_6->setText(QString::number(toQInt32(arr, byteOrder)));
            if (unsignedAndFloatAsHex) {
                if (byteOrder == QDataStream::LittleEndian) {
                    std::reverse(arr.begin(), arr.end());
                }
                lineEdit_7->setText("0x"+QString(arr.toHex()));
            }else{
                lineEdit_7->setText(QString::number(toQUInt32(arr, byteOrder)));
            }
        }
    }

    {
        QByteArray arr = _pdata->getData(byte, sizeof(long int));
        if (arr.size() == sizeof(long int)) {
            lineEdit_8->setText(QString::number(toQInt64(arr, byteOrder)));
            if (unsignedAndFloatAsHex) {
                if (byteOrder == QDataStream::LittleEndian) {
                    std::reverse(arr.begin(), arr.end());
                }
                lineEdit_9->setText("0x"+QString(arr.toHex()));
            }else{
                lineEdit_9->setText(QString::number(toQUInt64(arr, byteOrder)));
            }
        }
    }

    {
        QByteArray arr = _pdata->getData(byte, sizeof(double));
        if (arr.size() == sizeof(double)) {
            if (unsignedAndFloatAsHex) {
                if (byteOrder == QDataStream::LittleEndian) {
                    std::reverse(arr.begin(), arr.end());
                }
                lineEdit_10->setText("0x"+QString(arr.toHex()));
            }else{
                lineEdit_10->setText(QString::number(toQFloat64(arr, byteOrder)));
            }
        }
    }

    {
        QString    val;
        QByteArray arr = _pdata->getData(byte, 4);

        for (int i=0; i<arr.size(); i++) {

            unsigned char ch = arr[i];

            if (val != "") {
                val += " ";
            }

            val += QString("%1").arg(ushort(ch), int(2), int(16), QChar('0'));
        }

        lineEdit_11->setText(val);
    }

    {
        QString    val;
        QByteArray arr = _pdata->getData(byte, 4);

        for (int i=0; i<arr.size(); i++) {

            unsigned char ch = arr[i];

            if (val != "") {
                val += " ";
            }

            val += QString("%1").arg(ushort(ch), int(3), int(8), QChar('0'));
        }

        lineEdit_12->setText(val);
    }

    {
        QString    val;
        QByteArray arr = _pdata->getData(byte, 4);

        for (int i=0; i<arr.size(); i++) {

            unsigned char ch = arr[i];

            if (val != "") {
                val += " ";
            }

            val += QString("%1").arg(ushort(ch), int(8), int(2), QChar('0'));
        }

        lineEdit_13->setText(val);
    }

    {
        QString    val;
        QByteArray arr = _pdata->getData(byte, 4);

        // Print N bytes in their char value.
        if (charMode() == SeerHexWidget::AsciiCharMode) {
            for (int i=0; i<arr.size(); i++) {

                unsigned char ch = Seer::ucharToAscii( arr[i] );

                QChar symbol = QChar(ch);
                val += QString(symbol);
            }

        }else if (charMode() == SeerHexWidget::EbcdicCharMode) {
            for (int i=0; i<arr.size(); i++) {

                unsigned char ch = Seer::ebcdicToAscii( arr[i] );

                QChar symbol = QChar(ch);
                val += QString(symbol);
            }

        }else{
            // Don't print anything.
        }

        lineEdit_14->setText(val);
    }
}

void SeerHexWidget::create () {

    // Ignore changing of cursor positions.
    QObject::disconnect(plainTextEdit, &QPlainTextEdit::cursorPositionChanged,    this,  &SeerHexWidget::handleCursorPositionChanged);

    // Clear the current document. We're going to recreate it.
    plainTextEdit->clear();

    // Clear the checksum.
    lineEdit_15->setText("");

    // If there's no data, do nothing.
    if (!_pdata) {
        // Re-enable notification of changing of cursor positions.
        QObject::connect(plainTextEdit, &QPlainTextEdit::cursorPositionChanged,   this,  &SeerHexWidget::handleCursorPositionChanged);
        return;
    }

    // Set text formats.
    QTextCharFormat defaultFormat = plainTextEdit->currentCharFormat();
    QTextCharFormat grayFormat    = defaultFormat;
    grayFormat.setBackground(QBrush(Qt::lightGray));
    QTextCharFormat greenFormat   = defaultFormat;
    greenFormat.setBackground(QBrush(Qt::green));

    // Get a cursor
    QTextCursor cursor(plainTextEdit->textCursor());

    cursor.movePosition(QTextCursor::Start);

    // Go through the data, one byte at a time.
    for (int i=0; i<_pdata->size(); i+=bytesPerLine()) {

        QByteArray data = _pdata->getData(i, bytesPerLine());

        // Place a new hex address on the left side.
        if (i % bytesPerLine() == 0) {

            QString address = QString("0x%1").arg(i + addressOffset(), SeerHexWidget::HexFieldWidth-2, 16, QChar('0')); // -2 to allow '0x'.

            // Write adress to document.
            cursor.insertText (address, grayFormat);

            // Write spacer to document.
            cursor.insertText (QString(" "), defaultFormat);
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

            cursor.insertText (val, defaultFormat);

            // Write spacer to document.
            cursor.insertText (QString(" "), defaultFormat);
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

            // Write memory value to document
            cursor.insertText (val, defaultFormat);

            // Write spacer to document.
            cursor.insertText (QString(" "), defaultFormat);
        }

        // Print vertical line.
        // Write spacer to document.
        cursor.insertText (QString("| "), defaultFormat);

        // Print N bytes in their char value.
        if (charMode() == SeerHexWidget::AsciiCharMode) {
            for (int b=0; b<bytesPerLine() && i+b < _pdata->size(); b++) {

                unsigned char ch = Seer::ucharToAscii( data[b] );

                QChar symbol = QChar(ch);
                QString val(symbol);

                // Write display character to document.
                cursor.insertText (val, defaultFormat);
            }

        }else if (charMode() == SeerHexWidget::EbcdicCharMode) {
            for (int b=0; b<bytesPerLine() && i+b < _pdata->size(); b++) {

                unsigned char ch = Seer::ebcdicToAscii( data[b] );

                QChar symbol = QChar(ch);
                QString val(symbol);

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


    // Print checksum.
    {
        quint16 crc16    = qChecksum(_pdata->getData(), Qt::ChecksumIso3309);
        QString crc16str = QString::number(crc16);

        lineEdit_15->setText(crc16str);
    }

    handleByteOffsetChanged(_highlightByte);

    // Re-enable notification of changing of cursor positions.
    QObject::connect(plainTextEdit, &QPlainTextEdit::cursorPositionChanged, this,  &SeerHexWidget::handleCursorPositionChanged);
}

SeerHexWidget::DataStorageArray::DataStorageArray(const QByteArray& arr) {
    _data = arr;
}

QByteArray SeerHexWidget::DataStorageArray::getData(int position, int length) {
    return _data.mid(position, length);
}

QByteArray SeerHexWidget::DataStorageArray::getData() {
    return _data;
}

int SeerHexWidget::DataStorageArray::size() {
    return _data.size();
}

