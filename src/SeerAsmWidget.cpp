#include "SeerAsmWidget.h"
#include "SeerUtl.h"
#include <QtCore/QDebug>
#include <algorithm>

SeerAsmWidget::SeerAsmWidget(QWidget* parent) : QWidget(parent) {

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

    // Set text formats.
    _defaultFormat = plainTextEdit->currentCharFormat();
    _grayFormat    = _defaultFormat;
    _grayFormat.setBackground(QBrush(Qt::lightGray));

    // Connect things.
}

SeerAsmWidget::~SeerAsmWidget() {
}

QTextDocument* SeerAsmWidget::document () {

    return plainTextEdit->document();
}

QString SeerAsmWidget::toPlainText () {

    return plainTextEdit->toPlainText();
}

void SeerAsmWidget::setData (const QString& data) {


    // ^done,asm_insns=[
    //                  {address="0x000000000040093c",func-name="main()",offset="362",inst="mov    $0x400aa9,%edi"},
    //                  {address="0x0000000000400941",func-name="main()",offset="367",inst="call   0x400680 <puts@plt>"},
    //                  {address="0x0000000000400946",func-name="main()",offset="372",inst="mov    -0x30(%rbp),%rax"}
    //                 ]

    // Clear the document.
    plainTextEdit->clear();

    // Get a cursor
    QTextCursor cursor(plainTextEdit->textCursor());

    cursor.movePosition(QTextCursor::Start);

    // Prepare the 'data' for parsing.
    QString asm_insns_text = Seer::parseFirst(data, "asm_insns=", '[', ']', false);

    QStringList asm_list = Seer::parse(asm_insns_text, "", '{', '}', false);

    // Loop through the asm list to get maximum field widths.
    int address_width = QString("Address").length();
    int opcode_width  = QString("Opcode").length();
    int inst_width    = QString("Assembly").length();

    for ( const auto& asm_text : asm_list  ) {

        QString address_text = Seer::parseFirst(asm_text, "address=", '"', '"', false);
        QString opcodes_text = Seer::parseFirst(asm_text, "opcodes=", '"', '"', false);
        QString inst_text    = Seer::parseFirst(asm_text, "inst=",    '"', '"', false);

        address_width = std::max((qsizetype)address_width, (qsizetype)address_text.length());
        opcode_width  = std::max((qsizetype)opcode_width,  (qsizetype)opcodes_text.length());
        inst_width    = std::max((qsizetype)inst_width,    (qsizetype)inst_text.length());
    }

    // Write header.
    cursor.insertText (QString("Address").leftJustified(address_width, ' ', true), _grayFormat);
    cursor.insertText (QString(" "), _defaultFormat);
    cursor.insertText (QString("Opcodes").leftJustified(opcode_width, ' ', true), _defaultFormat);
    cursor.insertText (QString(" | "), _defaultFormat);
    cursor.insertText (QString("Assembly"), _defaultFormat);
    cursor.insertText (QString("\n"), _defaultFormat);

    // Loop through the asm list and print each line.
    for ( const auto& asm_text : asm_list  ) {

        // Get the strings, with padding.
        QString address_text = Seer::parseFirst(asm_text, "address=", '"', '"', false).leftJustified(address_width, ' ', true);
        QString opcodes_text = Seer::parseFirst(asm_text, "opcodes=", '"', '"', false).leftJustified(opcode_width,  ' ', true);
        QString inst_text    = Seer::parseFirst(asm_text, "inst=",    '"', '"', false).leftJustified(inst_width,    ' ', true);

        // Write the text, with spacers.
        cursor.insertText (address_text, _grayFormat);
        cursor.insertText (QString(" "), _defaultFormat);

        cursor.insertText (opcodes_text,   _defaultFormat);
        cursor.insertText (QString(" | "), _defaultFormat);

        cursor.insertText (inst_text, _defaultFormat);

        // Write eol to document.
        cursor.insertText (QString("\n"), _defaultFormat);
    }

    // Move to the start of the document.
    cursor.movePosition(QTextCursor::Start);

    plainTextEdit->setTextCursor(cursor);
}

