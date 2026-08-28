// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerEditorWidgetAssembly.h"
#include "SeerPlainTextEdit.h"
#include "SeerBreakpointCreateDialog.h"
#include "SeerPrintpointCreateDialog.h"
#include "SeerUtl.h"
#include <QtGui/QColor>
#include <QtGui/QPainter>
#include <QtGui/QTextBlock>
#include <QtGui/QFont>
#include <QtGui/QIcon>
#include <QtGui/QRadialGradient>
#include <QtGui/QHelpEvent>
#include <QtGui/QPainterPath>
#include <QtGui/QGuiApplication>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QMenu>
#include <QAction>
#include <QtWidgets/QToolTip>
#include <QtWidgets/QMessageBox>
#include <QtGui/QTextCursor>
#include <QtGui/QPalette>
#include <QtCore/QList>
#include <QtCore/QString>
#include <QtCore/QTextStream>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QDebug>

//
// Assembly Area
//

SeerEditorWidgetAssemblyArea::SeerEditorWidgetAssemblyArea(QWidget* parent) : SeerPlainTextEdit(parent) {

    _enableLineNumberArea = false;
    _enableOffsetArea     = false;
    _enableOpcodeArea     = false;
    _enableBreakPointArea = false;
    _enableMiniMapArea    = false;
    _enableSourceLines    = false;
    _sourceTabSize        = 4;
    _convertUppercase     = false;
    _miniMapArea          = 0;
    _miniMapPixmapDirty   = true;
    _miniMapContentHeight = 0;

    _addressLineMap.clear();
    _offsetLineMap.clear();
    _lineAddressMap.clear();
    _lineOffsetMap.clear();
    _lineOpcodeMap.clear();

    QFont font("monospace");
    font.setStyleHint(QFont::Monospace);
    setFont(font);

    setReadOnly(true);
    setTextInteractionFlags(textInteractionFlags() | Qt::TextSelectableByKeyboard);
    setLineWrapMode(QPlainTextEdit::NoWrap);

    _lineNumberArea = new SeerEditorWidgetAssemblyLineNumberArea(this);
    _offsetArea     = new SeerEditorWidgetAssemblyOffsetArea(this);
    _breakPointArea = new SeerEditorWidgetAssemblyBreakPointArea(this);
    _opcodeArea     = new SeerEditorWidgetAssemblyOpcodeArea(this);
    _miniMapArea    = new SeerEditorWidgetAssemblyMiniMapArea(this);

    enableLineNumberArea(true);
    enableOffsetArea(true);
    enableOpcodeArea(true);
    enableBreakPointArea(true);
    enableMiniMapArea(false);
    enableSourceLines(true);
    setConvertUppercase(false);

    QObject::connect(this, &SeerEditorWidgetAssemblyArea::blockCountChanged,                this, &SeerEditorWidgetAssemblyArea::updateMarginAreasWidth);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::blockCountChanged,                this, &SeerEditorWidgetAssemblyArea::invalidateMiniMapCache);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,                    this, &SeerEditorWidgetAssemblyArea::updateLineNumberArea);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,                    this, &SeerEditorWidgetAssemblyArea::updateOffsetArea);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,                    this, &SeerEditorWidgetAssemblyArea::updateOpcodeArea);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,                    this, &SeerEditorWidgetAssemblyArea::updateBreakPointArea);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,                    this, &SeerEditorWidgetAssemblyArea::updateMiniMapArea);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::highlighterSettingsChanged,       this, &SeerEditorWidgetAssemblyArea::handleHighlighterSettingsChanged);

    setCurrentLine("");

    updateMarginAreasWidth(0);

    // Forward the scroll events in the various areas to the text edit.
    SeerPlainTextWheelEventForwarder* lineNumberAreaWheelForwarder = new SeerPlainTextWheelEventForwarder(this);
    SeerPlainTextWheelEventForwarder* offsetAreaWheelForwarder     = new SeerPlainTextWheelEventForwarder(this);
    SeerPlainTextWheelEventForwarder* opcodeAreaWheelForwarder     = new SeerPlainTextWheelEventForwarder(this);
    SeerPlainTextWheelEventForwarder* breakPointAreaWheelForwarder = new SeerPlainTextWheelEventForwarder(this);
    SeerPlainTextWheelEventForwarder* miniMapAreaWheelForwarder    = new SeerPlainTextWheelEventForwarder(this);

    _lineNumberArea->installEventFilter(lineNumberAreaWheelForwarder);
    _offsetArea->installEventFilter(offsetAreaWheelForwarder);
    _breakPointArea->installEventFilter(breakPointAreaWheelForwarder);
    _opcodeArea->installEventFilter(opcodeAreaWheelForwarder);
    _miniMapArea->installEventFilter(miniMapAreaWheelForwarder);

    // Calling close() will clear the text document.
    close();
}

void SeerEditorWidgetAssemblyArea::enableLineNumberArea (bool flag) {

    _enableLineNumberArea = flag;

    updateMarginAreasWidth(0);
}

bool SeerEditorWidgetAssemblyArea::lineNumberAreaEnabled () const {

    return _enableLineNumberArea;
}

void SeerEditorWidgetAssemblyArea::enableOffsetArea (bool flag) {

    _enableOffsetArea = flag;

    updateMarginAreasWidth(0);
}

bool SeerEditorWidgetAssemblyArea::offsetAreaEnabled () const {

    return _enableOffsetArea;
}

void SeerEditorWidgetAssemblyArea::enableBreakPointArea (bool flag) {

    _enableBreakPointArea = flag;

    updateMarginAreasWidth(0);
}

bool SeerEditorWidgetAssemblyArea::breakPointAreaEnabled () const {

    return _enableBreakPointArea;
}

void SeerEditorWidgetAssemblyArea::enableOpcodeArea (bool flag) {

    _enableOpcodeArea = flag;

    updateMarginAreasWidth(0);
}

bool SeerEditorWidgetAssemblyArea::opcodeAreaEnabled () const {

    return _enableOpcodeArea;
}

void SeerEditorWidgetAssemblyArea::enableMiniMapArea (bool flag) {

    _enableMiniMapArea = flag;

    setVerticalScrollBarPolicy(flag ? Qt::ScrollBarAlwaysOff : Qt::ScrollBarAsNeeded);

    invalidateMiniMapCache();
    updateMarginAreasWidth(0);
}

bool SeerEditorWidgetAssemblyArea::miniMapAreaEnabled () const {

    return _enableMiniMapArea;
}

void SeerEditorWidgetAssemblyArea::enableSourceLines (bool flag) {

    _enableSourceLines = flag;

    QString addr = address();

    setAddress(addr, true);
}

bool SeerEditorWidgetAssemblyArea::sourceLinesEnabled () const {

    return _enableSourceLines;
}

void SeerEditorWidgetAssemblyArea::updateTextArea () {

    //
    // ^done,asm_insns=[
    //          src_and_asm_line={
    //                              line="72",
    //                              file="helloarray.cpp",
    //                              fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",
    //                              line_asm_insn=[
    //                                  {address="0x000000000040093c",func-name="main()",offset="362",opcodes="bf a9 0a 40 00",inst="mov    $0x400aa9,%edi"},
    //                                  {address="0x0000000000400941",func-name="main()",offset="367",opcodes="e8 3a fd ff ff",inst="call   0x400680 <puts@plt>"}
    //                              ]
    //                          },
    //          src_and_asm_line={
    //                              line="73",
    //                              file="helloarray.cpp",
    //                              fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",
    //                              line_asm_insn=[
    //                              ]
    //                          },
    //

    QApplication::setOverrideCursor(Qt::BusyCursor);

    // Clear the existing document.
    document()->clear();

    // Clear mappings.
    _addressLineMap.clear();
    _offsetLineMap.clear();
    _lineAddressMap.clear();
    _lineOffsetMap.clear();
    _lineOpcodeMap.clear();

    // Clear 'source line' selections.
    _sourceLinesExtraSelections.clear();

    // Get the list of source and assembly lines.
    QString asm_insns_text = Seer::parseFirst(_asm_insns_text, "asm_insns=", '[', ']', false);

    QStringList src_and_asm_list = Seer::parse(asm_insns_text, "src_and_asm_line=", '{', '}', false);
    QStringList asm_list         = Seer::parse(asm_insns_text, "", '{', '}', false);

    if (src_and_asm_list.size() > 0) {

        //qDebug() << "src_and_asm_text mode.";

        // Loop through the asm list and print each line.
        int lineno = 1;

        for ( const auto& src_and_asm_text : src_and_asm_list ) {

            // Get the strings, with padding.
            QString line_text     = Seer::parseFirst(src_and_asm_text, "line=",     '"', '"', false);
            QString file_text     = Seer::parseFirst(src_and_asm_text, "file=",     '"', '"', false);
            QString fullname_text = Seer::parseFirst(src_and_asm_text, "fullname=", '"', '"', false);

            // Print source line?
            if (sourceLinesEnabled() == true) {

                // Get source for 'line'
                QString sourceLine = sourceForLine(fullname_text, file_text, line_text.toInt());

                sourceLine = Seer::expandTabs(sourceLine, editorTabSize(), false); // Expand tabs.
                sourceLine = sourceLine.simplified(); // Remove blank spaces at front and end of line.

                // Write source line to the document.
                if (convertUppercase()) {
                    appendPlainText(sourceLine.toUpper());
                }else{
                    appendPlainText(sourceLine);
                }

                // Highlight it.
                QTextCharFormat sourceLinesFormat = highlighterSettings().get("Text");

                QTextBlock  block  = document()->findBlockByLineNumber(lineno-1);
                QTextCursor cursor = textCursor();

                cursor.setPosition(block.position());
                setTextCursor(cursor);

                QTextEdit::ExtraSelection selection;
                selection.format.setForeground(sourceLinesFormat.foreground());
                selection.format.setBackground(sourceLinesFormat.background());
                selection.format.setProperty(QTextFormat::FullWidthSelection, true);
                selection.cursor = textCursor();
                selection.cursor.clearSelection();

                _sourceLinesExtraSelections.append(selection);

                lineno++;
            }

            // Get the list of assembly lines.
            QString asm_insns_text = Seer::parseFirst(src_and_asm_text, "line_asm_insn=", '[', ']', false);

            QStringList asm_list = Seer::parse(asm_insns_text, "", '{', '}', false);

            for ( const auto& asm_text : asm_list  ) {

                // Get the strings, with padding.
                QString address_text  = Seer::parseFirst(asm_text, "address=",   '"', '"', false);
                QString funcname_text = Seer::parseFirst(asm_text, "func-name=", '"', '"', false);
                QString offset_num    = Seer::parseFirst(asm_text, "offset=",    '"', '"', false);
                QString opcodes_text  = Seer::parseFirst(asm_text, "opcodes=",   '"', '"', false);
                QString inst_text     = Seer::parseFirst(asm_text, "inst=",      '"', '"', false);

                inst_text = Seer::expandTabs(inst_text, editorTabSize(), true); // Expand tabs.

                // Write assembly line to the document.
                if (convertUppercase()) {
                    appendPlainText(inst_text.toUpper());
                }else{
                    appendPlainText(inst_text);
                }

                // Highlight it.
                QTextCharFormat sourceLinesFormat = highlighterSettings().get("Assembly Text");

                QTextBlock  block  = document()->findBlockByLineNumber(lineno-1);
                QTextCursor cursor = textCursor();

                cursor.setPosition(block.position());
                setTextCursor(cursor);

                QTextEdit::ExtraSelection selection;
                selection.format.setForeground(sourceLinesFormat.foreground());
                selection.format.setBackground(sourceLinesFormat.background());
                selection.format.setProperty(QTextFormat::FullWidthSelection, true);
                selection.cursor = textCursor();
                selection.cursor.clearSelection();

                _sourceLinesExtraSelections.append(selection);

                // Add to maps
                _addressLineMap.insert(address_text.toULongLong(0,0), lineno);
                _offsetLineMap.insert(offset_num.toULongLong(0,0), lineno);
                _lineAddressMap.insert(lineno, address_text);
                _lineOffsetMap.insert(lineno,  offset_num.toULongLong(0,0));
                _lineOpcodeMap.insert(lineno,  opcodes_text);

                lineno++;
            }
        }

    }else if (asm_list.size() > 0) {

        //qDebug() << "asm_text mode.";

        // Loop through the asm list and print each line.
        int lineno = 1;

        for ( const auto& asm_text : asm_list  ) {

            // Get the strings, with padding.
            QString address_text = Seer::parseFirst(asm_text, "address=", '"', '"', false);
            QString offset_num   = Seer::parseFirst(asm_text, "offset=",  '"', '"', false);
            QString opcodes_text = Seer::parseFirst(asm_text, "opcodes=", '"', '"', false);
            QString inst_text    = Seer::parseFirst(asm_text, "inst=",    '"', '"', false);

            inst_text = Seer::expandTabs(inst_text, editorTabSize(), true); // Expand tabs.

            // Write assembly line to the document.
            if (convertUppercase()) {
                appendPlainText(QString(" ") + inst_text.toUpper());
            }else{
                appendPlainText(QString(" ") + inst_text);
            }

            // Highlight it.
            QTextCharFormat sourceLinesFormat = highlighterSettings().get("Assembly Text");

            QTextBlock  block  = document()->findBlockByLineNumber(lineno-1);
            QTextCursor cursor = textCursor();

            cursor.setPosition(block.position());
            setTextCursor(cursor);

            QTextEdit::ExtraSelection selection;
            selection.format.setForeground(sourceLinesFormat.foreground());
            selection.format.setBackground(sourceLinesFormat.background());
            selection.format.setProperty(QTextFormat::FullWidthSelection, true);
            selection.cursor = textCursor();
            selection.cursor.clearSelection();

            _sourceLinesExtraSelections.append(selection);

            // Add to maps
            _addressLineMap.insert(address_text.toULongLong(0,0), lineno);
            _offsetLineMap.insert(offset_num.toULongLong(0,0), lineno);
            _lineAddressMap.insert(lineno, address_text);
            _lineOffsetMap.insert(lineno,  offset_num.toULongLong(0,0));
            _lineOpcodeMap.insert(lineno,  opcodes_text);

            lineno++;
        }

    }else{
        qDebug() << "src_and_asm_text and asm_text are both empty.";
    }

    // Refresh all the extra selections.
    refreshExtraSelections();

    // Move to the start of the document as a default.
    moveCursor(QTextCursor::Start);

    // Move to the line that has our address.
    setCurrentLine(_currentAddress);

    // Set the cursor back.
    QApplication::restoreOverrideCursor();

    invalidateMiniMapCache();
}

void SeerEditorWidgetAssemblyArea::updateMarginAreasWidth (int newBlockCount) {

    Q_UNUSED(newBlockCount);

    int leftMarginWidth  = lineNumberAreaWidth() + offsetAreaWidth() + breakPointAreaWidth() + opcodeAreaWidth();
    int rightMarginWidth = miniMapAreaWidth();

    setViewportMargins(leftMarginWidth, 0, rightMarginWidth, 0);
}

int SeerEditorWidgetAssemblyArea::lineNumberAreaWidth () {

    if (lineNumberAreaEnabled() == false) {
        return 0;
    }

    int chars  = 1;

    QMap<int,QString>::iterator b = _lineAddressMap.begin();
    QMap<int,QString>::iterator e = _lineAddressMap.end();

    while (b != e) {

        chars = qMax(chars, b->length());

        b++;
    }

    int space = 3 + fontMetrics().horizontalAdvance(QLatin1Char('9')) * chars;

    return space;
}

int SeerEditorWidgetAssemblyArea::miniMapAreaWidth () {

    if (miniMapAreaEnabled() == false) {
        return 0;
    }

    return 120;
}

int SeerEditorWidgetAssemblyArea::offsetAreaWidth () {

    if (offsetAreaEnabled() == false) {
        return 0;
    }

    qulonglong offset  = 0;

    QMap<int,qulonglong>::iterator b = _lineOffsetMap.begin();
    QMap<int,qulonglong>::iterator e = _lineOffsetMap.end();

    while (b != e) {

        offset = qMax(offset, b.value());

        b++;
    }

    QString tmp = QString("<+%1>").arg(offset);

    int space = 3 + fontMetrics().horizontalAdvance(QLatin1Char('9')) * tmp.length();

    return space;
}

int SeerEditorWidgetAssemblyArea::opcodeAreaWidth () {

    if (opcodeAreaEnabled() == false) {
        return 0;
    }

    int chars  = 1;

    QMap<int,QString>::iterator b = _lineOpcodeMap.begin();
    QMap<int,QString>::iterator e = _lineOpcodeMap.end();

    while (b != e) {

        chars = qMax(chars, b->length());

        b++;
    }

    int space = 3 + fontMetrics().horizontalAdvance(QLatin1Char('9')) * chars;

    return space;
}

int SeerEditorWidgetAssemblyArea::breakPointAreaWidth () {

    if (breakPointAreaEnabled() == false) {
        return 0;
    }

    int space = 3 + 20;

    return space;
}

void SeerEditorWidgetAssemblyArea::updateLineNumberArea (const QRect& rect, int dy) {

    if (lineNumberAreaEnabled() == false) {
        return;
    }

    if (dy) {
        _lineNumberArea->scroll(0, dy);
    }else{
        _lineNumberArea->update(0, rect.y(), _lineNumberArea->width(), rect.height());
    }

    if (rect.contains(viewport()->rect())) {
        updateMarginAreasWidth(0);
    }
}

void SeerEditorWidgetAssemblyArea::updateOffsetArea (const QRect& rect, int dy) {

    if (offsetAreaEnabled() == false) {
        return;
    }

    if (dy) {
        _offsetArea->scroll(0, dy);
    }else{
        _offsetArea->update(0, rect.y(), _offsetArea->width(), rect.height());
    }

    if (rect.contains(viewport()->rect())) {
        updateMarginAreasWidth(0);
    }
}

void SeerEditorWidgetAssemblyArea::updateOpcodeArea (const QRect& rect, int dy) {

    if (opcodeAreaEnabled() == false) {
        return;
    }

    if (dy) {
        _opcodeArea->scroll(0, dy);
    }else{
        _opcodeArea->update(0, rect.y(), _opcodeArea->width(), rect.height());
    }

    if (rect.contains(viewport()->rect())) {
        updateMarginAreasWidth(0);
    }
}

void SeerEditorWidgetAssemblyArea::updateBreakPointArea (const QRect& rect, int dy) {

    if (breakPointAreaEnabled() == false) {
        return;
    }

    if (dy) {
        _breakPointArea->scroll(0, dy);
    }else{
        _breakPointArea->update(0, rect.y(), _breakPointArea->width(), rect.height());
    }

    if (rect.contains(viewport()->rect())) {
        updateMarginAreasWidth(0);
    }
}

void SeerEditorWidgetAssemblyArea::updateMiniMapArea (const QRect& rect, int dy) {

    Q_UNUSED(rect);
    Q_UNUSED(dy);

    if (miniMapAreaEnabled() == false) {
        return;
    }

    // The mini-map's cached content is a whole-document rendering, not a
    // viewport-following strip, so there's nothing to scroll - just repaint
    // the (cheap) viewport-position overlay on top of the cached pixmap.
    _miniMapArea->update();
}

void SeerEditorWidgetAssemblyArea::invalidateMiniMapCache () {

    _miniMapPixmapDirty = true;

    if (_miniMapArea) {
        _miniMapArea->update();
    }
}

void SeerEditorWidgetAssemblyArea::lineNumberAreaPaintEvent (QPaintEvent* event) {

    if (lineNumberAreaEnabled() == false) {
        return;
    }

    QTextCharFormat format = highlighterSettings().get("Margin");

    QPainter painter(_lineNumberArea);
    painter.fillRect(event->rect(), format.background().color());
    painter.setPen(format.foreground().color());

    QFont font = painter.font();
    font.setItalic(format.fontItalic());
    font.setWeight(QFont::Weight(format.fontWeight()));
    painter.setFont(font);

    QTextBlock block       = firstVisibleBlock();
    int        blockNumber = block.blockNumber();
    int        top         = qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
    int        bottom      = top + qRound(blockBoundingRect(block).height());

    while (block.isValid() && top <= event->rect().bottom()) {

        if (block.isVisible() && bottom >= event->rect().top()) {

            QString address;

            if (_lineAddressMap.contains(blockNumber+1)) {
                if (convertUppercase()) {
                    address = _lineAddressMap[blockNumber+1].toUpper();
                }else{
                    address = _lineAddressMap[blockNumber+1];
                }
            }

            painter.drawText(0, top, _lineNumberArea->width(), fontMetrics().height(), Qt::AlignLeft, address);
        }

        block  = block.next();
        top    = bottom;
        bottom = top + qRound(blockBoundingRect(block).height());

        blockNumber++;
    }
}

void SeerEditorWidgetAssemblyArea::offsetAreaPaintEvent (QPaintEvent* event) {

    if (offsetAreaEnabled() == false) {
        return;
    }

    QTextCharFormat format = highlighterSettings().get("Margin");

    QPainter painter(_offsetArea);
    painter.fillRect(event->rect(), format.background().color());
    painter.setPen(format.foreground().color());

    QFont font = painter.font();
    font.setItalic(format.fontItalic());
    font.setWeight(QFont::Weight(format.fontWeight()));
    painter.setFont(font);

    QTextBlock block       = firstVisibleBlock();
    int        blockNumber = block.blockNumber();
    int        top         = qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
    int        bottom      = top + qRound(blockBoundingRect(block).height());

    while (block.isValid() && top <= event->rect().bottom()) {

        if (block.isVisible() && bottom >= event->rect().top()) {

            QString offset;

            if (_lineOffsetMap.contains(blockNumber+1)) {
                offset = QString("<+%1>").arg(_lineOffsetMap[blockNumber+1]);
            }

            painter.drawText(0, top, _offsetArea->width(), fontMetrics().height(), Qt::AlignRight, offset);
        }

        block  = block.next();
        top    = bottom;
        bottom = top + qRound(blockBoundingRect(block).height());

        blockNumber++;
    }
}

void SeerEditorWidgetAssemblyArea::breakPointAreaPaintEvent (QPaintEvent* event) {

    if (breakPointAreaEnabled() == false) {
        return;
    }

    QTextCharFormat format = highlighterSettings().get("Margin");

    QPainter painter(_breakPointArea);
    painter.fillRect(event->rect(), format.background().color());
    painter.setPen(format.foreground().color());

    QFont font = painter.font();
    font.setItalic(format.fontItalic());
    font.setWeight(QFont::Weight(format.fontWeight()));
    painter.setFont(font);

    QTextBlock block       = firstVisibleBlock();
    int        blockNumber = block.blockNumber();
    int        top         = qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
    int        bottom      = top + qRound(blockBoundingRect(block).height());

    while (block.isValid() && top <= event->rect().bottom()) {

        if (block.isVisible() && bottom >= event->rect().top()) {

            QString address = _lineAddressMap[blockNumber+1];

            if (address != "" && hasBreakpointAddress(address) == true) {
                if (breakpointAddressEnabled(address)) {
                    QRect rect(_breakPointArea->width() - 20, top, fontMetrics().height(), fontMetrics().height());

                    QPainterPath path;
                    path.addEllipse(rect);

                    QPointF bias = QPointF(rect.width() * .25 * 1.0, rect.height() * .25 * -1.0);

                    QRadialGradient gradient(rect.center(), rect.width() / 2.0, rect.center() + bias);
                    gradient.setColorAt(0.0, QColor(Qt::white));
                    gradient.setColorAt(0.9, QColor(Qt::red));
                    gradient.setColorAt(1.0, QColor(Qt::transparent));
                    painter.fillPath(path,QBrush(gradient));

                }else{
                    QRect rect(_breakPointArea->width() - 20, top, fontMetrics().height(), fontMetrics().height());

                    QPainterPath path;
                    path.addEllipse(rect);

                    QPointF bias = QPointF(rect.width() * .25 * 1.0, rect.height() * .25 * -1.0);

                    QRadialGradient gradient(rect.center(), rect.width() / 2.0, rect.center() + bias);
                    gradient.setColorAt(0.0, QColor(Qt::white));
                    gradient.setColorAt(0.9, QColor(Qt::darkGray));
                    gradient.setColorAt(1.0, Qt::transparent);
                    painter.fillPath(path,QBrush(gradient));
                }
            }
        }

        block  = block.next();
        top    = bottom;
        bottom = top + qRound(blockBoundingRect(block).height());

        blockNumber++;
    }
}

void SeerEditorWidgetAssemblyArea::opcodeAreaPaintEvent (QPaintEvent* event) {

    if (opcodeAreaEnabled() == false) {
        return;
    }

    QTextCharFormat format = highlighterSettings().get("Margin");

    QPainter painter(_opcodeArea);
    painter.fillRect(event->rect(), format.background().color());
    painter.setPen(format.foreground().color());

    QFont font = painter.font();
    font.setItalic(format.fontItalic());
    font.setWeight(QFont::Weight(format.fontWeight()));
    painter.setFont(font);

    QTextBlock block       = firstVisibleBlock();
    int        blockNumber = block.blockNumber();
    int        top         = qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
    int        bottom      = top + qRound(blockBoundingRect(block).height());

    while (block.isValid() && top <= event->rect().bottom()) {

        if (block.isVisible() && bottom >= event->rect().top()) {

            QString opcode;

            if (_lineOpcodeMap.contains(blockNumber+1)) {
                if (convertUppercase()) {
                    opcode = " " + _lineOpcodeMap[blockNumber+1].toUpper();
                }else{
                    opcode = " " + _lineOpcodeMap[blockNumber+1];
                }
            }

            painter.drawText(0, top, _opcodeArea->width(), fontMetrics().height(), Qt::AlignLeft, opcode);
        }

        block  = block.next();
        top    = bottom;
        bottom = top + qRound(blockBoundingRect(block).height());

        blockNumber++;
    }
}

void SeerEditorWidgetAssemblyArea::miniMapAreaPaintEvent (QPaintEvent* event) {

    Q_UNUSED(event);

    if (miniMapAreaEnabled() == false) {
        return;
    }

    QTextCharFormat marginFormat = highlighterSettings().get("Margin");

    // Rebuild the cached content pixmap if the document, theme, or size changed.
    // Rendering uses the document's real font (so glyph shaping stays correct)
    // and only shrinks the *rasterized* output via a scaled QPainter - unlike
    // shrinking the live font's point size, which is known to break tab-stop
    // rendering (see setEditorFont()'s comments).
    //
    // Text is drawn as plain strings in a single color - the real editor
    // colors whole lines differently (source vs. assembly) via
    // _sourceLinesExtraSelections, but at mini-map scale mixing those line
    // colors in just looks like wrong/inconsistent colors rather than useful
    // information, so the mini-map ignores it and uses one uniform color.
    if (_miniMapPixmapDirty || _miniMapPixmap.size() != _miniMapArea->size()) {

        QSize size = _miniMapArea->size();

        _miniMapPixmap = QPixmap(size);
        _miniMapPixmap.fill(marginFormat.background().color());

        int   maxChars    = 1;
        qreal docHeightPx = 0;

        for (QTextBlock block = document()->firstBlock(); block.isValid(); block = block.next()) {
            maxChars     = qMax(maxChars, block.text().length());
            docHeightPx += blockBoundingRect(block).height();   // also forces the block's layout
        }

        qreal docWidthPx = maxChars * qreal(fontMetrics().horizontalAdvance(QLatin1Char('9')));

        _miniMapContentHeight = 0;

        if (size.isEmpty() == false && docWidthPx > 0 && docHeightPx > 0) {

            qreal sx = size.width()  / docWidthPx;
            qreal sy = size.height() / docHeightPx;

            // Cap the vertical scale so very short files don't get stretched
            // into oversized, blurry-looking lines.
            qreal maxScaleY = 3.0 / qreal(qMax(1, fontMetrics().height()));
            sy = qMin(sy, maxScaleY);

            // Remember the actual on-screen height the content was rendered
            // at (which may be less than the widget's full height, since sy
            // is capped above) so the viewport overlay and click/drag-to-jump
            // math below use the same scale as the cached pixmap - otherwise
            // the overlay drifts out of sync with the rendered content for
            // any listing short enough to hit the cap.
            _miniMapContentHeight = docHeightPx * sy;

            QPainter docPainter(&_miniMapPixmap);
            docPainter.setRenderHint(QPainter::Antialiasing, true);
            docPainter.setRenderHint(QPainter::TextAntialiasing, true);
            docPainter.scale(sx, sy);
            docPainter.setFont(font());
            docPainter.setPen(marginFormat.foreground().color());

            qreal         y      = 0;
            QFontMetricsF fm     (font());
            qreal         ascent = fm.ascent();

            for (QTextBlock block = document()->firstBlock(); block.isValid(); block = block.next()) {

                QString text = block.text();
                text.replace(QLatin1Char('\t'), QLatin1String("    "));

                docPainter.drawText(QPointF(0, y + ascent), text);

                y += blockBoundingRect(block).height();
            }
        }

        _miniMapPixmapDirty = false;
    }

    QPainter painter(_miniMapArea);
    painter.drawPixmap(0, 0, _miniMapPixmap);

    // Overlay: highlight the portion of the document currently visible in the editor.
    // Use the content's actual rendered height (not the widget's full height)
    // so the overlay stays aligned with the cached pixmap even when the
    // vertical scale was capped (see above).
    int   totalLines = qMax(1, document()->blockCount());
    qreal pxPerLine  = _miniMapContentHeight / qreal(totalLines);

    int firstLine    = firstVisibleBlock().blockNumber();
    int visibleLines = qMax(1, int(viewport()->height() / qMax(1, fontMetrics().height())) + 1);

    QRectF overlay(0, firstLine * pxPerLine, _miniMapArea->width(), qMax(4.0, visibleLines * pxPerLine));

    QTextCharFormat currentLineFormat = highlighterSettings().get("Current Line");
    QColor          overlayColor      = currentLineFormat.background().color();
    overlayColor.setAlpha(90);

    painter.fillRect(overlay, overlayColor);
    painter.setPen(currentLineFormat.background().color());
    painter.drawRect(overlay.adjusted(0, 0, -1, -1));
}

void SeerEditorWidgetAssemblyArea::resizeEvent (QResizeEvent* e) {

    QPlainTextEdit::resizeEvent(e);

    QRect cr       = contentsRect();
    int   leftbias = 0;

    if (lineNumberAreaEnabled()) {
        _lineNumberArea->setGeometry (QRect(cr.left() + leftbias, cr.top(), lineNumberAreaWidth(), cr.height()));

        leftbias += lineNumberAreaWidth();
    }

    if (offsetAreaEnabled()) {
        _offsetArea->setGeometry (QRect(cr.left() + leftbias, cr.top(), offsetAreaWidth(), cr.height()));

        leftbias += offsetAreaWidth();
    }

    if (opcodeAreaEnabled()) {
        _opcodeArea->setGeometry (QRect(cr.left() + leftbias, cr.top(), opcodeAreaWidth(), cr.height()));

        leftbias += opcodeAreaWidth();
    }

    if (breakPointAreaEnabled()) {
        _breakPointArea->setGeometry (QRect(cr.left() + leftbias, cr.top(), breakPointAreaWidth(), cr.height()));

        leftbias += breakPointAreaWidth();
    }

    if (miniMapAreaEnabled()) {
        _miniMapArea->setGeometry (QRect(cr.right() - miniMapAreaWidth() + 1, cr.top(), miniMapAreaWidth(), cr.height()));

        // The widget's height changed, so the cached pixmap's vertical scale is stale.
        invalidateMiniMapCache();
    }
}

void SeerEditorWidgetAssemblyArea::contextMenuEvent (QContextMenuEvent* event) {

    showContextMenu(event);
}

void SeerEditorWidgetAssemblyArea::refreshExtraSelections () {

    //
    // Merge all the extra selections into one.
    //
    // The current line(s)
    // The searched text.
    //

    // Create an empty list of selections.
    QList<QTextEdit::ExtraSelection> extraSelections;

    // Append the 'source lines' extra selections.
    extraSelections.append(_sourceLinesExtraSelections);

    // Append the 'current lines' extra selections.
    extraSelections.append(_currentLinesExtraSelections);

    // Append the 'searched text' extra selections.
    extraSelections.append(_findExtraSelections);

    // Give the editor the list of selections.
    // This will remove the old selections and select the new ones.
    setExtraSelections(extraSelections);
}

void SeerEditorWidgetAssemblyArea::setAddress (const QString& address, bool force) {

    // Emit the signal to load the assembly for address 'address' if
    // it's not already loaded.
    if (_addressLineMap.contains(address.toULongLong(0,0)) == false || force == true) {

        // Hack to keep track of address when the assembly hasn't been loaded yet.
        _currentAddress = address;

        emit requestSourceAndAssembly(address);
    }
}

const QString& SeerEditorWidgetAssemblyArea::address () const {

    return _currentAddress;
}

bool SeerEditorWidgetAssemblyArea::setCurrentLine (const QString& address) {

    // Clear current line selections.
    _currentLinesExtraSelections.clear();

    // See scrollToLine().
    //
    // Initial lineno.
    bool ok     = false;
    int  lineno = 0;

    // Try parsing as an address.
    if (ok == false && address.startsWith("0x") == true) {

        qulonglong addr = address.toULongLong(&ok, 0); // Try it as an '0x.....'.

        if (ok) {
            if (_addressLineMap.contains(addr)) {
                lineno = _addressLineMap[addr];
            }else{
                ok = false;
            }
        }
    }

    // Try parsing as an offset.
    if (ok == false && address.startsWith("+") == true) {

        qulonglong offset = address.toULongLong(&ok, 0); // Try it as an '+.....'.

        if (ok) {
            if (_offsetLineMap.contains(offset)) {
                lineno = _offsetLineMap[offset];
            }else{
                ok = false;
            }
        }
    }

    // Try parsing as an integer.
    if (ok == false && address.startsWith("0x") == false && address.startsWith("+") == false) {
        lineno = address.toInt(&ok, 10); // Try it as an 'int'. lineno == 0 on error.
    }

    // Stop if no valid lineno.
    if (ok == false || lineno < 1) {
        //qDebug() << "address is not in current assembly: '" << address << "'";
        return false;
    }

    // Highlight if a valid line number is selected.
    QTextBlock  block  = document()->findBlockByLineNumber(lineno-1);
    QTextCursor cursor = textCursor();

    cursor.setPosition(block.position());
    setTextCursor(cursor);

    _currentLinesExtraSelections.clear();

    QTextCharFormat currentLineFormat = highlighterSettings().get("Current Line");

    QTextEdit::ExtraSelection selection;
    selection.format.setForeground(currentLineFormat.foreground());
    selection.format.setBackground(currentLineFormat.background());
    selection.format.setProperty(QTextFormat::FullWidthSelection, true);
    selection.cursor = textCursor();
    selection.cursor.clearSelection();

    _currentLinesExtraSelections.append(selection);

    // Scroll to the line.
    scrollToLine(address);

    // Refresh all the extra selections.
    refreshExtraSelections();

    return true;
}

const QString SeerEditorWidgetAssemblyArea::currentLine () const {

    // Get current lineno.
    int lineno = textCursor().blockNumber() + 1;

    QString address = _lineAddressMap[lineno];

    return address;
}

void SeerEditorWidgetAssemblyArea::scrollToLine (const QString& address) {

    // Just return.
    if (address == "") {
        return;
    }

    //
    // 'address' can be one of these forms:
    //
    //      0xdeadbeef      An address
    //      +1000           An offset
    //      10              A linenumber
    //
    // Test for which one and convert it to a linenumber using the maps:
    //
    //      _addressLineMap
    //      _offsetLineMap
    //
    //      Or just go to the line#
    //

    // Initial lineno.
    bool ok     = false;
    int  lineno = 0;

    // Try parsing as an address.
    if (ok == false && address.startsWith("0x") == true) {

        qulonglong addr = address.toULongLong(&ok, 0); // Try it as an '0x.....'.

        if (ok) {
            if (_addressLineMap.contains(addr)) {
                lineno = _addressLineMap[addr];
            }else{
                ok = false;
            }
        }
    }

    // Try parsing as an offset.
    if (ok == false && address.startsWith("+") == true) {

        qulonglong offset = address.toULongLong(&ok, 0); // Try it as an '+.....'.

        if (ok) {
            if (_offsetLineMap.contains(offset)) {
                lineno = _offsetLineMap[offset];
            }else{
                ok = false;
            }
        }
    }

    // Try parsing as an integer.
    if (ok == false && address.startsWith("0x") == false && address.startsWith("+") == false) {
        lineno = address.toInt(&ok, 10); // Try it as an 'int'. lineno == 0 on error.
    }

    // 'address' is meaningless. Just return.
    if (ok == false) {
        if (_currentAddress == "") {  // Hack when the assembly hasn't been loaded yet.
            QMessageBox::warning(this, "Warning.", "Address/offset '" + address + "' not found.");
        }
        return;
    }

    // Scroll to the first line if we went before it.
    if (lineno < 1) {
        lineno = 1;
    }

    // Scroll to the last line if we went past it.
    if (lineno > document()->blockCount()) {
        lineno = document()->blockCount();
    }

    QTextBlock  block  = document()->findBlockByLineNumber(lineno-1);
    QTextCursor cursor = textCursor();

    cursor.setPosition(block.position());
    setTextCursor(cursor);

    centerCursor();
}

void SeerEditorWidgetAssemblyArea::jumpToMiniMapY (int y) {

    if (miniMapAreaEnabled() == false) {
        return;
    }

    int totalLines = qMax(1, document()->blockCount());
    qreal pxPerLine = _miniMapContentHeight / qreal(totalLines);

    if (pxPerLine <= 0) {
        return;
    }

    int lineno = int(y / pxPerLine) + 1;

    // Reuse the existing address/offset/linenumber scroll-to entry point -
    // a plain integer string is parsed as a line number (see above).
    scrollToLine(QString::number(lineno));
}

void SeerEditorWidgetAssemblyArea::clearCurrentLines () {

    _currentLinesExtraSelections.clear();

    refreshExtraSelections();
}

int SeerEditorWidgetAssemblyArea::findText (const QString& text, QTextDocument::FindFlags flags) {

    _findExtraSelections.clear();

    if (document()) {

        QTextCharFormat matchFormat = highlighterSettings().get("Match");

        // Build a list of highlights for all matches.
        QTextCursor cursor(document());
        cursor = document()->find(text, cursor, flags);

        while (cursor.isNull() == false) {

            QTextEdit::ExtraSelection extra;
            extra.format = matchFormat;
            extra.cursor = cursor;

            _findExtraSelections.append(extra);

            cursor = document()->find(text, cursor, flags);
        }

        // Move to the next match after out current position.
        find(text, flags);
    }

    refreshExtraSelections();

    return _findExtraSelections.size();
}

void SeerEditorWidgetAssemblyArea::clearFindText () {

    _findExtraSelections.clear();

    refreshExtraSelections();
}

void SeerEditorWidgetAssemblyArea::clearBreakpoints () {

    _breakpointsNumbers.clear();
    _breakpointsAddresses.clear();
    _breakpointsEnableds.clear();

    repaint();
}

void SeerEditorWidgetAssemblyArea::addBreakpoint (int number, const QString& address, bool enabled) {

    _breakpointsNumbers.push_back(number);
    _breakpointsAddresses.push_back(address);
    _breakpointsEnableds.push_back(enabled);

    repaint();
}

bool SeerEditorWidgetAssemblyArea::hasBreakpointNumber (int number) const {
    return _breakpointsNumbers.contains(number);
}

bool SeerEditorWidgetAssemblyArea::hasBreakpointAddress (const QString& address) const {
    return _breakpointsAddresses.contains(address);
}

const QVector<int>& SeerEditorWidgetAssemblyArea::breakpointNumbers () const {
    return _breakpointsNumbers;
}

const QVector<QString>& SeerEditorWidgetAssemblyArea::breakpointAddresses () const {
    return _breakpointsAddresses;
}

const QVector<bool>& SeerEditorWidgetAssemblyArea::breakpointEnableds () const {
    return _breakpointsEnableds;
}

int SeerEditorWidgetAssemblyArea::breakpointAddressToNumber (const QString& address) const {

    // Map address to breakpoint number.
    int i = _breakpointsAddresses.indexOf(address);

    if (i < 0) {
        return 0;
    }

    return _breakpointsNumbers[i];
}

bool SeerEditorWidgetAssemblyArea::breakpointAddressEnabled (const QString& address) const {

    // Look for the address and get its index.
    int i = _breakpointsAddresses.indexOf(address);

    // Not found, return false.
    if (i < 0) {
        return false;
    }

    // Otherwise, return the proper status.
    return _breakpointsEnableds[i];
}

void SeerEditorWidgetAssemblyArea::breakpointToggle () {

    // Get current lineno.
    int lineno = textCursor().blockNumber() + 1;

    QString address = _lineAddressMap[lineno];

    if (address == "") {
        return;
    }

    // If there is a breakpoint on the line, toggle it.
    if (hasBreakpointAddress(address)) {

        // Toggle the breakpoint.
        // Enable if disabled. Disable if enabled.
        if (breakpointAddressEnabled(address) == false) {
            // Emit the enable breakpoint signal.
            emit enableBreakpoints(QString("%1").arg(breakpointAddressToNumber(address)));
        }else{
            // Emit the disable breakpoint signal.
            emit deleteBreakpoints(QString("%1").arg(breakpointAddressToNumber(address)));
        }

    // Otherwise, do a quick create of a new breakpoint.
    }else{
        emit insertBreakpoint(QString("-f *%1").arg(address));
    }
}

void SeerEditorWidgetAssemblyArea::showContextMenu (QMouseEvent* event) {

#if QT_VERSION >= 0x060000
    showContextMenu(event->pos(), event->globalPosition());
#else
    showContextMenu(event->pos(), event->globalPos());
#endif
}

void SeerEditorWidgetAssemblyArea::showContextMenu (QContextMenuEvent* event) {

    showContextMenu(event->pos(), event->globalPos());
}

void SeerEditorWidgetAssemblyArea::showContextMenu (const QPoint& pos, const QPointF& globalPos) {

    // Get the line number for the cursor position.
    QTextCursor cursor = cursorForPosition(pos);

    int lineno = cursor.blockNumber()+1;

    QString address = _lineAddressMap[lineno];

    // Create the menu actions.
    QAction* runToAddressAction;
    QAction* createBreakpointAction;
    QAction* deleteAction;
    QAction* enableAction;
    QAction* disableAction;
    QAction* addMemoryAddressVisualizerAction;
    QAction* addArrayAddressVisualizerAction;
    QAction* addMatrixAddressVisualizerAction;
    QAction* addStructAddressVisualizerAction;

    // Enable/disable them depending if the breakpoint already exists.
    if (hasBreakpointAddress(address) == true) {

        int breakno = breakpointAddressToNumber(address);

        runToAddressAction        = new QAction(QIcon(":/seer/resources/RelaxLightIcons/debug-execute-from-cursor.svg"), QString("Run to address %1").arg(address), this);
        createBreakpointAction    = new QAction(QIcon(":/seer/resources/RelaxLightIcons/document-new.svg"),              QString("Create breakpoint on address %1").arg(address), this);
        deleteAction              = new QAction(QIcon(":/seer/resources/RelaxLightIcons/edit-delete.svg"),               QString("Delete breakpoint %1 on address %2").arg(breakno).arg(address), this);
        enableAction              = new QAction(QIcon(":/seer/resources/RelaxLightIcons/list-add.svg"),                  QString("Enable breakpoint %1 on address %2").arg(breakno).arg(address), this);
        disableAction             = new QAction(QIcon(":/seer/resources/RelaxLightIcons/list-remove.svg"),               QString("Disable breakpoint %1 on address %2").arg(breakno).arg(address), this);

        runToAddressAction->setEnabled(true);
        createBreakpointAction->setEnabled(false);
        deleteAction->setEnabled(true);
        enableAction->setEnabled(true);
        disableAction->setEnabled(true);

    }else{
        runToAddressAction        = new QAction(QIcon(":/seer/resources/RelaxLightIcons/debug-execute-from-cursor.svg"), QString("Run to address %1").arg(address), this);
        createBreakpointAction    = new QAction(QIcon(":/seer/resources/RelaxLightIcons/document-new.svg"),              QString("Create breakpoint on address %1").arg(address), this);
        deleteAction              = new QAction(QIcon(":/seer/resources/RelaxLightIcons/edit-delete.svg"),               QString("Delete breakpoint on address %1").arg(address), this);
        enableAction              = new QAction(QIcon(":/seer/resources/RelaxLightIcons/list-add.svg"),                  QString("Enable breakpoint on address %1").arg(address), this);
        disableAction             = new QAction(QIcon(":/seer/resources/RelaxLightIcons/list-remove.svg"),               QString("Disable breakpoint on address %1").arg(address), this);

        runToAddressAction->setEnabled(true);
        createBreakpointAction->setEnabled(true);
        deleteAction->setEnabled(false);
        enableAction->setEnabled(false);
        disableAction->setEnabled(false);
    }

    addMemoryAddressVisualizerAction = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));
    addArrayAddressVisualizerAction  = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));
    addMatrixAddressVisualizerAction = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));
    addStructAddressVisualizerAction = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));

    QMenu menu("Breakpoints", this);
    menu.setTitle("Breakpoints");
    menu.addAction(runToAddressAction);
    menu.addAction(createBreakpointAction);
    menu.addAction(deleteAction);
    menu.addAction(enableAction);
    menu.addAction(disableAction);

    QMenu memoryVisualizerMenu("Add address to a Memory Visualizer");
    memoryVisualizerMenu.addAction(addMemoryAddressVisualizerAction);
    menu.addMenu(&memoryVisualizerMenu);

    QMenu arrayVisualizerMenu("Add address to an Array Visualizer");
    arrayVisualizerMenu.addAction(addArrayAddressVisualizerAction);
    menu.addMenu(&arrayVisualizerMenu);

    QMenu matrixVisualizerMenu("Add address to a Matrix Visualizer");
    matrixVisualizerMenu.addAction(addMatrixAddressVisualizerAction);
    menu.addMenu(&matrixVisualizerMenu);

    QMenu structVisualizerMenu("Add address to a Struct Visualizer");
    structVisualizerMenu.addAction(addStructAddressVisualizerAction);
    menu.addMenu(&structVisualizerMenu);

    // Enable/disable items based on something being selected or not.
    if (textCursor().selectedText() == "") {
        addMemoryAddressVisualizerAction->setEnabled(false);
        addArrayAddressVisualizerAction->setEnabled(false);
        addMatrixAddressVisualizerAction->setEnabled(false);
        addStructAddressVisualizerAction->setEnabled(false);
    }else{
        addMemoryAddressVisualizerAction->setEnabled(true);
        addArrayAddressVisualizerAction->setEnabled(true);
        addMatrixAddressVisualizerAction->setEnabled(true);
        addStructAddressVisualizerAction->setEnabled(true);
    }

    // Launch the menu. Get the response.
    QAction* action = menu.exec(globalPos.toPoint());

    // Do nothing.
    if (action == 0) {
        return;
    }

    // Handle running to an address.
    if (action == runToAddressAction) {

        // Emit the runToLine signal.
        emit runToAddress(address);

        return;
    }

    // Handle creating a new breakpoint.
    if (action == createBreakpointAction) {

        SeerBreakpointCreateDialog dlg(this);
        dlg.setAddress(address);

        int ret = dlg.exec();

        if (ret == 0) {
            return;
        }

        // Emit the create breakpoint signal.
        emit insertBreakpoint(dlg.breakpointText());

        return;
    }

    // Handle deleting a breakpoint.
    if (action == deleteAction) {

        // Emit the delete breakpoint signal.
        emit deleteBreakpoints(QString("%1").arg(breakpointAddressToNumber(address)));

        return;
    }

    // Handle enabling a breakpoint.
    if (action == enableAction) {

        // Emit the enable breakpoint signal.
        emit enableBreakpoints(QString("%1").arg(breakpointAddressToNumber(address)));

        return;
    }

    // Handle disabling a breakpoint.
    if (action == disableAction) {

        // Emit the disable breakpoint signal.
        emit disableBreakpoints(QString("%1").arg(breakpointAddressToNumber(address)));

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAddressVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addMemoryVisualizer(textCursor().selectedText());
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayAddressVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addArrayVisualizer(textCursor().selectedText());
        }

        return;
    }

    // Handle adding matrix to visualize.
    if (action == addMatrixAddressVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addMatrixVisualizer(textCursor().selectedText());
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructAddressVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addStructVisualizer(textCursor().selectedText());
        }

        return;
    }
}

void SeerEditorWidgetAssemblyArea::setQuickBreakpoint (QMouseEvent* event) {

    // Get the line number for the cursor position.
    QTextCursor cursor = cursorForPosition(event->pos());

    int lineno = cursor.blockNumber()+1;

    QString address = _lineAddressMap[lineno];

    // If there is a breakpoint on the line, toggle it.
    if (hasBreakpointAddress(address)) {

        // Toggle the breakpoint.
        // Enable if disabled. Disable if enabled.
        if (breakpointAddressEnabled(address) == false) {
            // Emit the enable breakpoint signal.
            emit enableBreakpoints(QString("%1").arg(breakpointAddressToNumber(address)));
        }else{
            // Emit the disable breakpoint signal.
            emit deleteBreakpoints(QString("%1").arg(breakpointAddressToNumber(address)));
        }

    // Otherwise, do a quick create of a new breakpoint.
    }else{
        emit insertBreakpoint(QString("-f *%1").arg(address));
    }
}

void SeerEditorWidgetAssemblyArea::setQuickRunToAddress (QMouseEvent* event) {

    // Get the line number for the cursor position.
    QTextCursor cursor = cursorForPosition(event->pos());

    int lineno = cursor.blockNumber()+1;

    QString address = _lineAddressMap[lineno];

    //qDebug() << "runToAddress" << address;

    // Emit the runToAddress signal.
    if (address != "") {
        emit runToAddress(address);
    }

    return;
}

void SeerEditorWidgetAssemblyArea::setHighlighterSettings (const SeerHighlighterSettings& settings) {

    _sourceHighlighterSettings = settings;

    emit highlighterSettingsChanged();
}

const SeerHighlighterSettings& SeerEditorWidgetAssemblyArea::highlighterSettings () const {

    return _sourceHighlighterSettings;
}

void SeerEditorWidgetAssemblyArea::setHighlighterEnabled (bool flag) {

    _sourceHighlighterEnabled = flag;

    emit highlighterSettingsChanged();
}

bool SeerEditorWidgetAssemblyArea::highlighterEnabled () const {

    return _sourceHighlighterEnabled;
}

QString SeerEditorWidgetAssemblyArea::sourceForLine (const QString& fullname, const QString& file, int line) {

    // A new file?  Zap the previously cached file.
    if (fullname != _fileFullname || file != _fileName) {
        _fileLines    = QStringList();
        _fileFullname = "";
        _fileName     = "";
    }

    // Nothing in cache?  Open the file and read it.
    if (_fileFullname == "" || _fileName == "") {
        _fileFullname = fullname;
        _fileName     = file;

        if (_fileFullname == "") {
            return "";
        }

        bool f = Seer::readFile(_fileFullname, _fileLines);
        if (f == false) {
            qDebug() << "Can't read:" << _fileFullname;
        }
    }

    // Return the requested line.
    // 'line' is 1 based. We want it to be 0 based.

    line --;

    if (line < 0 || line >= _fileLines.size()) {
        return "";
    }

    return _fileLines[line];
}

void SeerEditorWidgetAssemblyArea::setEditorFont (const QFont& font) {

    setFont(font);

    invalidateMiniMapCache();

    // See: SeerEditorSourceArea::setEditorFont()
}

const QFont& SeerEditorWidgetAssemblyArea::editorFont () const {

    return font();
}

void SeerEditorWidgetAssemblyArea::setEditorTabSize (int spaces) {

    _sourceTabSize = spaces;
}

int SeerEditorWidgetAssemblyArea::editorTabSize () const {

    return _sourceTabSize;
}

void SeerEditorWidgetAssemblyArea::setConvertUppercase (bool flag) {

    _convertUppercase = flag;
}

bool SeerEditorWidgetAssemblyArea::convertUppercase () const {

    return _convertUppercase;
}

void SeerEditorWidgetAssemblyArea::handleText (const QString& text) {

    if (text.startsWith("*stopped")) {

        // *stopped,
        //
        // reason="end-stepping-range",
        //
        // frame={addr="0x0000000000400b45",
        //        func="main",
        //        args=[{name="argc",value="1"},{name="argv",value="0x7fffffffd5b8"}],
        //        file="helloworld.cpp",
        //        fullname="/home/erniep/Development/Peak/src/Seer/helloworld/helloworld.cpp",
        //        line="7",
        //        arch="i386:x86-64"},
        //
        // thread-id="1",
        // stopped-threads="all",
        // core="6"

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString frame_text = Seer::parseFirst(newtext, "frame=", '{', '}', false);

        if (frame_text == "") {
            return;
        }

        QString addr_text     = Seer::parseFirst(frame_text, "addr=",     '"', '"', false);
        QString fullname_text = Seer::parseFirst(frame_text, "fullname=", '"', '"', false);
        QString file_text     = Seer::parseFirst(frame_text, "file=",     '"', '"', false);
        QString line_text     = Seer::parseFirst(frame_text, "line=",     '"', '"', false);

        //qDebug() << frame_text;
        //qDebug() << addr_text << fullname_text << file_text << line_text;

        // Will emit the load signal, if 'addr_text' is not already loaded.
        setAddress(addr_text);

        // Set to the line number.
        setCurrentLine(addr_text);

        return;

    }else if (text.startsWith("^done,stack=[") && text.endsWith("]")) {

        //qDebug() << ":stack:" << text;

        //
        // See SeerStackFramesBrowserWidget.cpp
        // ^done,stack=[
        //     ...
        // ]
        //

        // Now parse the table and re-add the current line, if possible.
        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString stack_text = Seer::parseFirst(newtext, "stack=", '[', ']', false);

        if (stack_text != "") {

            // Parse through the frame list and set the current lines that are in the frame list.
            QStringList frame_list = Seer::parse(newtext, "frame=", '{', '}', false);

            for ( const auto& frame_text : frame_list  ) {
                QString level_text    = Seer::parseFirst(frame_text, "level=",    '"', '"', false);
                QString addr_text     = Seer::parseFirst(frame_text, "addr=",     '"', '"', false);
                QString func_text     = Seer::parseFirst(frame_text, "func=",     '"', '"', false);
                QString file_text     = Seer::parseFirst(frame_text, "file=",     '"', '"', false);
                QString fullname_text = Seer::parseFirst(frame_text, "fullname=", '"', '"', false);
                QString line_text     = Seer::parseFirst(frame_text, "line=",     '"', '"', false);
                QString arch_text     = Seer::parseFirst(frame_text, "arch=",     '"', '"', false);

                // If the 'addr' works for the currently loaded assembly, then stop.
                bool f = setCurrentLine(addr_text);
                if (f == true) {
                    break;
                }
            }
        }

    }else if (text.startsWith("^done,asm_insns=")) {

        _asm_insns_text = text;

        updateTextArea(); // This function does all the work on _asm_insns_text.

    }else if (text.startsWith("^error,msg=\"-data-disassemble:")) {

        QString error_text = Seer::parseFirst(text, "msg=", '"', '"', false);

        QMessageBox::warning(this, "Warning.", error_text);
    }

}

void SeerEditorWidgetAssemblyArea::handleHighlighterSettingsChanged () {

    // When this handler is called, the new Highlight settings have
    // already been set. So we just need to redo the text area.
    updateTextArea();

    // Note.
    // The margins are automatically updated by their own paint events.
    // The new highlighter settings will be used.
}

//
// LineNumber Area.
//

SeerEditorWidgetAssemblyLineNumberArea::SeerEditorWidgetAssemblyLineNumberArea(SeerEditorWidgetAssemblyArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
}

QSize SeerEditorWidgetAssemblyLineNumberArea::sizeHint () const {
    return QSize(_editorWidget->lineNumberAreaWidth(), 0);
}

void SeerEditorWidgetAssemblyLineNumberArea::paintEvent (QPaintEvent* event) {
    _editorWidget->lineNumberAreaPaintEvent(event);
}

void SeerEditorWidgetAssemblyLineNumberArea::mouseDoubleClickEvent (QMouseEvent* event) {

    if (event->button() == Qt::LeftButton) {
        if (QApplication::keyboardModifiers().testFlag(Qt::ControlModifier) == true) {
            _editorWidget->setQuickRunToAddress(event);
        }else{
            _editorWidget->setQuickBreakpoint(event);
        }

    }else{
        QWidget::mouseDoubleClickEvent(event);
    }
}

void SeerEditorWidgetAssemblyLineNumberArea::mouseMoveEvent (QMouseEvent* event) {

    QWidget::mouseMoveEvent(event);
}

void SeerEditorWidgetAssemblyLineNumberArea::mousePressEvent (QMouseEvent* event) {

    if (event->button() == Qt::RightButton) {
        _editorWidget->showContextMenu(event);

    }else{
        QWidget::mousePressEvent(event);
    }
}

void SeerEditorWidgetAssemblyLineNumberArea::mouseReleaseEvent (QMouseEvent* event) {

    QWidget::mouseReleaseEvent(event);
}

//
// Offset Area.
//

SeerEditorWidgetAssemblyOffsetArea::SeerEditorWidgetAssemblyOffsetArea(SeerEditorWidgetAssemblyArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
}

QSize SeerEditorWidgetAssemblyOffsetArea::sizeHint () const {
    return QSize(_editorWidget->offsetAreaWidth(), 0);
}

void SeerEditorWidgetAssemblyOffsetArea::paintEvent (QPaintEvent* event) {
    _editorWidget->offsetAreaPaintEvent(event);
}

void SeerEditorWidgetAssemblyOffsetArea::mouseDoubleClickEvent (QMouseEvent* event) {

    if (event->button() == Qt::LeftButton) {
        if (QApplication::keyboardModifiers().testFlag(Qt::ControlModifier) == true) {
            _editorWidget->setQuickRunToAddress(event);
        }else{
            _editorWidget->setQuickBreakpoint(event);
        }

    }else{
        QWidget::mouseDoubleClickEvent(event);
    }
}

void SeerEditorWidgetAssemblyOffsetArea::mouseMoveEvent (QMouseEvent* event) {

    QWidget::mouseMoveEvent(event);
}

void SeerEditorWidgetAssemblyOffsetArea::mousePressEvent (QMouseEvent* event) {

    if (event->button() == Qt::RightButton) {
        _editorWidget->showContextMenu(event);

    }else{
        QWidget::mousePressEvent(event);
    }
}

void SeerEditorWidgetAssemblyOffsetArea::mouseReleaseEvent (QMouseEvent* event) {

    QWidget::mouseReleaseEvent(event);
}

//
// Opcode Area.
//

SeerEditorWidgetAssemblyOpcodeArea::SeerEditorWidgetAssemblyOpcodeArea(SeerEditorWidgetAssemblyArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
}

QSize SeerEditorWidgetAssemblyOpcodeArea::sizeHint () const {
    return QSize(_editorWidget->opcodeAreaWidth(), 0);
}

void SeerEditorWidgetAssemblyOpcodeArea::paintEvent (QPaintEvent* event) {
    _editorWidget->opcodeAreaPaintEvent(event);
}

void SeerEditorWidgetAssemblyOpcodeArea::mouseDoubleClickEvent (QMouseEvent* event) {

    if (event->button() == Qt::LeftButton) {
        if (QApplication::keyboardModifiers().testFlag(Qt::ControlModifier) == true) {
            _editorWidget->setQuickRunToAddress(event);
        }else{
            _editorWidget->setQuickBreakpoint(event);
        }

    }else{
        QWidget::mouseDoubleClickEvent(event);
    }
}

void SeerEditorWidgetAssemblyOpcodeArea::mouseMoveEvent (QMouseEvent* event) {

    QWidget::mouseMoveEvent(event);
}

void SeerEditorWidgetAssemblyOpcodeArea::mousePressEvent (QMouseEvent* event) {

    if (event->button() == Qt::RightButton) {
        _editorWidget->showContextMenu(event);

    }else{
        QWidget::mousePressEvent(event);
    }
}

void SeerEditorWidgetAssemblyOpcodeArea::mouseReleaseEvent (QMouseEvent* event) {

    QWidget::mouseReleaseEvent(event);
}

//
// Breakpoint Area.
//

SeerEditorWidgetAssemblyBreakPointArea::SeerEditorWidgetAssemblyBreakPointArea(SeerEditorWidgetAssemblyArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
}

QSize SeerEditorWidgetAssemblyBreakPointArea::sizeHint () const {
    return QSize(_editorWidget->breakPointAreaWidth(), 0);
}

void SeerEditorWidgetAssemblyBreakPointArea::paintEvent (QPaintEvent* event) {
    _editorWidget->breakPointAreaPaintEvent(event);
}

void SeerEditorWidgetAssemblyBreakPointArea::mouseDoubleClickEvent (QMouseEvent* event) {

    if (event->button() == Qt::LeftButton) {
        if (QApplication::keyboardModifiers().testFlag(Qt::ControlModifier) == true) {
            _editorWidget->setQuickRunToAddress(event);
        }else{
            _editorWidget->setQuickBreakpoint(event);
        }

    }else{
        QWidget::mouseDoubleClickEvent(event);
    }
}

void SeerEditorWidgetAssemblyBreakPointArea::mouseMoveEvent (QMouseEvent* event) {

    QWidget::mouseMoveEvent(event);
}

void SeerEditorWidgetAssemblyBreakPointArea::mousePressEvent (QMouseEvent* event) {

    if (event->button() == Qt::RightButton) {
        _editorWidget->showContextMenu(event);

    }else{
        QWidget::mousePressEvent(event);
    }
}

void SeerEditorWidgetAssemblyBreakPointArea::mouseReleaseEvent (QMouseEvent* event) {

    QWidget::mouseReleaseEvent(event);
}

SeerEditorWidgetAssemblyMiniMapArea::SeerEditorWidgetAssemblyMiniMapArea(SeerEditorWidgetAssemblyArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
    _dragging     = false;
}

QSize SeerEditorWidgetAssemblyMiniMapArea::sizeHint () const {
    return QSize(_editorWidget->miniMapAreaWidth(), 0);
}

void SeerEditorWidgetAssemblyMiniMapArea::paintEvent (QPaintEvent* event) {
    _editorWidget->miniMapAreaPaintEvent(event);
}

void SeerEditorWidgetAssemblyMiniMapArea::mousePressEvent (QMouseEvent* event) {

    if (event->button() == Qt::LeftButton) {
        _dragging = true;
        _editorWidget->jumpToMiniMapY(event->pos().y());
    }else{
        QWidget::mousePressEvent(event);
    }
}

void SeerEditorWidgetAssemblyMiniMapArea::mouseMoveEvent (QMouseEvent* event) {

    if (_dragging && (event->buttons() & Qt::LeftButton)) {
        _editorWidget->jumpToMiniMapY(event->pos().y());
    }else{
        QWidget::mouseMoveEvent(event);
    }
}

void SeerEditorWidgetAssemblyMiniMapArea::mouseReleaseEvent (QMouseEvent* event) {

    _dragging = false;

    QWidget::mouseReleaseEvent(event);
}

