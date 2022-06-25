#include "SeerEditorWidget.h"
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
#include <QtWidgets/QAction>
#include <QtWidgets/QToolTip>
#include <QtWidgets/QMessageBox>
#include <QtGui/QTextCursor>
#include <QtGui/QPalette>
#include <QtCore/QList>
#include <QtCore/QString>
#include <QtCore/QTextStream>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QRegExp>
#include <QtCore/QDebug>

SeerEditorWidgetAssemblyArea::SeerEditorWidgetAssemblyArea(QWidget* parent) : SeerPlainTextEdit(parent) {

    _enableLineNumberArea = false;
    _enableBreakPointArea = false;
    _enableMiniMapArea    = false;
    _addressLineMap.clear();
    _lineAddressMap.clear();

    QFont font("Source Code Pro");
    font.setStyleHint(QFont::Monospace);
    setFont(font);

    setReadOnly(true);
    setTextInteractionFlags(textInteractionFlags() | Qt::TextSelectableByKeyboard);
    setLineWrapMode(QPlainTextEdit::NoWrap);

    _lineNumberArea = new SeerEditorWidgetAssemblyLineNumberArea(this);
    _breakPointArea = new SeerEditorWidgetAssemblyBreakPointArea(this);
    _miniMapArea    = new SeerEditorWidgetAssemblyMiniMapArea(this);
    _miniMapPixmap  = 0;

    enableLineNumberArea(true); // XXX True
    enableBreakPointArea(true); // XXX True
    enableMiniMapArea(false);   // Doesn't work yet. Need to work on the "mini" part.

    QObject::connect(this, &SeerEditorWidgetAssemblyArea::blockCountChanged,        this, &SeerEditorWidgetAssemblyArea::updateMarginAreasWidth);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,            this, &SeerEditorWidgetAssemblyArea::updateLineNumberArea);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,            this, &SeerEditorWidgetAssemblyArea::updateBreakPointArea);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,            this, &SeerEditorWidgetAssemblyArea::updateMiniMapArea);

    setCurrentLine("");

    updateMarginAreasWidth(0);

    // Forward the scroll events in the various areas to the text edit.
    SeerPlainTextWheelEventForwarder* lineNumberAreaWheelForwarder = new SeerPlainTextWheelEventForwarder(this);
    SeerPlainTextWheelEventForwarder* breakPointAreaWheelForwarder = new SeerPlainTextWheelEventForwarder(this);
    SeerPlainTextWheelEventForwarder* miniMapAreaWheelForwarder    = new SeerPlainTextWheelEventForwarder(this);

    _lineNumberArea->installEventFilter(lineNumberAreaWheelForwarder);
    _breakPointArea->installEventFilter(breakPointAreaWheelForwarder);
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

void SeerEditorWidgetAssemblyArea::enableBreakPointArea (bool flag) {
    _enableBreakPointArea = flag;

    updateMarginAreasWidth(0);
}

bool SeerEditorWidgetAssemblyArea::breakPointAreaEnabled () const {
    return _enableBreakPointArea;
}

void SeerEditorWidgetAssemblyArea::enableMiniMapArea (bool flag) {
    _enableMiniMapArea = flag;

    updateMarginAreasWidth(0);
}

bool SeerEditorWidgetAssemblyArea::miniMapAreaEnabled () const {
    return _enableMiniMapArea;
}

void SeerEditorWidgetAssemblyArea::updateMarginAreasWidth (int newBlockCount) {

    Q_UNUSED(newBlockCount);

    int leftMarginWidth  = lineNumberAreaWidth() + breakPointAreaWidth();
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

int SeerEditorWidgetAssemblyArea::breakPointAreaWidth () {

    if (breakPointAreaEnabled() == false) {
        return 0;
    }

    int space = 3 + 20;

    return space;
}

int SeerEditorWidgetAssemblyArea::miniMapAreaWidth () {

    if (miniMapAreaEnabled() == false) {
        return 0;
    }

    int space = 3 + 75;

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

    if (miniMapAreaEnabled() == false) {
        return;
    }

    if (dy) {
        _miniMapArea->scroll(0, dy);
    }else{
        _miniMapArea->update(0, rect.y(), _miniMapArea->width(), rect.height());
    }

    if (rect.contains(viewport()->rect())) {
        updateMarginAreasWidth(0);
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
    font.setWeight(format.fontWeight());
    painter.setFont(font);

    QTextBlock block       = firstVisibleBlock();
    int        blockNumber = block.blockNumber();
    int        top         = qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
    int        bottom      = top + qRound(blockBoundingRect(block).height());

    while (block.isValid() && top <= event->rect().bottom()) {

        if (block.isVisible() && bottom >= event->rect().top()) {
            QString address;

            if (_lineAddressMap.contains(blockNumber+1)) {
                address = _lineAddressMap[blockNumber+1];
            }

            painter.drawText(0, top, _lineNumberArea->width(), fontMetrics().height(), Qt::AlignLeft, address);
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
    font.setWeight(format.fontWeight());
    painter.setFont(font);

    QTextBlock block       = firstVisibleBlock();
    int        blockNumber = block.blockNumber();
    int        top         = qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
    int        bottom      = top + qRound(blockBoundingRect(block).height());

    while (block.isValid() && top <= event->rect().bottom()) {

        if (block.isVisible() && bottom >= event->rect().top()) {

            // XXX

        }

        block  = block.next();
        top    = bottom;
        bottom = top + qRound(blockBoundingRect(block).height());

        blockNumber++;
    }
}

void SeerEditorWidgetAssemblyArea::miniMapAreaPaintEvent (QPaintEvent* event) {

    //
    // This doesn't work yet.
    // There is nothing 'mini' about the view. Need to shrink the text somehow.
    // Then add a 'focus' box that can be interacted with to scroll through the text.
    //

    if (miniMapAreaEnabled() == false) {
        return;
    }

    qDebug() << "Top:" << event->rect().top() << " Right:" << event->rect().right() << " Width:" << event->rect().width() << " Height:" << event->rect().height();

    if (_miniMapPixmap == 0) {

        int pixmapWidth  = 0;
        int pixmapHeight = 0;

        QFont font("Source Code Pro");
        font.setStyleHint(QFont::Monospace);
      //font.setPointSize(2);

        QFontMetrics fm(font);

        {
            QTextBlock block = document()->begin();

            while (block.isValid()) {

                if (fm.horizontalAdvance(block.text()) > pixmapWidth) {
                    pixmapWidth = fm.horizontalAdvance(block.text());
                }

                pixmapHeight += fm.height();

                block = block.next();
            }
        }

        qDebug() << "PIXMAP = " << pixmapWidth << " x " << pixmapHeight;

        QTextCharFormat format = highlighterSettings().get("Margin");

        _miniMapPixmap = new QPixmap(pixmapWidth, pixmapHeight);
        _miniMapPixmap->fill(format.background().color());

        QPainter painter(_miniMapPixmap);
        painter.setPen(format.foreground().color());
        painter.setFont(font);

        QTextBlock block       = document()->begin();
        int        top         = qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
        int        bottom      = top + qRound(blockBoundingRect(block).height());

        while (block.isValid()) {

            painter.drawText(0, top, block.text());

            block = block.next();
            top    = bottom;
            bottom = top + qRound(blockBoundingRect(block).height());
        }
    }


    /*
    QRectF target(10.0, 20.0, 80.0, 60.0);
    QRectF source(0.0, 0.0, 70.0, 40.0);

    QRect target(event->rect());
    QRect source(event->rect());

    QPainter painter(_miniMapPixmap);
    painter.drawPixmap(0, 0, *_miniMapPixmap);
    //painter.drawPixmap(target, *_miniMapPixmap, source);

    QPainter painter(_miniMapArea);
    painter.drawPixmap(0, 0, *_miniMapPixmap);
    */

    QFont font("Source Code Pro");
    font.setStyleHint(QFont::Monospace);
  //font.setPointSize(2);

    QTextCharFormat format = highlighterSettings().get("Margin");

    QPainter painter(_miniMapArea);
    painter.fillRect(event->rect(), format.background().color());
    painter.setPen(format.foreground().color());
    painter.setFont(font);

    QTextBlock block       = firstVisibleBlock();
    int        top         = qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
    int        bottom      = top + qRound(blockBoundingRect(block).height());

    while (block.isValid() && top <= event->rect().bottom()) {

        if (block.isVisible() && bottom >= event->rect().top()) {
            painter.drawText(0, top, _miniMapArea->width(), painter.fontMetrics().height(), Qt::AlignLeft, block.text());
        }

        block  = block.next();
        top    = bottom;
        bottom = top + qRound(blockBoundingRect(block).height());
      //bottom = top + painter.fontMetrics().height();
    }
}

void SeerEditorWidgetAssemblyArea::resizeEvent (QResizeEvent* e) {

    QPlainTextEdit::resizeEvent(e);

    QRect cr = contentsRect();

    if (lineNumberAreaEnabled()) {
        _lineNumberArea->setGeometry (QRect(cr.left(), cr.top(), lineNumberAreaWidth(), cr.height()));
    }

    if (breakPointAreaEnabled()) {
        _breakPointArea->setGeometry (QRect(cr.left() + lineNumberAreaWidth(), cr.top(), breakPointAreaWidth(), cr.height()));
    }

    if (miniMapAreaEnabled()) {
        _miniMapArea->setGeometry (QRect(cr.right() - miniMapAreaWidth() - verticalScrollBar()->width(), cr.top(), miniMapAreaWidth(), cr.height()));
    }
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

    // Append the 'current lines' extra selections.
    extraSelections.append(_currentLinesExtraSelections);

    // Append the 'searched text' extra selections.
    extraSelections.append(_findExtraSelections);

    // Give the editor the list of selections.
    // This will remove the old selections and select the new ones.
    setExtraSelections(extraSelections);
}

void SeerEditorWidgetAssemblyArea::setCurrentLine (const QString& address) {

    // Clear current line selections.
    _currentLinesExtraSelections.clear();

    //
    // Initial lineno.
    //
    // Try parsing as an integer.
    bool ok;
    int lineno = address.toInt(&ok, 10); // Try it as an 'int'. lineno == 0 on error.

    // Try parsing as an address.
    if (ok == false) {

        qulonglong addr = address.toULongLong(&ok, 0); // Try it as an '0x.....'.

        if (ok) {
            if (_addressLineMap.contains(addr)) {
                lineno = _addressLineMap[addr];
            }
        }
    }

    //qDebug() << address << lineno;

    // Highlight if a valid line number is selected.
    if (lineno >= 1) {

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
    }

    // Scroll to the line.
    scrollToLine(address);

    // Refresh all the extra selections.
    refreshExtraSelections();
}

void SeerEditorWidgetAssemblyArea::scrollToLine (const QString& address) {

    //
    // Initial lineno.
    //
    // Try parsing as an integer.
    bool ok;
    int lineno = address.toInt(&ok, 10); // Try it as an 'int'. lineno == 0 on error.

    // Try parsing as an address.
    if (ok == false) {

        qulonglong addr = address.toULongLong(&ok, 0); // Try it as an '0x.....'.

        if (ok) {
            if (_addressLineMap.contains(addr)) {
                lineno = _addressLineMap[addr];
            }
        }
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

void SeerEditorWidgetAssemblyArea::handleText (const QString& text) {

    //qDebug() << text;

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

        // Emit the signal to load the assembly for address 'addr'
        if (_addressLineMap.contains(addr_text.toULongLong(0,0)) == false) {
            _currentAddress = addr_text;
            emit requestAssembly(addr_text);
        }

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

        // Now parse the table and re-add the breakpoints.
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

            }
        }

    }else if (text.startsWith("^done,asm_insns=")) {

        // Clear the existing document.
        document()->clear();

        // Clear mappings.
        _addressLineMap.clear();
        _lineAddressMap.clear();

        // Get the list of assembly lines.
        QString asm_insns_text = Seer::parseFirst(text, "asm_insns=", '[', ']', false);

        QStringList asm_list = Seer::parse(asm_insns_text, "", '{', '}', false);

        // Loop through the asm list and find the width of each field.
        int address_width = 0;
        int offset_width  = 0;
        int opcodes_width = 0;
        int inst_width    = 0;

        for ( const auto& asm_text : asm_list  ) {

            //qDebug() << asm_text;

            // Get the strings, with padding.
            QString address_text =        Seer::parseFirst(asm_text, "address=", '"', '"', false);
            QString offset_text  = "<+" + Seer::parseFirst(asm_text, "offset=",  '"', '"', false) + ">";
            QString opcodes_text =        Seer::parseFirst(asm_text, "opcodes=", '"', '"', false);
            QString inst_text    =        Seer::parseFirst(asm_text, "inst=",    '"', '"', false);

            address_width = qMax(address_width, address_text.length());
            offset_width  = qMax(offset_width,  offset_text.length());
            opcodes_width = qMax(opcodes_width, opcodes_text.length());
            inst_width    = qMax(inst_width,    inst_text.length());
        }

        // Loop through the asm list and print each line.

        int lineno = 1;

        for ( const auto& asm_text : asm_list  ) {

            // Get the strings, with padding.
            QString address_text =        Seer::parseFirst(asm_text, "address=", '"', '"', false);
            QString offset_text  = "<+" + Seer::parseFirst(asm_text, "offset=",  '"', '"', false) + ">";
            QString opcodes_text =        Seer::parseFirst(asm_text, "opcodes=", '"', '"', false);
            QString inst_text    =        Seer::parseFirst(asm_text, "inst=",    '"', '"', false);

            //qDebug() << inst_text;

            // Write assembly line to the document.
            appendPlainText(QString(" ") +
                            offset_text.leftJustified(offset_width,   ' ') + " | " +
                            opcodes_text.leftJustified(opcodes_width, ' ') + " | " +
                            inst_text.leftJustified(inst_width,       ' '));

            // Add to maps
            _addressLineMap.insert(address_text.toULongLong(0,0), lineno);
            _lineAddressMap.insert(lineno, address_text);

            lineno++;
        }

        // Move to the start of the document.  XXX Move to the line that has our address.
        // setTextCursor(cursor);
        moveCursor(QTextCursor::Start);

        // Set to the line number.
        setCurrentLine(_currentAddress);

        _currentAddress = "";
    }
}

