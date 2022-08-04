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

//
// Assembly Area
//

SeerEditorWidgetAssemblyArea::SeerEditorWidgetAssemblyArea(QWidget* parent) : SeerPlainTextEdit(parent) {

    _enableLineNumberArea = false;
    _enableBreakPointArea = false;
    _enableMiniMapArea    = false;
    _addressLineMap.clear();
    _offsetLineMap.clear();
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

    enableLineNumberArea(true);
    enableBreakPointArea(true);
    enableMiniMapArea(false);   // Doesn't work yet. Need to work on the "mini" part.

    QObject::connect(this, &SeerEditorWidgetAssemblyArea::blockCountChanged,                this, &SeerEditorWidgetAssemblyArea::updateMarginAreasWidth);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,                    this, &SeerEditorWidgetAssemblyArea::updateLineNumberArea);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,                    this, &SeerEditorWidgetAssemblyArea::updateBreakPointArea);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,                    this, &SeerEditorWidgetAssemblyArea::updateMiniMapArea);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::highlighterSettingsChanged,       this, &SeerEditorWidgetAssemblyArea::handleHighlighterSettingsChanged);

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

        emit requestAssembly(address);
    }
}

const QString& SeerEditorWidgetAssemblyArea::address () const {

    return _currentAddress;
}

bool SeerEditorWidgetAssemblyArea::setCurrentLine (const QString& address) {

    //qDebug() << address;

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

    //qDebug() << address << lineno;

    // Stop if no valid lineno.
    if (lineno < 1) {
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

void SeerEditorWidgetAssemblyArea::scrollToLine (const QString& address) {

    //qDebug() << address;

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

    //qDebug() << address << lineno << ok;

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

void SeerEditorWidgetAssemblyArea::showContextMenu (QMouseEvent* event) {

    showContextMenu(event->pos(), event->globalPos());
}

void SeerEditorWidgetAssemblyArea::showContextMenu (QContextMenuEvent* event) {

    showContextMenu(event->pos(), event->globalPos());
}

void SeerEditorWidgetAssemblyArea::showContextMenu (const QPoint& pos, const QPoint& globalPos) {

    // Get the line number for the cursor position.
    QTextCursor cursor = cursorForPosition(pos);

    int lineno = cursor.blockNumber()+1;

    QString address = _lineAddressMap[lineno];

    // Create the menu actions.
    QAction* createBreakpointAction;
    QAction* deleteAction;
    QAction* enableAction;
    QAction* disableAction;
    QAction* runToAddressAction;
    QAction* addMemoryAddressVisualizerAction;
    QAction* addArrayAddressVisualizerAction;

    // Enable/disable them depending if the breakpoint already exists.
    if (hasBreakpointAddress(address) == true) {

        int breakno = breakpointAddressToNumber(address);

        createBreakpointAction    = new QAction(QIcon(":/seer/resources/RelaxLightIcons/document-new.svg"), QString("Create breakpoint on address %1").arg(address), this);
        deleteAction              = new QAction(QIcon(":/seer/resources/RelaxLightIcons/edit-delete.svg"),  QString("Delete breakpoint %1 on address %2").arg(breakno).arg(address), this);
        enableAction              = new QAction(QIcon(":/seer/resources/RelaxLightIcons/list-add.svg"),     QString("Enable breakpoint %1 on address %2").arg(breakno).arg(address), this);
        disableAction             = new QAction(QIcon(":/seer/resources/RelaxLightIcons/list-remove.svg"),  QString("Disable breakpoint %1 on address %2").arg(breakno).arg(address), this);
        runToAddressAction        = new QAction(QString("Run to address %1").arg(address), this);

        createBreakpointAction->setEnabled(false);
        deleteAction->setEnabled(true);
        enableAction->setEnabled(true);
        disableAction->setEnabled(true);
        runToAddressAction->setEnabled(true);

    }else{
        createBreakpointAction    = new QAction(QIcon(":/seer/resources/RelaxLightIcons/document-new.svg"), QString("Create breakpoint on address %1").arg(address), this);
        deleteAction              = new QAction(QIcon(":/seer/resources/RelaxLightIcons/edit-delete.svg"),  QString("Delete breakpoint on address %1").arg(address), this);
        enableAction              = new QAction(QIcon(":/seer/resources/RelaxLightIcons/list-add.svg"),     QString("Enable breakpoint on address %1").arg(address), this);
        disableAction             = new QAction(QIcon(":/seer/resources/RelaxLightIcons/list-remove.svg"),  QString("Disable breakpoint on address %1").arg(address), this);
        runToAddressAction        = new QAction(QString("Run to address %1").arg(address), this);

        createBreakpointAction->setEnabled(true);
        deleteAction->setEnabled(false);
        enableAction->setEnabled(false);
        disableAction->setEnabled(false);
        runToAddressAction->setEnabled(true);
    }

    addMemoryAddressVisualizerAction = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));
    addArrayAddressVisualizerAction  = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));

    QMenu menu("Breakpoints", this);
    menu.setTitle("Breakpoints");
    menu.addAction(createBreakpointAction);
    menu.addAction(deleteAction);
    menu.addAction(enableAction);
    menu.addAction(disableAction);
    menu.addAction(runToAddressAction);

    QMenu memoryVisualizerMenu("Add address to a Memory Visualizer");
    memoryVisualizerMenu.addAction(addMemoryAddressVisualizerAction);
    menu.addMenu(&memoryVisualizerMenu);

    QMenu arrayVisualizerMenu("Add address to an Array Visualizer");
    arrayVisualizerMenu.addAction(addArrayAddressVisualizerAction);
    menu.addMenu(&arrayVisualizerMenu);

    // Enable/disable items based on something being selected or not.
    if (textCursor().selectedText() == "") {
        addMemoryAddressVisualizerAction->setEnabled(false);
        addArrayAddressVisualizerAction->setEnabled(false);
    }else{
        addMemoryAddressVisualizerAction->setEnabled(true);
        addArrayAddressVisualizerAction->setEnabled(true);
    }

    // Launch the menu. Get the response.
    QAction* action = menu.exec(globalPos);

    // Do nothing.
    if (action == 0) {
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

        //qDebug() << dlg.breakpointText();

        // Emit the create breakpoint signal.
        emit insertBreakpoint(dlg.breakpointText());

        return;
    }

    // Handle deleting a breakpoint.
    if (action == deleteAction) {

        //qDebug() << "deleteBreakpoints" << lineno;

        // Emit the delete breakpoint signal.
        emit deleteBreakpoints(QString("%1").arg(breakpointAddressToNumber(address)));

        return;
    }

    // Handle enabling a breakpoint.
    if (action == enableAction) {

        //qDebug() << "enableBreakpoints" << lineno;

        // Emit the enable breakpoint signal.
        emit enableBreakpoints(QString("%1").arg(breakpointAddressToNumber(address)));

        return;
    }

    // Handle disabling a breakpoint.
    if (action == disableAction) {

        //qDebug() << "disableBreakpoints" << lineno;

        // Emit the disable breakpoint signal.
        emit disableBreakpoints(QString("%1").arg(breakpointAddressToNumber(address)));

        return;
    }

    // Handle running to a line number.
    if (action == runToAddressAction) {

        //qDebug() << "runToAddress" << lineno;

        // Emit the runToLine signal.
        emit runToAddress(address);

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAddressVisualizerAction) {

        //qDebug() << "addMemoryVisualizer" << lineno;

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addMemoryVisualize(textCursor().selectedText());
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayAddressVisualizerAction) {

        //qDebug() << "addArrayVisualizer" << lineno;

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addArrayVisualize(textCursor().selectedText());
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
            emit disableBreakpoints(QString("%1").arg(breakpointAddressToNumber(address)));
        }

    // Otherwise, do a quick create of a new breakpoint.
    }else{
        emit insertBreakpoint(QString("-f *%1").arg(address));
    }
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

        //qDebug() << "asm_insns";

        // Clear the existing document.
        document()->clear();

        // Clear mappings.
        _addressLineMap.clear();
        _offsetLineMap.clear();
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
            QString offset_num   =        Seer::parseFirst(asm_text, "offset=",  '"', '"', false);
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
            QString offset_num   =        Seer::parseFirst(asm_text, "offset=",  '"', '"', false);
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
            _offsetLineMap.insert(offset_num.toULongLong(0,0), lineno);
            _lineAddressMap.insert(lineno, address_text);

            lineno++;
        }

        // Move to the start of the document as a default.
        moveCursor(QTextCursor::Start);

        // Move to the line that has our address.
        setCurrentLine(_currentAddress);

        // _currentAddress = "";  // Do we need to reset this?
    }
}

void SeerEditorWidgetAssemblyArea::handleHighlighterSettingsChanged () {

    // Set base color for background and text color.
    // Use the palette to do this. Some people say to use the stylesheet.
    // But the palettle method works (for now).
    QTextCharFormat format = highlighterSettings().get("Text");

    QPalette p = palette();
    p.setColor(QPalette::Base, format.background().color());
    p.setColor(QPalette::Text, format.foreground().color());
    setPalette(p);

    // Note. The margins are automatically updated by their own paint events.
    //       The new highlighter settings will be used.
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
        _editorWidget->setQuickBreakpoint(event);

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
        _editorWidget->setQuickBreakpoint(event);

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

//
// MiniMap Area
//

SeerEditorWidgetAssemblyMiniMapArea::SeerEditorWidgetAssemblyMiniMapArea(SeerEditorWidgetAssemblyArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
}

QSize SeerEditorWidgetAssemblyMiniMapArea::sizeHint () const {
    return QSize(_editorWidget->miniMapAreaWidth(), 0);
}

void SeerEditorWidgetAssemblyMiniMapArea::paintEvent (QPaintEvent* event) {
    _editorWidget->miniMapAreaPaintEvent(event);
}

void SeerEditorWidgetAssemblyMiniMapArea::mouseDoubleClickEvent (QMouseEvent* event) {

    QTextCursor  cursor = _editorWidget->cursorForPosition(event->pos());

    qDebug() << cursor.blockNumber()+1;

    QWidget::mouseDoubleClickEvent(event);
}

void SeerEditorWidgetAssemblyMiniMapArea::mouseMoveEvent (QMouseEvent* event) {

    QTextCursor  cursor = _editorWidget->cursorForPosition(event->pos());

    qDebug() << cursor.blockNumber()+1;

    QWidget::mouseMoveEvent(event);
}

void SeerEditorWidgetAssemblyMiniMapArea::mousePressEvent (QMouseEvent* event) {

    QTextCursor  cursor = _editorWidget->cursorForPosition(event->pos());

    qDebug() << cursor.blockNumber()+1;

    QWidget::mousePressEvent(event);
}

void SeerEditorWidgetAssemblyMiniMapArea::mouseReleaseEvent (QMouseEvent* event) {

    QTextCursor  cursor = _editorWidget->cursorForPosition(event->pos());

    qDebug() << cursor.blockNumber()+1;

    QWidget::mouseReleaseEvent(event);
}

