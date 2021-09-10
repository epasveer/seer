#include "SeerEditorWidget.h"
#include "SeerBreakpointCreateDialog.h"
#include "SeerUtl.h"
#include <QtGui/QColor>
#include <QtGui/QPainter>
#include <QtGui/QTextBlock>
#include <QtGui/QFont>
#include <QtGui/QIcon>
#include <QtGui/QRadialGradient>
#include <QtGui/QHelpEvent>
#include <QtGui/QPainterPath>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QMenu>
#include <QtWidgets/QAction>
#include <QtWidgets/QToolTip>
#include <QtGui/QTextCursor>
#include <QtGui/QPalette>
#include <QtCore/QList>
#include <QtCore/QString>
#include <QtCore/QTextStream>
#include <QtCore/QFile>
#include <QtCore/QDebug>

SeerEditorWidgetSourceArea::SeerEditorWidgetSourceArea(QWidget *parent) : QPlainTextEdit(parent) {

    _enableLineNumberArea = false;
    _enableBreakPointArea = false;
    _enableMiniMapArea    = false;
    _ctrlKeyPressed       = false;

  //QFont font("Monospace");
  //font.setStyleHint(QFont::TypeWriter);
    QFont font("Source Code Pro");
    font.setStyleHint(QFont::Monospace);
    setFont(font);

    setReadOnly(true);
    setTextInteractionFlags(textInteractionFlags() | Qt::TextSelectableByKeyboard);
    setLineWrapMode(QPlainTextEdit::NoWrap);

    _lineNumberArea = new SeerEditorWidgetLineNumberArea(this);
    _breakPointArea = new SeerEditorWidgetBreakPointArea(this);
    _miniMapArea    = new SeerEditorWidgetMiniMapArea(this);
    _miniMapPixmap  = 0;

    enableLineNumberArea(true);
    enableBreakPointArea(true);
    enableMiniMapArea(false);  // Doesn't work yet. Need to work on the "mini" part.

  //QObject::connect(this, &SeerEditorWidgetSourceArea::cursorPositionChanged, this, &SeerEditorWidgetSourceArea::highlightCurrentLine);
    QObject::connect(this, &SeerEditorWidgetSourceArea::blockCountChanged,     this, &SeerEditorWidgetSourceArea::updateMarginAreasWidth);
    QObject::connect(this, &SeerEditorWidgetSourceArea::updateRequest,         this, &SeerEditorWidgetSourceArea::updateLineNumberArea);
    QObject::connect(this, &SeerEditorWidgetSourceArea::updateRequest,         this, &SeerEditorWidgetSourceArea::updateBreakPointArea);
    QObject::connect(this, &SeerEditorWidgetSourceArea::updateRequest,         this, &SeerEditorWidgetSourceArea::updateMiniMapArea);

    setCurrentLine(0);

    updateMarginAreasWidth(0);

    // Calling close() will clear the text document.
    close();
}

void SeerEditorWidgetSourceArea::enableLineNumberArea (bool flag) {
    _enableLineNumberArea = flag;

    updateMarginAreasWidth(0);
}

bool SeerEditorWidgetSourceArea::lineNumberAreaEnabled () const {
    return _enableLineNumberArea;
}

void SeerEditorWidgetSourceArea::enableBreakPointArea (bool flag) {
    _enableBreakPointArea = flag;

    updateMarginAreasWidth(0);
}

bool SeerEditorWidgetSourceArea::breakPointAreaEnabled () const {
    return _enableBreakPointArea;
}

void SeerEditorWidgetSourceArea::enableMiniMapArea (bool flag) {
    _enableMiniMapArea = flag;

    updateMarginAreasWidth(0);
}

bool SeerEditorWidgetSourceArea::miniMapAreaEnabled () const {
    return _enableMiniMapArea;
}

void SeerEditorWidgetSourceArea::updateMarginAreasWidth (int newBlockCount) {

    Q_UNUSED(newBlockCount);

    int leftMarginWidth  = lineNumberAreaWidth() + breakPointAreaWidth();
    int rightMarginWidth = miniMapAreaWidth();

    setViewportMargins(leftMarginWidth, 0, rightMarginWidth, 0);
}

int SeerEditorWidgetSourceArea::lineNumberAreaWidth () {

    if (lineNumberAreaEnabled() == false) {
        return 0;
    }

    int digits = 1;
    int max    = qMax(1, blockCount());

    while (max >= 10) {
        max /= 10;
        digits++;
    }

    int space = 3 + fontMetrics().horizontalAdvance(QLatin1Char('9')) * digits;

    return space;
}

int SeerEditorWidgetSourceArea::breakPointAreaWidth () {

    if (breakPointAreaEnabled() == false) {
        return 0;
    }

    int space = 3 + 20;

    return space;
}

int SeerEditorWidgetSourceArea::miniMapAreaWidth () {

    if (miniMapAreaEnabled() == false) {
        return 0;
    }

    int space = 3 + 75;

    return space;
}

void SeerEditorWidgetSourceArea::updateLineNumberArea (const QRect& rect, int dy) {

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

void SeerEditorWidgetSourceArea::updateBreakPointArea (const QRect& rect, int dy) {

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

void SeerEditorWidgetSourceArea::updateMiniMapArea (const QRect& rect, int dy) {

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

void SeerEditorWidgetSourceArea::lineNumberAreaPaintEvent (QPaintEvent* event) {

    if (lineNumberAreaEnabled() == false) {
        return;
    }

    QPainter painter(_lineNumberArea);
    painter.fillRect(event->rect(), Qt::lightGray);
    painter.setPen(Qt::black);

    QTextBlock block       = firstVisibleBlock();
    int        blockNumber = block.blockNumber();
    int        top         = qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
    int        bottom      = top + qRound(blockBoundingRect(block).height());

    while (block.isValid() && top <= event->rect().bottom()) {

        if (block.isVisible() && bottom >= event->rect().top()) {
            QString number = QString::number(blockNumber + 1);
            painter.drawText(0, top, _lineNumberArea->width(), fontMetrics().height(), Qt::AlignRight, number);
        }

        block  = block.next();
        top    = bottom;
        bottom = top + qRound(blockBoundingRect(block).height());

        blockNumber++;
    }
}

void SeerEditorWidgetSourceArea::breakPointAreaPaintEvent (QPaintEvent* event) {

    if (breakPointAreaEnabled() == false) {
        return;
    }

    QPainter painter(_breakPointArea);
    painter.fillRect(event->rect(), Qt::lightGray);

    QTextBlock block       = firstVisibleBlock();
    int        blockNumber = block.blockNumber();
    int        top         = qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
    int        bottom      = top + qRound(blockBoundingRect(block).height());

    while (block.isValid() && top <= event->rect().bottom()) {

        if (block.isVisible() && bottom >= event->rect().top()) {

            if (hasBreakpointLine(blockNumber+1)) {
                if (breakpointLineEnabled(blockNumber+1)) {
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

void SeerEditorWidgetSourceArea::miniMapAreaPaintEvent (QPaintEvent* event) {

    //
    // This doesn't work yet.
    // There is nothing 'mini' about the view. Need to shrink the text somehow.
    // Then add a 'focus' box that can be interacted with to scroll through the text.
    //

    if (miniMapAreaEnabled() == false) {
        return;
    }

    qDebug() << __PRETTY_FUNCTION__ << ":" << "Top:" << event->rect().top() << " Right:" << event->rect().right() << " Width:" << event->rect().width() << " Height:" << event->rect().height();

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

        qDebug() << __PRETTY_FUNCTION__ << ":" << "PIXMAP = " << pixmapWidth << " x " << pixmapHeight;

        _miniMapPixmap = new QPixmap(pixmapWidth, pixmapHeight);
        _miniMapPixmap->fill(Qt::lightGray);

        QPainter painter(_miniMapPixmap);
        painter.setPen(Qt::black);
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

    QPainter painter(_miniMapArea);
    painter.fillRect(event->rect(), Qt::lightGray);
    painter.setPen(Qt::black);
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

void SeerEditorWidgetSourceArea::resizeEvent (QResizeEvent* e) {

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

void SeerEditorWidgetSourceArea::contextMenuEvent (QContextMenuEvent* event) {

    showContextMenu(event);
}

void SeerEditorWidgetSourceArea::keyPressEvent (QKeyEvent* event) {

    if (event->key() == Qt::Key_F && event->modifiers() == Qt::ControlModifier) {
        emit showSearchBar(true);
    }

    QPlainTextEdit::keyPressEvent(event);
}

void SeerEditorWidgetSourceArea::mouseReleaseEvent (QMouseEvent* event) {

    QPlainTextEdit::mouseReleaseEvent(event);

    if (textCursor().selectedText() == "") {
        // Nothing selected, clear current expression.
        clearExpression();
        return;
    }

    _selectedExpressionCursor = textCursor();
    _selectedExpressionValue  = "";
    _selectedExpressionId     = Seer::createID();

    if (_ctrlKeyPressed == true) {
        _selectedExpressionName = QString("*") + textCursor().selectedText();
    }else{
        _selectedExpressionName = textCursor().selectedText();
    }

    emit evaluateVariableExpression(_selectedExpressionId, _selectedExpressionName);
}

bool SeerEditorWidgetSourceArea::event(QEvent* event) {

    // Handle the ToolTip event.
    if (event->type() == QEvent::ToolTip) {

        QHelpEvent* helpEvent = static_cast<QHelpEvent*>(event);

        // Massage the event location to account for the linenumber and breakpoint widgets.
        QPoint pos = QPoint(helpEvent->pos().x() - _lineNumberArea->width() - _breakPointArea->width(), helpEvent->pos().y());

        // Create a cursor at the position so we can get the text underneath at the cursor.
        QTextCursor cursor = cursorForPosition(pos);
        cursor.select(QTextCursor::WordUnderCursor);

        // If the text isn't empty, display a took tip.
        if (cursor.selectedText().isEmpty() == false && cursor.selectedText() == _selectedExpressionCursor.selectedText()) {
          //QToolTip::showText(helpEvent->globalPos(), QString("%1 = %2").arg(_selectedExpressionName).arg(_selectedExpressionValue) );
            QToolTip::showText(helpEvent->globalPos(), _selectedExpressionValue);

        // Otherwise, hide any old one.
        }else{
            QToolTip::hideText();
        }

        return true;
    }

    // Handle the Key Press/Release event.
    // To handle when Ctrl is pressed or released.
    if (event->type() == QEvent::KeyPress) {
        QKeyEvent* keyEvent = (QKeyEvent*)event;

        if (keyEvent->key() == Qt::Key_Control) {
            _ctrlKeyPressed = true;
        }
    }

    if (event->type() == QEvent::KeyRelease) {
        QKeyEvent* keyEvent = (QKeyEvent*)event;

        if (keyEvent->key() == Qt::Key_Control) {
            _ctrlKeyPressed = false;
        }
    }


    // Pass any others to the base class.
    return QPlainTextEdit::event(event);
}

void SeerEditorWidgetSourceArea::highlightCurrentLines () {

    // Any line will be highlighted with a yellow line.
    QColor yellowLineColor = QColor(Qt::yellow).lighter(160);
    QColor grayLineColor   = QColor(Qt::lightGray).lighter(120);

    // Create an empty list of selections.
    QList<QTextEdit::ExtraSelection> extraSelections;

    // Loop through each highlighted line. Get its text cursor.
    for (int i=0; i<_currentLineCursors.size(); i++) {

        // Create a selection at the cursor.
        QTextEdit::ExtraSelection selection;
        selection.format.setBackground((i == 0 ? yellowLineColor : grayLineColor));
        selection.format.setProperty(QTextFormat::FullWidthSelection, true);
        selection.cursor = _currentLineCursors[i];
        selection.cursor.clearSelection();

        // Add it to the list.
        extraSelections.append(selection);
    }

    // Give the editor the list of selections.
    setExtraSelections(extraSelections);
}

void SeerEditorWidgetSourceArea::highlightCurrentLine () {

    QList<QTextEdit::ExtraSelection> extraSelections;

    QColor lineColor = QColor(Qt::yellow).lighter(160);

    QTextEdit::ExtraSelection selection;
    selection.format.setBackground(lineColor);
    selection.format.setProperty(QTextFormat::FullWidthSelection, true);
    selection.cursor = textCursor();
    selection.cursor.clearSelection();

    extraSelections.append(selection);

    setExtraSelections(extraSelections);
}

void SeerEditorWidgetSourceArea::unhighlightCurrentLine () {

    QList<QTextEdit::ExtraSelection> extraSelections;

    // No need to unhighlight the lines. Creating an empty extraSelections list will do that.
    setExtraSelections(extraSelections);
}

bool SeerEditorWidgetSourceArea::isOpen () const {

    if (_fullname != "") {
        return true;
    }

    return false;
}

void SeerEditorWidgetSourceArea::open (const QString& fullname, const QString& file) {

    // Close the previous file, if any.
    if (isOpen()) {
        close();
    }

    // Save the filename.
    _fullname = fullname;
    _file     = file;

    setDocumentTitle(_fullname);

    // If the filename is null, don't do anything.
    if (_fullname == "") {
        return;
    }

    // Open the file.
    QFile inputFile(_fullname);

    inputFile.open(QIODevice::ReadOnly);

    if (!inputFile.isOpen()) {
        return;
    }

    // Read the file.
    QTextStream stream(&inputFile);

    QString line = stream.readLine();
    QString document;

    while (!line.isNull()) {

        if (document != "") {
            document += "\n";
        }

        document += line;

        line = stream.readLine();
    };

    // Put the contents in the editor.
    setPlainText(document);

    // Set the text cursor to the first line.
    QTextCursor cursor = textCursor();
    cursor.movePosition(QTextCursor::Start, QTextCursor::MoveAnchor, 1);
    setTextCursor(cursor);
}

const QString& SeerEditorWidgetSourceArea::fullname () const {
    return _fullname;
}

const QString& SeerEditorWidgetSourceArea::file () const {
    return _file;
}

void SeerEditorWidgetSourceArea::close () {
    setDocumentTitle("");
    setPlainText("");

    clearExpression();
}

void SeerEditorWidgetSourceArea::setCurrentLine (int lineno) {

    //qDebug() << __PRETTY_FUNCTION__ << ":" << lineno << file();

    // Highlight if a valid line number is selected.
    if (lineno < 1) {
        unhighlightCurrentLine();
        return;
    }

    QTextBlock  block  = document()->findBlockByLineNumber(lineno-1);
    QTextCursor cursor = textCursor();

    cursor.setPosition(block.position());
    setTextCursor(cursor);

    highlightCurrentLine();
}

void SeerEditorWidgetSourceArea::scrollToLine (int lineno) {

    //qDebug() << __PRETTY_FUNCTION__ << ":" << lineno << file();

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

void SeerEditorWidgetSourceArea::clearCurrentLines () {

    //qDebug() << __PRETTY_FUNCTION__ << ":" << file();

    _currentLineCursors.resize(0);

    highlightCurrentLines();
}

void SeerEditorWidgetSourceArea::addCurrentLine (int lineno) {

    //qDebug() << __PRETTY_FUNCTION__ << ":" << lineno << file();

    QTextBlock  block  = document()->findBlockByLineNumber(lineno-1);
    QTextCursor cursor = textCursor();

    cursor.setPosition(block.position());

    _currentLineCursors.push_back(cursor);

    highlightCurrentLines();
}


void SeerEditorWidgetSourceArea::clearBreakpoints () {
    _breakpointsLineNumbers.clear();
    _breakpointsNumbers.clear();
    _breakpointsEnableds.clear();

    repaint();
}

void SeerEditorWidgetSourceArea::addBreakpoint (int number, int lineno, bool enabled) {
    _breakpointsNumbers.push_back(number);
    _breakpointsLineNumbers.push_back(lineno);
    _breakpointsEnableds.push_back(enabled);

    repaint();
}

bool SeerEditorWidgetSourceArea::hasBreakpointNumber (int number) const {
    return _breakpointsLineNumbers.contains(number);
}

bool SeerEditorWidgetSourceArea::hasBreakpointLine (int lineno) const {
    return _breakpointsLineNumbers.contains(lineno);
}

const QVector<int>& SeerEditorWidgetSourceArea::breakpointNumbers () const {
    return _breakpointsNumbers;
}

const QVector<int>& SeerEditorWidgetSourceArea::breakpointLines () const {
    return _breakpointsLineNumbers;
}

const QVector<bool>& SeerEditorWidgetSourceArea::breakpointEnableds () const {
    return _breakpointsEnableds;
}

int SeerEditorWidgetSourceArea::breakpointLineToNumber (int lineno) const {

    // Map lineno to breakpoint number.
    int i = _breakpointsLineNumbers.indexOf(lineno);

    if (i < 0) {
        return 0;
    }

    return _breakpointsNumbers[i];
}

bool SeerEditorWidgetSourceArea::breakpointLineEnabled (int lineno) const {

    // Look for the lineno and get its index.
    int i = _breakpointsLineNumbers.indexOf(lineno);

    // Not found, return false.
    if (i < 0) {
        return false;
    }

    // Otherwise, return the proper status.
    return _breakpointsEnableds[i];
}

void SeerEditorWidgetSourceArea::showContextMenu (QMouseEvent* event) {

    showContextMenu(event->pos(), event->globalPos());
}

void SeerEditorWidgetSourceArea::showContextMenu (QContextMenuEvent* event) {

    showContextMenu(event->pos(), event->globalPos());
}

void SeerEditorWidgetSourceArea::showContextMenu (const QPoint& pos, const QPoint& globalPos) {

    // Get the line number for the cursor position.
    QTextCursor cursor = cursorForPosition(pos);

    int lineno = cursor.blockNumber()+1;

    // Create the menu actions.
    QAction* createAction;
    QAction* deleteAction;
    QAction* enableAction;
    QAction* disableAction;
    QAction* runToLineAction;
    QAction* addVariableExpressionAction;
    QAction* addVariableAsteriskExpressionAction;
    QAction* addVariableAmpersandExpressionAction;
    QAction* addVariableAsteriskAmpersandExpressionAction;
    QAction* addMemoryVisualizerAction;
    QAction* addMemoryAsteriskVisualizerAction;
    QAction* addMemoryAmpersandVisualizerAction;

    // Enable/disable them depending if the breakpoint already exists.
    if (hasBreakpointLine(lineno) == true) {

        int breakno = breakpointLineToNumber(lineno);

        createAction    = new QAction(QIcon::fromTheme("document-new"), QString("Create breakpoint on line %1").arg(lineno), this);
        deleteAction    = new QAction(QIcon::fromTheme("edit-delete"),  QString("Delete breakpoint %1 on line %2").arg(breakno).arg(lineno), this);
        enableAction    = new QAction(QIcon::fromTheme("list-add"),     QString("Enable breakpoint %1 on line %2").arg(breakno).arg(lineno), this);
        disableAction   = new QAction(QIcon::fromTheme("list-remove"),  QString("Disable breakpoint %1 on line %2").arg(breakno).arg(lineno), this);
        runToLineAction = new QAction(QString("Run to line %1").arg(lineno), this);

        createAction->setEnabled(false);
        deleteAction->setEnabled(true);
        enableAction->setEnabled(true);
        disableAction->setEnabled(true);
        runToLineAction->setEnabled(true);

    }else{
        createAction    = new QAction(QIcon::fromTheme("document-new"), QString("Create breakpoint on line %1").arg(lineno), this);
        deleteAction    = new QAction(QIcon::fromTheme("edit-delete"),  QString("Delete breakpoint on line %1").arg(lineno), this);
        enableAction    = new QAction(QIcon::fromTheme("list-add"),     QString("Enable breakpoint on line %1").arg(lineno), this);
        disableAction   = new QAction(QIcon::fromTheme("list-remove"),  QString("Disable breakpoint on line %1").arg(lineno), this);
        runToLineAction = new QAction(QString("Run to line %1").arg(lineno), this);

        createAction->setEnabled(true);
        deleteAction->setEnabled(false);
        enableAction->setEnabled(false);
        disableAction->setEnabled(false);
        runToLineAction->setEnabled(true);
    }

    addVariableExpressionAction                  = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));
    addVariableAsteriskExpressionAction          = new QAction(QString("\"*%1\"").arg(textCursor().selectedText()));
    addVariableAmpersandExpressionAction         = new QAction(QString("\"&&%1\"").arg(textCursor().selectedText()));
    addVariableAsteriskAmpersandExpressionAction = new QAction(QString("\"*&&%1\"").arg(textCursor().selectedText()));
    addMemoryVisualizerAction                    = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));
    addMemoryAsteriskVisualizerAction            = new QAction(QString("\"*%1\"").arg(textCursor().selectedText()));
    addMemoryAmpersandVisualizerAction           = new QAction(QString("\"&&%1\"").arg(textCursor().selectedText()));

    QMenu menu("Breakpoints", this);
    menu.setTitle("Breakpoints");
    menu.addAction(createAction);
    menu.addAction(deleteAction);
    menu.addAction(enableAction);
    menu.addAction(disableAction);
    menu.addAction(runToLineAction);

    QMenu trackerMenu("Add variable to Tracker");
    trackerMenu.addAction(addVariableExpressionAction);
    trackerMenu.addAction(addVariableAsteriskExpressionAction);
    trackerMenu.addAction(addVariableAmpersandExpressionAction);
    trackerMenu.addAction(addVariableAsteriskAmpersandExpressionAction);
    menu.addMenu(&trackerMenu);

    QMenu memoryVisualizerMenu("Add variable to a Memory Visualizer");
    memoryVisualizerMenu.addAction(addMemoryVisualizerAction);
    memoryVisualizerMenu.addAction(addMemoryAsteriskVisualizerAction);
    memoryVisualizerMenu.addAction(addMemoryAmpersandVisualizerAction);
    menu.addMenu(&memoryVisualizerMenu);

    // Enable/disable items based on something being selected or not.
    if (textCursor().selectedText() == "") {
        addVariableExpressionAction->setEnabled(false);
        addVariableAsteriskExpressionAction->setEnabled(false);
        addVariableAmpersandExpressionAction->setEnabled(false);
        addVariableAsteriskAmpersandExpressionAction->setEnabled(false);
        addMemoryVisualizerAction->setEnabled(false);
        addMemoryAsteriskVisualizerAction->setEnabled(false);
        addMemoryAmpersandVisualizerAction->setEnabled(false);
    }else{
        addVariableExpressionAction->setEnabled(true);
        addVariableAsteriskExpressionAction->setEnabled(true);
        addVariableAmpersandExpressionAction->setEnabled(true);
        addVariableAsteriskAmpersandExpressionAction->setEnabled(true);
        addMemoryVisualizerAction->setEnabled(true);
        addMemoryAsteriskVisualizerAction->setEnabled(true);
        addMemoryAmpersandVisualizerAction->setEnabled(true);
    }

    // Launch the menu. Get the response.
    QAction* action = menu.exec(globalPos);

    // Do nothing.
    if (action == 0) {
        return;
    }

    // Handle creating a new breakpoint.
    if (action == createAction) {

        SeerBreakpointCreateDialog dlg(this);
        dlg.setFilename(fullname());
        dlg.setLineNumber(QString("%1").arg(lineno));

        int ret = dlg.exec();

        if (ret == 0) {
            return;
        }

        //qDebug() << __PRETTY_FUNCTION__ << ":" << dlg.breakpointText();

        // Emit the create breakpoint signal.
        emit insertBreakpoint(dlg.breakpointText());

        return;
    }

    // Handle deleting a breakpoint.
    if (action == deleteAction) {

        //qDebug() << __PRETTY_FUNCTION__ << ":" << "deleteBreakpoints" << lineno;

        // Emit the delete breakpoint signal.
        emit deleteBreakpoints(QString("%1").arg(breakpointLineToNumber(lineno)));

        return;
    }

    // Handle enabling a breakpoint.
    if (action == enableAction) {

        //qDebug() << __PRETTY_FUNCTION__ << ":" << "enableBreakpoints" << lineno;

        // Emit the enable breakpoint signal.
        emit enableBreakpoints(QString("%1").arg(breakpointLineToNumber(lineno)));

        return;
    }

    // Handle disabling a breakpoint.
    if (action == disableAction) {

        //qDebug() << __PRETTY_FUNCTION__ << ":" << "disableBreakpoints" << lineno;

        // Emit the disable breakpoint signal.
        emit disableBreakpoints(QString("%1").arg(breakpointLineToNumber(lineno)));

        return;
    }

    // Handle running to a line number.
    if (action == runToLineAction) {

        //qDebug() << __PRETTY_FUNCTION__ << ":" << "runToLine" << lineno;

        // Emit the runToLine signal.
        emit runToLine(fullname(), lineno);

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableExpressionAction) {

        //qDebug() << __PRETTY_FUNCTION__ << ":" << "addVariableExpression" << lineno;

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addVariableExpression(textCursor().selectedText());
            emit refreshVariableValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableAsteriskExpressionAction) {

        //qDebug() << __PRETTY_FUNCTION__ << ":" << "addVariableAsteriskExpression" << lineno;

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addVariableExpression(QString("*") + textCursor().selectedText());
            emit refreshVariableValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableAmpersandExpressionAction) {

        //qDebug() << __PRETTY_FUNCTION__ << ":" << "addVariableAmpersandExpression" << lineno;

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addVariableExpression(QString("&") + textCursor().selectedText());
            emit refreshVariableValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableAsteriskAmpersandExpressionAction) {

        //qDebug() << __PRETTY_FUNCTION__ << ":" << "addVariableAsteriskAmpersandExpression" << lineno;

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addVariableExpression(QString("*&") + textCursor().selectedText());
            emit refreshVariableValues();
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryVisualizerAction) {

        //qDebug() << __PRETTY_FUNCTION__ << ":" << "addMemoryVisualizer" << lineno;

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addMemoryVisualize(textCursor().selectedText());
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAsteriskVisualizerAction) {

        //qDebug() << __PRETTY_FUNCTION__ << ":" << "addMemoryAsteriskVisualizer" << lineno;

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addMemoryVisualize(QString("*") + textCursor().selectedText());
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAmpersandVisualizerAction) {

        //qDebug() << __PRETTY_FUNCTION__ << ":" << "addMemoryAmpersandVisualizer" << lineno;

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addMemoryVisualize(QString("&") + textCursor().selectedText());
        }

        return;
    }
}

void SeerEditorWidgetSourceArea::setQuickBreakpoint (QMouseEvent* event) {

    // Get the line number for the cursor position.
    QTextCursor cursor = cursorForPosition(event->pos());

    int lineno = cursor.blockNumber()+1;

    // If there is a breakpoint on the line, toggle it.
    if (hasBreakpointLine(lineno)) {

        // Toggle the breakpoint.
        // Enable if disabled. Disable if enabled.
        if (breakpointLineEnabled(lineno) == false) {
            // Emit the enable breakpoint signal.
            emit enableBreakpoints(QString("%1").arg(breakpointLineToNumber(lineno)));
        }else{
            // Emit the disable breakpoint signal.
            emit disableBreakpoints(QString("%1").arg(breakpointLineToNumber(lineno)));
        }

    // Otherwise, do a quick create of a new breakpoint.
    }else{
        emit insertBreakpoint(QString("-f --source %1 --line %2").arg(fullname()).arg(lineno));
    }
}

void SeerEditorWidgetSourceArea::clearExpression() {

    _selectedExpressionId     = 0;
    _selectedExpressionCursor = QTextCursor();
    _selectedExpressionName   = "";
    _selectedExpressionValue  = "";
}

void SeerEditorWidgetSourceArea::handleText (const QString& text) {

    if (text.startsWith("*stopped,reason=\"end-stepping-range\"")) {

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

        QString fullname_text = Seer::parseFirst(frame_text, "fullname=", '"', '"', false);
        QString file_text     = Seer::parseFirst(frame_text, "file=",     '"', '"', false);
        QString line_text     = Seer::parseFirst(frame_text, "line=",     '"', '"', false);

        //qDebug() << __PRETTY_FUNCTION__ << ":" << frame_text;
        //qDebug() << __PRETTY_FUNCTION__ << ":" << fullname_text << file_text << line_text;

        // Read the file if it hasn't been read before or if we are reading a different file.
        if (fullname_text != fullname()) {
            open(fullname_text, file_text);
        }

        // Set to the line number.
        setCurrentLine(line_text.toInt());

        return;

    }else if (text.startsWith("*stopped,reason=\"breakpoint-hit\"")) {
        // *stopped,
        //
        // reason="breakpoint-hit",
        //
        // disp="del",
        // bkptno="1",
        //
        // frame={addr="0x0000000000400b07",
        //        func="main",
        //        args=[{name="argc",value="1"},{name="argv",value="0x7fffffffd5b8"}],
        //        file="helloworld.cpp",
        //        fullname="/home/erniep/Development/Peak/src/Seer/helloworld/helloworld.cpp",
        //        line="7",
        //        arch="i386:x86-64"},
        //
        // thread-id="1",
        // stopped-threads="all",
        // core="1"

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString frame_text = Seer::parseFirst(newtext, "frame=", '{', '}', false);

        if (frame_text == "") {
            return;
        }

        QString fullname_text = Seer::parseFirst(frame_text, "fullname=", '"', '"', false);
        QString file_text     = Seer::parseFirst(frame_text, "file=",     '"', '"', false);
        QString line_text     = Seer::parseFirst(frame_text, "line=",     '"', '"', false);

        //qDebug() << __PRETTY_FUNCTION__ << ":" << frame_text;
        //qDebug() << __PRETTY_FUNCTION__ << ":" << fullname_text << file_text << line_text;

        // Read the file if it hasn't been read before or if we are reading a different file.
        if (fullname_text != fullname()) {
            open(fullname_text, file_text);
        }

        // Set to the line number.
        setCurrentLine(line_text.toInt());

        return;

    }else if (text.startsWith("*stopped,reason=\"function-finished\"")) {

        // *stopped,
        //
        // reason="function-finished",
        //
        // frame={addr="0x0000000000400b40",
        //        func="main",
        //        args=[{name="argc",value="1"},{name="argv",value="0x7fffffffd5b8"}],
        //        file="helloworld.cpp",
        //        fullname="/home/erniep/Development/Peak/src/Seer/helloworld/helloworld.cpp",
        //        line="11",
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

        QString fullname_text = Seer::parseFirst(frame_text, "fullname=", '"', '"', false);
        QString file_text     = Seer::parseFirst(frame_text, "file=",     '"', '"', false);
        QString line_text     = Seer::parseFirst(frame_text, "line=",     '"', '"', false);

        //qDebug() << __PRETTY_FUNCTION__ << ":" << frame_text;
        //qDebug() << __PRETTY_FUNCTION__ << ":" << fullname_text << file_text << line_text;

        // Read the file if it hasn't been read before or if we are reading a different file.
        if (fullname_text != fullname()) {
            open(fullname_text, file_text);
        }

        // Set to the line number.
        setCurrentLine(line_text.toInt());

        return;

    }else if (text.startsWith("*stopped,reason=\"location-reached\"")) {

        // *stopped,
        //
        // reason="location-reached",
        //
        // frame={addr=\"0x0000000000400607\",
        //        func=\"main\",
        //        args=[],
        //        file=\"helloonefile.cpp\",
        //        fullname=\"/home/erniep/Development/Peak/src/Seer/helloonefile/helloonefile.cpp\",
        //        line=\"35\",
        //        arch=\"i386:x86-64\"},
        //
        // thread-id=\"1\",
        // stopped-threads=\"all\",
        // core=\"1\"

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString frame_text = Seer::parseFirst(newtext, "frame=", '{', '}', false);

        if (frame_text == "") {
            return;
        }

        QString fullname_text = Seer::parseFirst(frame_text, "fullname=", '"', '"', false);
        QString file_text     = Seer::parseFirst(frame_text, "file=",     '"', '"', false);
        QString line_text     = Seer::parseFirst(frame_text, "line=",     '"', '"', false);

        //qDebug() << __PRETTY_FUNCTION__ << ":" << frame_text;
        //qDebug() << __PRETTY_FUNCTION__ << ":" << fullname_text << file_text << line_text;

        // Read the file if it hasn't been read before or if we are reading a different file.
        if (fullname_text != fullname()) {
            open(fullname_text, file_text);
        }

        // Set to the line number.
        setCurrentLine(line_text.toInt());

        return;

    }else if (text.startsWith("*stopped,reason=\"signal-received\"")) {

        // *stopped,
        //
        // reason=\"signal-received\",
        // signal-name=\"SIGSEGV\",
        // signal-meaning=\"Segmentation fault\",
        //
        // frame={addr=\"0x00007ffff712a420\",
        //        func=\"raise\",
        //        args=[],
        //        from=\"/lib64/libc.so.6\",
        //        arch=\"i386:x86-64\"},
        //
        // thread-id=\"1\",
        // stopped-threads=\"all\",
        // core=\"6\"

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString frame_text = Seer::parseFirst(newtext, "frame=", '{', '}', false);

        if (frame_text == "") {
            return;
        }

        QString fullname_text = Seer::parseFirst(frame_text, "fullname=", '"', '"', false);
        QString file_text     = Seer::parseFirst(frame_text, "file=",     '"', '"', false);
        QString line_text     = Seer::parseFirst(frame_text, "line=",     '"', '"', false);

        //qDebug() << __PRETTY_FUNCTION__ << ":" << frame_text;
        //qDebug() << __PRETTY_FUNCTION__ << ":" << fullname_text << file_text << line_text;

        // Read the file if it hasn't been read before or if we are reading a different file.
        if (fullname_text != fullname()) {
            open(fullname_text, file_text);
        }

        // Set to the line number.
        setCurrentLine(line_text.toInt());

        return;

    }else if (text.contains(QRegExp("^([0-9]+)\\^done,value="))) {

        // 10^done,value="1"
        // 11^done,value="0x7fffffffd538"

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _selectedExpressionId) {

            _selectedExpressionValue = Seer::filterEscapes(Seer::parseFirst(text, "value=", '"', '"', false));

            //qDebug() << __PRETTY_FUNCTION__ << ":" << _selectedExpressionValue;
        }

        return;

    }else if (text.contains(QRegExp("^([0-9]+)\\^error,msg="))) {

        // 12^error,msg="No symbol \"return\" in current context."
        // 13^error,msg="No symbol \"cout\" in current context."

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _selectedExpressionId) {

            _selectedExpressionValue = Seer::filterEscapes(Seer::parseFirst(text, "msg=", '"', '"', false));

            //qDebug() << __PRETTY_FUNCTION__ << ":" << _selectedExpressionValue;
        }

        return;

    }else{
        // Ignore others.
        return;
    }

    qDebug() << __PRETTY_FUNCTION__ << ":" << text;
}

