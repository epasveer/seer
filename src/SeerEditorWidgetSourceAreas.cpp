#include "SeerEditorWidgetSource.h"
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
#include <QtCore/QCoreApplication>
#include <QtCore/QProcess>

SeerEditorWidgetSourceArea::SeerEditorWidgetSourceArea(QWidget* parent) : SeerPlainTextEdit(parent) {

    _fileWatcher                = 0;
    _enableLineNumberArea       = false;
    _enableBreakPointArea       = false;
    _sourceHighlighter          = 0;
    _sourceHighlighterEnabled   = true;
    _sourceTabSize              = 4;
    _selectedExpressionId       = Seer::createID();
    _selectedBreakpointId       = Seer::createID();

    QFont font("monospace");
    font.setStyleHint(QFont::Monospace);

    setEditorFont(font);
    setEditorTabSize(4);
    setExternalEditorCommand("");

    setReadOnly(true);
    setTextInteractionFlags(textInteractionFlags() | Qt::TextSelectableByKeyboard);
    setLineWrapMode(QPlainTextEdit::NoWrap);

    _lineNumberArea = new SeerEditorWidgetSourceLineNumberArea(this);
    _breakPointArea = new SeerEditorWidgetSourceBreakPointArea(this);
    _breakPointArea->setMouseTracking(true);


    enableLineNumberArea(true);
    enableBreakPointArea(true);

    QObject::connect(this, &SeerEditorWidgetSourceArea::blockCountChanged,                  this, &SeerEditorWidgetSourceArea::updateMarginAreasWidth);
    QObject::connect(this, &SeerEditorWidgetSourceArea::updateRequest,                      this, &SeerEditorWidgetSourceArea::updateLineNumberArea);
    QObject::connect(this, &SeerEditorWidgetSourceArea::updateRequest,                      this, &SeerEditorWidgetSourceArea::updateBreakPointArea);
    QObject::connect(this, &SeerEditorWidgetSourceArea::highlighterSettingsChanged,         this, &SeerEditorWidgetSourceArea::handleHighlighterSettingsChanged);

    setCurrentLine(0);

    updateMarginAreasWidth(0);

    // Forward the scroll events in the various areas to the text edit.
    SeerPlainTextWheelEventForwarder* lineNumberAreaWheelForwarder = new SeerPlainTextWheelEventForwarder(this);
    SeerPlainTextWheelEventForwarder* breakPointAreaWheelForwarder = new SeerPlainTextWheelEventForwarder(this);

    _lineNumberArea->installEventFilter(lineNumberAreaWheelForwarder);
    _breakPointArea->installEventFilter(breakPointAreaWheelForwarder);

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

void SeerEditorWidgetSourceArea::updateMarginAreasWidth (int newBlockCount) {

    Q_UNUSED(newBlockCount);

    int leftMarginWidth  = lineNumberAreaWidth() + breakPointAreaWidth();
    int rightMarginWidth = 0;

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

void SeerEditorWidgetSourceArea::lineNumberAreaPaintEvent (QPaintEvent* event) {

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

void SeerEditorWidgetSourceArea::resizeEvent (QResizeEvent* e) {

    QPlainTextEdit::resizeEvent(e);

    QRect cr = contentsRect();

    if (lineNumberAreaEnabled()) {
        _lineNumberArea->setGeometry (QRect(cr.left(), cr.top(), lineNumberAreaWidth(), cr.height()));
    }

    if (breakPointAreaEnabled()) {
        _breakPointArea->setGeometry (QRect(cr.left() + lineNumberAreaWidth(), cr.top(), breakPointAreaWidth(), cr.height()));
    }
}

void SeerEditorWidgetSourceArea::contextMenuEvent (QContextMenuEvent* event) {

    showContextMenu(event);
}

void SeerEditorWidgetSourceArea::mouseReleaseEvent (QMouseEvent* event) {

    QPlainTextEdit::mouseReleaseEvent(event);

    if (textCursor().selectedText() == "") {
        // Nothing selected, clear current expression.
        clearExpression();
        return;
    }

    _selectedExpressionCursor   = textCursor();
    _selectedExpressionPosition = event->pos();
    _selectedExpressionValue    = "";

    // Look for a keyboard modifier to prepend a '*', '&', or '*&'.
    Qt::KeyboardModifiers modifiers = QGuiApplication::keyboardModifiers();

    if (modifiers == Qt::ControlModifier) {
        _selectedExpressionName = QString("*") + textCursor().selectedText();

    }else if (modifiers == Qt::ShiftModifier) {
        _selectedExpressionName = QString("&") + textCursor().selectedText();

    }else if (modifiers == (Qt::ShiftModifier | Qt::ControlModifier)) {
        _selectedExpressionName = QString("*&") + textCursor().selectedText();

    }else{
        _selectedExpressionName = textCursor().selectedText();
    }

    emit addVariableLoggerExpression(_selectedExpressionName); // For the variable logger.
}

bool SeerEditorWidgetSourceArea::event(QEvent* event) {

    // Handle the ToolTip event.
    if (event->type() == QEvent::ToolTip) {

        while (1) { // Create a region of the code that does one pass.

            // Convert the event to a Help event.
            QHelpEvent* helpEvent = static_cast<QHelpEvent*>(event);

            // Massage the event location to account for the linenumber and breakpoint widgets.
            QPoint pos = QPoint(helpEvent->pos().x() - _lineNumberArea->width() - _breakPointArea->width(), helpEvent->pos().y());

            // Create a cursor at the position so we can get the text underneath at the cursor.
            QTextCursor cursor = cursorForPosition(pos);
            cursor.select(QTextCursor::WordUnderCursor);

            // If the hover text is empty, do nothing. Reset things. Exit this function.
            QString word = cursor.selectedText();

            if (word.isEmpty() == true) {
                hideExpressionTooltip();
                break;
            }

            // Is our cursor the same as the previous.
            if (cursor == _selectedExpressionCursor) {

                // Same word as before? Display the tooltip value.
                if (word == _selectedExpressionName) {

                    // If the tooltip is already visible, refreshen it with
                    // the same value, possibly at a new postion.
                    if (QToolTip::isVisible()) {
                        _selectedExpressionPosition = helpEvent->globalPos();

                        showExpressionTooltip();

                    // If not already visible, refreshen its value.
                    }else{
                        emit evaluateVariableExpression(_selectedExpressionId, _selectedExpressionName); // For the tooltip.
                    }

                // Otherwise, hide any old one.
                }else{
                    hideExpressionTooltip();
                }

            // Otherwise it's a different spot. Create a new request to get the variable's value.
            }else{

                hideExpressionTooltip();

                _selectedExpressionCursor   = cursor;
                _selectedExpressionPosition = helpEvent->globalPos();
                _selectedExpressionName     = word;
                _selectedExpressionValue    = "";

                emit evaluateVariableExpression(_selectedExpressionId, _selectedExpressionName); // For the tooltip.
            }

            break;
        }

        return true;
    }

    // Pass any others to the base class.
    return QPlainTextEdit::event(event);
}

void SeerEditorWidgetSourceArea::showExpressionTooltip () {

    // qDebug() << "Tooltip:" << _selectedExpressionPosition << _selectedExpressionName << _selectedExpressionValue;

    QToolTip::hideText();
    QToolTip::showText(_selectedExpressionPosition, _selectedExpressionName + ": " + Seer::elideText(_selectedExpressionValue, Qt::ElideRight, 100));
}

void SeerEditorWidgetSourceArea::hideExpressionTooltip () {

    QToolTip::hideText();

    _selectedExpressionCursor   = QTextCursor();
    _selectedExpressionPosition = QPoint();
    _selectedExpressionName     = "";
    _selectedExpressionValue    = "";
}

void SeerEditorWidgetSourceArea::refreshExtraSelections () {

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

bool SeerEditorWidgetSourceArea::isOpen () const {

    if (_fullname != "") {
        return true;
    }

    return false;
}

void SeerEditorWidgetSourceArea::open (const QString& fullname, const QString& file, const QString& alternateDirectory) {

    // Delete old file watcher, if any.
    if (_fileWatcher) {
        delete _fileWatcher; _fileWatcher = 0;
    }

    // Close the previous file, if any.
    if (isOpen()) {
        close();
    }

    // Save the filename.
    _fullname           = fullname;
    _file               = file;
    _alternateDirectory = alternateDirectory;

    setDocumentTitle(_fullname);

    // If the filename or file is null, it's a placeholder
    // widget. Show the Seer icon!
    if (_fullname == "" || _file == "") {
        setStyleSheet("QPlainTextEdit {"
            "background: url(:/seer/resources/seergdb_512x512.png) center no-repeat;"
            "border: none;"
            "}");
        return;
    }

    //
    // Open the file.
    //
    // See findFile() to see the search paths and order.
    //
    QString filename = findFile(_file, _fullname, _alternateDirectory, _alternateDirectories);

    if (filename == "") {
        QMessageBox::critical(this, "Can't find source file.",  "Can't find : " + _file + "\nIts location may have changed.");

        emit showAlternateBar(true);

        return;
    }

    QFile inputFile(filename);

    inputFile.open(QIODevice::ReadOnly);

    if (!inputFile.isOpen()) {

        QMessageBox::critical(this, "Can't read source file.",  "Can't read : " + filename + "\nThe file is there but can't be opened.");

        emit showAlternateBar(true);

        return;
    }

    // Read the file.
    QTextStream stream(&inputFile);

    QString line = stream.readLine();
    QString text;

    while (!line.isNull()) {

        // Expand tabs
        line = Seer::expandTabs(line, editorTabSize(), false);

        // Build up text for file.
        text += line + "\n";

        // Read next line, if available.
        line = stream.readLine();
    };

    // Put the contents in the editor.
    openText(text, _fullname);

    // Watch the file.
    _fileWatcher = new QFileSystemWatcher(this);
    _fileWatcher->addPath(filename);

    QObject::connect(_fileWatcher, &QFileSystemWatcher::fileChanged,      this, &SeerEditorWidgetSourceArea::handleWatchFileModified);
}

void SeerEditorWidgetSourceArea::openText (const QString& text, const QString& file) {

    // Put the contents in the editor.
    setPlainText(text);

    // Set the text cursor to the first line.
    QTextCursor cursor = textCursor();
    cursor.movePosition(QTextCursor::Start, QTextCursor::MoveAnchor, 1);
    setTextCursor(cursor);

    // Add a syntax highlighter for C++ files.
    if (_sourceHighlighter) {
        delete _sourceHighlighter; _sourceHighlighter = 0;
    }

    QRegularExpression cpp_re("(?:" + _sourceHighlighterSettings.sourceSuffixes() + ")$");
    if (file.contains(cpp_re)) {
        _sourceHighlighter = new SeerCppSourceHighlighter(0);

        if (highlighterEnabled()) {
            _sourceHighlighter->setDocument(document());
        }else{
            _sourceHighlighter->setDocument(0);
        }

        _sourceHighlighter->setHighlighterSettings(_sourceHighlighterSettings);
        _sourceHighlighter->rehighlight();
    }
}

void SeerEditorWidgetSourceArea::reload () {

    // Open the same file again.
    QString fullname           = _fullname;
    QString file               = _file;
    QString alternateDirectory = _alternateDirectory;

    if (fullname == "") return;
    if (file     == "") return;

    // Save old cursor and scroll position.
    int blockno    = textCursor().blockNumber();
    int posinblock = textCursor().positionInBlock();
    int vscrollpos = verticalScrollBar()->value();

    // Reload file.
    open(fullname, file, alternateDirectory);

    // Restore to old cursor and scroll position.
    QTextCursor c = textCursor();

    c.movePosition(QTextCursor::Start, QTextCursor::MoveAnchor, 1);
    c.movePosition(QTextCursor::Down,  QTextCursor::MoveAnchor, blockno);
    c.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, posinblock);

    setTextCursor(c);

    ensureCursorVisible();

    verticalScrollBar()->setValue(vscrollpos);

    // Refresh any breakpoints or current lines in the stackframes.
    emit refreshBreakpointsStackFrames();
}

void SeerEditorWidgetSourceArea::close () {

    setDocumentTitle("");
    setPlainText("");

    clearExpression();

    // Delete old file watcher, if any.
    if (_fileWatcher) {
        delete _fileWatcher; _fileWatcher = 0;
    }
}

const QString& SeerEditorWidgetSourceArea::fullname () const {
    return _fullname;
}

const QString& SeerEditorWidgetSourceArea::file () const {
    return _file;
}

void SeerEditorWidgetSourceArea::setAlternateDirectory (const QString& alternateDirectory) {

    _alternateDirectory = alternateDirectory;
}

const QString& SeerEditorWidgetSourceArea::alternateDirectory () const {

    return _alternateDirectory;
}

void SeerEditorWidgetSourceArea::setAlternateDirectories (const QStringList& alternateDirectories) {

    _alternateDirectories = alternateDirectories;
}

const QStringList& SeerEditorWidgetSourceArea::alternateDirectories () const {

    return _alternateDirectories;
}

QString SeerEditorWidgetSourceArea::findFile (const QString& file, const QString& fullname, const QString& alternateDirectory, const QStringList& alternateDirectories) {

    //
    // This function returns the filename to use to load the source file.
    //
    // 'file' is the short version of the filename. eg: 'source.cpp'
    // 'fullname' is the long version. eg: '/path/to/locate/source.cpp'
    // 'alternateDirectory' is the alternate directory to use if 'fullname' is not found.
    // 'alternateDirectories' is a list of alternate directories if 'fullname' is not found
    // and if 'file' is not in 'alternateDirectory'.
    //
    // 'alternateDirectory' can be blank ("") and usually is. If it isn't blank, then it
    // takes presedence over 'fullname' and 'alternateDirectories'.
    //
    // 'alternateDirectories' is a list of directory locations. It defines the search order.
    // Once a location has the 'file', the search stops and that directory is used.
    //
    // If the 'file' can't be found in any of the locations, a "" is returned.
    //
    // One note about 'fullname'. This is the path that gdb knows of. Gdb extracts the path
    // from the debug information in the executable when it was compiled and linked.
    //

    // Use 'alternateDirectory', if provided.
    if (alternateDirectory != "") {

        QString filename = alternateDirectory + "/" + file;

        if (QFileInfo::exists(filename) == false) {
            return "";
        }

        return filename;
    }

    // Handle 'fullname'.
    if (QFileInfo::exists(fullname) == true) {
        return fullname;
    }

    // Handle 'alternateDirectories'.
    QStringListIterator iter(alternateDirectories);

    while (iter.hasNext()) {

        QString filename = iter.next() + "/" + file;

        if (QFileInfo::exists(filename) == true) {
            return filename;
        }
    }

    // Not found anywhere.
    return "";
}

void SeerEditorWidgetSourceArea::setCurrentLine (int lineno) {

    // Clear current line selections.
    _currentLinesExtraSelections.clear();

    // Highlight if a valid line number is selected.
    if (lineno >= 1) {

        QTextBlock  block  = document()->findBlockByLineNumber(lineno-1);
        QTextCursor cursor = textCursor();

        cursor.setPosition(block.position());
        setTextCursor(cursor);

        _currentLinesExtraSelections.clear();

        QTextCharFormat lineFormat = highlighterSettings().get("Current Line");

        QTextEdit::ExtraSelection selection;
        selection.format.setForeground(lineFormat.foreground());
        selection.format.setBackground(lineFormat.background());
        selection.format.setProperty(QTextFormat::FullWidthSelection, true);
        selection.cursor = textCursor();
        selection.cursor.clearSelection();

        _currentLinesExtraSelections.append(selection);
    }

    // Refresh all the extra selections.
    refreshExtraSelections();
}

void SeerEditorWidgetSourceArea::scrollToLine (int lineno) {

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

    _currentLinesExtraSelections.clear();

    refreshExtraSelections();
}

void SeerEditorWidgetSourceArea::addCurrentLine (int lineno, int level) {

    // Any line will be highlighted with a yellow line.
    // The 'yellow' color is for the current line of the most recent stack frame.
    // The 'grey' color is for older stack frames.
    QTextCharFormat lineFormat;
    if (level == 0) {
        lineFormat = highlighterSettings().get("Current Line");
    }else{
        lineFormat = highlighterSettings().get("Calling Line");
    }

    // Create a selection at the cursor.
    QTextBlock  block  = document()->findBlockByLineNumber(lineno-1);
    QTextCursor cursor = textCursor();

    cursor.setPosition(block.position());

    QTextEdit::ExtraSelection selection;
    selection.format.setForeground(lineFormat.foreground());
    selection.format.setBackground(lineFormat.background());
    selection.format.setProperty(QTextFormat::FullWidthSelection, true);
    selection.cursor = cursor;
    selection.cursor.clearSelection();

    // Add it to the extra selection list.
    _currentLinesExtraSelections.append(selection);

    // Refresh all the extra selections.
    refreshExtraSelections();
}

int SeerEditorWidgetSourceArea::findText (const QString& text, QTextDocument::FindFlags flags) {

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

void SeerEditorWidgetSourceArea::clearFindText () {

    _findExtraSelections.clear();

    refreshExtraSelections();
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
    return _breakpointsNumbers.contains(number);
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

#if QT_VERSION >= 0x060000
    showContextMenu(event->pos(), event->globalPosition());
#else
    showContextMenu(event->pos(), event->globalPos());
#endif
}

void SeerEditorWidgetSourceArea::showContextMenu (QContextMenuEvent* event) {

    showContextMenu(event->pos(), event->globalPos());
}

void SeerEditorWidgetSourceArea::showContextMenu (const QPoint& pos, const QPointF& globalPos) {

    // Get the line number for the cursor position.
    QTextCursor cursor = cursorForPosition(pos);

    int lineno = cursor.blockNumber()+1;

    // Create the menu actions.
    QAction* createBreakpointAction;
    QAction* createPrintpointAction;
    QAction* deleteAction;
    QAction* enableAction;
    QAction* disableAction;
    QAction* runToLineAction;
    QAction* openExternalEditor;
    QAction* addVariableLoggerExpressionAction;
    QAction* addVariableLoggerAsteriskExpressionAction;
    QAction* addVariableLoggerAmpersandExpressionAction;
    QAction* addVariableLoggerAsteriskAmpersandExpressionAction;
    QAction* addVariableTrackerExpressionAction;
    QAction* addVariableTrackerAsteriskExpressionAction;
    QAction* addVariableTrackerAmpersandExpressionAction;
    QAction* addVariableTrackerAsteriskAmpersandExpressionAction;
    QAction* addMemoryVisualizerAction;
    QAction* addMemoryAsteriskVisualizerAction;
    QAction* addMemoryAmpersandVisualizerAction;
    QAction* addArrayVisualizerAction;
    QAction* addArrayAsteriskVisualizerAction;
    QAction* addArrayAmpersandVisualizerAction;
    QAction* addStructVisualizerAction;
    QAction* addStructAsteriskVisualizerAction;
    QAction* addStructAmpersandVisualizerAction;

    // Enable/disable them depending if the breakpoint already exists.
    if (hasBreakpointLine(lineno) == true) {

        int breakno = breakpointLineToNumber(lineno);

        runToLineAction           = new QAction(QString("Run to line %1").arg(lineno), this);
        createBreakpointAction    = new QAction(QIcon(":/seer/resources/RelaxLightIcons/document-new.svg"), QString("Create breakpoint on line %1").arg(lineno), this);
        createPrintpointAction    = new QAction(QIcon(":/seer/resources/RelaxLightIcons/document-new.svg"), QString("Create printpoint on line %1").arg(lineno), this);
        deleteAction              = new QAction(QIcon(":/seer/resources/RelaxLightIcons/edit-delete.svg"),  QString("Delete breakpoint %1 on line %2").arg(breakno).arg(lineno), this);
        enableAction              = new QAction(QIcon(":/seer/resources/RelaxLightIcons/list-add.svg"),     QString("Enable breakpoint %1 on line %2").arg(breakno).arg(lineno), this);
        disableAction             = new QAction(QIcon(":/seer/resources/RelaxLightIcons/list-remove.svg"),  QString("Disable breakpoint %1 on line %2").arg(breakno).arg(lineno), this);
        openExternalEditor        = new QAction(QIcon(":/seer/resources/RelaxLightIcons/document-new.svg"), QString("Open external editor on line %1").arg(lineno), this);

        runToLineAction->setEnabled(true);
        createBreakpointAction->setEnabled(false);
        createPrintpointAction->setEnabled(false);
        deleteAction->setEnabled(true);
        enableAction->setEnabled(true);
        disableAction->setEnabled(true);
        openExternalEditor->setEnabled(true);

    }else{
        runToLineAction           = new QAction(QString("Run to line %1").arg(lineno), this);
        createBreakpointAction    = new QAction(QIcon(":/seer/resources/RelaxLightIcons/document-new.svg"), QString("Create breakpoint on line %1").arg(lineno), this);
        createPrintpointAction    = new QAction(QIcon(":/seer/resources/RelaxLightIcons/document-new.svg"), QString("Create printpoint on line %1").arg(lineno), this);
        deleteAction              = new QAction(QIcon(":/seer/resources/RelaxLightIcons/edit-delete.svg"),  QString("Delete breakpoint on line %1").arg(lineno), this);
        enableAction              = new QAction(QIcon(":/seer/resources/RelaxLightIcons/list-add.svg"),     QString("Enable breakpoint on line %1").arg(lineno), this);
        disableAction             = new QAction(QIcon(":/seer/resources/RelaxLightIcons/list-remove.svg"),  QString("Disable breakpoint on line %1").arg(lineno), this);
        openExternalEditor        = new QAction(QIcon(":/seer/resources/RelaxLightIcons/document-new.svg"), QString("Open file in external editor"), this);

        runToLineAction->setEnabled(true);
        createBreakpointAction->setEnabled(true);
        createPrintpointAction->setEnabled(true);
        deleteAction->setEnabled(false);
        enableAction->setEnabled(false);
        disableAction->setEnabled(false);
        openExternalEditor->setEnabled(true);
    }

    addVariableLoggerExpressionAction                   = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));
    addVariableLoggerAsteriskExpressionAction           = new QAction(QString("\"*%1\"").arg(textCursor().selectedText()));
    addVariableLoggerAmpersandExpressionAction          = new QAction(QString("\"&&%1\"").arg(textCursor().selectedText()));
    addVariableLoggerAsteriskAmpersandExpressionAction  = new QAction(QString("\"*&&%1\"").arg(textCursor().selectedText()));
    addVariableTrackerExpressionAction                  = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));
    addVariableTrackerAsteriskExpressionAction          = new QAction(QString("\"*%1\"").arg(textCursor().selectedText()));
    addVariableTrackerAmpersandExpressionAction         = new QAction(QString("\"&&%1\"").arg(textCursor().selectedText()));
    addVariableTrackerAsteriskAmpersandExpressionAction = new QAction(QString("\"*&&%1\"").arg(textCursor().selectedText()));
    addMemoryVisualizerAction                           = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));
    addMemoryAsteriskVisualizerAction                   = new QAction(QString("\"*%1\"").arg(textCursor().selectedText()));
    addMemoryAmpersandVisualizerAction                  = new QAction(QString("\"&&%1\"").arg(textCursor().selectedText()));
    addArrayVisualizerAction                            = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));
    addArrayAsteriskVisualizerAction                    = new QAction(QString("\"*%1\"").arg(textCursor().selectedText()));
    addArrayAmpersandVisualizerAction                   = new QAction(QString("\"&&%1\"").arg(textCursor().selectedText()));
    addStructVisualizerAction                           = new QAction(QString("\"%1\"").arg(textCursor().selectedText()));
    addStructAsteriskVisualizerAction                   = new QAction(QString("\"*%1\"").arg(textCursor().selectedText()));
    addStructAmpersandVisualizerAction                  = new QAction(QString("\"&&%1\"").arg(textCursor().selectedText()));

    QMenu menu("Breakpoints", this);
    menu.setTitle("Breakpoints");
    menu.addAction(runToLineAction);
    menu.addAction(createBreakpointAction);
    menu.addAction(createPrintpointAction);
    menu.addAction(deleteAction);
    menu.addAction(enableAction);
    menu.addAction(disableAction);
    menu.addAction(openExternalEditor);

    QMenu loggerMenu("Add variable to Logger");
    loggerMenu.addAction(addVariableLoggerExpressionAction);
    loggerMenu.addAction(addVariableLoggerAsteriskExpressionAction);
    loggerMenu.addAction(addVariableLoggerAmpersandExpressionAction);
    loggerMenu.addAction(addVariableLoggerAsteriskAmpersandExpressionAction);
    menu.addMenu(&loggerMenu);

    QMenu trackerMenu("Add variable to Tracker");
    trackerMenu.addAction(addVariableTrackerExpressionAction);
    trackerMenu.addAction(addVariableTrackerAsteriskExpressionAction);
    trackerMenu.addAction(addVariableTrackerAmpersandExpressionAction);
    trackerMenu.addAction(addVariableTrackerAsteriskAmpersandExpressionAction);
    menu.addMenu(&trackerMenu);

    QMenu memoryVisualizerMenu("Add variable to a Memory Visualizer");
    memoryVisualizerMenu.addAction(addMemoryVisualizerAction);
    memoryVisualizerMenu.addAction(addMemoryAsteriskVisualizerAction);
    memoryVisualizerMenu.addAction(addMemoryAmpersandVisualizerAction);
    menu.addMenu(&memoryVisualizerMenu);

    QMenu arrayVisualizerMenu("Add variable to an Array Visualizer");
    arrayVisualizerMenu.addAction(addArrayVisualizerAction);
    arrayVisualizerMenu.addAction(addArrayAsteriskVisualizerAction);
    arrayVisualizerMenu.addAction(addArrayAmpersandVisualizerAction);
    menu.addMenu(&arrayVisualizerMenu);

    QMenu structVisualizerMenu("Add variable to a Struct Visualizer");
    structVisualizerMenu.addAction(addStructVisualizerAction);
    structVisualizerMenu.addAction(addStructAsteriskVisualizerAction);
    structVisualizerMenu.addAction(addStructAmpersandVisualizerAction);
    menu.addMenu(&structVisualizerMenu);

    // Enable/disable items based on something being selected or not.
    if (textCursor().selectedText() == "") {
        addVariableLoggerExpressionAction->setEnabled(false);
        addVariableLoggerAsteriskExpressionAction->setEnabled(false);
        addVariableLoggerAmpersandExpressionAction->setEnabled(false);
        addVariableLoggerAsteriskAmpersandExpressionAction->setEnabled(false);
        addVariableTrackerExpressionAction->setEnabled(false);
        addVariableTrackerAsteriskExpressionAction->setEnabled(false);
        addVariableTrackerAmpersandExpressionAction->setEnabled(false);
        addVariableTrackerAsteriskAmpersandExpressionAction->setEnabled(false);
        addMemoryVisualizerAction->setEnabled(false);
        addMemoryAsteriskVisualizerAction->setEnabled(false);
        addMemoryAmpersandVisualizerAction->setEnabled(false);
        addArrayVisualizerAction->setEnabled(false);
        addArrayAsteriskVisualizerAction->setEnabled(false);
        addArrayAmpersandVisualizerAction->setEnabled(false);
        addStructVisualizerAction->setEnabled(false);
        addStructAsteriskVisualizerAction->setEnabled(false);
        addStructAmpersandVisualizerAction->setEnabled(false);
    }else{
        addVariableLoggerExpressionAction->setEnabled(true);
        addVariableLoggerAsteriskExpressionAction->setEnabled(true);
        addVariableLoggerAmpersandExpressionAction->setEnabled(true);
        addVariableLoggerAsteriskAmpersandExpressionAction->setEnabled(true);
        addVariableTrackerExpressionAction->setEnabled(true);
        addVariableTrackerAsteriskExpressionAction->setEnabled(true);
        addVariableTrackerAmpersandExpressionAction->setEnabled(true);
        addVariableTrackerAsteriskAmpersandExpressionAction->setEnabled(true);
        addMemoryVisualizerAction->setEnabled(true);
        addMemoryAsteriskVisualizerAction->setEnabled(true);
        addMemoryAmpersandVisualizerAction->setEnabled(true);
        addArrayVisualizerAction->setEnabled(true);
        addArrayAsteriskVisualizerAction->setEnabled(true);
        addArrayAmpersandVisualizerAction->setEnabled(true);
        addStructVisualizerAction->setEnabled(true);
        addStructAsteriskVisualizerAction->setEnabled(true);
        addStructAmpersandVisualizerAction->setEnabled(true);
    }

    // Launch the menu. Get the response.
    QAction* action = menu.exec(globalPos.toPoint());

    // Do nothing.
    if (action == 0) {
        return;
    }

    // Handle running to a line number.
    if (action == runToLineAction) {

        // Emit the runToLine signal.
        emit runToLine(fullname(), lineno);

        return;
    }

    // Handle creating a new breakpoint.
    if (action == createBreakpointAction) {

        SeerBreakpointCreateDialog dlg(this);
        dlg.setFilename(fullname());
        dlg.setLineNumber(QString("%1").arg(lineno));

        int ret = dlg.exec();

        if (ret == 0) {
            return;
        }

        // Emit the create breakpoint signal.
        emit insertBreakpoint(dlg.breakpointText());

        return;
    }

    // Handle creating a new printpoint.
    if (action == createPrintpointAction) {

        SeerPrintpointCreateDialog dlg(this);
        dlg.setFilename(fullname());
        dlg.setLineNumber(QString("%1").arg(lineno));

        int ret = dlg.exec();

        if (ret == 0) {
            return;
        }

        // Build a printpoint specification.
        QString type       = dlg.dprintfType();
        QString function   = dlg.dprintfFunction();
        QString channel    = dlg.dprintfChannel();
        QString parameters = dlg.printpointParameters();

        // If nothing, just return.
        if (parameters == "" || type == "") {
            return;
        }

        // Emit the create breakpoint signal.
        emit insertPrintpoint(type, function, channel, parameters);

        return;
    }

    // Handle deleting a breakpoint.
    if (action == deleteAction) {

        // Emit the delete breakpoint signal.
        emit deleteBreakpoints(QString("%1").arg(breakpointLineToNumber(lineno)));

        return;
    }

    // Handle enabling a breakpoint.
    if (action == enableAction) {

        // Emit the enable breakpoint signal.
        emit enableBreakpoints(QString("%1").arg(breakpointLineToNumber(lineno)));

        return;
    }

    // Handle disabling a breakpoint.
    if (action == disableAction) {

        // Emit the disable breakpoint signal.
        emit disableBreakpoints(QString("%1").arg(breakpointLineToNumber(lineno)));

        return;
    }

    // Handle open code editor at a line number.
    if (action == openExternalEditor) {

        // External editor examples:
        //  $ export SEERGDB_CustomCodeEditor='geany "%{file}":%{line}'
        //  $ export SEERGDB_CustomCodeEditor='kate --line %{line} "%{file}"'
        //  $ export SEERGDB_CustomCodeEditor='gedit "%{file}" +%{line}'
        //  $ export SEERGDB_CustomCodeEditor='konsole -e vim "%{file}" +%{line}'
        //  QString codeEditorCmd = []() { const char* pc = std::getenv("SEERGDB_CustomCodeEditor"); if(pc) return pc; return ""; } ();
        QString codeEditorCmd = externalEditorCommand();

        if (codeEditorCmd == "") {
            QMessageBox::critical(this, "Error!",  "External editor not set.\n\nSee the Editor Config page to set the editor to use.");
            return;
        }

        // Mark cursor as busy.
        QApplication::setOverrideCursor(Qt::BusyCursor);

        codeEditorCmd.replace("%{file}", fullname());
        codeEditorCmd.replace("%{line}", QString::number(lineno));

        QProcess* process = new QProcess(this);
        process->startCommand(codeEditorCmd);

        bool f = process->waitForStarted(5000);

        // Set the cursor back.
        QApplication::restoreOverrideCursor();

        if (f == false) {
            QMessageBox::critical(this, "Error!",  "Launching external editor failed.\n\nCommand: '" + codeEditorCmd + "'");
            qDebug().nospace() << "Launching external editor failed. Command: '" << codeEditorCmd << "'";
            qDebug().nospace() << "Error: '" << process->error() << "'";
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerExpressionAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addVariableLoggerExpression(textCursor().selectedText());
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAsteriskExpressionAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addVariableLoggerExpression(QString("*") + textCursor().selectedText());
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAmpersandExpressionAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addVariableLoggerExpression(QString("&") + textCursor().selectedText());
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAsteriskAmpersandExpressionAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addVariableLoggerExpression(QString("*&") + textCursor().selectedText());
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerExpressionAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addVariableTrackerExpression(textCursor().selectedText());
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAsteriskExpressionAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addVariableTrackerExpression(QString("*") + textCursor().selectedText());
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAmpersandExpressionAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addVariableTrackerExpression(QString("&") + textCursor().selectedText());
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAsteriskAmpersandExpressionAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addVariableTrackerExpression(QString("*&") + textCursor().selectedText());
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addMemoryVisualize(textCursor().selectedText());
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAsteriskVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addMemoryVisualize(QString("*") + textCursor().selectedText());
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAmpersandVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addMemoryVisualize(QString("&") + textCursor().selectedText());
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addArrayVisualize(textCursor().selectedText());
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayAsteriskVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addArrayVisualize(QString("*") + textCursor().selectedText());
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayAmpersandVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addArrayVisualize(QString("&") + textCursor().selectedText());
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addStructVisualize(textCursor().selectedText());
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructAsteriskVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addStructVisualize(QString("*") + textCursor().selectedText());
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructAmpersandVisualizerAction) {

        // Emit the signals.
        if (textCursor().selectedText() != "") {
            emit addStructVisualize(QString("&") + textCursor().selectedText());
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
            emit deleteBreakpoints(QString("%1").arg(breakpointLineToNumber(lineno)));
        }

    // Otherwise, do a quick create of a new breakpoint.
    }else{
        emit insertBreakpoint(QString("-f --source \"%1\" --line %2").arg(fullname()).arg(lineno));
    }
}

void SeerEditorWidgetSourceArea::setQuickRunToLine (QMouseEvent* event) {

    // Get the line number for the cursor position.
    QTextCursor cursor = cursorForPosition(event->pos());

    int lineno = cursor.blockNumber()+1;

    // Emit the runToLine signal.
    emit runToLine(fullname(), lineno);

    return;
}

void SeerEditorWidgetSourceArea::showBreakpointToolTip (QMouseEvent* event) {

    // Get the line number for the cursor position.
    QTextCursor cursor = cursorForPosition(event->pos());

    int lineno = cursor.blockNumber()+1;

    // If there is a breakpoint on the line, ask for its information.
    if (hasBreakpointLine(lineno)) {

        _selectedBreakpointPosition = event->pos();

        // Emit the info breakpoint signal.
        emit infoBreakpoint(_selectedBreakpointId, QString("%1").arg(breakpointLineToNumber(lineno)));

    }else{
        _selectedBreakpointPosition = QPoint();
    }
}

void SeerEditorWidgetSourceArea::clearExpression() {

    _selectedExpressionCursor   = QTextCursor();
    _selectedExpressionPosition = QPoint();
    _selectedExpressionName     = "";
    _selectedExpressionValue    = "";
    _selectedBreakpointPosition = QPoint();
}

void SeerEditorWidgetSourceArea::setHighlighterSettings (const SeerHighlighterSettings& settings) {

    _sourceHighlighterSettings = settings;

    emit highlighterSettingsChanged();
}

const SeerHighlighterSettings& SeerEditorWidgetSourceArea::highlighterSettings () const {

    return _sourceHighlighterSettings;
}

void SeerEditorWidgetSourceArea::setHighlighterEnabled (bool flag) {

    _sourceHighlighterEnabled = flag;

    emit highlighterSettingsChanged();
}

bool SeerEditorWidgetSourceArea::highlighterEnabled () const {

    return _sourceHighlighterEnabled;
}

void SeerEditorWidgetSourceArea::setEditorFont (const QFont& font) {

    setFont(font);

    /*

     * None of the tabstops work. Setting tabstops never look the same
     * as replacing tabs with spaces. Not sure why.
     * Tried setTabStopDistance() and setTabArray(). Same problem.
     * Zooming in and out in QPlainTextEdit will also result in bad
     * tabstops.

    QFont f = font;

    f.setLetterSpacing(QFont::PercentageSpacing, 100);
    f.setWordSpacing(0.0);
    f.setFixedPitch(true);
    f.setKerning(false);

    setFont(f);

    qreal tabspace = fontMetrics().horizontalAdvance(QLatin1Char(' ')) * 4;

  //setTabStopDistance(tabspace);

    QList<qreal> tabstops;
    qreal        tabpos = 0.0;

    for (int i=0; i<100; i++) {
        tabstops.append(tabpos);
        tabpos += tabspace;
    }

    QTextOption opt = document()->defaultTextOption();
    opt.setFlags(QTextOption::ShowTabsAndSpaces);
    opt.setTabArray(tabstops);
    document()->setDefaultTextOption(opt);

    */
}

const QFont& SeerEditorWidgetSourceArea::editorFont () const {

    return font();
}

void SeerEditorWidgetSourceArea::setEditorTabSize (int spaces) {

    _sourceTabSize = spaces;
}

int SeerEditorWidgetSourceArea::editorTabSize () const {

    return _sourceTabSize;
}

void SeerEditorWidgetSourceArea::setExternalEditorCommand (const QString& externalEditorCommand) {

    _externalEditorCommand = externalEditorCommand;
}

const QString& SeerEditorWidgetSourceArea::externalEditorCommand () {

    return _externalEditorCommand;
}

void SeerEditorWidgetSourceArea::handleText (const QString& text) {

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

        QString fullname_text = Seer::parseFirst(frame_text, "fullname=", '"', '"', false);
        QString file_text     = Seer::parseFirst(frame_text, "file=",     '"', '"', false);
        QString line_text     = Seer::parseFirst(frame_text, "line=",     '"', '"', false);

        // Read the file if it hasn't been read before or if we are reading a different file.
        if (fullname_text != fullname()) {
            open(fullname_text, QFileInfo(file_text).fileName());
        }

        // Move to the line number.
        setCurrentLine(line_text.toInt());

        return;

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,value="))) {

        // 10^done,value="1"
        // 11^done,value="0x7fffffffd538"

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _selectedExpressionId) {

            _selectedExpressionValue = Seer::filterEscapes(Seer::parseFirst(text, "value=", '"', '"', false));

            showExpressionTooltip();
        }

        return;

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,BreakpointTable="))) {

        // 11^done,BreakpointTable={...}

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _selectedBreakpointId && _selectedBreakpointPosition != QPoint()) {
            handleBreakpointToolTip(_selectedBreakpointPosition, text);
        }

        return;

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^error,msg="))) {

        // 12^error,msg="No symbol \"return\" in current context."
        // 13^error,msg="No symbol \"cout\" in current context."

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _selectedExpressionId) {

            _selectedExpressionValue = Seer::filterEscapes(Seer::parseFirst(text, "msg=", '"', '"', false));

            // Refresh the tooltip event.
            QHelpEvent* event = new QHelpEvent(QEvent::ToolTip, _selectedExpressionPosition, this->mapToGlobal(_selectedExpressionPosition));

            QCoreApplication::postEvent(this, event);
        }

        if (id_text.toInt() == _selectedBreakpointId && _selectedBreakpointPosition != QPoint()) {
            qDebug() << "Error displaying breakpoint info as a ToolTip";
        }

        return;

    }else{
        // Ignore others.
        return;
    }
}

void SeerEditorWidgetSourceArea::handleHighlighterSettingsChanged () {

    // Set base color for background and text color.
    // Use the palette to do this. Some people say to use the stylesheet.
    // But the palettle method works (for now).
    QTextCharFormat format = highlighterSettings().get("Text");

    QPalette p = palette();
    p.setColor(QPalette::Base, format.background().color());
    p.setColor(QPalette::Text, format.foreground().color());
    setPalette(p);

    // Update the syntax highlighter.
    if (_sourceHighlighter) {

        if (highlighterEnabled()) {
            _sourceHighlighter->setDocument(document());
        }else{
            _sourceHighlighter->setDocument(0);
        }

        _sourceHighlighter->setHighlighterSettings(highlighterSettings());
        _sourceHighlighter->rehighlight();
    }

    // Note. The margins are automatically updated by their own paint events.
    //       The new highlighter settings will be used.
}

void SeerEditorWidgetSourceArea::handleWatchFileModified (const QString& path) {

    Q_UNUSED(path);

    emit showReloadBar(true);
}

void SeerEditorWidgetSourceArea::handleBreakpointToolTip (QPoint pos, const QString& text) {

    // 11^7^done,BreakpointTable={...}
    // 7^done,BreakpointTable={
    //                          nr_rows="1",
    //                          nr_cols="6",
    //                          hdr=[],
    //                          body=[
    //                                 bkpt={
    //                                        number="2",
    //                                        type="breakpoint",
    //                                        disp="keep",
    //                                        enabled="y",
    //                                        addr="0x0000000000400ccd",
    //                                        func="main(int, char**)",
    //                                        file="helloworld.cpp",
    //                                        fullname="/nas/erniep/Development/seer/tests/helloworld/helloworld.cpp",
    //                                        line="30",
    //                                        thread-groups=["i1"],
    //                                        times="1",
    //                                        original-location="-source /nas/erniep/Development/seer/tests/helloworld/helloworld.cpp -line 30"
    //                                      }
    //                               ]
    //                        }

    QString newtext = Seer::filterEscapes(text); // Filter escaped characters.
    QString tooltiptext;

    //
    // Parse 'body' text.
    //
    QString body_text = Seer::parseFirst(newtext, "body=", '[', ']', false);

    if (body_text != "") {

        QStringList bkpt_list = Seer::parse(body_text, "bkpt=", '{', '}', false);

        // Construct the tooltip.
        tooltiptext += "Breakpoint information\n\n";
        for (const auto& bkpt_text : bkpt_list) {

            QStringList item_list = Seer::parseCommaList(bkpt_text);

            for (const auto& item_text : item_list) {
                tooltiptext += item_text + "\n";
            }

            break; // Take the first breakpoint in case, somehow, more are returned.
        }
    }

    QToolTip::showText(mapToGlobal(pos), tooltiptext);
}

//
// LineNumber area.
//

SeerEditorWidgetSourceLineNumberArea::SeerEditorWidgetSourceLineNumberArea(SeerEditorWidgetSourceArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
}

QSize SeerEditorWidgetSourceLineNumberArea::sizeHint () const {
    return QSize(_editorWidget->lineNumberAreaWidth(), 0);
}

void SeerEditorWidgetSourceLineNumberArea::paintEvent (QPaintEvent* event) {
    _editorWidget->lineNumberAreaPaintEvent(event);
}

void SeerEditorWidgetSourceLineNumberArea::mouseDoubleClickEvent (QMouseEvent* event) {

    if (event->button() == Qt::LeftButton) {
        if (QApplication::keyboardModifiers().testFlag(Qt::ControlModifier) == true) {
            _editorWidget->setQuickRunToLine(event);
        }else{
            _editorWidget->setQuickBreakpoint(event);
        }

    }else{
        QWidget::mouseDoubleClickEvent(event);
    }
}

void SeerEditorWidgetSourceLineNumberArea::mouseMoveEvent (QMouseEvent* event) {

    QWidget::mouseMoveEvent(event);
}

void SeerEditorWidgetSourceLineNumberArea::mousePressEvent (QMouseEvent* event) {

    if (event->button() == Qt::RightButton) {
        _editorWidget->showContextMenu(event);

    }else{
        QWidget::mousePressEvent(event);
    }

}

void SeerEditorWidgetSourceLineNumberArea::mouseReleaseEvent (QMouseEvent* event) {

    QWidget::mouseReleaseEvent(event);
}

//
// Breakpoints Area.
//

SeerEditorWidgetSourceBreakPointArea::SeerEditorWidgetSourceBreakPointArea(SeerEditorWidgetSourceArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
}

QSize SeerEditorWidgetSourceBreakPointArea::sizeHint () const {
    return QSize(_editorWidget->breakPointAreaWidth(), 0);
}

void SeerEditorWidgetSourceBreakPointArea::paintEvent (QPaintEvent* event) {
    _editorWidget->breakPointAreaPaintEvent(event);
}

void SeerEditorWidgetSourceBreakPointArea::mouseDoubleClickEvent (QMouseEvent* event) {

    if (event->button() == Qt::LeftButton) {

        if (QApplication::keyboardModifiers().testFlag(Qt::ControlModifier) == true) {
            _editorWidget->setQuickRunToLine(event);
        }else{
            _editorWidget->setQuickBreakpoint(event);
        }

    }else{
        QWidget::mouseDoubleClickEvent(event);
    }
}

void SeerEditorWidgetSourceBreakPointArea::mouseMoveEvent (QMouseEvent* event) {

    QWidget::mouseMoveEvent(event);
}

void SeerEditorWidgetSourceBreakPointArea::mousePressEvent (QMouseEvent* event) {

    if (event->button() == Qt::RightButton) {
        _editorWidget->showContextMenu(event);
    }else if (event->button() == Qt::LeftButton) {
        _editorWidget->showBreakpointToolTip(event);
    }else{
        QWidget::mousePressEvent(event);
    }
}

void SeerEditorWidgetSourceBreakPointArea::mouseReleaseEvent (QMouseEvent* event) {

    QWidget::mouseReleaseEvent(event);
}

