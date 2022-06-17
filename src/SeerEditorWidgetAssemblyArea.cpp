#include "SeerEditorWidget.h"
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

SeerEditorWidgetAssemblyArea::SeerEditorWidgetAssemblyArea(QWidget* parent) : QPlainTextEdit(parent) {

    _enableLineNumberArea = false;
    _enableBreakPointArea = false;
    _enableMiniMapArea    = false;

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
    enableMiniMapArea(false);    // Doesn't work yet. Need to work on the "mini" part.

    QObject::connect(this, &SeerEditorWidgetAssemblyArea::blockCountChanged,        this, &SeerEditorWidgetAssemblyArea::updateMarginAreasWidth);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,            this, &SeerEditorWidgetAssemblyArea::updateLineNumberArea);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,            this, &SeerEditorWidgetAssemblyArea::updateBreakPointArea);
    QObject::connect(this, &SeerEditorWidgetAssemblyArea::updateRequest,            this, &SeerEditorWidgetAssemblyArea::updateMiniMapArea);

    setCurrentLine(0);

    updateMarginAreasWidth(0);

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

    int digits = 1;
    int max    = qMax(1, blockCount());

    while (max >= 10) {
        max /= 10;
        digits++;
    }

    int space = 3 + fontMetrics().horizontalAdvance(QLatin1Char('9')) * digits;

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
            QString number = QString::number(blockNumber + 1);
            painter.drawText(0, top, _lineNumberArea->width(), fontMetrics().height(), Qt::AlignRight, number);
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

void SeerEditorWidgetAssemblyArea::setCurrentLine (int lineno) {
    // XXX
}

void SeerEditorWidgetAssemblyArea::scrollToLine (int lineno) {

    //qDebug() << lineno << file();

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
        emit requestAssembly(addr_text);

        // Set to the line number.
        setCurrentLine(line_text.toInt());

        return;

    }else if (text.startsWith("^done,asm_insns=")) {

        // Clear the existing document.
        document()->clear();

        // Get the list of assembly lines.
        QString asm_insns_text = Seer::parseFirst(text, "asm_insns=", '[', ']', false);

        QStringList asm_list = Seer::parse(asm_insns_text, "", '{', '}', false);

        // Loop through the asm list and print each line.
        for ( const auto& asm_text : asm_list  ) {

            qDebug() << asm_text;

            // Get the strings, with padding.
            QString address_text = Seer::parseFirst(asm_text, "address=", '"', '"', false);
            QString opcodes_text = Seer::parseFirst(asm_text, "opcodes=", '"', '"', false);
            QString inst_text    = Seer::parseFirst(asm_text, "inst=",    '"', '"', false);

            //qDebug() << inst_text;

            // Write assembly line to the document.
            appendPlainText(inst_text);
        }

        // Move to the start of the document.  XXX Move to the line that has our address.
        // setTextCursor(cursor);
    }
}

