/**
 * \file
 *
 * \author Mattia Basaglia
 *
 * \copyright Copyright (C) 2012-2020 Mattia Basaglia
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
#include "QHistoryLineEdit.h"
#include <QtWidgets/QCompleter>
#include <QtWidgets/QAbstractItemView>
#include <QtWidgets/QScrollBar>
#include <QAction>
#include <QtGui/QKeyEvent>
#include <QtGui/QWheelEvent>
#include <QtCore/QDebug>

QHistoryLineEdit::QHistoryLineEdit (const QString& contents, QWidget* parent) : QLineEdit(contents, parent), _currentLine(0), _completer(0), _completionMinchars(1), _completionMax(0) {

    enableReturnPressedOnClear();

    QObject::connect(this,  &QLineEdit::returnPressed,      this, &QHistoryLineEdit::execute);
}

QHistoryLineEdit::QHistoryLineEdit (QWidget* parent) : QLineEdit(parent), _currentLine(0), _completer(0), _completionMinchars(1), _completionMax(0) {

    enableReturnPressedOnClear();

    QObject::connect(this,  &QLineEdit::returnPressed,      this, &QHistoryLineEdit::execute);
}

/**
 * \brief Enable "return press" signal on Clear action.
 */
void QHistoryLineEdit::enableReturnPressedOnClear() {

    for (int i=0; i <children().size(); i++) {

        QAction* myClearAction(qobject_cast<QAction*>(children().at(i)));

        if (myClearAction) {

            //qDebug() << myClearAction->objectName();

            // Look for only the QLineEdit clear action.
            // This name could change or be different.
            if (myClearAction->objectName() == "_q_qlineeditclearaction") {
                connect(myClearAction, &QAction::triggered, this, &QLineEdit::returnPressed, Qt::QueuedConnection);
            }
        }
    }
}

/**
 * \brief Number of available lines
 */
int QHistoryLineEdit::lineCount () const {

    return _lines.size();
}

/**
 * \brief Overwrite the line history
 */
void QHistoryLineEdit::setHistory (const QStringList& history) {

    _lines       = history;
    _currentLine = _lines.size();
}

/**
 * \brief Stored history
 */
QStringList QHistoryLineEdit::history () const {

    return _lines;
}

/**
 * \brief Sets the completer used on a per-word completion
 *
 * Unlike setCompleter(), this suggests completion at every entered word
 *
 * If \c completer is null it will remove the current completer
 */
void QHistoryLineEdit::setWordCompleter (QCompleter* comp) {

    if ( _completer ) {

        QObject::disconnect(_completer, 0, this, 0);

        _completer->setWidget(0);
    }

    _completer = comp;

    if ( comp ) {

        /// \todo should set these only when on focus
        QObject::connect(_completer, QOverload<const QString &>::of(&QCompleter::activated),         this,      &QHistoryLineEdit::autoComplete);
        QObject::connect(_completer, QOverload<const QString &>::of(&QCompleter::highlighted),       this,      &QHistoryLineEdit::autoComplete);

        _completer->setWidget(this);
    }
}

/**
 * \brief Sets a prefix that is ignored by the word completer
 */
void QHistoryLineEdit::setWordCompleterPrefix (const QString& prefix) {
    _completionPrefix = prefix;
}

/**
 * \brief Sets the minimum number of characters required to display the word completer
 */
void QHistoryLineEdit::setWordCompleterMinChars (int minChars) {
    _completionMinchars = minChars;
}

/**
 * \brief Sets the maximum number of suggestions that the completer should show.
 *
 * If more than this many suggestions are found the completer isn't shown
 */
void QHistoryLineEdit::setWordCompleterMaxSuggestions (int max) {
    _completionMax = max;
}

/**
 * \brief Executes the current line
 */
void QHistoryLineEdit::execute () {

    // Ignore blank lines.
    if (text() == "") {
        return;
    }

    // Add the line if it doesn't equal the previous (that is store in the history).
    if ( _lines.empty() || _lines.back() != text() ) {
        _lines << text();
    }

    // Set our current position in the history list.
    _currentLine = _lines.size();

    // If it wants to, the calling widget should clear the text line.
    // clear();

    // Emit the entered text.
    emit lineExecuted(_lines.back());
}

void QHistoryLineEdit::keyPressEvent (QKeyEvent* ev) {

    if ( ev->key() == Qt::Key_Up ) {
        previousLine();
        return;

    } else if ( ev->key() == Qt::Key_Down ) {
        nextLine();
        return;

    } else if (_completer && _completer->popup() && _completer->popup()->isVisible()) {
        switch (ev->key()) {
            case Qt::Key_Enter:
            case Qt::Key_Return:
            case Qt::Key_F4:
            case Qt::Key_Select:
                _completer->popup()->hide();
                return;
        }
    }

    QLineEdit::keyPressEvent(ev);

    if (_completer) {

        QString current = current_word();
        _completer->setCompletionPrefix(current);

        if ( current.size() < _completionMinchars || _completer->completionCount() == 0 || (_completionMax > 0 && _completer->completionCount() > _completionMax) ) {

            _completer->popup()->hide();

        } else {
            // Get the selection status
            int sel       = selectionStart();
            int sellength = selectedText().size();

            // Get the current cursor position
            int c = cursorPosition();

            // Get the start of the current word
            setCursorPosition(_wordStart());

            // Get the cursor rectangle at the beginning of the current word
            QRect rect = cursorRect();

            // Restore cursor position (clears the selection)
            setCursorPosition(c);

            // If we had a selection
            if ( sel != -1 ) {
                // If the selection started at the cursor,
                // it needs to start at the far end and go back
                // (otherwise it moves the cursor at the end)
                if ( sel == c )
                    setSelection(sel+sellength, -sellength);
                else
                    setSelection(sel, sellength);
            }

            // Set the rectangle to the appropriate width
            rect.setWidth( _completer->popup()->sizeHintForColumn(0) + _completer->popup()->verticalScrollBar()->sizeHint().width());

            // Display the _completer under the rectangle
            _completer->complete(rect);
        }
    }
}

void QHistoryLineEdit::wheelEvent (QWheelEvent* ev) {

    if ( ev->angleDelta().y() > 0 ) {
        previousLine();
    }else{
        nextLine();
    }
}

void QHistoryLineEdit::focusOutEvent (QFocusEvent* ev) {

    QLineEdit::focusOutEvent(ev);

    emit lostFocus();
}

void QHistoryLineEdit::previousLine () {

    if ( _lines.empty() ) {
        return;
    }

    if ( !text().isEmpty() && ( _currentLine >= _lines.size() || text() != _lines[_currentLine] ) ) {
        _unfinished = text();
    }

    if ( _currentLine > 0 ) {
        _currentLine--;
    }

    setText(_lines[_currentLine]);
}

void QHistoryLineEdit::nextLine () {

    if ( _lines.empty() ) {
        return;
    }

    _currentLine++;

    if ( _currentLine >= _lines.size() ) {

        setText(_unfinished);
        _unfinished = "";
        _currentLine = _lines.size();

    } else {
        setText(_lines[_currentLine]);
    }
}

/**
 * \brief Current word being edited (used to fire the completer)
 */
QString QHistoryLineEdit::current_word () const {

    int completion_index = _wordStart();

    return text().mid(completion_index, cursorPosition() - completion_index);
}

/**
 * \brief Autocompletes the current word
 */
void QHistoryLineEdit::autoComplete (const QString& completion) {

    int completion_index = _wordStart();

    setText(text().replace( completion_index, cursorPosition() - completion_index, completion));

    setCursorPosition(completion_index+completion.size());
}

/**
 * \brief Returns the index of the character starting the currently edited word
 */
int QHistoryLineEdit::_wordStart () const {

    // lastIndexOf returns the index of the last space or -1 if there are no spaces
    // so that + 1 returns the index of the character starting the word or 0
    int after_space = text().left(cursorPosition()).lastIndexOf(' ') + 1;

    if ( text().right(text().size()-after_space).startsWith(_completionPrefix) ) {
        after_space += _completionPrefix.size();
    }

    return after_space;
}

