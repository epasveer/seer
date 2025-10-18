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
#pragma once

#include <QtWidgets/QLineEdit>

/**
 * \brief Line edit providing a history of entered text
 */
class QHistoryLineEdit : public QLineEdit {

    Q_OBJECT

    public:
        QHistoryLineEdit(const QString& contents, QWidget* parent = 0);
        QHistoryLineEdit(QWidget* parent = 0);

        void        enableReturnPressedOnClear          ();

        int         lineCount                           () const;

        void        setHistory                          (const QStringList& history);
        QStringList history() const;

        void        setWordCompleter                    (QCompleter* completer);
        void        setWordCompleterPrefix              (const QString& prefix);
        void        setWordCompleterMinChars            (int minChars);
        void        setWordCompleterMaxSuggestions      (int max);

    public slots:
        void        execute                             ();

    signals:
        void        lineExecuted                        (QString text);
        void        lostFocus                           ();
        void        gainedFocus                         ();

    protected:
        void        keyPressEvent                       (QKeyEvent*   event) Q_DECL_OVERRIDE;
        void        wheelEvent                          (QWheelEvent* event) Q_DECL_OVERRIDE;
        void        focusOutEvent                       (QFocusEvent* event) Q_DECL_OVERRIDE;
        void        focusInEvent                        (QFocusEvent* event) Q_DECL_OVERRIDE;

        void        previousLine                        ();
        void        nextLine                            ();

        QString     current_word                        () const;

    private slots:
        void        autoComplete                        (const QString& completion);

    private:
        int         _wordStart                          () const;

        int         _currentLine;
        QStringList _lines;
        QString     _unfinished;
        QCompleter* _completer;
        QString     _completionPrefix;
        int         _completionMinchars;
        int         _completionMax;
};

