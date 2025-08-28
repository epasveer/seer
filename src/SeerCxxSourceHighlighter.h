// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtGui/QSyntaxHighlighter>
#include <QtGui/QTextDocument>
#include <QtGui/QTextCharFormat>
#include <QtCore/QString>
#include <QtCore/QRegularExpression>
#include <QtCore/QVector>

class SeerCxxSourceHighlighter : public QSyntaxHighlighter {

    Q_OBJECT

    public:
        SeerCxxSourceHighlighter (QTextDocument* parent = 0);

    protected:
        void            highlightBlock                  (const QString& text) override;
        int             highlight                       (const QString& text, int state);

        static bool     isCppKeyword                    (const QString& word);
};

