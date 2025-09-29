// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerHighlighterSettings.h"
#include "SeerSourceHighlighter.h"

#include <QtGui/QSyntaxHighlighter>
#include <QtGui/QTextDocument>
#include <QtGui/QTextCharFormat>
#include <QtCore/QString>
#include <QtCore/QRegularExpression>
#include <QtCore/QVector>

class SeerOdinSourceHighlighter : public SeerSourceHighlighter {

    Q_OBJECT

    public:
        SeerOdinSourceHighlighter (QTextDocument* parent = 0);

        virtual const SeerHighlighterSettings&  highlighterSettings             () override;
        virtual void                            setHighlighterSettings          (const SeerHighlighterSettings& settings) override;

    protected:
        virtual void                            highlightBlock                  (const QString& text) override;

    private:
        struct HighlightingRule {
            QRegularExpression pattern;
            QTextCharFormat    format;
        };

        QVector<HighlightingRule>       _highlightingRules;

        QRegularExpression              _commentStartExpression;
        QRegularExpression              _commentEndExpression;

        SeerHighlighterSettings         _highlighterSettings;
        QTextCharFormat                 _keywordFormat;
        QTextCharFormat                 _classFormat;
        QTextCharFormat                 _singleLineCommentFormat;
        QTextCharFormat                 _multiLineCommentFormat;
        QTextCharFormat                 _quotationFormat;
        QTextCharFormat                 _functionFormat;
};

