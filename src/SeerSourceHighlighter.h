#pragma once

#include <QtGui/QSyntaxHighlighter>
#include "SeerHighlighterSettings.h"

class SeerSourceHighlighter : public QSyntaxHighlighter {

    Q_OBJECT

    public:
        SeerSourceHighlighter (QTextDocument* parent = 0);

        virtual const SeerHighlighterSettings&  highlighterSettings             () = 0;
        virtual void                            setHighlighterSettings          (const SeerHighlighterSettings& settings) = 0;

    protected:
        virtual void                            highlightBlock                  (const QString& text) override = 0;
};