#pragma once

#include "SeerLogWidget.h"

class SeerCaretAsteriskLogWidget : public SeerLogWidget {

    Q_OBJECT

    public:
        explicit SeerCaretAsteriskLogWidget (QWidget* parent = 0);
       ~SeerCaretAsteriskLogWidget ();

        void                processText                 (const QString& text);
};

