#pragma once

#include "SeerLogWidget.h"

class SeerTildeLogWidget : public SeerLogWidget {

    Q_OBJECT

    public:
        explicit SeerTildeLogWidget (QWidget* parent = 0);
       ~SeerTildeLogWidget ();

        void                processText                 (const QString& text);
};

