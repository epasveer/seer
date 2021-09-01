#pragma once

#include "SeerLogWidget.h"

class SeerTildeEqualAmpersandLogWidget : public SeerLogWidget {

    Q_OBJECT

    public:
        explicit SeerTildeEqualAmpersandLogWidget (QWidget* parent = 0);
       ~SeerTildeEqualAmpersandLogWidget ();

        void                processText                 (const QString& text);
};

