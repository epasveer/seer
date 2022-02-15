#pragma once

#include "SeerLogWidget.h"

class SeerGdbLogWidget : public SeerLogWidget {

    Q_OBJECT

    public:
        explicit SeerGdbLogWidget (QWidget* parent = 0);
       ~SeerGdbLogWidget ();

        void                processText                 (const QString& text);
};

