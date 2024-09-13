#pragma once

#include "SeerLogWidget.h"

class SeerSeerLogWidget : public SeerLogWidget {

    Q_OBJECT

    public:
        explicit SeerSeerLogWidget (QWidget* parent = 0);
       ~SeerSeerLogWidget ();

        void                processText                 (const QString& text);
        QString             logFilename                 () const;
        QString             logMessage                  () const;
};

