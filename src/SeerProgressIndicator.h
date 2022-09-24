#pragma once
#include "QProgressIndicator.h"

class SeerProgressIndicator : public QProgressIndicator {

    Q_OBJECT

    public:
        SeerProgressIndicator(QWidget* parent = 0);
       ~SeerProgressIndicator();

    protected:
        void                    writeSettings                       ();
        void                    readSettings                        ();

    protected slots:
        void                    handleShowContextMenu    (const QPoint& point);

    private:
};

