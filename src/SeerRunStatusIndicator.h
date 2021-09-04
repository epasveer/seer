#pragma once

#include <QtWidgets/QLabel>
#include <QtCore/QString>

class SeerRunStatusIndicator : public QLabel {

    enum RunStatus {
        Idle    = 0,
        Stopped = 1,
        Running = 2
    };

    Q_OBJECT

    public:
        explicit SeerRunStatusIndicator(QWidget* parent = 0);
       ~SeerRunStatusIndicator ();

        void                                setRunStatus                (SeerRunStatusIndicator::RunStatus status);
        SeerRunStatusIndicator::RunStatus   runStatus                   () const;

    public slots:
        void                                handleText                  (const QString& text);

    protected:

    private:
        SeerRunStatusIndicator::RunStatus   _runStatus;
};

