#pragma once

#include <QtWidgets/QLabel>
#include <QtCore/QString>

class SeerRunStatusIndicator : public QLabel {

    Q_OBJECT

    public:
        enum RunStatus {
            Idle    = 0,
            Stopped = 1,
            Running = 2
        };

        explicit SeerRunStatusIndicator(QWidget* parent = 0);
       ~SeerRunStatusIndicator ();

        void                                setRunStatus                (SeerRunStatusIndicator::RunStatus status);
        SeerRunStatusIndicator::RunStatus   runStatus                   () const;

    signals:
        void                                statusChanged               (SeerRunStatusIndicator::RunStatus status);

    public slots:
        void                                handleText                  (const QString& text);

    protected:

    private:
        SeerRunStatusIndicator::RunStatus   _runStatus;
};

