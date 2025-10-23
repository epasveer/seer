#pragma once

#include <QWidget>
#include <QtCore/QString>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QLabel>

class SeerRunStatusIndicatorBox : public QGroupBox {

    Q_OBJECT

    public:
        enum RunStatus {
            Idle    = 0,
            Stopped = 1,
            Stop_By_Breakpoint = 2,
            Running = 3,
            Disconnect = 4               // specifically for openocd
        };

        explicit SeerRunStatusIndicatorBox(QWidget* parent = 0);
       ~SeerRunStatusIndicatorBox ();

        void                                    setRunStatus                (SeerRunStatusIndicatorBox::RunStatus status);
        SeerRunStatusIndicatorBox::RunStatus    runStatus                   () const;
        void                                    setCore                     (int coreIdx);
        QGroupBox*                              indicatorBox                ();

    signals:
        void                                    statusChanged               (SeerRunStatusIndicatorBox::RunStatus status);

    public slots:
        void                                    handleText                  (const QString& text);
        void                                    handleTerminate             ();

    protected:

    private:
        SeerRunStatusIndicatorBox::RunStatus   _runStatus;
        QGroupBox           *groupBox;
        QLabel              *coreLabel;
        QLabel              *statusLabel;
        QHBoxLayout         *layout;
};

