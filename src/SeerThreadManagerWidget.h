#pragma once

#include "SeerThreadIdsBrowserWidget.h"
#include "SeerThreadFramesBrowserWidget.h"

#include <QtWidgets/QWidget>

#include "ui_SeerThreadManagerWidget.h"

class SeerThreadManagerWidget : public QWidget, protected Ui::SeerThreadManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerThreadManagerWidget (QWidget* parent = 0);
       ~SeerThreadManagerWidget ();

        SeerThreadIdsBrowserWidget*                     threadIdsBrowserWidget              ();
        SeerThreadFramesBrowserWidget*                  threadFramesBrowserWidget           ();

    signals:
        void                                            schedulerLockingModeChanged         (const QString& mode);
        void                                            scheduleMultipleModeChanged         (const QString& mode);
        void                                            forkFollowsModeChanged              (const QString& mode);


    public slots:
        void                                            setSchedulerLockingMode             (const QString& mode);
        QString                                         schedulerLockingMode                () const;

        void                                            setScheduleMultipleMode             (const QString& mode);
        QString                                         scheduleMultipleMode                () const;

        void                                            setForkFollowsMode                  (const QString& mode);
        QString                                         forkFollowsMode                     () const;

    private slots:
        void                                            handleRefreshToolButtonClicked      ();
        void                                            handleSchedulerLockingComboBox      (int index);
        void                                            handleScheduleMultipleComboBox      (int index);
        void                                            handleForkFollowComboBox            (int index);

    private:
        SeerThreadIdsBrowserWidget*                     _threadIdsBrowserWidget;
        SeerThreadFramesBrowserWidget*                  _threadFramesBrowserWidget;
};

