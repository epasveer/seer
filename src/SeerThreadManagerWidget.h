#pragma once

#include "SeerThreadFramesBrowserWidget.h"
#include "SeerThreadIdsBrowserWidget.h"
#include "SeerThreadGroupsBrowserWidget.h"

#include <QtWidgets/QWidget>

#include "ui_SeerThreadManagerWidget.h"

class SeerThreadManagerWidget : public QWidget, protected Ui::SeerThreadManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerThreadManagerWidget (QWidget* parent = 0);
       ~SeerThreadManagerWidget ();

        SeerThreadFramesBrowserWidget*                  threadFramesBrowserWidget           ();
        SeerThreadIdsBrowserWidget*                     threadIdsBrowserWidget              ();
        SeerThreadGroupsBrowserWidget*                  threadGroupsBrowserWidget           ();

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
        SeerThreadFramesBrowserWidget*                  _threadFramesBrowserWidget;
        SeerThreadIdsBrowserWidget*                     _threadIdsBrowserWidget;
        SeerThreadGroupsBrowserWidget*                  _threadGroupsBrowserWidget;
};

