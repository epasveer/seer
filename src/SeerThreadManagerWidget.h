// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerThreadFramesBrowserWidget.h"
#include "SeerThreadIdsBrowserWidget.h"
#include "SeerThreadGroupsBrowserWidget.h"
#include "SeerAdaTasksBrowserWidget.h"

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
        SeerAdaTasksBrowserWidget*                      adaTasksBrowserWidget               ();

    signals:
        void                                            schedulerLockingModeChanged         (const QString& mode);
        void                                            scheduleMultipleModeChanged         (const QString& mode);
        void                                            forkFollowsModeChanged              (const QString& mode);

    protected:
        void                                            writeSettings                       ();
        void                                            readSettings                        ();

    public slots:
        void                                            setSchedulerLockingMode             (const QString& mode);
        QString                                         schedulerLockingMode                () const;

        void                                            setScheduleMultipleMode             (const QString& mode);
        QString                                         scheduleMultipleMode                () const;

        void                                            setForkFollowsMode                  (const QString& mode);
        QString                                         forkFollowsMode                     () const;

    private slots:
        void                                            handleRefreshToolButtonClicked      ();
        void                                            handleHelpToolButtonClicked         ();
        void                                            handleSchedulerLockingComboBox      (int index);
        void                                            handleScheduleMultipleComboBox      (int index);
        void                                            handleForkFollowComboBox            (int index);
        void                                            handleTabMoved                      (int from, int to);
        void                                            handleTabChanged                    (int index);

    private:
        SeerThreadFramesBrowserWidget*                  _threadFramesBrowserWidget;
        SeerThreadIdsBrowserWidget*                     _threadIdsBrowserWidget;
        SeerThreadGroupsBrowserWidget*                  _threadGroupsBrowserWidget;
        SeerAdaTasksBrowserWidget*                      _adaTasksBrowserWidget;
};

