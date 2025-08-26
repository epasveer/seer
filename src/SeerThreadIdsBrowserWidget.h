// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerThreadIdsBrowserWidget.h"

class SeerThreadIdsBrowserWidget : public QWidget, protected Ui::SeerThreadIdsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerThreadIdsBrowserWidget (QWidget* parent = 0);
       ~SeerThreadIdsBrowserWidget ();

    public slots:
        void                handleText                      (const QString& text);
        void                handleStoppingPointReached      ();
        void                handleSessionTerminated         ();
        void                refresh                         ();

    protected slots:
        void                handleItemClicked               (QTreeWidgetItem* item, int column);
        void                handleGdbNextToolButton         ();
        void                handleGdbStepToolButton         ();
        void                handleGdbFinishToolButton       ();
        void                handleGdbContinueToolButton     ();
        void                handleGdbInterruptToolButton    ();

    signals:
        void                refreshThreadIds                ();
        void                selectedThread                  (int threadid);
        void                nextThreadId                    (int threadid);
        void                stepThreadId                    (int threadid);
        void                finishThreadId                  (int threadid);
        void                continueThreadId                (int threadid);
        void                interruptThreadId               (int threadid);

    protected:
        void                showEvent                       (QShowEvent* event);

    private:
};

