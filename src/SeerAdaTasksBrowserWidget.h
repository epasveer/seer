// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerAdaTasksBrowserWidget.h"

class SeerAdaTasksBrowserWidget : public QWidget, protected Ui::SeerAdaTasksBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerAdaTasksBrowserWidget (QWidget* parent = 0);
       ~SeerAdaTasksBrowserWidget ();

    public slots:
        void                handleText                      (const QString& text);
        void                handleStoppingPointReached      ();
        void                handleSessionTerminated         ();
        void                refresh                         ();

    protected slots:
        void                handleItemClicked               (QTreeWidgetItem* item, int column);

    signals:
        void                refreshAdaTasks                 ();
        void                selectedThread                  (int threadid);

    protected:
        void                showEvent                       (QShowEvent* event);

    private:
};

