// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "ui_SeerSignalValuesBrowserWidget.h"
#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QDebug>

class SeerSignalValuesBrowserWidget : public QWidget, protected Ui::SeerSignalValuesBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerSignalValuesBrowserWidget (QWidget* parent = 0);
       ~SeerSignalValuesBrowserWidget ();

    public:

    public slots:
        void                        handleText                              (const QString& text);
        void                        handleStoppingPointReached              ();
        void                        handleSessionTerminated                 ();
        void                        refresh                                 ();

    protected slots:

    signals:
        void                        refreshSignalValues                     ();

    protected:
        void                        showEvent                               (QShowEvent* event);

    private:

};

