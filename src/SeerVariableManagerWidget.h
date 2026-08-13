// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerVariableTrackerBrowserWidget.h"
#include "SeerVariableLoggerBrowserWidget.h"
#include "SeerRegisterValuesBrowserWidget.h"
#include "SeerSignalValuesBrowserWidget.h"

#include <QtWidgets/QWidget>
#include <QtWidgets/QTabWidget>

#include "ui_SeerVariableManagerWidget.h"

class SeerVariableManagerWidget : public QWidget, protected Ui::SeerVariableManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerVariableManagerWidget (QWidget* parent = 0);
       ~SeerVariableManagerWidget ();

        SeerVariableTrackerBrowserWidget*               variableTrackerBrowserWidget        ();
        SeerVariableLoggerBrowserWidget*                variableLoggerBrowserWidget         ();
        SeerRegisterValuesBrowserWidget*                registerValuesBrowserWidget         ();
        SeerSignalValuesBrowserWidget*                  signalValuesBrowserWidget           ();

    protected:
        void                                            writeSettings                       ();
        void                                            readSettings                        ();

    private slots:
        void                                            handleRefreshToolButtonClicked      ();
        void                                            handleHelpToolButtonClicked         ();
        void                                            handleTabMoved                      (int from, int to);
        void                                            handleTabChanged                    (int index);
        void                                            handleRaiseLoggerTab                ();
        void                                            handleRaiseTrackerTab               ();
        void                                            handleTabsContextMenuButtonClicked  ();

    private:
        SeerVariableTrackerBrowserWidget*               _variableTrackerBrowserWidget;
        SeerVariableLoggerBrowserWidget*                _variableLoggerBrowserWidget;
        SeerRegisterValuesBrowserWidget*                _registerValuesBrowserWidget;
        SeerSignalValuesBrowserWidget*                  _signalValuesBrowserWidget;
};

class SeerVariableManagerEventFilter : public QObject {

    Q_OBJECT

    public:
        explicit SeerVariableManagerEventFilter(QTabWidget* tabWidget, QObject *parent = nullptr) : QObject(parent), _tabWidget(tabWidget) {}
       ~SeerVariableManagerEventFilter() = default;

    protected:
        bool                                            eventFilter                             (QObject *watched, QEvent *event) override;

    private:
        QTabWidget*                                     _tabWidget;
};

