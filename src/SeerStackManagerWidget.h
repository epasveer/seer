// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerStackFramesBrowserWidget.h"
#include "SeerStackArgumentsBrowserWidget.h"
#include "SeerStackLocalsBrowserWidget.h"
#include "SeerStackDumpBrowserWidget.h"

#include <QtWidgets/QWidget>
#include <QtWidgets/QTabWidget>

#include "ui_SeerStackManagerWidget.h"

class SeerStackManagerWidget : public QWidget, protected Ui::SeerStackManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerStackManagerWidget (QWidget* parent = 0);
       ~SeerStackManagerWidget ();

        SeerStackFramesBrowserWidget*                   stackFramesBrowserWidget            ();
        SeerStackArgumentsBrowserWidget*                stackArgumentsBrowserWidget         ();
        SeerStackLocalsBrowserWidget*                   stackLocalsBrowserWidget            ();
        SeerStackDumpBrowserWidget*                     stackDumpBrowserWidget              ();

    signals:
        void                                            refreshThreadFrames                 ();

    protected:
        void                                            writeSettings                       ();
        void                                            readSettings                        ();

    public slots:
        void                                            handleText                          (const QString& text);
        void                                            handleStoppingPointReached          ();
        void                                            handleSessionTerminated             ();
        void                                            refresh                             ();

    private slots:
        void                                            handleRefreshToolButtonClicked      ();
        void                                            handleHelpToolButtonClicked         ();
        void                                            handleTabMoved                      (int from, int to);
        void                                            handleTabChanged                    (int index);
        void                                            handleTabsContextMenuButtonClicked  ();

    private:
        SeerStackFramesBrowserWidget*                   _stackFramesBrowserWidget;
        SeerStackArgumentsBrowserWidget*                _stackArgumentsBrowserWidget;
        SeerStackLocalsBrowserWidget*                   _stackLocalsBrowserWidget;
        SeerStackDumpBrowserWidget*                     _stackDumpBrowserWidget;
};

class SeerStackManagerEventFilter : public QObject {

    Q_OBJECT

    public:
        explicit SeerStackManagerEventFilter(QTabWidget* tabWidget, QObject *parent = nullptr) : QObject(parent), _tabWidget(tabWidget) {}
       ~SeerStackManagerEventFilter() = default;

    protected:
        bool                                            eventFilter                             (QObject *watched, QEvent *event) override;

    private:
        QTabWidget*                                     _tabWidget;
};

