// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerSourceBrowserWidget.h"
#include "SeerFunctionBrowserWidget.h"
#include "SeerTypeBrowserWidget.h"
#include "SeerStaticBrowserWidget.h"
#include "SeerLibraryBrowserWidget.h"
#include "SeerAdaExceptionsBrowserWidget.h"
#include "SeerSkipBrowserWidget.h"

#include <QtWidgets/QWidget>
#include <QtWidgets/QTabWidget>

#include "ui_SeerSourceSymbolLibraryManagerWidget.h"

class SeerSourceSymbolLibraryManagerWidget : public QWidget, protected Ui::SeerSourceSymbolLibraryManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerSourceSymbolLibraryManagerWidget (QWidget* parent = 0);
       ~SeerSourceSymbolLibraryManagerWidget ();

        SeerSourceBrowserWidget*                        sourceBrowserWidget                     ();
        SeerFunctionBrowserWidget*                      functionBrowserWidget                   ();
        SeerTypeBrowserWidget*                          typeBrowserWidget                       ();
        SeerStaticBrowserWidget*                        staticBrowserWidget                     ();
        SeerLibraryBrowserWidget*                       libraryBrowserWidget                    ();
        SeerAdaExceptionsBrowserWidget*                 adaExceptionsBrowserWidget              ();
        SeerSkipBrowserWidget*                          skipBrowserWidget                       ();

    protected:
        void                                            writeSettings                           ();
        void                                            readSettings                            ();

    private slots:
        void                                            handleRefreshToolButtonClicked          ();
        void                                            handleHelpToolButtonClicked             ();
        void                                            handleTabMoved                          (int from, int to);
        void                                            handleTabChanged                        (int index);
        void                                            handleTabsContextMenuButtonClicked      ();

    private:
        SeerSourceBrowserWidget*                        _sourceBrowserWidget;
        SeerFunctionBrowserWidget*                      _functionBrowserWidget;
        SeerTypeBrowserWidget*                          _typeBrowserWidget;
        SeerStaticBrowserWidget*                        _staticBrowserWidget;
        SeerLibraryBrowserWidget*                       _libraryBrowserWidget;
        SeerAdaExceptionsBrowserWidget*                 _adaExceptionsBrowserWidget;
        SeerSkipBrowserWidget*                          _skipBrowserWidget;
        QTabWidget*                                     _dumpTab;
};

class SeerSourceSymbolLibraryEventFilter : public QObject {

    Q_OBJECT

    public:
        explicit SeerSourceSymbolLibraryEventFilter(QTabWidget* tabWidget, QObject *parent = nullptr) : QObject(parent), _tabWidget(tabWidget) {}
       ~SeerSourceSymbolLibraryEventFilter() = default;

    protected:
        bool                                            eventFilter                             (QObject *watched, QEvent *event) override;

    private:
        QTabWidget*                                     _tabWidget;
};