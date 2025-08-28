// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerSkipBrowserWidget.h"

class SeerSkipBrowserWidget : public QWidget, protected Ui::SeerSkipBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerSkipBrowserWidget (QWidget* parent = 0);
       ~SeerSkipBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleSessionTerminated     ();
        void                refresh                     ();

    protected slots:
        void                handleAddToolButton         ();
        void                handleDeleteToolButton      ();
        void                handleEnableToolButton      ();
        void                handleDisableToolButton     ();
        void                handleSaveToolButton        ();
        void                handleLoadToolButton        ();

    signals:
        void                refreshSkipList             ();
        void                addSkip                     (const QString& skipMode, const QString& skipParameters);
        void                deleteSkips                 (const QString& skips);
        void                enableSkips                 (const QString& skips);
        void                disableSkips                (const QString& skips);

    protected:
    private:
};

