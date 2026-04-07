// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerLibraryBrowserWidget.h"

class SeerLibraryBrowserWidget : public QWidget, protected Ui::SeerLibraryBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerLibraryBrowserWidget (QWidget* parent = 0);
       ~SeerLibraryBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleSessionTerminated     ();
        void                refresh                     ();

    protected slots:
        void                handleSearchLineEdit        (const QString& text);

    signals:
        void                refreshLibraryList          ();

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

