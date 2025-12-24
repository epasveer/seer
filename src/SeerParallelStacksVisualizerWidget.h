// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include "ui_SeerParallelStacksVisualizerWidget.h"

class SeerParallelStacksVisualizerWidget : public QWidget, protected Ui::SeerParallelStacksVisualizerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerParallelStacksVisualizerWidget (QWidget* parent = 0);
       ~SeerParallelStacksVisualizerWidget ();

    signals:
        void                refreshParallelStackFrames          (int id);

    public slots:
        void                handleText                          (const QString& text);

    protected slots:
        void                handleRefreshButton                 ();
        void                handleHelpButton                    ();
        void                handlePrintButton                   ();
        void                handleSaveButton                    ();

    protected:
        void                writeSettings                       ();
        void                readSettings                        ();
        void                resizeEvent                         (QResizeEvent* event);

    private:
        int                 _id;
};

