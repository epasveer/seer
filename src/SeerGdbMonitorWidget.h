// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include "ui_SeerGdbMonitorWidget.h"

class SeerGdbMonitorWidget : public QWidget, protected Ui::SeerGdbMonitorWidgetForm {

    Q_OBJECT

    public:
        explicit SeerGdbMonitorWidget (QWidget* parent = 0);
       ~SeerGdbMonitorWidget ();

    signals:
        void                executeGdbMonitorCommand            (int monitorid, QString command);

    public slots:
        void                handleText                          (const QString& text);

    protected slots:
        void                handleCommandLineEdit               ();
        void                handleClearButton                   ();
        void                handleSaveButton                    ();
        void                handlePrintButton                   ();
        void                handleHelpButton                    ();
        void                handleMacroToolButtonClicked        (QAbstractButton* button);

    protected:
        void                writeSettings                       ();
        void                readSettings                        ();
        void                resizeEvent                         (QResizeEvent* event);

    private:
        int                 _monitorId;
};

