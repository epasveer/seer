// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "ui_SeerHelpPageDialog.h"

class SeerHelpPageDialog: public QDialog, protected Ui::SeerHelpPageDialogForm {

    Q_OBJECT

    public:

        SeerHelpPageDialog(QDialog* parent = 0);
       ~SeerHelpPageDialog();

        void                        loadFile                            (const QString& filename);
        void                        loadText                            (const QString& text);

    signals:
    public slots:
    protected:
        void                        writeSettings                       ();
        void                        readSettings                        ();
        void                        resizeEvent                         (QResizeEvent* event);

    protected slots:
        void                        handlePrintToolButton               ();
        void                        handleOkPushButton                  ();

    private:
};

