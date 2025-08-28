// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "ui_SeerAsmWidget.h"

class SeerAsmWidget: public QWidget, protected Ui::SeerAsmWidgetForm {

    Q_OBJECT

    public:

        SeerAsmWidget(QWidget* parent = 0);
       ~SeerAsmWidget();

        QTextDocument*              document                            ();
        QString                     toPlainText                         ();

    signals:

    public slots:
        void                        setData                             (const QString& data);

    protected:

    protected slots:

    private:
        QTextCharFormat             _defaultFormat;
        QTextCharFormat             _grayFormat;
};

