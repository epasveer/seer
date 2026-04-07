// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include "ui_SeerRRConfigPage.h"

class SeerRRConfigPage : public QWidget, protected Ui::SeerRRConfigPage {

    Q_OBJECT

    public:
        explicit SeerRRConfigPage (QWidget* parent = 0);
       ~SeerRRConfigPage ();

        QString                 rrProgram                                       () const;
        QString                 rrArguments                                     () const;
        QString                 gdbArguments                                    () const;

        void                    setRRProgram                                    (const QString& program);
        void                    setRRArguments                                  (const QString& arguments);
        void                    setGdbArguments                                 (const QString& arguments);

        void                    reset                                           ();

    protected slots:
        void                    handleRRProgramToolButton                       ();
};

