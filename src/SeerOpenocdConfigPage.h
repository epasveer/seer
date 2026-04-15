// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>

#include "ui_SeerOpenocdConfigPage.h"

class SeerOpenocdConfigPage : public QWidget, protected Ui::SeerOpenocdConfigPage {

    Q_OBJECT

    public:
        explicit SeerOpenocdConfigPage (QWidget* parent = 0);
       ~SeerOpenocdConfigPage ();

        QString                 openocdGdbExe                                   () const;
        QString                 openocdExe                                      () const;
        QString                 openocdGdbPort                                  () const;
        QString                 openocdTelnetPort                               () const;

        void                    reset                                           ();

        void                    setOpenocdGdbExe                                (const QString& program);
        void                    setOpenocdExe                                   (const QString& program);
        void                    setOpenocdGdbPort                               (const QString& port   );
        void                    setOpenocdTelnetPort                            (const QString& port   );

    private:
        QString                 _gdbExe;
        QString                 _openocdExe;
        QString                 _gdbPort;
        QString                 _telnetPort;
};

