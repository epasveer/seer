// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>

#include "ui_SeerAssemblyConfigPage.h"

class SeerAssemblyConfigPage : public QWidget, protected Ui::SeerAssemblyConfigPage {

    Q_OBJECT

    public:
        explicit SeerAssemblyConfigPage (QWidget* parent = 0);
       ~SeerAssemblyConfigPage ();

        QString                 showAssemblyTabOnStartupMode                    () const;
        bool                    keepAssemblyTabOnTop                            () const;
        QString                 disassemblyFlavor                               () const;
        QString                 symbolDemagling                                 () const;
        bool                    showAddressColumn                               () const;
        bool                    showOffsetColumn                                () const;
        bool                    showOpcodeColumn                                () const;
        bool                    showSourceLines                                 () const;
        QString                 registerFormat                                  () const;
        QString                 disassemblyMode                                 () const;
        int                     disassemblyBytes                                () const;

        void                    setShowAssemblyTabOnStartupMode                 (const QString& mode);
        void                    setKeepAssemblyTabOnTop                         (bool flag);
        void                    setDisassemblyFlavor                            (const QString& flavor);
        void                    setSymbolDemagling                              (const QString& onoff);
        void                    setShowAddressColumn                            (bool flag);
        void                    setShowOffsetColumn                             (bool flag);
        void                    setShowOpcodeColumn                             (bool flag);
        void                    setShowSourceLines                              (bool flag);
        void                    setRegisterFormat                               (const QString& format);
        void                    setDisassemblyMode                              (const QString& mode, int bytes);

        void                    reset                                           ();

    protected slots:
};

