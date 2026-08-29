// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerParallelStacksCommon.h"
#include <QtWidgets/QDialog>
#include <QtCore/QString>
#include <QtCore/QVector>

#include "ui_SeerParallelStacksSettingsDialog.h"

class SeerParallelStacksSettingsDialog : public QDialog, protected Ui::SeerParallelStacksSettingsDialogForm {

    Q_OBJECT

    public:
        explicit SeerParallelStacksSettingsDialog (QWidget* parent = 0);
       ~SeerParallelStacksSettingsDialog ();

        void                        setSettings                         (const SeerParallelStacksSettings& settings);
        SeerParallelStacksSettings  settings                            () const;

        void                        setShowMinimap                      (const QString& when);
        QString                     showMinimap                         () const;

        void                        setShowFullFunctionName             (bool flag);
        bool                        showFullFunctionName                () const;
        void                        setFunctionNameLength               (int length);
        int                         functionNameLength                  () const;

        void                        setShowFullStackSize                (bool flag);
        bool                        showFullStackSize                   () const;
        void                        setStackSize                        (int size);
        int                         stackSize                           () const;

    public slots:

    private slots:
        void                        handleFunctionNameLengthClicked     ();
        void                        handleStackFrameSizeClicked         ();

    protected:
        void                        writeSettings                       ();
        void                        readSettings                        ();
        void                        resizeEvent                         (QResizeEvent* event);

    private:
};

