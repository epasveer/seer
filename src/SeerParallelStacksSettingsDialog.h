// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>
#include <QtCore/QVector>

#include "ui_SeerParallelStacksSettingsDialog.h"

class SeerParallelStacksSettingsDialog : public QDialog, protected Ui::SeerParallelStacksSettingsDialogForm {

    Q_OBJECT

    public:
        explicit SeerParallelStacksSettingsDialog (QWidget* parent = 0);
       ~SeerParallelStacksSettingsDialog ();

        void                        setShowMinimap              (const QString& when);
        QString                     showMinimap                 () const;

        void                        setShowFullFunctionName     (bool flag);
        bool                        showFullFunctionName        () const;
        void                        setFunctionNameLenght       (int length);
        int                         functionNameLenght          () const;

        void                        setShowFullStackSize        (bool flag);
        bool                        showFullStackSize           () const;
        void                        setStackSize                (int size);
        int                         stackSize                   () const;

    public slots:

    private slots:

    protected:
        void                        writeSettings               ();
        void                        readSettings                ();
        void                        resizeEvent                 (QResizeEvent* event);

    private:
};

