// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>

#include "ui_SeerMacroEditorDialog.h"

class SeerMacroEditorDialog : public QDialog, protected Ui::SeerMacroEditorDialog {

    Q_OBJECT

    public:
        explicit SeerMacroEditorDialog (QWidget* parent = 0);
       ~SeerMacroEditorDialog ();

        void                    setMacroName            (const QString& name);

        void                    setMacroNickname        (const QString& nickname);
        QString                 macroNickname           () const;

        void                    setCommands             (const QStringList& commands);
        QStringList             commands                () const;

    public slots:

    private:
};

