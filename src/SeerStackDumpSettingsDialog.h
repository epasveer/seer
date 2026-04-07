// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QDialog>
#include <QtGui/QColor>
#include <QtCore/QString>

#include "ui_SeerStackDumpSettingsDialog.h"

class SeerStackDumpSettingsDialog : public QDialog, protected Ui::SeerStackDumpSettingsDialogForm {

    Q_OBJECT

    public:
        explicit SeerStackDumpSettingsDialog (QWidget* parent = 0);
       ~SeerStackDumpSettingsDialog ();

        void                        setStackPointerExpression   (const QString& expression);
        QString                     stackPointerExpression      () const;

        void                        setStackPointerColor        (const QColor& color);
        QColor                      stackPointerColor           () const;

        void                        setBytesBeforeSP            (int nbytes);
        int                         bytesBeforeSP               () const;

        void                        setBytesAfterSP             (int nbytes);
        int                         bytesAfterSP                () const;

        void                        setAsciiBytes               (int nbytes);
        int                         asciiBytes                  () const;

    public slots:

    private slots:

    protected:

    private:
};

