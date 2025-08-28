// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerStackFramesBrowserWidget.h"

class SeerStackFramesBrowserWidget : public QWidget, protected Ui::SeerStackFramesBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerStackFramesBrowserWidget (QWidget* parent = 0);
       ~SeerStackFramesBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                handleSessionTerminated     ();
        void                refresh                     ();

    protected slots:
        void                handleItemClicked           (QTreeWidgetItem* item, int column);
        void                handleItemEntered           (QTreeWidgetItem* item, int column);

    signals:
        void                refreshStackFrames          ();
        void                selectedFile                (QString file, QString fullname, int lineno);
        void                selectedAddress             (QString address);
        void                selectedFrame               (int frameno);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
        QString             _previousStackFrameText;
};

