// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerTypeBrowserWidget.h"

class SeerTypeBrowserWidget : public QWidget, protected Ui::SeerTypeBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerTypeBrowserWidget (QWidget* parent = 0);
       ~SeerTypeBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleSessionTerminated     ();
        void                refresh                     ();

    protected slots:
        void                handleSearchLineEdit        ();
        void                handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                handleItemEntered           (QTreeWidgetItem* item, int column);

    signals:
        void                refreshTypeList             (int id, const QString& typeRegex);
        void                selectedFile                (QString file, QString fullname, int lineno);

    protected:
    private:
        int                 _id;
};

