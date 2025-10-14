// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include "ui_SeerCheckpointsBrowserWidget.h"

class SeerCheckpointsBrowserWidget : public QWidget, protected Ui::SeerCheckpointsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerCheckpointsBrowserWidget (QWidget* parent = 0);
       ~SeerCheckpointsBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                handleSessionTerminated     ();

    private slots:
        void                handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                handleRefreshToolButton     ();
        void                handleAddToolButton         ();
        void                handleDeleteToolButton      ();
        void                handleSelectToolButton      ();

    signals:
        void                refreshCheckpointsList      ();
        void                insertCheckpoint            ();
        void                selectCheckpoint            (QString checkpoint);
        void                deleteCheckpoints           (QString checkpoints);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

