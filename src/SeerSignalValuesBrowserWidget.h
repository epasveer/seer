// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "ui_SeerSignalValuesBrowserWidget.h"
#include <QtWidgets/QWidget>
#include <QtWidgets/QMenu>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QVector>
#include <QtCore/QDebug>

class SeerSignalValuesBrowserWidget : public QWidget, protected Ui::SeerSignalValuesBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerSignalValuesBrowserWidget (QWidget* parent = 0);
       ~SeerSignalValuesBrowserWidget ();

    public:

    public slots:
        void                        handleText                              (const QString& text);
        void                        handleStoppingPointReached              ();
        void                        handleSessionTerminated                 ();
        void                        refresh                                 ();

    protected slots:
        void                        handleItemDoubleClicked                 (QTreeWidgetItem* item, int column);
        void                        handleItemEntered                       (QTreeWidgetItem* item, int column);
        void                        handleContextMenu                       (const QPoint& pos);
        void                        handleNewProfile                        ();
        void                        handleModifyProfile                     ();
        void                        handleDeleteProfile                     ();
        void                        handleShowHideSignals                   ();
        void                        handleProfileChanged                    (int index);

    signals:
        void                        refreshSignalNames                      ();
        void                        refreshSignalValues                     (QString names);
        void                        setSignalValue                          (QString name, QString stop, QString print, QString pass);

    protected:
        void                        showEvent                               (QShowEvent* event);
        void                        readSettings                            ();
        void                        writeSettings                           ();
        bool                        readProfileSettings                     (const QString& profileName, QStringList& signalNames, QVector<bool>& signalEnabled);
        void                        writeProfileSettings                    (const QString& profileName, const QStringList& signalNames, const QVector<bool>& signalEnabled);
        void                        deleteProfileSettings                   (const QString& profileName);

    private:
        void                        _editItem                               (QTreeWidgetItem* item);
        bool                        _needsSignalNames;
        QStringList                 _signalNames;
        QVector<bool>               _signalEnabled;
        QAction*                    _newProfileAction;
        QAction*                    _modifyProfileAction;
        QAction*                    _deleteProfileAction;
};

