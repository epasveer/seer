// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "ui_SeerRegisterValuesBrowserWidget.h"
#include <QtWidgets/QWidget>
#include <QtWidgets/QMenu>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QVector>
#include <QtCore/QDebug>

class SeerRegisterValuesBrowserWidget : public QWidget, protected Ui::SeerRegisterValuesBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerRegisterValuesBrowserWidget (QWidget* parent = 0);
       ~SeerRegisterValuesBrowserWidget ();

    public:
        void                        setRegisterFormat                       (QString fmt);

    public slots:
        void                        handleText                              (const QString& text);
        void                        handleStoppingPointReached              ();
        void                        handleSessionTerminated                 ();
        void                        refresh                                 ();

    protected slots:
        void                        handleItemEntered                       (QTreeWidgetItem* item, int column);
        void                        handleContextMenu                       (const QPoint& pos);
        void                        handleIndexEditingFinished              (const QModelIndex& index);
        void                        handleFormatChanged                     (int index);
        void                        handleColumnSelected                    (int logicalIndex);
        void                        handleNewProfile                        ();
        void                        handleModifyProfile                     ();
        void                        handleDeleteProfile                     ();
        void                        handleShowHideRegisters                 ();
        void                        handleProfileChanged                    (int index);

    signals:
        void                        refreshRegisterNames                    ();
        void                        refreshRegisterValues                   (QString fmt);
        void                        setRegisterValue                        (QString fmt, QString name, QString value);

    protected:
        void                        showEvent                               (QShowEvent* event);
        void                        readSettings                            ();
        void                        writeSettings                           ();
        bool                        readProfileSettings                     (const QString& profileName, QStringList& registerNames, QVector<bool>& registerEnabled);
        void                        writeProfileSettings                    (const QString& profileName, const QStringList& registerNames, const QVector<bool>& registerEnabled);
        void                        deleteProfileSettings                   (const QString& profileName);

    private:
        bool                        _needsRegisterNames;
        QStringList                 _registerNames;
        QVector<bool>               _registerEnabled;
        QAction*                    _newProfileAction;
        QAction*                    _modifyProfileAction;
        QAction*                    _deleteProfileAction;

};

