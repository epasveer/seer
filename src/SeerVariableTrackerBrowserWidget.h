// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtGui/QAction>
#include "ui_SeerVariableTrackerBrowserWidget.h"

class SeerVariableTrackerBrowserWidget : public QWidget, protected Ui::SeerVariableTrackerBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerVariableTrackerBrowserWidget (QWidget* parent = 0);
       ~SeerVariableTrackerBrowserWidget ();

        const QString&      defaultFormat                   () const;

    public slots:
        void                handleText                      (const QString& text);
        void                handleStoppingPointReached      ();
        void                handleSessionTerminated         ();
        void                refresh                         ();
        void                refreshValues                   ();

    private slots:
        void                handleAddLineEdit               ();
        void                handleDeleteToolButton          ();
        void                handleDeleteAllToolButton       ();
        void                handleItemEntered               (QTreeWidgetItem* item, int column);
        void                handleItemExpanded              (QTreeWidgetItem* item);
        void                handleItemCollapsed             (QTreeWidgetItem* item);
        void                handleContextMenu               (const QPoint& pos);
        void                handleRememberToolButton        (bool checked);
        void                handleFormatMenuTriggered       (QAction* action);

    signals:
        void                refreshVariableTrackerNames     ();
        void                refreshVariableTrackerValues    ();
        void                addVariableExpression           (QString expression);
        void                deleteVariableExpressions       (QString expressionids);
        void                addMemoryVisualizer             (QString expression);
        void                addArrayVisualizer              (QString expression);
        void                addMatrixVisualizer             (QString expression);
        void                addStructVisualizer             (QString expression);
        void                raiseTab                        ();

    protected:
        void                handleItemCreate                (QTreeWidgetItem* item, const QString& value_text, const QString& old_text);
        void                handleItemCreate                (QTreeWidgetItem* parentItem, const QString& id_text, const QString& name_text, const QString& value_text, const QString& old_text);
        void                showEvent                       (QShowEvent* event);

    private:
        void                setDefaultFormat                (const QString& format);
        void                readSettings                    ();
        void                writeSettings                   ();

        QString             _defaultFormat;     // "" (natural), "x", "o", "d", "t", or "f".
        QMenu*              _formatMenu;

};

