#pragma once

#include <QtWidgets/QWidget>
#include <QtGui/QKeyEvent>
#include "ui_SeerVarVisualizerWidget.h"

class SeerVarVisualizerWidget : public QWidget, protected Ui::SeerVarVisualizerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerVarVisualizerWidget (QWidget* parent = 0);
       ~SeerVarVisualizerWidget ();

        void                setVariableName                     (const QString& name);
        QString             variableName                        () const;

    signals:
        void                varObjCreate                        (int expressionid, QString expression);
        void                varObjListChildren                  (int expressionid, QString objname);
        void                varObjUpdate                        (int expressionid, QString objname);
        void                varObjDelete                        (int expressionid, QString objname);
        void                varObjAssign                        (int expressionid, QString objname, QString value);
        void                varObjAttributes                    (int objid,        QString objname);
        void                addMemoryVisualize                  (QString expression);
        void                addArrayVisualize                   (QString expression);
        void                addVarVisualize                     (QString expression);

    public slots:
        void                handleText                          (const QString& text);

    protected slots:
        void                handleRefreshButton                 ();
        void                handleDebugCheckBox                 ();
        void                handleVariableNameLineEdit          ();
        void                handleIndexEditingStarted           (const QModelIndex& index);
        void                handleIndexEditingFinished          (const QModelIndex& index);
        void                handleContextMenu                   (const QPoint&    pos);
        void                handleItemEntered                   (QTreeWidgetItem* item, int column);
        void                handleItemExpanded                  (QTreeWidgetItem* item);
        void                handleItemCollapsed                 (QTreeWidgetItem* item);
        void                handleExpandAll                     ();
        void                handleCollapseAll                   ();
        void                handleResizeColumns                 ();
        void                handleHideDebugColumns              (bool flag);

    protected:
        QTreeWidgetItem*    findItem                            (const QString& text, Qt::MatchFlags flags, int column);
        void                writeSettings                       ();
        void                readSettings                        ();
        void                resizeEvent                         (QResizeEvent* event);

    private:
        void                expandItem                          (QTreeWidgetItem* item);
        void                collapseItem                        (QTreeWidgetItem* item);
        QString             fullVariableName                    (QTreeWidgetItem* item);
        QString             toolTipText                         (QTreeWidgetItem* item);
        void                debug                               (QString message,  QTreeWidgetItem* item);
        void                deleteItems                         (QList<QTreeWidgetItem*> items);

        int                 _variableId;
        QString             _variableName;
        QString             _previousEditName;
        QString             _previousEditValue;
};

