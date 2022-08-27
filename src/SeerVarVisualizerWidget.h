#pragma once

#include <QtWidgets/QWidget>
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
        void                addMemoryVisualize                  (QString expression);
        void                addArrayVisualize                   (QString expression);
        void                addStructVisualize                  (QString expression);

    public slots:
        void                handleText                          (const QString& text);

    protected slots:
        void                handleExpandAllButton               ();
        void                handleCollapseAllButton             ();
        void                handleRefreshButton                 ();
        void                handleDetailedCheckBox              ();
        void                handleVariableNameLineEdit          ();
        void                handleContextMenu                   (const QPoint&    pos);
        void                handleItemEntered                   (QTreeWidgetItem* item, int column);
        void                handleItemExpanded                  (QTreeWidgetItem* item);
        void                handleResizeColumns                 ();
        void                handleHideDetailedColumns           (bool flag);

    protected:
        void                writeSettings                       ();
        void                readSettings                        ();
        void                resizeEvent                         (QResizeEvent* event);

    private:
        void                expandItem                          (QTreeWidgetItem* item);
        void                collapseItem                        (QTreeWidgetItem* item);

        int                 _variableId;
        QString             _variableName;
};

