#pragma once

#include <QtWidgets/QWidget>
#include "ui_SeerStructVisualizerWidget.h"

class SeerStructVisualizerWidget : public QWidget, protected Ui::SeerStructVisualizerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerStructVisualizerWidget (QWidget* parent = 0);
       ~SeerStructVisualizerWidget ();

        void                setVariableName                     (const QString& name);
        QString             variableName                        () const;

    signals:
        void                evaluateVariableExpression          (int expressionid, QString expression);
        void                addMemoryVisualize                  (QString expression);
        void                addArrayVisualize                   (QString expression);
        void                addStructVisualize                  (QString expression);

    public slots:
        void                handleText                          (const QString& text);

    protected slots:
        void                handleRefreshButton                 ();
        void                handleVariableNameLineEdit          ();
        void                handleContextMenu                   (const QPoint&    pos);
        void                handleItemEntered                   (QTreeWidgetItem* item, int column);
        void                handleItemExpanded                  (QTreeWidgetItem* item);

    protected:
        void                handleItemCreate                    (QTreeWidgetItem* parentItem, const QString& value_text);
        void                writeSettings                       ();
        void                readSettings                        ();
        void                resizeEvent                         (QResizeEvent* event);

    private:
        int                 _variableId;
};

