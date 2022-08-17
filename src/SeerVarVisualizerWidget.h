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

