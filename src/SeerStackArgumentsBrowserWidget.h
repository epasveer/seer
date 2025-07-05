#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerStackArgumentsBrowserWidget.h"

class SeerStackArgumentsBrowserWidget : public QWidget, protected Ui::SeerStackArgumentsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerStackArgumentsBrowserWidget (QWidget* parent = 0);
       ~SeerStackArgumentsBrowserWidget ();

    public slots:
        void                handleText                          (const QString& text);
        void                handleStoppingPointReached          ();
        void                handleSessionTerminated             ();
        void                refresh                             ();

    protected slots:
        void                handleContextMenu                   (const QPoint& pos);
        void                handleItemExpanded                  (QTreeWidgetItem* item);
        void                handleItemCollapsed                 (QTreeWidgetItem* item);
        void                handleItemEntered                   (QTreeWidgetItem* item, int column);

    signals:
        void                refreshStackArguments               ();
        void                addVariableLoggerExpression         (QString expression);
        void                addVariableTrackerExpression        (QString expression);
        void                refreshVariableTrackerValues        ();
        void                addMemoryVisualizer                 (QString expression);
        void                addArrayVisualizer                  (QString expression);
        void                addMatrixVisualizer                 (QString expression);
        void                addStructVisualizer                 (QString expression);

    protected:
        void                handleItemCreate                    (QTreeWidgetItem* parentItem, const QString& level_text, const QString& name_text, const QString& value_text);
        void                showEvent                           (QShowEvent* event);

    private:
};

