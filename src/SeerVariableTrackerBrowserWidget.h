#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerVariableTrackerBrowserWidget.h"

class SeerVariableTrackerBrowserWidget : public QWidget, protected Ui::SeerVariableTrackerBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerVariableTrackerBrowserWidget (QWidget* parent = 0);
       ~SeerVariableTrackerBrowserWidget ();

    public slots:
        void                handleText                      (const QString& text);
        void                handleStoppingPointReached      ();
        void                refresh                         ();
        void                refreshValues                   ();

    private slots:
        void                handleAddLineEdit               ();
        void                handleDeleteToolButton          ();
        void                handleDeleteAllToolButton       ();
        void                handleItemEntered               (QTreeWidgetItem* item, int column);

    signals:
        void                refreshVariableTrackerNames     ();
        void                refreshVariableTrackerValues    ();
        void                addVariableExpression           (QString expression);
        void                deleteVariableExpressions       (QString expressionids);

    protected:
        void                showEvent                       (QShowEvent* event);

    private:

};

