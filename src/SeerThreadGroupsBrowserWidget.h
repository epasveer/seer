#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerThreadGroupsBrowserWidget.h"

class SeerThreadGroupsBrowserWidget : public QWidget, protected Ui::SeerThreadGroupsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerThreadGroupsBrowserWidget (QWidget* parent = 0);
       ~SeerThreadGroupsBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                refresh                     ();

    protected slots:
        void                handleItemEntered           (QTreeWidgetItem* item, int column);

    signals:
        void                refreshThreadGroups         ();
        void                refreshThreadIds            ();

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

