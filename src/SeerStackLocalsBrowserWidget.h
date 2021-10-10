#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerStackLocalsBrowserWidget.h"

class SeerStackLocalsBrowserWidget : public QWidget, protected Ui::SeerStackLocalsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerStackLocalsBrowserWidget (QWidget* parent = 0);
       ~SeerStackLocalsBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                refresh                     ();

    protected slots:
        void                handleItemExpanded          (QTreeWidgetItem* item);
        void                handleItemCollapsed         (QTreeWidgetItem* item);
        void                handleItemEntered           (QTreeWidgetItem* item, int column);

    signals:
        void                refreshStackLocals          ();

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
        int                 _frameNumber;
};

