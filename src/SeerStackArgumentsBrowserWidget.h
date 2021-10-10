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
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                refresh                     ();

    protected slots:
        void                handleItemEntered           (QTreeWidgetItem* item, int column);

    signals:
        void                refreshStackArguments       ();

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

