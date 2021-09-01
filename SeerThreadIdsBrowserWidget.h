#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerThreadIdsBrowserWidget.h"

class SeerThreadIdsBrowserWidget : public QWidget, protected Ui::SeerThreadIdsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerThreadIdsBrowserWidget (QWidget* parent = 0);
       ~SeerThreadIdsBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                handleStoppingPointReached  ();
        void                refresh                     ();

    protected slots:

    signals:
        void                refreshThreadIds            ();
        void                selectedThread              (int threadid);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

