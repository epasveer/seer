#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerRegisterValuesBrowserWidget.h"

class SeerRegisterValuesBrowserWidget : public QWidget, protected Ui::SeerRegisterValuesBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerRegisterValuesBrowserWidget (QWidget* parent = 0);
       ~SeerRegisterValuesBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                refresh                     ();

    protected slots:
        void                handleItemEntered           (QTreeWidgetItem* item, int column);

    signals:
        void                refreshRegisterNames        ();
        void                refreshRegisterValues       ();

    protected:
        void                showEvent                   (QShowEvent* event);

    private:

};

