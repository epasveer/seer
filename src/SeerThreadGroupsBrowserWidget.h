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
        void                handleText                      (const QString& text);
        void                handleSessionTerminated         ();
        void                refresh                         ();

    protected slots:
        void                handleItemEntered               (QTreeWidgetItem* item, int column);
        void                handleGdbRunToolButton          ();
        void                handleGdbStartToolButton        ();
        void                handleGdbContinueToolButton     ();
        void                handleGdbInterruptToolButton    ();

    signals:
        void                refreshThreadGroups             ();
        void                refreshThreadIds                ();
        void                runThreadGroup                  (QString threadGroup);
        void                startThreadGroup                (QString threadGroup);
        void                continueThreadGroup             (QString threadGroup);
        void                interruptThreadGroup            (QString threadGroup);

    protected:
        void                showEvent                       (QShowEvent* event);

    private:
};

