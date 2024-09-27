#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerStackDumpBrowserWidget.h"

class SeerStackDumpBrowserWidget : public QWidget, protected Ui::SeerStackDumpBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerStackDumpBrowserWidget (QWidget* parent = 0);
       ~SeerStackDumpBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                refresh                     ();

    protected slots:

    signals:
        void                refreshStackPointer         (int expressionid, QString expression);
        void                refreshStackDump            ();

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
        int                 _spExpressionId;
};

