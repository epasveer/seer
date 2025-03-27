#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerSkipBrowserWidget.h"

class SeerSkipBrowserWidget : public QWidget, protected Ui::SeerSkipBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerSkipBrowserWidget (QWidget* parent = 0);
       ~SeerSkipBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                refresh                     ();

    protected slots:

    signals:

    protected:
    private:
        int                 _id;
};

