#pragma once

#include "SeerThreadIdsBrowserWidget.h"
#include "SeerThreadFramesBrowserWidget.h"

#include <QtWidgets/QWidget>

#include "ui_SeerThreadManagerWidget.h"

class SeerThreadManagerWidget : public QWidget, protected Ui::SeerThreadManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerThreadManagerWidget (QWidget* parent = 0);
       ~SeerThreadManagerWidget ();

        SeerThreadIdsBrowserWidget*                     threadIdsBrowserWidget              ();
        SeerThreadFramesBrowserWidget*                  threadFramesBrowserWidget           ();

    signals:
    public slots:
    private slots:
        void                                            handleRefreshToolButtonClicked      ();

    private:
        SeerThreadIdsBrowserWidget*                     _threadIdsBrowserWidget;
        SeerThreadFramesBrowserWidget*                  _threadFramesBrowserWidget;
};

