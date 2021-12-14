#pragma once

#include "SeerStackFramesBrowserWidget.h"
#include "SeerStackArgumentsBrowserWidget.h"
#include "SeerStackLocalsBrowserWidget.h"

#include <QtWidgets/QWidget>

#include "ui_SeerStackManagerWidget.h"

class SeerStackManagerWidget : public QWidget, protected Ui::SeerStackManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerStackManagerWidget (QWidget* parent = 0);
       ~SeerStackManagerWidget ();

        SeerStackFramesBrowserWidget*                   stackFramesBrowserWidget            ();
        SeerStackArgumentsBrowserWidget*                stackArgumentsBrowserWidget         ();
        SeerStackLocalsBrowserWidget*                   stackLocalsBrowserWidget            ();

    signals:
        void                                            refreshThreadFrames                 ();

    public slots:
        void                                            handleText                          (const QString& text);
        void                                            handleStoppingPointReached          ();
        void                                            refresh                             ();

    private slots:
        void                                            handleRefreshToolButtonClicked      ();

    private:
        SeerStackFramesBrowserWidget*                   _stackFramesBrowserWidget;
        SeerStackArgumentsBrowserWidget*                _stackArgumentsBrowserWidget;
        SeerStackLocalsBrowserWidget*                   _stackLocalsBrowserWidget;
};

