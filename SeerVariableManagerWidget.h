#pragma once

#include "SeerVariableTrackerBrowserWidget.h"
#include "SeerVariableLoggerBrowserWidget.h"
#include "SeerRegisterValuesBrowserWidget.h"

#include <QtWidgets/QWidget>

#include "ui_SeerVariableManagerWidget.h"

class SeerVariableManagerWidget : public QWidget, protected Ui::SeerVariableManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerVariableManagerWidget (QWidget* parent = 0);
       ~SeerVariableManagerWidget ();

        SeerVariableTrackerBrowserWidget*               variableTrackerBrowserWidget        ();
        SeerVariableLoggerBrowserWidget*                variableLoggerBrowserWidget           ();
        SeerRegisterValuesBrowserWidget*                registerValuesBrowserWidget         ();

    signals:
    public slots:
    private slots:
        void                                            handleRefreshToolButtonClicked      ();

    private:
        SeerVariableTrackerBrowserWidget*               _variableTrackerBrowserWidget;
        SeerVariableLoggerBrowserWidget*                _variableLoggerBrowserWidget;
        SeerRegisterValuesBrowserWidget*                _registerValuesBrowserWidget;
};

